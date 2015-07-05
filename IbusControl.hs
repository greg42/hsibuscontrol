{-                                                                               
 - ----------------------------------------------------------------------------  
 - "THE BEER-WARE LICENSE" (Revision 42):                                        
 - <code@gregorkopf.de> wrote this file. As long as you retain this notice you   
 - can do whatever you want with this stuff. If we meet some day, and you        
 - think this stuff is worth it, you can buy me a beer in return Gregor Kopf     
 - ----------------------------------------------------------------------------  
 -}

{-# LANGUAGE FlexibleContexts #-}

{-| An IBUS to Kodi bridge with CD player emulation for stereo sound -}
module Main (main) where

import           Network.OpenBM
import           Network.OpenBM.Service
import           Network.OpenBM.Events
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           System.Environment
import           System.Exit
import           Data.Word
import           Data.List
import           Control.Monad
import           Network.HTTP.Base
import           Network.HTTP
import qualified System.Hardware.GPIO.Pin as Raspi
import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Concurrent
import           Control.Concurrent.Timer
import           Control.Concurrent.Suspend.Lifted
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM
import           System.Log.Logger
import           Control.Exception
import           Data.Array

myName :: String
myName = "IbusControl"

debug :: String -> IO ()
debug = debugM myName

warn :: String -> IO ()
warn = warningM myName

-- Helper function
hex :: [Word8] -> ByteString
hex = BS.pack

-- | The configuration data used by this module
data Configuration = Configuration {
      cfgIbusHost :: String -- ^ The host name of the OpenBM server
    , cfgIbusPort :: Int -- ^ The OpenBM server's port number
    , cfgKodiHost :: String -- ^ The host name of the Kodi system
    , cfgKodiPort :: Int -- ^ Kodi's port number
    , cfgVideoPin :: Int -- ^ The reverse camera override pin number
   }

-- | The default configuration
defaultConfig :: Configuration
defaultConfig = Configuration {
      cfgIbusHost = "127.0.0.1"
    , cfgIbusPort = 4287
    , cfgKodiHost = "127.0.0.1"
    , cfgKodiPort = 8888
    , cfgVideoPin = 11 -- This is pin number 7 as per the pin mapping.
   }

-- | An IBUS message for announcing a CD changer
cdAnnounce :: OpenBMMessage
cdAnnounce = message CDPlayer Broadcast [0x02, 0x01]

-- | An IBUS message indicating that a CD changer is connected (in response to
-- the radio's request)
cdActive :: OpenBMMessage
cdActive = message CDPlayer Broadcast [0x02, 0x00]

-- | An IBUS message indicating that a CD is currently playing (disc 6,
-- track 66).
cdPlaying :: OpenBMMessage
cdPlaying = message CDPlayer Radio [0x39, 0x02, 0x09, 0x00, 0x20, 0x00, 0x06, 
                                    0x42]

-- | An IBUS message indicating that the CD is currently not playing (disc 6,
-- track 66).
cdNotPlaying :: OpenBMMessage
cdNotPlaying = message CDPlayer Radio [0x39, 0x00, 0x02, 0x00, 0x20, 0x00, 0x06, 
                                       0x42]

-- | The current state of the CD player (only used internally)
data CdPlayerState = CdPlayerState {
   cdPlayerPlaying    :: Bool
 }
 deriving (Show, Eq)

-- | This function performs the CD player emulation. It responds to the radio's
-- poll requests and makes the system think that a player is connected.
cdPlayerEmulator :: (MonadState CdPlayerState m, Functor m) => IbusEvent -> m OpenBMMessage
cdPlayerEmulator evt = do
   playing  <- cdPlayerPlaying <$> get
   case evt of
      CdPing      -> return cdActive
      CdGetState  -> return (if playing then cdPlaying else cdNotPlaying)
      CdPlay      -> modify (\s -> s {cdPlayerPlaying = True}) >> return cdPlaying
      CdPause     -> modify (\s -> s {cdPlayerPlaying = True}) >> return cdPlaying
      CdStop      -> modify (\s -> s {cdPlayerPlaying = False}) >> return cdNotPlaying
      CdPrevTrack -> modify (\s -> s {cdPlayerPlaying = True}) >> return cdPlaying
      CdNextTrack -> modify (\s -> s {cdPlayerPlaying = True}) >> return cdPlaying

-- | The CD player main thread
cdPlayerThread :: OpenBMHandle -> OpenBMHandle -> StateT CdPlayerState IO (TVar CdPlayerState)
cdPlayerThread rcv send = do
   let sendAnnounce = sendOpenBMHandle send cdAnnounce
   liftIO $ sendAnnounce
   timer <- liftIO $ newTimer
   liftIO $ repeatedStart timer sendAnnounce (sDelay 30) 
   
   curState <- get
   tv <- liftIO $ newTVarIO curState

   liftIO $ forkIO $ (>> return ()) . (flip runStateT) curState $ forever $ do
      evt <- messageToEvent <$> (liftIO $ recvOpenBMHandle rcv)
      when (evt == CdGetState) $
         (liftIO $ repeatedRestart timer >> return ())
      response <- cdPlayerEmulator evt
      liftIO $ sendOpenBMHandle send response
      curState <- get
      liftIO $ atomically $ writeTVar tv curState
   return tv

-- | Sends an RPC request to Kodi
sendKodiRPCRaw :: String -> ReaderT Configuration IO String
sendKodiRPCRaw rq = do
   host <- cfgKodiHost <$> ask
   port <- (show . cfgKodiPort) <$> ask
   let request = "http://" ++ host ++ ":" ++ port ++ "/jsonrpc?request=" ++ (urlEncode rq)
   liftIO $ simpleHTTP (getRequest request) >>= getResponseBody

sendKodiRPC :: String -> String -> ReaderT Configuration IO String
sendKodiRPC method args = 
   let params = if length args > 0 then (", \"params\": " ++ args) else ""
       req = "{\"jsonrpc\": \"2.0\", \"method\": \"" ++ method ++ "\"" ++ params ++ ", \"id\": 0}"
   in sendKodiRPCRaw req

sendKodiRPC_ :: String -> String -> ReaderT Configuration IO ()
sendKodiRPC_ method args = sendKodiRPC method args >> return ()

sendKodiPlayer :: String -> ReaderT Configuration IO ()
sendKodiPlayer = sendKodiRPC_ "Player.GoTo" 

-- | A command that can be sent to Kodi
data KodiCommand = KodiPlayNext
                 | KodiPlayPrev
                 | KodiLeft
                 | KodiRight
                 | KodiUp
                 | KodiDown
                 | KodiSelect
                 | KodiBack
                 | KodiHome
                 deriving (Show, Eq)

-- | This thread is responsible for sending events to Kodi. It reads from a
-- TCHan and translates KodiCommands to actual Kodi RPC events.
kodiManager :: TChan KodiCommand -> ReaderT Configuration IO ()
kodiManager chan = forever $ do
   action <- liftIO $ atomically $ readTChan chan
   case action of
      KodiPlayNext -> sendKodiPlayer "{\"playerid\": 0, \"to\": \"next\"}"
      KodiPlayPrev -> sendKodiPlayer "{\"playerid\": 0, \"to\": \"previous\"}"
      KodiLeft     -> sendKodiRPC_ "Input.Left" ""
      KodiRight    -> sendKodiRPC_ "Input.Right" ""
      KodiUp       -> sendKodiRPC_ "Input.Up" ""
      KodiDown     -> sendKodiRPC_ "Input.Down" ""
      KodiSelect   -> sendKodiRPC_ "Input.Select" ""
      KodiBack     -> sendKodiRPC_ "Input.Back" ""
      KodiHome     -> sendKodiRPC_ "Input.Home" ""

-- | This function interprets a number of Ibus messages and forwards the
-- respective button events to Kodi.
kodiButtonForwarder :: TVar Bool -> OpenBMHandle -> TChan KodiCommand -> IO ()
kodiButtonForwarder tv hdl chan = forever $ do
   event  <- messageToEvent <$> recvOpenBMHandle hdl
   debug $ show event
   active <- readTVarIO tv
   when active $ case event of
      SteeringWheelUpButtonReleased   -> atomically $ writeTChan chan KodiPlayNext
      SteeringWheelDownButtonReleased -> atomically $ writeTChan chan KodiPlayPrev
      LeftButtonReleased              -> atomically $ writeTChan chan KodiLeft
      RightButtonReleased             -> atomically $ writeTChan chan KodiRight
      NavigationKnobTurnedLeft i      -> sequence_ $ take i $ repeat $ (atomically $ writeTChan chan KodiUp)
      NavigationKnobTurnedRight i     -> sequence_ $ take i $ repeat $ (atomically $ writeTChan chan KodiDown)
      NavigationKnobReleased          -> atomically $ writeTChan chan KodiSelect
      ReverseTapeButtonPressed False  -> atomically $ writeTChan chan KodiBack
      ReverseTapeButtonPressed True   -> atomically $ writeTChan chan KodiHome
      _                               -> return ()

-- | Activates the video signal on the reverse camera input.
activateVideo :: Raspi.Pin -> IO ()
activateVideo pin = Raspi.set pin Raspi.Zero

-- | Deactivates the reverse camera video signal.
deactivateVideo :: Raspi.Pin -> IO ()
deactivateVideo pin = Raspi.set pin Raspi.One

-- | The current state of the radio (radio, tape or CD playing)
data RadioState = StateRadio
                | StateCD
                | StateTape
                deriving(Show, Eq)

-- | This thread is responsible for observing the current state of the
-- radio.
radioStateObserver :: TVar RadioState -> OpenBMHandle -> IO ()
radioStateObserver tv rcv = forever $ do
      event <- messageToEvent <$> (liftIO $ recvOpenBMHandle rcv)
      debug $ show event
      let stateChange = case event of
                         WriteTitle _ x | "FM"  `isInfixOf` x -> Just StateRadio
                                        | "FMD" `isInfixOf` x -> Just StateRadio
                                        | "FMD" `isInfixOf` x -> Just StateRadio 
                                        | "LWA" `isInfixOf` x -> Just StateRadio 
                                        | "SW"  `isInfixOf` x -> Just StateRadio 
                                        | "SWA" `isInfixOf` x -> Just StateRadio 
                                        | "MW"  `isInfixOf` x -> Just StateRadio 
                                        | "MWA" `isInfixOf` x -> Just StateRadio 
                                        | "NO TAPE" `isInfixOf` x -> Just StateTape
                                        | "CD" `isInfixOf` x      -> Just StateCD
                                        | otherwise               -> Nothing
                         WriteIndexMK34 _ x | "NO DISC" `isInfixOf` x -> Just StateCD
                                            | otherwise               -> Nothing
                         _ -> Nothing
      case stateChange of
         Nothing -> return ()
         Just s  -> atomically $ writeTVar tv s

-- | Switch AV iutput on or off
data AVCommand = AVActive
               | AVInactive
               deriving (Show, Eq)

-- | The AV manager is responsible for showing the raspberry video output
-- on the screen and for activating the CD player in case audio is playing.
avManager :: TChan AVCommand -> ReaderT Configuration IO ()
avManager chan = do
   -- Initialize raspberry GPIO pin
   liftIO $ debug "Initializing Rasperry IO pin"
   pinNum <- cfgVideoPin <$> ask
   mPin   <- liftIO $ try $ Raspi.init pinNum Raspi.Out
   pin <- case mPin of
            Right p -> return p
            Left  e -> do let e' = e :: SomeException
                          liftIO $ warn "Pin appears to be already in use. Trying again."
                          liftIO $ (try $ unInit pinNum :: IO (Either SomeException ()))
                          liftIO $ Raspi.init pinNum Raspi.Out

   forever $ do
      cmd <- liftIO $ atomically $ readTChan chan
      liftIO $ debug $ show cmd
      case cmd of
        AVActive   -> liftIO $ activateVideo   pin
        AVInactive -> liftIO $ deactivateVideo pin
   where unInit n = Raspi.openWriteClose Raspi.global_UNEXPORT_PATH (show (Raspi.pinMapping ! n))

-- | The main Ibus/OpenBM thread. This is where the program logic is
-- implemented.
openBMMain :: OpenBMServiceContext -> IO ()
openBMMain ctx = do
 
   -- The CD player thread will emulate a CD player on the IBUS
   debug "Starting CD player emulator thread."
   (rcv, send) <- openBMRegisterInterest ctx (isToDevice CDPlayer)
   curCdState  <- evalStateT (cdPlayerThread rcv send) 
                             (CdPlayerState {cdPlayerPlaying = True})

   -- The Kodi manager thread will forward events to Kodi
   debug "Starting Kodi event manager thread."
   kodiEventQueue <- newTChanIO
   forkIO $ runReaderT (kodiManager kodiEventQueue) defaultConfig

   -- The Kodi button forwarder thread will read button events from the IBUS
   -- and send the respective events to the Kodi manager thread
   debug "Starting Kodi button forwarder thread."
   kodiInputActive <- newTVarIO False
   (rcvBut, _)     <- openBMRegisterInterest ctx (\msg -> isFromDevice SteeringWheel msg || isFromDevice BordMonitor msg)
   forkIO $ kodiButtonForwarder kodiInputActive rcvBut kodiEventQueue

   -- The radio state observer will help us figure out what state the radio
   -- currently is in.
   debug "Starting radio state observer thread."
   curRadioState <- newTVarIO StateRadio
   (rcvBm, _)    <- openBMRegisterInterest ctx (isFromDevice Radio)
   forkIO $ (radioStateObserver curRadioState rcvBm)

   -- Take care of the AV output
   debug "Starting AV manager thread."
   avChan <- newTChanIO
   forkIO $ runReaderT (avManager avChan) defaultConfig

   debug "Starting main event loop."
   -- Alright, now take care of the video output
   last <- newTVarIO StateRadio
   forever $ do
      rst <- readTVarIO curRadioState
      old <- readTVarIO last
      when (rst /= old) $ do
         case rst of
            StateCD -> atomically $ do writeTChan avChan AVActive
                                       writeTVar kodiInputActive True
            _       -> atomically $ do writeTChan avChan AVInactive
                                       writeTVar kodiInputActive False
         atomically $ writeTVar last rst
      threadDelay $ 100000 -- 100 milliseconds

   return ()

main = do
   args <- getArgs
   when (length args < 2) $ do
      putStrLn "Usage: IbusControl openbmServer openbmPort"
      exitWith (ExitFailure 1)
   ctx' <- openBMServiceCreate (args !! 0) (read $ args !! 1)
   when (length args > 2 && (args !! 2) == "debug") $
      updateGlobalLogger myName (setLevel DEBUG)
   case ctx' of
      Left e -> do putStrLn $ e
                   exitWith (ExitFailure 1)
      Right ctx -> openBMMain ctx
