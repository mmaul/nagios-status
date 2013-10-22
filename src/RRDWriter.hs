{-# LANGUAGE BangPatterns, FlexibleContexts, OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}

-- |
-- File        : NagiosStatus
-- Copyright   : (c) Mike Maul 2013
--
-- License     : BSD3
-- Maintainer  : mike.maul@gmail.com
-- Stability   : experimental
-- Portability : GHC
-- Description : Ultimately will place host-status
--               preformance-data into a RRD database
--               right now will watch status.dat and parse
--               when changed.
import qualified Data.HashMap.Strict as HMap
import System.FSNotify
import Filesystem
import Filesystem.Path.CurrentOS as O
import Data.Text as T
import Data.Maybe (catMaybes, isJust, fromJust)
import qualified Data.Map as Map
import System.Console.CmdArgs.Explicit
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import System.Exit
import qualified Data.Either as E
import NagiosStatus
import Config
import Text.Parsec.Error
import Data.Maybe
import RRD
--import Data.HashMap.Strict as Map
-------------
--- Types ---
-------------
data HostStatusSummary = HostStatusSummary {
  hssHostName::T.Text,
  hssCheckCommand::T.Text,
  hssCurrentState::Int,
  hssPluginOutput::Text,
  hssPerformanceData::Float,
  hssLastCheck::Int
  } deriving (Show)
 
--status_path =  "/home/mmaul/hacking/haskell/yesod/magic-mirror"

fileToBeWatched::O.FilePath->Event->Bool
fileToBeWatched fn e = 
  case e of
    Added f t 
      | f == fn -> True
    Modified f t 
      | f == fn -> True
    otherwise -> False

onlyHostStatus::[Section]->[Section]
onlyHostStatus section =  Prelude.filter (\s -> case s of 
          HostStatus _ -> True
          _ -> False) section

getFileName::Event->String
getFileName e = 
  O.encodeString $ case e of
    Added f t -> f
    Modified f t -> f 
    Removed f t -> f

shsHelper::String->IO Float
shsHelper v = do
  print v
  f <-parsePreformanceData v 
  print f
  return (case f of
    Right s ->  s
    _ ->  0.0)

shsHelper1:: Section -> IO HostStatusSummary 
shsHelper1 sv = do
  let (HostStatus v) = sv  
  xv <- shsHelper (fromJust $ Map.lookup "performance_data" v)      
  return HostStatusSummary {
           hssHostName = (pack $ fromJust $ Map.lookup "host_name" v), 
           hssCheckCommand =   (pack $ fromJust $ Map.lookup "check_command" v),
           hssCurrentState = (read (fromJust $ Map.lookup "current_state" v))::Int,
           hssPluginOutput =  pack $ (fromJust ( Map.lookup "plugin_output" v)),
           hssPerformanceData = xv,
           hssLastCheck = (read (fromJust $ Map.lookup "last_check" v))::Int
         }
  
summarizeHostStatus::Event -> IO [IO HostStatusSummary]
summarizeHostStatus evt = do
 status <- (readStatus $ getFileName evt) -- ::IO (Either Text.Parsec.Error.ParseError [Section]) 
 let Right r = status 
 return  (Prelude.map (shsHelper1)
      (onlyHostStatus r))
        
   

main :: IO ()
main = do
  -- Process command line arguments
  !args <- processArgs argsMode
  when (isJust $ Prelude.lookup flHelp args) $ do
    print $ helpText [] HelpFormatDefault argsMode
    exitSuccess

  let debug      = isJust $ Prelude.lookup flDebug args
      configFile = case (Prelude.lookup flConfig) args of
                     Just x -> x
                     Nothing -> error "Missing config file (-c)"
  -- Load the config file
  wc <- loadConfig configFile debug
  print wc
  
  --print (maybeToList . resolveToRrdConfigs wc)
  let wd = O.fromText $ wcVar wc
      rrdMaps = getit wc "external" -- :: WriterConfig -> Text -> t -> Config.RrdConfigMap
      checkMap = Map.fromList [((pack "-"),1)]--Map.empty -- ::Map.HashMap T.Text Int
  print wd
  man <- startManager
  watchTree man wd 
    (fileToBeWatched 
      (O.fromText $ T.append (T.append  (wcVar wc) "/") "status.dat")) 
    (\e -> do
      hs <- summarizeHostStatus e
      --writeValues hs
--  let r = RrdConfig (T.pack "check-dns-server-alive") 60 [ T.pack "DS:Milliseconds:DERIVE:120:0:U" ] [ T.pack "RRA:AVERAGE:0.5:1:2880",T.pack "RRA:AVERAGE:0.5:15:1344",T.pack "RRA:AVERAGE:0.5:1440:730"::Text ]
-- updateRrd r (T.pack "x.rrd") 1381553944  55
      mapM_ (\x -> do 
        hss <- x 
        let lastCheck = hssLastCheck hss
            v = round((hssPerformanceData hss) * 1000.0::Float)
            hostName = hssHostName hss
            r = fromJust $ HMap.lookup (hssCheckCommand hss) rrdMaps
            lastLastCheck = Map.lookup hostName checkMap
        --if ((not (isJust lastLastCheck)) or (not ((fromJust lastLastCheck) == lastCheck)))
        if (not (isJust lastLastCheck)) || (not $ (fromJust lastLastCheck) == lastCheck)
            then do print r
                    print v
                    updateRrd r (T.append hostName ".rrd") lastCheck v
            else do  
               print ("Last check has not changed for " ++ (unpack hostName))
               print lastCheck
               --print (fromJust lastLastCheck)
                --(print hss) 
                --return hss
        -- when ((isJust lastLastCheck) and ((fromJust lastLastCheck) == lastCheck))  
        --if (isJust lastLastCheck) 
        --   then do
        --        print "X" 
        --    --print ("Last check has not changed for " ++ (unpack hostName))
        --    --return hs
            
        ) hs
      --print xs
      )
  print "press retrun to stop"
  getLine
  print "watching stopped, press retrun to exit"
  stopManager man
  getLine
  return ()

getit :: WriterConfig -> Text -> RrdConfigMap
getit wc probe =
  let probes = wcProbes wc
      pc = fromJust $ HMap.lookup probe probes
  in
  pcRrdConfigs pc -- (HMap.lookup pn (pcRrdConfigs pc))
--resolveToRrdConfigs :: WriterConfig -> Value -> Maybe (Integer, [(RrdConfig, Value)])
--resolveToRrdConfigs wc v = do
--  addr <- getAddress v
--  pc <- HashMap.lookup addr $ wcProbes wc
--  msg <- getMessage v
--  sync <- getMessageSync msg
--  ds <- getMessageData msg
--  return (sync, lookupRrdConfigs pc ds)

--lookupRrdConfigs :: ProbeConfig -> [T.Text] -> [RrdConfig]
--lookupRrdConfigs pc ds = do
--  var <- ds
--  maybe []  Map.lookup var $ pcRrdConfigs pc


--writeValues :: Integer -> [(RrdConfig, Value)] -> IO ()
--writeValues sync ds = mapM_ (uncurry $ writeValue sync) ds

--writeValue :: Integer -> RrdConfig -> Value -> IO ()
--writeValue sync rrd (Number n) = updateRrd rrd sync n
--writeValue _ _ _               = return ()


-- Command line argument processing
argsMode :: Mode [(Name, String)]
argsMode =
  (modeEmpty []) {
    modeNames = [ "RRDWriter" ]
  , modeHelp = "Nagios status.dat RRD writer"
  , modeGroupFlags = toGroup [
      flagNone [flDebug, "D"] (\v -> (flDebug, ""):v) "Enable debug output"
    , flagReq [flConfig, "c"] (updateArg flConfig) "FILE" "Path to config FILE"
    , flagHelpSimple ((flHelp, ""):)
    ]
  }
  where
    updateArg fl x v = Right $ (fl, x):v

flDebug     = "debug"
flConfig    = "config"
flHelp      = "help"
