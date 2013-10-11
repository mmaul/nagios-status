import NagiosStatus
import System.Environment
import Data.Map as Map
import Data.Maybe
----------------------
-------- MAIN --------
----------------------

onlyHostStatus::[Section]->[Section]
onlyHostStatus section =  Prelude.filter (\s -> case s of 
          HostStatus _ -> True
          _ -> False) section

summarizeHostStatus::String -> IO [[String]]
summarizeHostStatus fn = do
   status <- (readStatus fn)  
   return $ Prelude.map (\(HostStatus v) -> 
       (catMaybes [(Map.lookup "host_name" v),
        (Map.lookup "check_command" v),
        (Map.lookup "plugin_output" v),
        (Map.lookup "current_state" v)
       ])  )
    (case status of
        Right st -> (onlyHostStatus st)
        _ -> [])
   
main = do
  args  <- getArgs
  stats <- summarizeHostStatus (args!!0)
  print stats
