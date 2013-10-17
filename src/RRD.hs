{-# LANGUAGE OverloadedStrings #-}

module RRD (
  updateRrd
) where

import Bindings.Librrd
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Format as TF
import qualified Data.Text.Lazy as TL
import Foreign.C
import Foreign.Marshal.Array
import qualified System.IO as IO
import System.Posix.Files

import Config

updateRrd :: (Real a, Real b) => RrdConfig -> T.Text -> a -> b -> IO ()
updateRrd rrd rrdFilename timestamp x = do
    let rrdPath = T.unpack rrdFilename
    exists <- fileExist rrdPath
    when (not exists) $ librrdCall "rrd_create" c'rrd_create createArgs
    librrdCall "rrd_updatev" c'rrd_update updateArgs
    print$ updateArgs!!2
  where
    createArgs = [ "create"
                 , rrdFilename
                 , "--step"
                 , TL.toStrict $ TF.format "{}" $ TF.Only (rrdStep rrd) ]
                 ++ rrdDataSources rrd
                 ++ rrdArchives rrd
    updateArgs = [ "update"
                 , rrdFilename
                 , TL.toStrict $ TF.format "{}:{}" [TF.fixed 0 timestamp, TF.shortest x] ]

librrdCall name f args = do
    argv <- mapM (newCString . T.unpack) args >>= newArray
    let argc = (CInt . fromIntegral . length) args
    f argc argv >>= checkResult
  where
    checkResult res
      | res /= CInt 0 = do err <- c'rrd_get_error
                           errStr <- peekCString err
                           IO.hPutStrLn IO.stderr $ name ++ " failed: " ++ errStr
                           -- Workaround a librrd issue where the rrd_get_error
                           -- buffer gets filled with garbage after awhile...
                           -- this is effectively memset(err, 0, strlen(err))
                           errLen <- lengthArray0 0 err
                           pokeArray0 0 err $ replicate errLen 0
      | otherwise     = return ()


