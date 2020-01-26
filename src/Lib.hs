module Lib
  ( libMain
  ) where

import           Compiler           (compile)
import           Parser             (parser)
import           RIO
import           RIO.List           (headMaybe)
import           System.Environment (getArgs)

libMain :: IO ()
libMain =
  runSimpleApp $ do
    ep <- getEntryPoint
    case ep of
      Just entryPoint -> do
        ports <- parser entryPoint
        compile ports entryPoint
      Nothing -> logError "Error: Enter elm file entry point"

getEntryPoint :: RIO SimpleApp (Maybe FilePath)
getEntryPoint = headMaybe <$> liftIO getArgs
