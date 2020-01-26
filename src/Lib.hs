module Lib
  ( libMain
  ) where

import           Compiler     (compile)
import           Parser       (parser)
import           RIO

libMain :: IO ()
libMain =
  runSimpleApp $ do
    ports <- parser entryPoint
    compile ports entryPoint

entryPoint :: FilePath
entryPoint = "example/project/src/Main.elm"
