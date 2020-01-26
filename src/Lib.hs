module Lib
  ( libMain
  ) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           RIO
import Parser (parser)
import Compiler (compile)

libMain :: IO ()
libMain =
  runSimpleApp $ do
    ports <- parser entryPoint
    compile ports entryPoint

entryPoint :: FilePath
entryPoint = "example/project/src/Main.elm"
