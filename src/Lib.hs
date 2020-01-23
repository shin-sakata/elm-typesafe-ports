module Lib
  ( libMain
  ) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           RIO
import Parser (parser)

libMain :: IO ()
libMain =
  runSimpleApp $ do
    parser entryPoint

entryPoint :: FilePath
entryPoint = "/Users/shintaro.sakata/projects/hc/hc-hataractive/app/ModernJs/seamless/src/Main.elm"

