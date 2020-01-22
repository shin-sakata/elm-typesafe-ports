module Lib
  ( someFunc
  ) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           RIO

someFunc :: IO ()
someFunc = runSimpleApp $ do
  file <- liftIO $ ioFile entryPoint
  logInfo $ display file

entryPoint :: FilePath
entryPoint = "/Users/shintaro.sakata/projects/hc/hc-hataractive/app/ModernJs/seamless/src/Model.elm"

ioFile :: FilePath -> IO T.Text
ioFile = TIO.readFile

