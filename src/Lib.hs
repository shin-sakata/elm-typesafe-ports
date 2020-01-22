module Lib
  ( someFunc
  ) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           RIO

someFunc :: IO ()
someFunc = runSimpleApp $ logInfo "someFunc"
