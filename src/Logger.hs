module Logger where

import           RIO

debugList :: Show a => [a] -> RIO SimpleApp ()
debugList = mapM_ (logDebug . displayShow)