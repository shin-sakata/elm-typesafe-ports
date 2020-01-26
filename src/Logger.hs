module Logger where

import           RIO

debugList :: Show a => Utf8Builder -> [a] -> RIO SimpleApp ()
debugList msg = mapM_ (\item -> logDebug $ msg <> ": " <> displayShow item)
