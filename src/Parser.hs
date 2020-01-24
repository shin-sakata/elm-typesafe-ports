module Parser
  ( parser
  ) where

import           Imports (allImportModules)
import           Modules (filterPortModule)
import           Ports   (allPorts)
import           Prelude (print)
import           RIO

parser :: FilePath -> RIO SimpleApp ()
parser entryPoint = do
  allImportModules <- allImportModules entryPoint
  allPortsModules <- filterPortModule allImportModules
  ports <- allPorts entryPoint allPortsModules
  return ()
