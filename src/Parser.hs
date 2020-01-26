module Parser
  ( parser
  ) where

import           Imports (allImportModules)
import           Modules (filterPortModule)
import           Ports   (allPorts, Port)
import           Prelude (print)
import           RIO

parser :: FilePath -> RIO SimpleApp [Port]
parser entryPoint = do
  allImportModules <- allImportModules entryPoint
  allPortsModules <- filterPortModule allImportModules
  allPorts entryPoint allPortsModules