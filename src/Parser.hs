module Parser
  ( parser
  ) where

import           Imports (allImportModules)
import           Modules (filterPortModule)
import           Ports   (Port, allPorts)
import           RIO

parser :: FilePath -> RIO SimpleApp [Port]
parser entryPoint = do
  allImportModules <- allImportModules entryPoint
  allPortModules <- filterPortModule allImportModules
  allPorts entryPoint allPortModules
