module Parser
  ( parser
  ) where

import           Imports (allImportModules)
import           Modules (filterPortModule)
import           Ports   (allPorts)
import           Prelude (print)
import           Compiler (compile)
import           RIO

parser :: FilePath -> RIO SimpleApp ()
parser entryPoint = do
  allImportModules <- allImportModules entryPoint
  allPortsModules <- filterPortModule allImportModules
  ports <- allPorts entryPoint allPortsModules
  compile ports
  return ()
