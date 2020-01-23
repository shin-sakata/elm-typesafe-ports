module Parser
  ( parser
  ) where

import           Imports (importsPaths)
import           Modules (portModule)
import           Prelude (print)
import           RIO

parser :: FilePath -> IO ()
parser entryPoint = do
  allImportModules <- importsPaths [] entryPoint
  allPortsModules <- catMaybes <$> mapM portModule (nubOrd allImportModules)
  print allPortsModules
