module Parser
  ( parser
  ) where

import           Imports (importsPaths)
import           Modules (portModule)
import           Ports   (parseMod2Ports)
import           Prelude (print)
import           RIO

parser :: FilePath -> RIO SimpleApp ()
parser entryPoint = do
  allImportModules <- liftIO $ importsPaths [] entryPoint
  allPortsModules <- liftIO $ catMaybes <$> mapM portModule (nubOrd allImportModules)
  ports <- liftIO $ mapM (parseMod2Ports entryPoint) allPortsModules
  logInfo $ displayShow ports
