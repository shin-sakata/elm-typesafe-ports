module Modules
  ( filterPortModule
  ) where

import           Control.Applicative       (empty)
import           Filesystem                (isFile)
import           Filesystem.Path.CurrentOS (fromText)
import           Helper                    (ident, searchString)
import           Logger                    (debugList)
import           RIO                       hiding (many, try)
import qualified RIO.List                  as L
import           RIO.Text                  (pack)
import           Text.Parsec               hiding ((<|>))
import           Text.Parsec.Text          (Parser, parseFromFile)

filterPortModule :: [FilePath] -> RIO SimpleApp [FilePath]
filterPortModule paths = do
  ports <- liftIO $ catMaybes <$> mapM portModule paths
  debugList "port module" ports
  return ports

portModule :: FilePath -> IO (Maybe String)
portModule filePath = do
  isFile_ <- isFile $ fromText $ pack filePath
  if isFile_
    then do
      mods <- parseFromFile (many modParser) filePath
      case mods of
        Right mods -> return $ L.headMaybe mods
        Left _     -> return Nothing
    else return Nothing

modParser :: Parser String
modParser =
  try (searchPortModule >> spaces >> ident <* (spaces >> string "exposing" >> spaces >> char '(')) <|> (eof >> empty)

searchPortModule :: Parser String
searchPortModule = searchString "port module"
