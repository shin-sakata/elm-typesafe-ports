module Imports
  ( allImportModules
  ) where

import           Control.Applicative       (empty)
import           RIO.Text                  (pack)
import           Filesystem                (isFile)
import           Filesystem.Path.CurrentOS (fromText)
import           Helper                    (ident, mod2Path, searchString)
import           RIO                       hiding (many, try)
import           Text.Parsec               hiding ((<|>))
import           Text.Parsec.Text          (Parser, parseFromFile)
import           Logger                    (debugList)

allImportModules :: FilePath -> RIO SimpleApp [FilePath]
allImportModules entryPoint = do
  mods <- liftIO $ nubOrd <$> importsPaths [] entryPoint
  debugList "import modules" $ entryPoint : mods
  return $ entryPoint : mods

importsPaths :: [FilePath] -> FilePath -> IO [FilePath]
importsPaths state path = do
  isFile_ <- isFile $ fromText $ pack path
  if isFile_
    then do
      imps <- imports path
      let paths = map (mod2Path path) imps
       in do impss <- mapM (importsPaths state) paths
             return $ state ++ paths ++ concat impss
    else return state

impParser :: Parser String
impParser = try (searchImport >> spaces >> ident) <|> (eof >> empty)

imports :: FilePath -> IO [String]
imports filePath = do
  imps <- parseFromFile (many impParser) filePath
  case imps of
    Right imps -> return imps
    Left _     -> return []

searchImport :: Parser String
searchImport = searchString "import"
