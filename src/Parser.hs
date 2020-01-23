module Parser
  ( parser
  ) where

import           Control.Applicative       (empty)
import           Data.Text                 (pack)
import           Filesystem                (isFile)
import           Filesystem.Path.CurrentOS (fromText)
import           Prelude                   (print)
import           RIO                       hiding (many, try)
import           RIO.FilePath              as FP
import qualified RIO.List                  as L
import           Text.Parsec               hiding ((<|>))
import           Text.Parsec.Text          (Parser, parseFromFile)

parser :: FilePath -> IO ()
parser entryPoint = do
  allImportModules <- importsPaths [] entryPoint
  allPortsModules <- catMaybes <$> mapM portModule (nubOrd allImportModules)
  print allPortsModules

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

mod2Path :: FilePath -> String -> FilePath
mod2Path entryPoint mod =
  takeDirectory entryPoint </>
  map
    (\c ->
       if c == '.'
         then '/'
         else c)
    mod ++
  ".elm"

imports :: FilePath -> IO [String]
imports filePath = do
  imps <- parseFromFile (many impParser) filePath
  case imps of
    Right imps -> return imps
    Left _     -> return []

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

impParser :: Parser String
impParser = try (searchImport >> spaces >> ident) <|> (eof >> empty)

modParser :: Parser String
modParser =
  try (searchPortModule >> spaces >> ident <* (spaces >> string "exposing" >> spaces >> char '(')) <|> (eof >> empty)

searchImport :: Parser String
searchImport = searchString "import"

searchPortModule :: Parser String
searchPortModule = searchString "port module"

searchString :: String -> Parser String
searchString str = search $ string str

ident :: Parser String
ident = many1 (alphaNum <|> char '.' <|> char '_')

search :: Parser a -> Parser a
search expr =
  try expr <|> do
    anyChar
    search expr
