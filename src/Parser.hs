module Parser
  ( parser
  ) where

import           Control.Applicative hiding (many)
import           Data.Char
import qualified Prelude             as P
import           RIO                 hiding (many, try)
import           RIO.FilePath        as FP
import qualified RIO.List            as L
import           Text.Parsec         hiding ((<|>))
import           Text.Parsec.Text    (Parser, parseFromFile)

parser :: FilePath -> IO ()
parser entryPoint = do
  p <- portModule entryPoint
  i <- imports entryPoint
  P.print i
  P.print p

imports :: FilePath -> IO [String]
imports filePath = do
  Right imps <- parseFromFile (many impParser) filePath
  return imps

portModule :: FilePath -> IO (Maybe String)
portModule filePath = do
  Right mods <- parseFromFile (many modParser) filePath
  return $ L.headMaybe mods

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
