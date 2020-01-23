module Helper
  ( searchString
  , ident
  ) where

import           RIO              hiding (many, try)
import           Text.Parsec      hiding ((<|>))
import           Text.Parsec.Text (Parser)

searchString :: String -> Parser String
searchString str = search $ string str

ident :: Parser String
ident = many1 (alphaNum <|> char '.' <|> char '_')

search :: Parser a -> Parser a
search expr =
  try expr <|> do
    anyChar
    search expr
