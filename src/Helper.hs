module Helper
  ( searchString
  , ident
  , search
  , mod2Path
  ) where

import           RIO              hiding (many, try)
import           RIO.FilePath     (takeDirectory, (</>))
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
