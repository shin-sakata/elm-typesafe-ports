module Parser
  ( parser
  ) where

import           Control.Applicative hiding (many)
import           Data.Char
import qualified Prelude             as P
import           RIO                 hiding (try, many)
import           Text.Parsec         hiding ((<|>))
import           Text.Parsec.Text
import Text.Parsec.Combinator
import Text.Parsec.Language

data Stmt
  = Import String
  | Module String
  | Other
  deriving (Show)

parser :: IO ()
parser = do
  P.putStrLn "start"
  parseTest (many importParser) "import Alert"

importParser :: Parser Stmt
importParser = do
  imp <- string "import" >> spaces >> tok
  return $ Import imp

tok :: Parser String
tok = do
  many1 (alphaNum <|> char '_'<|> char '.')


keyword :: String -> Parser String
keyword keyword = spaces *> string keyword <* spaces


--data Expr
--  = Value Double
--  | Plus Expr Expr
--  | Minus Expr Expr
--  | Times Expr Expr
--  | Divide Expr Expr
--  deriving (Show)
--
--digitToDouble :: Char -> Double
--digitToDouble = fromIntegral . digitToInt
--
--symbol xs = do
--  result <- string xs
--  spaces
--  return result
--
--num = do
--  xs <- many $ digitToDouble <$> digit
--  dot <- optionMaybe (char '.')
--  ys <- many $ digitToDouble <$> digit
--  spaces
--  return $ Value $ foldl f 0 xs + foldl g 0 ys
--  where
--    f x y = x * 10 + y
--    g x y = x + y * 0.1
--
--parens = do
--  symbol "("
--  result <- expr
--  symbol ")"
--  return result
--
--term = try parens <|> num
--
--op0 = (Times <$ symbol "*") <|> (Divide <$ symbol "/")
--
--op1 = (Plus <$ symbol "+") <|> (Minus <$ symbol "-")
--
--expr = do
--  spaces
--  term `chainl1` op0 `chainl1` op1
--
--subMain = print $ parse expr "" ("1.2+2.0-3" :: String)
-- Right (Minus (Plus (Value 1.2) (Value 2.0)) (Value 3.0))
