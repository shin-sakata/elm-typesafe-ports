module Ports
  ( allPorts
  , Port
  ) where

import           Control.Applicative       (empty)
import           Data.Text                 (pack)
import           Filesystem                (isFile)
import           Filesystem.Path.CurrentOS (fromText)
import           Helper                    (ident, mod2Path, search,
                                            searchString)
import           Logger                    (debugList)
import           Prelude                   (print)
import           RIO                       hiding (many, try)
import           RIO.FilePath              ((</>))
import qualified RIO.List                  as L
import           Text.Parsec               hiding ((<|>))
import           Text.Parsec.Text          (Parser, parseFromFile)

data Port =
  Port
    { name :: String
    , args :: [String]
    }

instance Show Port where
  show (Port name args) = "\nport " ++ name ++ " : (" ++ show args ++ ") -> Cmd msg"

allPorts :: FilePath -> [FilePath] -> RIO SimpleApp [Port]
allPorts entryPoint modPaths = do
  ports <- liftIO $ join <$> mapM (parseMod2Ports entryPoint) modPaths
  debugList ports
  return ports

parseMod2Ports :: FilePath -> String -> IO [Port]
parseMod2Ports dir mod = parsePorts (mod2Path dir mod)

parsePorts :: FilePath -> IO [Port]
parsePorts path = do
  ports <- parseFromFile (many portParser) path
  case ports of
    Right ps -> return ps
    Left _   -> return []

portParser :: Parser Port
portParser =
  try
    (do name <- searchPortName
        spaces
        args <- argsParser
        spaces
        string "->"
        return $ Port name args) <|>
  (eof >> empty)

argsParser :: Parser [String]
argsParser = many (try (string "String") <|> string "()")

searchPortName :: Parser String
searchPortName =
  search $ do
    string "port"
    spaces
    name <- ident
    spaces
    char ':'
    spaces
    return name
