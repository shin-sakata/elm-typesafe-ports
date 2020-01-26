module Ports
  ( allPorts
  , Port(..)
  , Args(..)
  , ElmType(..)
  ) where

import           Control.Applicative       (empty)
import           Data.Text                 (pack, unpack)
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
    { name :: Text
    , args :: Args
    }

newtype Args =
  Args [ElmType]
  deriving (Show)

data ElmType
  = String_
  | Bool_
  | Int_
  | Float_
  | Void_
  | Any_
  deriving Show

instance Show Port where
  show (Port name args) = show $ "port " <> name <> pack " : (" <> pack (show args) <> ") -> Cmd msg"

allPorts :: FilePath -> [FilePath] -> RIO SimpleApp [Port]
allPorts entryPoint modPaths = do
  ports <- liftIO $ join <$> mapM (parseMod2Ports entryPoint) modPaths
  debugList "export port" ports
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
        return $ Port (pack name) (Args $ genElmType <$> args)) <|>
  (eof >> empty)

genElmType :: String -> ElmType
genElmType "String" = String_
genElmType "Bool"   = Bool_
genElmType "Int"    = Int_
genElmType "Float"  = Float_
genElmType "()"     = Void_
genElmType _        = Any_

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
