module Compiler where

import           Ports            (Args (..), ElmType (..), Port (..))
import           RIO
import           RIO.List.Partial (head)
import qualified RIO.Text         as T

compile :: [Port] -> FilePath -> RIO SimpleApp ()
compile ports entryPoint = do
  liftIO $ writeFileUtf8 (entryPoint <> ".d.ts") $ template ports
  logInfo $ displayShow $ "Success! Generated to " <> (entryPoint <> ".d.ts")

template :: [Port] -> Text
template ports =
  export $
  buildNameSpace "Elm" $
  buildNameSpace "Main" $
  buildInterface "App" "ports: Ports;" <== buildInterface "Args" ("node: HTMLElement;" <> newLine <> "flags: any;") <==
  buildInterface "Ports" (compilePorts ports) <==
  buildInterface "Subscribe<T>" "subscribe(callback: (value: T) => any): void;" <==
  buildInterface "Send<T>" "send(value: T): void;" <==
  "function init(args: Args): App;"

compilePorts :: [Port] -> Text
compilePorts []           = ""
compilePorts (port:ports) = compilePort port <> newLine <> compilePorts ports

compilePort :: Port -> Text
compilePort port = name port <> ": Subscribe<" <> compileArgs (args port) <> ">"

compileArgs :: Args -> Text
compileArgs (Args args) = typeCompatible (head args)

typeCompatible :: ElmType -> Text
typeCompatible elmType =
  case elmType of
    Bool_   -> "boolean"
    Int_    -> "number"
    Float_  -> "number"
    String_ -> "string"
    Any_    -> "any"

export :: Text -> Text
export content = "export" <-> content <> newLine

buildInterface :: Text -> Text -> Text
buildInterface name prototype = interface <-> name <~ prototype ~> empty

buildNameSpace :: Text -> Text -> Text
buildNameSpace name content = namespace <-> name <~ content ~> empty

-- RBrace
(~>) :: Text -> Text -> Text
(~>) content text = content <> "}" <> text

-- LBrace & Indent content
(<~) :: Text -> Text -> Text
(<~) text content = text <> " {" <> newLine <--> content

(<==) :: Text -> Text -> Text
(<==) l r = l <> newLine <> newLine <> r

-- Indent content
(<-->) :: Text -> Text -> Text
(<-->) text content = text <> offsideTrap content

-- "l_text" <-> "r_text" = "l_text r_text"
(<->) :: Text -> Text -> Text
(<->) l r = l <> whiteSpace <> r

offsideTrap :: Text -> Text
offsideTrap content = concatLines (map (\line -> whiteSpaces indentSize <> line) (T.lines content))

concatLines :: [Text] -> Text
concatLines lines = T.concat $ map (<> newLine) lines

whiteSpaces :: Int -> Text
whiteSpaces n = T.replicate n whiteSpace

indentSize = 4

newLine = "\n"

whiteSpace :: Text
whiteSpace = " "

namespace = "namespace"

interface = "interface"

empty :: Text
empty = mempty
