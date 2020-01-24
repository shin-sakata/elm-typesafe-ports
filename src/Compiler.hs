module Compiler where

import           Ports (Port)
import           RIO
import qualified RIO.Text as T
import Prelude (print)

compile :: [Port] -> RIO SimpleApp ()
compile ports =
  liftIO $ writeFileUtf8 "./sample.d.ts"
    $ export $ buildNameSpace "Elm"
      $ buildNameSpace "Main"
        $ buildInterface "App" ""

export :: Text -> Text
export content =
  "export" <-> content

buildInterface :: Text -> prototype -> Text
buildInterface name prototype =
  interface <-> name <~
      "ports: Ports;"
  ~> empty

buildNameSpace :: Text -> Text -> Text
buildNameSpace name content =
  namespace <-> name <~
    content
  ~> empty


-- RBrace
(~>) :: Text -> Text -> Text
(~>) content text = content <> "}" <> text


-- LBrace & Indent content
(<~) :: Text -> Text -> Text
(<~) text content =
  text <> " {" <> newLine <--> content


-- Indent content
(<-->) :: Text -> Text -> Text
(<-->) text content =
  text <> offsideTrap content


-- "l_text" <-> "r_text" = "l_text r_text"
(<->) :: Text -> Text -> Text
(<->) l r =
  l <> whiteSpace <> r


offsideTrap :: Text -> Text
offsideTrap content =
  concatLines (map (\line -> whiteSpaces indentSize <> line) (T.lines content))

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