{-# LANGUAGE TemplateHaskell #-}
module Text.Language.Json where

import Prelude hiding ((.), map)


import Text.Syntax
import Text.Syntax.IsoM

import Text.Syntax.Isomorphism (elements, codepoint, hexer, map)
import Control.Isomorphism.Partial (element, subset, ignore)

import Control.Isomorphism.Partial.TH (defineIsomorphisms)

import Data.Char (Char, isControl)

import Data.Map (Map)

import Control.Category ((.))
import Control.Monad.Reader (ask)

data Hole = Hole


-- Abstract Syntax

data JValue
    = JString String
    | JNumber String
    | JObject (Map String JValue)
    | JArray [JValue]
    | JBoolean Bool
    | JNull
    deriving (Show, Eq)


$(defineIsomorphisms ''JValue)


-- Configuration

data JsonConfig = JsonConfig {
    indentDepth :: Int,
    indentOneLevel :: String,
    spaceAfterColon :: Bool,
    unicodeEscape :: Bool
} deriving (Show)

defaultConfig = JsonConfig {
    indentDepth = 1,
    indentOneLevel = "    ",
    spaceAfterColon = True,
    unicodeEscape = True
}

-- Config writing / reading isomorphisms

indent :: IsoM JsonConfig String ()
indent = IsoM f g where
    f _ = return ()
    g () = do
            JsonConfig {indentDepth = depth, indentOneLevel = oneLevel} <- ask
            return $ concat $ replicate depth oneLevel

increaseIndent :: JsonConfig -> JsonConfig
increaseIndent c = c {indentDepth = 1 + indentDepth c}


spaceFromConfig :: IsoM JsonConfig String ()
spaceFromConfig = IsoM f g where
    f _ = return ()
    g () = do
            JsonConfig {spaceAfterColon = space} <- ask
            return $ if space then " " else ""


-- JSON string syntax

escape = elements [
        ('"', '"'),
        ('\\', '\\'),
        ('/', '/'),
        ('b', '\b'),
        ('f', '\f'),
        ('n', '\n'),
        ('r', '\r'),
        ('t', '\t')
    ]


string :: Syntax delta => delta JsonConfig String
string = between (text "\"") (text "\"") (many char) where

    char = bareChar <|> escapeChar <|> unicodeEscapeChar

    bareChar = subset isBare <$> token where
        isBare '"' = False
        isBare '\\' = False
        isBare c = not $ isControl c

    escapeChar = escape <$> text "\\" *> token

    unicodeEscapeChar = (codepoint . hexer) <$> text "\\u" *> many1 digit


-- Syntax

newline :: Syntax delta => delta JsonConfig ()
newline = ignore "\n" <$> many space

ignoreSpace :: Syntax delta => delta JsonConfig ()
ignoreSpace  =   ignore "" <$> many space

json :: Syntax delta => delta JsonConfig JValue
json = indented value where

    value
        =   literal
        <|> jString <$> string
        <|> jNumber <$> number
        <|> jArray  <$> array
        <|> jObject . map <$> object

    indented = between (indent <$$> many space) ignoreSpace

    literal
        =   jNull                    <$> text "null"
        <|> element (JBoolean False) <$> text "false"
        <|> element (JBoolean True)  <$> text "true"

    number = many1 digit

    block opening closing separator element =
        between
            (text opening <* newline)
            (newline *> indented (text closing))
            (sepBy (increaseIndent <-$> indented element) $ text separator <* newline)

    array = block "[" "]" "," value

    object = block "{" "}" "," pair where
        colon = between ignoreSpace (spaceFromConfig <$$> many space) $ text ":"
        pair = string <* colon <*> value
