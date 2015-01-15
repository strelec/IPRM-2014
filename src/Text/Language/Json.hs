{-# LANGUAGE TemplateHaskell #-}
module Text.Language.Json where

import Prelude hiding ((.), map)


import Text.Syntax

import Text.Syntax.Isomorphism (elements, codepoint, hexer, map)
import Control.Isomorphism.Partial (element, subset, ignore)

import Control.Isomorphism.Partial.TH (defineIsomorphisms)

import Data.Char (Char, isControl)

import Data.Map (Map)

import Control.Category ((.))
import Control.Monad.Reader (MonadReader, ask, local, runReader)

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
    indent :: Int,
    oneLevelIndent :: String
} deriving (Show)

defaultConfig = JsonConfig {
    indent = 1,
    oneLevelIndent = "    "
}


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


string :: Syntax delta => delta String
string = between (text "\"") (text "\"") (many char) where

    char = bareChar <|> escapeChar <|> unicodeEscapeChar

    bareChar = subset isBare <$> token where
        isBare '"' = False
        isBare '\\' = False
        isBare c = not $ isControl c

    escapeChar = escape <$> text "\\" *> token

    unicodeEscapeChar = (codepoint . hexer) <$> text "\\u" *> many1 digit


-- Syntax

data Hole = Hole


ind :: IsoM [Char] ()
ind = IsoM f g where
    f _ = return ()
    g () = do
            depth <- ask
            return $ '\n' : replicate depth ' '

json :: Syntax delta => delta JValue
json = value where

    value = (ind <$$> many space) *> rawValue

    rawValue
        =   literal
        <|> jString <$> string
        <|> jNumber <$> number
        <|> jArray  <$> array
        <|> jObject . map <$> object

    literal
        =   jNull                    <$> text "null"
        <|> element (JBoolean False) <$> text "false"
        <|> element (JBoolean True)  <$> text "true"

    number = many1 digit


    sep c = between skipSpace optSpace $ text c

    array = between (text "[") (text "]") (sepBy value $ sep ",")

    object = (+5) <-$> between (text "{") (text "}") (sepBy pair $ sep ",") where
        pair = string <* sep ":" <*> value
