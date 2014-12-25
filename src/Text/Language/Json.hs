{-# LANGUAGE TemplateHaskell #-}
module Text.Language.Json where

import Prelude hiding ((.))


import Text.Syntax

import Text.Syntax.Isomorphism (elements, codepoint, hexer)

import Control.Isomorphism.Partial

import Control.Isomorphism.Partial.TH (defineIsomorphisms)

import Data.Scientific

import Data.Char (isControl)

import Control.Category ((.))


-- Abstract Syntax

data JValue
    = String
    | Scientific
    | JObject [(String, JValue)]
    | JArray [JValue]
    | JTrue
    | JFalse
    | JNull


$(defineIsomorphisms ''JValue)

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



