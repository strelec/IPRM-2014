{-# LANGUAGE TemplateHaskell #-}

module Text.Language.Json where

import Text.Syntax.Isomorphism (elements)

import Control.Isomorphism.Partial.TH (defineIsomorphisms)

import Data.Scientific

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

escapeCharacter = elements [
        ('"', "\""),
        ('\\', "\\"),
        ('/', "/"),
        ('b', "\b"),
        ('f', "\f"),
        ('n', "\n"),
        ('r', "\r"),
        ('t', "\t")
    ]



-- Syntax



