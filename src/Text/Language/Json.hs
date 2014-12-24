{-# LANGUAGE TemplateHaskell #-}

module Text.Language.Json where

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

-- Syntax



