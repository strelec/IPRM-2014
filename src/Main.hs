module Main where

import Prelude hiding (print)

import Text.Language.Json

-- TODO: remove later
import Text.Syntax.Classes

import Text.Syntax.Parser.Naive
import Text.Syntax.Printer.Naive

import Data.Maybe (fromJust)

defaultConfig = JsonConfig {
	indentDepth = 1,
	indentOneLevel = "    ",
	unicodeEscape = True
}

-- not a final version, handle failure
handle :: String -> String
handle s = fromJust $ print json (head $ parse json s) defaultConfig

main :: IO ()
main = interact handle

