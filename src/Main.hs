module Main where

import Prelude hiding (print)

import Text.Language.Json (json, defaultConfig)
-- import Text.Language.C (c, defaultConfig, CConfig)

import Text.Syntax.Parser.Naive (parse)
import Text.Syntax.Printer.Naive (print)

import Data.Maybe (fromMaybe)



handle :: String -> String
handle s =
	case parse json s of
		[x] -> fromMaybe errorMsg printMaybe where
			printMaybe = print json x defaultConfig
			errorMsg   = "Printer error. This should not happen. Open a bug."
		_   -> "You have a syntax error in the file."


main :: IO ()
main = interact handle
