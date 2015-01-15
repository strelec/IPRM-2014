module Main where

import Prelude hiding (print)

import Text.Language.Json

import Text.Syntax.Parser.Naive
import Text.Syntax.Printer.Naive

import Data.Maybe (fromJust)

-- not a final version, handle failure
handle :: String -> String
handle s = fromJust $ print json (head $ parse json s) 10

main :: IO ()
main = interact handle

