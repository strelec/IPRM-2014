module Text.Syntax.Isomorphism where

import Prelude (Ord, String, Int, (.), ($), fst, fmap)

import Control.Isomorphism.Partial.Unsafe (Iso (Iso))

import Data.Bimap (Bimap, lookup, lookupR)
import qualified Data.Bimap as Bimap

import Data.Map (Map, fromList, toList)

import Data.Char (Char, ord, chr, intToDigit)

import Data.Maybe (Maybe(Just), listToMaybe)

import Numeric (readHex, showHex)

-- bijection between two sets, uses Data.Bimap
elements :: (Ord alpha, Ord beta) => [(alpha, beta)] -> Iso alpha beta
elements pairs = Iso f g where
	m = Bimap.fromList pairs

	f = (`lookup` m)
	g = (`lookupR` m)


-- isomorphism to convert Char <-> unicode codepoint
codepoint :: Iso Int Char
codepoint = Iso (Just . chr) (Just . ord)

-- hex number to decimal and back
hexer :: Iso String Int
hexer = Iso f g where
	f = fmap fst . listToMaybe . readHex
	g = Just . (`showHex` "")


map :: Ord alpha => Iso [(alpha, beta)] (Map alpha beta)
map = Iso (Just . fromList) (Just . toList)
