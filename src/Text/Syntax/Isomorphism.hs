module Text.Syntax.Isomorphism where

import Prelude (Ord, String, Int, (.), ($), fst, fmap)

import Control.Isomorphism.Partial.Unsafe

import Data.Bimap (Bimap, fromList, lookup, lookupR)

import Data.Char (Char, ord, chr, intToDigit)

import Data.Maybe (Maybe(Just), listToMaybe)

import Numeric

-- bijection between two sets, uses Data.Bimap
elements :: (Ord alpha, Ord beta) => [(alpha, beta)] -> Iso alpha beta
elements pairs = Iso f g where
	m = fromList pairs

	f = (`lookup` m)
	g = (`lookupR` m)


-- isomorphism to convert Char <-> unicode codepoint
codepoint :: Iso Int Char
codepoint = Iso (Just . chr) (Just . ord)

-- hex number to decimal and back
hexer :: Iso String Int
hexer = Iso f g where
	f x = fst `fmap` listToMaybe (readHex x)
	g = Just . (`showHex` "")
