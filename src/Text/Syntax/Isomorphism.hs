module Text.Syntax.Isomorphism where

import Prelude (Ord)

import Control.Isomorphism.Partial.Unsafe

import Data.Bimap (Bimap, fromList, lookup, lookupR)


elements :: (Ord alpha, Ord beta) => [(alpha, beta)] -> Iso alpha beta
elements pairs = Iso f g where
	m = fromList pairs

	f = (`lookup` m)
	g = (`lookupR` m)