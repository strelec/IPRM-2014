module Text.Syntax.Util where

import Prelude


-- Can be removed with GHC 7.10.1
uncons :: [alpha] -> Maybe (alpha, [alpha])
uncons [] = Nothing
uncons (x:xs) = Just (x, xs)