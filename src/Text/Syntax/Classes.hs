module Text.Syntax.Classes where

import Prelude ()

import Control.Isomorphism.Partial (IsoFunctor)
import Data.Eq (Eq)
import Data.Char (Char)

infixl 3 <|> 
infixr 6 <*>

class ProductFunctor f where
  (<*>) :: f alpha -> f beta -> f (alpha, beta)

class Alternative f where
  (<|>) :: f alpha -> f alpha -> f alpha
  empty :: f alpha

class (IsoFunctor delta, ProductFunctor delta, Alternative delta) 
   => Syntax delta where
  -- (<$>)   ::  Iso alpha beta -> delta alpha -> delta beta
  -- (<*>)   ::  delta alpha -> delta beta -> delta (alpha, beta)
  -- (<|>)   ::  delta alpha -> delta alpha -> delta alpha
  -- empty   ::  delta alpha
  pure   ::  Eq alpha => alpha -> delta alpha
  token  ::  delta Char
