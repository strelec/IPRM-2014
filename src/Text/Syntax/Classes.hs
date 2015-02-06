module Text.Syntax.Classes where

import Prelude (Char, Eq)

import Text.Syntax.IsoM (IsoM)
import Control.Isomorphism.Partial.Prim (Iso)


infixl 3 <|>
infix  5 <$>
infix  5 <$$>
infixr 6 <*>

class IsoFunctor f where
  (<$>) :: Iso alpha beta -> f c alpha -> f c beta
  (<$$>) :: IsoM c alpha beta -> f c alpha -> f c beta
  (<-$>) :: (c -> c) -> f c alpha -> f c alpha

class ProductFunctor f where
  (<*>) :: f c alpha -> f c beta -> f c (alpha, beta)

class Alternative f where
  (<|>) :: f c alpha -> f c alpha -> f c alpha
  empty :: f c alpha

class (IsoFunctor delta, ProductFunctor delta, Alternative delta)
   => Syntax delta where
  -- (<$>)   ::  Iso alpha beta -> delta alpha -> delta beta
  -- (<$$>)  ::  IsoM alpha beta -> delta alpha -> delta beta
  -- (<-$>)  ::  (Config -> Config) -> f alpha -> f alpha
  -- (<*>)   ::  delta alpha -> delta beta -> delta (alpha, beta)
  -- (<|>)   ::  delta alpha -> delta alpha -> delta alpha
  -- empty   ::  delta alpha
  pure   ::  Eq alpha => alpha -> delta c alpha
  token  ::  delta c Char
