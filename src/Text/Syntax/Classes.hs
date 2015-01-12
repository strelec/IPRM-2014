module Text.Syntax.Classes where

import Prelude (Maybe, Char)

import Control.Monad (Monad)

import Control.Isomorphism.Partial.Unsafe (Iso (Iso))

import Data.Eq (Eq)


data (Monad m1, Monad m2) => IsoM alpha beta m1 m2
  = IsoM (alpha -> m1 beta) (beta -> m2 alpha)


applyM :: (Monad m1, Monad m2) => IsoM alpha beta m1 m2 -> alpha -> m1 beta
applyM (IsoM f g) = f

unapplyM :: (Monad m1, Monad m2) => IsoM alpha beta m1 m2 -> beta -> m2 alpha
unapplyM (IsoM f g) = g

fromIso :: Iso alpha beta -> IsoM alpha beta Maybe Maybe
fromIso (Iso f g) = IsoM f g


infixl 3 <|>
infix  5 <$>
infix  5 <$$>
infixr 6 <*>

class IsoFunctor f where
  (<$>) :: Iso alpha beta -> f alpha -> f beta
  (<$$>) :: IsoM alpha beta m1 m2 -> f alpha -> f alpha

class ProductFunctor f where
  (<*>) :: f alpha -> f beta -> f (alpha, beta)

class Alternative f where
  (<|>) :: f alpha -> f alpha -> f alpha
  empty :: f alpha

class (IsoFunctor delta, ProductFunctor delta, Alternative delta)
   => Syntax delta where
  -- (<$>)   ::  Iso alpha beta -> delta alpha -> delta beta
  -- (<$$>)  ::  IsoM alpha beta -> delta alpha -> delta beta
  -- (<*>)   ::  delta alpha -> delta beta -> delta (alpha, beta)
  -- (<|>)   ::  delta alpha -> delta alpha -> delta alpha
  -- empty   ::  delta alpha
  pure   ::  Eq alpha => alpha -> delta alpha
  token  ::  delta Char
