module Text.Syntax.Classes where

import Prelude (Maybe, Char, Int)

import Control.Monad (Monad)
import Control.Monad.Reader (ReaderT)

import Control.Isomorphism.Partial.Unsafe (Iso (Iso))

import Data.Eq (Eq)

type MaybeR = ReaderT Int Maybe

data IsoM alpha beta
  = IsoM (alpha -> Maybe beta) (beta -> MaybeR alpha)


applyM :: IsoM alpha beta -> alpha -> Maybe beta
applyM (IsoM f g) = f

unapplyM :: IsoM alpha beta -> beta -> MaybeR alpha
unapplyM (IsoM f g) = g

--fromIso :: Iso alpha beta -> IsoM alpha beta
--fromIso (Iso f g) = IsoM f g


infixl 3 <|>
infix  5 <$>
infix  5 <$$>
infixr 6 <*>

class IsoFunctor f where
  (<$>) :: Iso alpha beta -> f alpha -> f beta
  (<$$>) :: IsoM alpha beta -> f alpha -> f beta

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
