module Text.Syntax.Classes where

import Prelude (Maybe, Char, Int, (.), String, Bool, Show)

import Control.Monad (Monad)
import Control.Monad.Reader (ReaderT, lift)

import Control.Isomorphism.Partial.Unsafe (Iso (Iso))

import Data.Eq (Eq)

-- TODO: make this polimorphic

data JsonConfig = JsonConfig {
    indentDepth :: Int,
    indentOneLevel :: String,
    spaceAfterColon :: Bool,
    unicodeEscape :: Bool
} deriving (Show)

type Config = JsonConfig

type MaybeR = ReaderT Config Maybe

data IsoM alpha beta
  = IsoM (alpha -> Maybe beta) (beta -> MaybeR alpha)


applyM :: IsoM alpha beta -> alpha -> Maybe beta
applyM (IsoM f g) = f

unapplyM :: IsoM alpha beta -> beta -> MaybeR alpha
unapplyM (IsoM f g) = g

fromIso :: Iso alpha beta -> IsoM alpha beta
fromIso (Iso f g) = IsoM f (lift . g)


infixl 3 <|>
infix  5 <$>
infix  5 <$$>
infixr 6 <*>

class IsoFunctor f where
  (<$>) :: Iso alpha beta -> f alpha -> f beta
  (<$$>) :: IsoM alpha beta -> f alpha -> f beta
  (<-$>) :: (Config -> Config) -> f alpha -> f alpha

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
