module Text.Syntax.IsoM where

import Prelude (Maybe, Int, (.), String, Bool, Show)

import Control.Monad.Reader (ReaderT, lift)

import Control.Isomorphism.Partial.Unsafe (Iso (Iso))

-- TODO: make this polimorphic

data JsonConfig = JsonConfig {
    indentDepth :: Int,
    indentOneLevel :: String,
    spaceAfterColon :: Bool,
    unicodeEscape :: Bool
} deriving (Show)

type Config = JsonConfig


-- data ConfigC = ConfigC {
		-- brackets::Bool,
		-- ifNewLine::Bool
-- } deriving (Show)

-- type Config = ConfigC

type MaybeR = ReaderT Config Maybe

data IsoM alpha beta
  = IsoM (alpha -> Maybe beta) (beta -> MaybeR alpha)


applyM :: IsoM alpha beta -> alpha -> Maybe beta
applyM (IsoM f g) = f

unapplyM :: IsoM alpha beta -> beta -> MaybeR alpha
unapplyM (IsoM f g) = g

fromIso :: Iso alpha beta -> IsoM alpha beta
fromIso (Iso f g) = IsoM f (lift . g)