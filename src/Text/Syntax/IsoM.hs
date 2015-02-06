module Text.Syntax.IsoM where

import Prelude (Maybe, Int, (.), String, Bool, Show)

import Control.Monad.Reader (ReaderT, lift)

import Control.Isomorphism.Partial.Unsafe (Iso (Iso))


type MaybeR c = ReaderT c Maybe

data IsoM c alpha beta
  = IsoM (alpha -> Maybe beta) (beta -> MaybeR c alpha)


applyM :: IsoM c alpha beta -> alpha -> Maybe beta
applyM (IsoM f g) = f

unapplyM :: IsoM c alpha beta -> beta -> MaybeR c alpha
unapplyM (IsoM f g) = g

fromIso :: Iso alpha beta -> IsoM c alpha beta
fromIso (Iso f g) = IsoM f (lift . g)