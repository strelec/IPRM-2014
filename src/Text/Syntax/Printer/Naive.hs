module Text.Syntax.Printer.Naive where

import Prelude (String, Maybe, const)

import Control.Category ()
import Control.Isomorphism.Partial (IsoFunctor ((<$>)), unapply)
import Control.Monad (Monad, return, fail, (>>=), liftM2, mplus)
import Control.Monad.Reader (ReaderT, runReaderT, lift)

import Data.Eq (Eq ((==)))
import Data.Function (($))
import Data.List ((++))
import Data.Char (Char)

import Text.Syntax.Classes (ProductFunctor ((<*>)), Alternative ((<|>), empty), Syntax (pure, token))


-- printer

newtype Printer alpha = Printer (alpha -> ReaderT Char Maybe String)

print :: Printer alpha -> alpha -> Maybe String
print (Printer p) s = runReaderT (p s) 'u'


instance IsoFunctor Printer where
  iso <$> Printer p
    = Printer (\b -> lift (unapply iso b) >>= p)

instance ProductFunctor Printer where
  Printer p <*> Printer q
    = Printer (\(x, y) -> liftM2 (++) (p x) (q y))

instance Alternative Printer where
  Printer p <|> Printer q
    = Printer (\s -> mplus (p s) (q s))

  empty = Printer $ const $ fail "Empty."

instance Syntax Printer where
  pure x = Printer (\y -> if x == y
                          then return ""
                          else fail "Pure constraint failed.")
  token = Printer (\t -> return [t])
