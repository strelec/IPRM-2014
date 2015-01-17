module Text.Syntax.Printer.Naive where

import Prelude (String, Maybe, Int, Char, const, (.), ($), (==), (++))

import Text.Syntax.Classes (IsoFunctor ((<$>), (<$$>), (<-$>)), ProductFunctor ((<*>)), Alternative ((<|>), empty), Syntax (pure, token))
import Text.Syntax.IsoM (MaybeR, Config, unapplyM, fromIso)

import Control.Monad (Monad, return, fail, (>=>), liftM2, mplus)
import Control.Monad.Reader (runReaderT, local)



newtype Printer alpha = Printer (alpha -> MaybeR String)

print :: Printer alpha -> alpha -> Config -> Maybe String
print (Printer p) s config = runReaderT (p s) config


instance IsoFunctor Printer where
  iso <$> Printer p
    = fromIso iso <$$> Printer p

  isoM <$$> Printer p
    = Printer $ unapplyM isoM >=> p

  transform <-$> Printer p
    = Printer $ local transform . p

instance ProductFunctor Printer where
  Printer p <*> Printer q
    = Printer (\(x, y) -> liftM2 (++) (p x) (q y))

instance Alternative Printer where
  Printer p <|> Printer q
    = Printer (\s -> p s `mplus` q s)

  empty = Printer $ const $ fail "Empty."

instance Syntax Printer where
  pure x = Printer (\y -> if x == y
                          then return ""
                          else fail "Pure constraint failed.")
  token = Printer (\t -> return [t])
