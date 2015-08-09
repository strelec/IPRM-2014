module Text.Syntax.Parser.Naive where

import Prelude (String, ($), map, (.), (++), const)

import Text.Syntax.Classes (IsoFunctor ((<$>), (<$$>), (<-$>)), ProductFunctor ((<*>)), Alternative ((<|>), empty), Syntax (pure, token))
import Text.Syntax.IsoM (applyM, fromIso)

import Control.Monad (Monad, return, fail, (>>=))

import Data.Maybe (Maybe (Just), maybeToList)
import Data.List (uncons)


newtype Parser c alpha
  = Parser (String -> [(alpha, String)])

parse :: Parser c alpha -> String -> [alpha]
parse (Parser p) s = [ x | (x, "") <- p s ]

parseM :: Monad m => Parser c alpha -> String -> m alpha
parseM p s
  =  case parse p s of
       []        ->  fail "parse error"
       [result]  ->  return result
       _         ->  fail "ambiguous input"


instance IsoFunctor Parser where
  iso <$> Parser p
    = fromIso iso <$$> Parser p

  isoM <$$> Parser p
    = Parser (\s ->  [  (y, s')
                     |  (x, s')  <-  p s
                     ,  Just y   <-  [applyM isoM x] ])
  _ <-$> p = p

instance ProductFunctor Parser where
  Parser p <*> Parser q
    = Parser (\s ->  [  ((x, y), s'')
                     |  (x,  s')   <- p  s
                     ,  (y,  s'')  <- q  s' ])

instance Alternative Parser where
  Parser p <|> Parser q
    = Parser (\s -> p s ++ q s)
  empty = Parser $ const []

instance Syntax Parser where
  pure x =  Parser (\s -> [(x, s)])
  token  =  Parser $ maybeToList . uncons
