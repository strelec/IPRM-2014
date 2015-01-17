module Text.Syntax.Parser.Naive where

import Prelude (String, ($), map, (.), (++), const)

import Text.Syntax.Util (uncons)

import Control.Isomorphism.Partial (apply)
import Text.Syntax.Classes (IsoFunctor, (<$>), (<$$>), (<-$>), applyM, fromIso)
import Control.Monad (Monad, return, fail, (>>=))

import Data.Maybe (Maybe (Just), maybeToList)

import Text.Syntax.Classes (ProductFunctor, Alternative, Syntax, (<*>), (<|>), empty, pure, token)

-- parser

newtype Parser alpha
  = Parser (String -> [(alpha, String)])

parse :: Parser alpha -> String -> [alpha]
parse (Parser p) s = [ x | (x, "") <- p s ]

parseM :: Monad m => Parser alpha -> String -> m alpha
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