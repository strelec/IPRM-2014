module Text.Syntax.Combinators
  (  -- * Lexemes
     text
  ,  letter
  ,  digit
  ,  space
     -- * Repetition
  ,  many
  ,  many1
  ,  sepBy
  ,  chainl1
     -- * Sequencing
  ,  (*>)
  ,  (<*)
  ,  between
     -- * Alternation
  ,  (<+>)
  ,  optional) where

import Prelude (String, Char)

import Data.Char (isLetter, isDigit, isSpace)

import Control.Category ((.))
import Control.Isomorphism.Partial.Constructors (nothing, just, nil, cons, left, right)
import Control.Isomorphism.Partial.Derived (foldl)
import Control.Isomorphism.Partial.Prim (Iso, inverse, element, unit, commute, ignore, subset)

import Text.Syntax.Classes ((<$>))

import Data.Maybe (Maybe)
import Data.Either (Either)

import Text.Syntax.Classes

-- derived combinators
many :: Syntax delta => delta alpha -> delta [alpha]
many p
  =    nil   <$>  pure ()
  <|>  cons  <$>  p
             <*>  many p

many1 :: Syntax delta => delta alpha -> delta [alpha]
many1 p = cons <$> p <*> many p

infixl 4 <+>

(<+>) :: Syntax delta => delta alpha -> delta beta -> delta (Either alpha beta)
p <+> q = (left <$> p) <|> (right <$> q)

-- | `text` parses\/prints a fixed text and consumes\/produces a unit value.
text :: Syntax delta => String -> delta ()
text []      =    pure ()
text (c:cs)  =    inverse (element ((), ()))
             <$>  (inverse (element c) <$> token)
             <*>  text cs

-- | This variant of `<*>` ignores its left result.
-- In contrast to its counterpart derived from the `Applicative` class, the ignored
-- parts have type `delta ()` rather than `delta beta` because otherwise information relevant
-- for pretty-printing would be lost.

(*>) :: Syntax delta => delta () -> delta alpha -> delta alpha
p *> q = inverse unit . commute <$> p <*> q

-- | This variant of `<*>` ignores its right result.
-- In contrast to its counterpart derived from the `Applicative` class, the ignored
-- parts have type `delta ()` rather than `delta beta` because otherwise information relevant
-- for pretty-printing would be lost.

(<*) :: Syntax delta => delta alpha -> delta () -> delta alpha
p <* q = inverse unit <$> p <*> q

-- | The `between` function combines `*>` and `<*` in the obvious way.
between :: Syntax delta => delta () -> delta () -> delta alpha -> delta alpha
between p q r = p *> r <* q

-- | The `chainl1` combinator is used to parse a
-- left-associative chain of infix operators.
chainl1 :: Syntax delta => delta alpha -> delta beta -> Iso (alpha, (beta, alpha)) alpha -> delta alpha
chainl1 arg op f
  = foldl f <$> arg <*> many (op <*> arg)

optional :: Syntax delta => delta alpha -> delta (Maybe alpha)
optional x  = just <$> x <|> nothing <$> text ""

sepBy :: Syntax delta => delta alpha -> delta () -> delta [alpha]
sepBy x sep
  =    nil <$> text ""
  <|>  cons <$> x <*> many (sep *> x)


letter :: Syntax delta => delta Char
letter  =  subset isLetter <$> token

digit :: Syntax delta => delta Char
digit   =  subset isDigit  <$> token

-- any whitespace character ( , \n, \r, \t, ...)
space :: Syntax delta => delta Char
space   =  subset isSpace  <$> token