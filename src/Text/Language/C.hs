{-# LANGUAGE TemplateHaskell #-}
module Text.Language.C where

import Prelude hiding ((.), map)


import Text.Syntax
import Text.Syntax.IsoM

import Text.Syntax.Isomorphism (elements, codepoint, hexer, map)
import Control.Isomorphism.Partial (element, subset, ignore)

import Control.Isomorphism.Partial.TH (defineIsomorphisms)

import Data.Char (Char, isControl)

import Data.Map (Map)

import Control.Category ((.))
import Control.Monad.Reader (ask)

import Control.Isomorphism.Partial.Unsafe (Iso (Iso))

defaultConfig = ConfigC {
    indentDepth = -1, --file zaenkrat parsamo kot block in vsak block doda indent, tko da  je -1 da je začetni indent 0
    indentOneLevel = "\t",
	brackets = True,
	ifNewLine = True}


nil :: Iso () ([a])
cons :: Iso (a, [a]) ([a])

nil = Iso (\ () -> Just [])
			(\ xs -> case xs of
						[] -> Just ()
						(x:xs) -> Nothing)

cons = Iso (\ (x, xs) -> Just (x:xs))
			(\ xs -> case xs of
						[] -> Nothing
						(x:xs) -> Just (x,xs))

						
inverse :: Iso alpha beta -> Iso beta alpha
inverse (Iso f g) = Iso g f



$(defineIsomorphisms ''Either)


listCases:: Iso (Either () (a,[a])) [a]
listCases = Iso f g where
	f (Left()) = Just []
	f (Right(x,xs)) = Just (x:xs)
	g [] = Just (Left())
	g (x:xs) = Just (Right(x,xs))

-- Abstract Syntax

data Block
	= SingleBlock ElementC
	| MultiBlock [ElementC]
	deriving (Show, Eq)

data ElementC
	= Element1 Statement
	| Element2 Expression
	deriving (Show, Eq)
	
data Expression
    = Variable String
    | Literal Integer
    | UnaryOp Operator Expression
    | BinOp Expression Operator Expression
	deriving (Show, Eq)

data Statement
	= IfThen Expression Block
	| IfThenElse Expression Block Block
	-- | Return Expression
	deriving (Show, Eq)

data Operator
	= AddOp
	| MulOp
	| DivOp
	| SubOp
	| EqOp
	deriving(Show, Eq)
	
$(defineIsomorphisms ''Block)
$(defineIsomorphisms ''ElementC)
$(defineIsomorphisms ''Expression)
$(defineIsomorphisms ''Statement)
$(defineIsomorphisms ''Operator)


ops :: Syntax f => f Operator
ops = mulOp <$> text "*" 
	<|> addOp <$> text "+" 
	<|> divOp <$> text "/" 
	<|> subOp <$> text "-"
	<|> eqOp <$> text "=="

skipSpace, optSpace, sepSpace::Syntax d => d()
skipSpace = ignore "" <$> many space
optSpace = ignore " " <$> many space
sepSpace = text " " <* skipSpace

newline :: Syntax delta => delta ()
newline = ignore "\n" <$> many space


integer::Syntax d => d Integer
integer = Iso read' show' <$> many digit where
	read' s = case[x|(x,"")<-reads s] of
				[] -> Nothing
				(x:_) -> Just x
	show' x = Just(show x)			
			
			
			
keywords = ["else","for","while","True","False","global","switch","case","return"]

identifier :: Syntax f => f [Char]
identifier = subset (`notElem` keywords) . cons <$> letter <*> many (letter <|> digit)

keyword::Syntax d => String -> d ()
keyword s = inverse right <$> (identifier <+> text s)

parens :: Syntax d => d a -> d a
parens = between (text "(" <* skipSpace) (skipSpace *> text ")")

curly_parens :: Syntax d => d a -> d a
curly_parens = between 
				((indent <$$> many space) *>  text "{" <* newline)
				((indent <$$> many space) *> text "}" <* newline)

spacedOps :: Syntax d => d Operator
spacedOps = between optSpace optSpace ops

priority:: Operator -> Integer
priority MulOp = 1
priority DivOp = 1
priority SubOp = 2
priority AddOp = 2
priority EqOp = 3


indent :: IsoM String ()
indent = IsoM f g where
    f _ = return ()
    g () = do
            ConfigC {indentDepth = depth, indentOneLevel = oneLevel} <- ask
            return $ concat $ replicate depth oneLevel

increaseIndent :: ConfigC -> ConfigC
increaseIndent c = c {indentDepth = 1 + indentDepth c}


expression:: Syntax d => d Expression
expression = exp 3 where
	exp 0 = literal <$> integer
			<|> variable <$> identifier
			<|> parens expression
			

	exp 1 = chainl1 (exp 0) spacedOps (binOpPrio 1)
	exp 2 = chainl1 (exp 1) spacedOps (binOpPrio 2)
	exp 3 = chainl1 (exp 2) spacedOps (binOpPrio 3)

	binOpPrio n = binOp . subset (\ (x,(op,y)) -> priority op == n)
	-- (unaryOp <$> (SubOp <+> AddOp) <*> expression)


statement :: Syntax d => d Statement
statement = 
	ifThenElse <$> keyword "if" *> optSpace *> parens expression 
				<*> block 
				<*> keyword "else" *> block
	<|> ifThen <$> keyword "if" *> optSpace *> parens expression 
				<*> block

elementC :: Syntax d => d ElementC
elementC = 
	element1 <$> (indent <$$> many space) *> statement
	<|> element2 <$> (indent <$$> many space) *> expression <* skipSpace <* text ";" <* newline

block :: Syntax d => d Block
block = 
	singleBlock <$> newline *> (increaseIndent <-$> elementC)
	<|> multiBlock <$> newline *> curly_parens (increaseIndent <-$> many elementC)

