{-# LANGUAGE TemplateHaskell #-}
module Text.Language.C where

import Prelude hiding ((.), map)


import Text.Syntax
import Text.Syntax.IsoM

import Text.Syntax.Isomorphism (elements, codepoint, hexer, map)
import Control.Isomorphism.Partial (element, subset, ignore)
import Control.Isomorphism.Partial.Prim (inverse)
import Control.Isomorphism.Partial.Constructors (nil, cons, listCases, right)

import Control.Isomorphism.Partial.TH (defineIsomorphisms)

import Data.Char (Char, isControl)

import Data.Map (Map)

import Control.Category ((.))
import Control.Monad.Reader (ask)

import Control.Isomorphism.Partial.Unsafe (Iso (Iso))

defaultConfig = ConfigC {
    indentDepth = 0,
    indentOneLevel = "\t",
	brackets = True,
	ifNewLine = True}



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
	| Real Double
	| Cstring String
    -- | UnaryOp Operator Expression
    | BinOp Expression Operator Expression
	| Fprint String
	deriving (Show, Eq)

data Statement
	= IfThen Expression Block
	| IfThenElse Expression Block Block
	| WhileStat Expression Block
	| Function String String [String] Block
	-- | Return Expression
	deriving (Show, Eq)

data ImportC
	= Import1 String

data FileElement
	= FileImport ImportC
	| FileElementC ElementC

data File
	= File1 [FileElement]

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
$(defineIsomorphisms ''File)
$(defineIsomorphisms ''ImportC)
$(defineIsomorphisms ''FileElement)


unOps :: Syntax f => f Operator
unOps = addOp <$> text "+" 
	<|> subOp <$> text "-"
	
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
integer = Iso read' show' <$> (many digit <|> (cons <$> (thisChar '-') <*> many digit)) where
	read' s = case[x|(x,"")<-reads s] of
				[] -> Nothing
				(x:_) -> Just x
	show' x = Just(show x)			

double::Syntax d => d Double
double = Iso read' show' <$> ((many3 digit (thisChar '.')) <|> (cons <$> (thisChar '-') <*> many3 digit (thisChar '.'))) where
	read' s = case[x|(x,"") <- (reads s::[(Double,String)])] of
				(x:_) -> Just x
				[] -> Nothing
	show' x = Just(show x)			
		
			
			
keywords = ["else","for","while","True","False","global","switch","case","return", "#include", "printf"]

identifier :: Syntax f => f [Char]
identifier = subset (`notElem` keywords) . cons <$> letter <*> many (letter <|> digit)

fileName :: Syntax f => f [Char]
fileName = subset (`notElem` keywords) . cons <$> letter <*> many3 (letter <|> digit) ((subset (\x -> x == '.')) <$> token)

keyword::Syntax d => String -> d ()
keyword s = inverse right <$> (identifier <+> text s)

parens :: Syntax d => d a -> d a
parens = between (text "(" <* skipSpace) (skipSpace *> text ")")

curly_parens :: Syntax d => d a -> d a
curly_parens = between 
				((indent <$$> many space) *> text "{" <* newline)
				((indent <$$> many space) *> text "}")

-- optParens :: Syntax d => d a -> d a
-- optParens = between (newline <|> (newline *> text "{" <* skipSpace))
					-- ((skipSpace *> text "}" <* newline) <|> newline)


spacedOps :: Syntax d => d Operator
spacedOps = between optSpace optSpace ops


escape = elements [
        ('"', '"'),
        ('\\', '\\'),
        ('/', '/'),
        ('b', '\b'),
        ('f', '\f'),
        ('n', '\n'),
        ('r', '\r'),
        ('t', '\t')
    ]


string :: Syntax delta => delta String
string = between (text "\"") (text "\"") (many char) where

    char = bareChar <|> escapeChar <|> unicodeEscapeChar

    bareChar = subset isBare <$> token where
        isBare '"' = False
        isBare '\\' = False
        isBare c = not $ isControl c

    escapeChar = escape <$> text "\\" *> token

    unicodeEscapeChar = (codepoint . hexer) <$> text "\\u" *> many1 digit

	

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
			<|> cstring <$> string
			<|> real <$> double
			<|> variable <$> identifier
			<|> fprint <$> keyword "printf" *> parens string
			<|> parens expression
			
			

	exp 1 = chainl1 (exp 0) spacedOps (binOpPrio 1)
	exp 2 = chainl1 (exp 1) spacedOps (binOpPrio 2)
	exp 3 = chainl1 (exp 2) spacedOps (binOpPrio 3)

	binOpPrio n = binOp . subset (\ (x,(op,y)) -> priority op == n)


statement :: Syntax d => d Statement
statement = 
	ifThenElse <$> keyword "if" *> optSpace *> parens expression 
				<*> block 
				<*> newline*> (indent <$$> many space) *> keyword "else" *> block
	<|> ifThen <$> keyword "if" *> optSpace *> parens expression 
				<*> block
	<|> whileStat <$> keyword "while" *> optSpace *> parens expression
					<*> block
	-- tip funkcije je zaenkrat obravnavan kot spremenljivka, ceprav bi morale bit 
	-- dolocene moznosti
	<|> function <$> identifier <*> sepSpace *> identifier <*> optSpace *> parens (many identifier) <*> block
	-- <|> caseStat <$> keyword "case" *> optSpace *> parens expression
					-- <*> block

elementC :: Syntax d => d ElementC
elementC = 
	element1 <$> (indent <$$> many space) *> statement <* newline
	<|> element2 <$> (indent <$$> many space) *> expression <* skipSpace <* text ";" <* newline

br1 :: IsoM String ()
br1 = IsoM f g where
    f _ = return ()
    g () = do
            ConfigC {brackets = br, indentDepth = depth, indentOneLevel = oneLevel} <- ask
            return $ (if br then concat $ replicate depth oneLevel ++["{\n"] else "\n")

br2 :: IsoM String ()
br2 = IsoM f g where
    f _ = return ()
    g () = do
            ConfigC {brackets = br, indentDepth = depth, indentOneLevel = oneLevel} <- ask
            return $ (if br then concat $ replicate depth oneLevel ++["}"] else "")


br3 :: Syntax d => IsoM (d ElementC) (d ElementC)
-- br3 :: IsoM ElementC ElementC
br3 = IsoM f g where
    f x = return x
    g x = do
            ConfigC {brackets = br} <- ask
            return (if br then text "{" *> x <* text "}" else x)


block :: Syntax d => d Block
block = 
	multiBlock <$> newline *> curly_parens (increaseIndent <-$> many2 elementC)
	
	-- <|> singleBlock <$> (br3 <$$> sb)
	-- <|> singleBlock <$> (curly_parens (sb) <|> newline *> sb)
	<|> singleBlock <$> (br1 <$$> many3 space (thisChar '{')) *> sb <* (br2 <$$> many3 space (thisChar '}'))
	
	where
	
	-- sb = return $ (increaseIndent <-$> elementC)
	sb = (increaseIndent <-$> elementC)
	

importC :: Syntax d => d ImportC
importC = 
	import1 <$> keyword "#include" *> sepSpace *> between (text "<") (text ">") fileName <* newline

fileElement	 :: Syntax d => d FileElement
fileElement = 
	fileImport <$> importC
	<|> fileElementC <$> elementC
	

file :: Syntax d => d File
file = file1 <$> many4 (fileImport <$> importC) (fileElementC <$> elementC)
