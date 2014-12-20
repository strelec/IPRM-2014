module Text.Language.C where


-- Abstract Syntax

data Block = [Element]

data Element = Statement | Expression

data Expression
    = Variable String
    | Literal Integer
    | UnaryOp String Expression
    | BinOp Expression String Expression
	deriving (Show, Eq)

data Statement
	= IfThen Expression Block
	| IfThenElse Expression Block Block
	| Return Expression
	deriving (Show, Eq)


$(defineIsomorphisms ''Block)
$(defineIsomorphisms ''Element)
$(defineIsomorphisms ''Expression)
$(defineIsomorphisms ''Statement)

-- Config

