module Csili.Frontend.SyntaxTree
( SyntaxTree(..)
, Net(..)
, Instance
, Template
, Place
, Symbol
, Var
, Pattern
, ConstructionRule
, Effect
, TokenTypeName
, TokenType
, Term(..)
, blackToken
, zero
, one
) where

import Data.Text (Text)

data SyntaxTree = SyntaxTree
    { tokenTypes :: [TokenType]
    , mainNet :: Net
    , nets :: [(Template, Net)]
    }

data Net = Net
    { instances :: [(Instance, Template)]
    , interface :: ([Place], [Place])
    , internalPlaces :: [Place]
    , marking :: [(Place, Term)]
    , transitions :: [(Text, ([Pattern], [ConstructionRule], [Effect]))]
    }

type Instance = Text
type Template = Text
type Place = [Text]
type Symbol = Text
type Var = Text
type Pattern = (Place, Term)
type ConstructionRule = (Place, Term)
type Effect = (Place, Term)
type TokenTypeName = Text
type TokenTypeVariable = Text
type TokenType = ((TokenTypeName, [TokenTypeVariable]), [Term])

data Term
    = Function Symbol [Term]
    | Variable Var
    | Wildcard
    deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Built-in Terms
--------------------------------------------------------------------------------

blackToken :: Term
blackToken = Function "blackToken" []

zero :: Term
zero = Function "zero" []

one :: Term
one = Function "one" []
