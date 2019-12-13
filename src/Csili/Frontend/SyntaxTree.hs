module Csili.Frontend.SyntaxTree
( SyntaxTree(..)
, Symbol
, Var
, Pattern
, ConstructionRule
, Effect
, Term(..)
, blackToken
, zero
, one
) where

import Data.Text (Text)

data SyntaxTree = SyntaxTree
    { interface :: ([Text], [Text])
    , internalPlaces :: [Text]
    , marking :: [(Text, Term)]
    , transitions :: [(Text, ([Pattern], [ConstructionRule], [Effect]))]
    }

type Symbol = Text
type Var = Text
type Pattern = (Text, Term)
type ConstructionRule = (Text, Term)
type Effect = (Text, Term)

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
