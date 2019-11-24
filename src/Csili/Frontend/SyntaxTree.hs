module Csili.Frontend.SyntaxTree
( SyntaxTree(..)
, Symbol
, Var
, Pattern
, Production
, Term (..)
) where

import Data.Text (Text)

data SyntaxTree = SyntaxTree
    { interface :: ([Text], [Text])
    , internalPlaces :: [Text]
    , marking :: [(Text, Term)]
    , transitions :: [(Text, ([Pattern], [Production]))]
    }

type Symbol = Text
type Var = Text
type Pattern = (Text, Term)
type Production = (Text, Term)

data Term
    = Function Symbol [Term]
    | IntTerm Int
    | Variable Var
    | Wildcard
    deriving (Show, Eq, Ord)
