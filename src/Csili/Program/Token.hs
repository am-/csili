module Csili.Program.Token
( Symbol(..)
, Token(..)
, TokenType(..)
, TokenTypeConstructors(..)
, TokenTypeArgument(..)

, blackToken
, false
, true
, zero
, one
, nil
, cons
) where

import System.IO (Handle)
import Data.Map.Strict (Map)
import Data.Text (Text)

newtype Symbol = Symbol Text
    deriving (Show, Eq, Ord)

data Token
    = FunctionToken Symbol [Token]
    | Resource Handle
    deriving (Show, Eq)

newtype TokenType = TokenType Text
    deriving (Show, Eq, Ord)

newtype TokenTypeConstructors = TokenTypeConstructors (Map Symbol [TokenTypeArgument])
    deriving (Show, Eq)

newtype TokenTypeArgument = TokenTypeArgument TokenType
    deriving (Show, Eq, Ord)

blackToken :: Token
blackToken = FunctionToken (Symbol "blackToken") []

zero :: Token
zero = FunctionToken (Symbol "zero") []

one :: Token
one = FunctionToken (Symbol "one") []

false :: Token
false = FunctionToken (Symbol "false") []

true :: Token
true = FunctionToken (Symbol "true") []

nil :: Token
nil = FunctionToken (Symbol "nil") []

cons :: Token -> Token -> Token
cons x xs = FunctionToken (Symbol "cons") [x, xs]
