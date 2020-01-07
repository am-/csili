module Csili.Program.Token
( Symbol(..)
, Token(..)
, TokenTypeName(..)
, TokenTypeDefinition(..)
, TokenTypeConstructors(..)
, TokenType(..)
, TokenTypeVariable(..)

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

data TokenTypeDefinition = TokenTypeDefinition TokenTypeName [TokenTypeVariable]
    deriving (Show, Eq, Ord)

data TokenTypeName = TokenTypeName Text
    deriving (Show, Eq, Ord)

newtype TokenTypeVariable = TokenTypeVariable Text
    deriving (Show, Eq, Ord)

newtype TokenTypeConstructors = TokenTypeConstructors (Map Symbol [TokenType])
    deriving (Show, Eq)

data TokenType
    = TokenType TokenTypeName [TokenType]
    | TokenTypeSubstitution TokenTypeVariable
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
