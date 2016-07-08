{-# LANGUAGE LambdaCase #-}

module Csili.Types
( Term(..)
, isConstant
, isFunction
, isFunctionSymbol
, isVariable
  
, Computation(..)
, Rule
, Symbol(..)
, Var(..)
, Place(..)
, unpackPlace
, Transition(..)
, unpackTransition
, Resource(..)
) where

import Data.Text (Text)

type Rule = (Term, Term)

data Term
    = Function Symbol [Term]
    | Variable Var
    deriving (Show, Eq, Ord)

isFunction :: Term -> Bool
isFunction = \case
    Function _ [] -> False
    Function _ (_:_) -> True
    Variable _ -> False

isConstant :: Term -> Bool
isConstant = \case
    Function _ [] -> True
    Function _ (_:_) -> False
    Variable _ -> False

isFunctionSymbol :: Term -> Bool
isFunctionSymbol = \case
    Function _ _ -> True
    Variable _ -> False

isVariable :: Term -> Bool
isVariable = \case
    Function _ _ -> False
    Variable _ -> True

newtype Symbol = Symbol Text
              deriving (Show, Eq, Ord)

newtype Var = Var Text
            deriving (Show, Eq, Ord)


data Computation
    = EffectFree Term
    | Effectful Resource [Term]
    deriving (Show, Eq, Ord)

newtype Place = Place Text
              deriving (Show, Eq, Ord)

unpackPlace :: Place -> Text
unpackPlace (Place place) = place

newtype Transition = Transition Text
                   deriving (Show, Eq, Ord)

unpackTransition :: Transition -> Text
unpackTransition (Transition transition) = transition

newtype Resource = Resource Text
                 deriving (Show, Eq, Ord)
