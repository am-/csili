{-# LANGUAGE LambdaCase #-}

module Csili.Types
( Term(..)
, isConstant
, isFunction
, isFunctionSymbol
, isVariable
, isPromise
, isFuture
  
, Rule
, Symbol(..)
, Var(..)
, Place(..)
, Transition(..)
, Resource(..)
) where

import Data.Text (Text)

type Rule = (Term, Term)

data Term
    = Function Symbol [Term]
    | Variable Var
    | Promise Resource [Term]
    | Future Int
    deriving (Show, Eq, Ord)

isFunction :: Term -> Bool
isFunction = \case
    Function _ [] -> False
    Function _ (_:_) -> True
    Variable _ -> False
    Promise _ _ -> False
    Future _ -> False

isConstant :: Term -> Bool
isConstant = \case
    Function _ [] -> True
    Function _ (_:_) -> False
    Variable _ -> False
    Promise _ _ -> False
    Future _ -> False

isFunctionSymbol :: Term -> Bool
isFunctionSymbol = \case
    Function _ _ -> True
    Variable _ -> False
    Promise _ _ -> False
    Future _ -> False

isVariable :: Term -> Bool
isVariable = \case
    Function _ _ -> False
    Variable _ -> True
    Promise _ _ -> False
    Future _ -> False

isPromise :: Term -> Bool
isPromise = \case
    Function _ _ -> False
    Variable _ -> False
    Promise _ _ -> True
    Future _ -> False

isFuture :: Term -> Bool
isFuture = \case
    Function _ _ -> False
    Variable _ -> False
    Promise _ _ -> False
    Future _ -> True

newtype Symbol = Symbol Text
              deriving (Show, Eq, Ord)

newtype Var = Var Text
            deriving (Show, Eq, Ord)



newtype Place = Place Text
              deriving (Show, Eq, Ord)

newtype Transition = Transition Text
                   deriving (Show, Eq, Ord)

newtype Resource = Resource Text
                 deriving (Show, Eq, Ord)
