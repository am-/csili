{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Csili.Types
( Rule
, Term(..)
, isConstant
, isFunction
, isFunctionSymbol
, isVariable

, Matcher(..)
, Computation(..)
, Effect(..)
, Symbol(..)
, Var(..)
, Place(..)
, unpackPlace
, Transition(..)
, unpackTransition

, Collectible(..)
) where

import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set

type Rule = (Term, Term)

data Term
    = Function Symbol [Term]
    | Variable Var
    | IntTerm Int
    deriving (Show, Eq, Ord)

isFunction :: Term -> Bool
isFunction = \case
    Function _ [] -> False
    Function _ (_:_) -> True
    Variable _ -> False
    IntTerm _ -> False

isConstant :: Term -> Bool
isConstant = \case
    Function _ [] -> True
    Function _ (_:_) -> False
    Variable _ -> False
    IntTerm _ -> True

isFunctionSymbol :: Term -> Bool
isFunctionSymbol = \case
    Function _ _ -> True
    Variable _ -> False
    IntTerm _ -> False

isVariable :: Term -> Bool
isVariable = \case
    Function _ _ -> False
    Variable _ -> True
    IntTerm _ -> False

newtype Symbol = Symbol Text
              deriving (Show, Eq, Ord)

newtype Var = Var Text
            deriving (Show, Eq, Ord)

data Matcher
    = Pattern Term
    | PromisePending Var
    | PromiseBroken Term
    | PromiseKept Term
    deriving (Show, Eq, Ord)

data Computation
    = EffectFree Term
    | Effectful Effect [Term]
    deriving (Show, Eq, Ord)

newtype Effect = Effect Text
                 deriving (Show, Eq, Ord)

newtype Place = Place Text
              deriving (Show, Eq, Ord)

unpackPlace :: Place -> Text
unpackPlace (Place place) = place

newtype Transition = Transition Text
                   deriving (Show, Eq, Ord)

unpackTransition :: Transition -> Text
unpackTransition (Transition transition) = transition

--------------------------------------------------------------------------------
-- Functions for building collections
--------------------------------------------------------------------------------

class Ord b => Collectible a b where
    collect :: a -> Set b

instance Collectible Term Symbol where
    collect = \case
        Function symbol args -> Set.insert symbol (Set.unions (map collect args))
        Variable _ -> Set.empty
        IntTerm _ -> Set.empty

instance Collectible Computation Symbol where
    collect = \case
        EffectFree term -> collect term
        Effectful _ terms -> Set.unions (map collect terms)

instance Collectible Matcher Symbol where
    collect = \case
        Pattern term -> collect term
        PromisePending _ -> Set.empty
        PromiseBroken term -> collect term
        PromiseKept term -> collect term

instance Collectible Term Var where
    collect = \case
        Function _ terms -> Set.unions (map collect terms)
        Variable var -> Set.singleton var
        IntTerm _ -> Set.empty

instance Collectible Computation Var where
    collect = \case
        EffectFree term -> collect term
        Effectful _ terms -> Set.unions (map collect terms)

instance Collectible Matcher Var where
    collect = \case
        Pattern term -> collect term
        PromisePending var -> Set.singleton var
        PromiseBroken term -> collect term
        PromiseKept term -> collect term
