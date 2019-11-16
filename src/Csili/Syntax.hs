{-# LANGUAGE MultiParamTypeClasses #-}

module Csili.Syntax
( Term(..)

, Symbol(..)
, Var(..)
, Place(..)
, Transition(..)

, Collectible(..)
) where

import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set

data Term
    = Function Symbol [Term]
    | IntTerm Int
    | Variable Var
    | Wildcard
    deriving (Show, Eq, Ord)

newtype Symbol = Symbol Text
    deriving (Show, Eq, Ord)

newtype Var = Var Text
    deriving (Show, Eq, Ord)

newtype Place = Place Text
    deriving (Show, Eq, Ord)

newtype Transition = Transition Text
    deriving (Show, Eq, Ord)

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
        Wildcard -> Set.empty

instance Collectible Term Var where
    collect = \case
        Function _ terms -> Set.unions (map collect terms)
        Variable var -> Set.singleton var
        IntTerm _ -> Set.empty
        Wildcard -> Set.empty
