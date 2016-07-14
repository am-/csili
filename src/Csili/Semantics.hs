{-# LANGUAGE LambdaCase #-}

module Csili.Semantics
( module Csili.Types
, Semantics(..)
, empty
, transitions
, places
, symbols

, orderRule
, orderTerm
) where

import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)

import Csili.Types

--------------------------------------------------------------------------------
-- Petri Net Semantics
--------------------------------------------------------------------------------

data Semantics = Semantics
    { rules :: [Rule]
    , marking :: Map Place Term
    , patterns :: Map Transition (Map Place Matcher)
    , applications :: Map Transition (Map Place Computation)
    } deriving (Show, Eq)

empty :: Semantics
empty = Semantics
      { rules = []
      , marking = Map.empty
      , patterns = Map.empty
      , applications = Map.empty
      }

isValid :: Rule -> Bool
isValid = uncurry (Set.isSubsetOf `on` collectVariables) . swap
  where
    collectVariables :: Term -> Set Var
    collectVariables = collect

transitions :: Semantics -> Set Transition
transitions sem = Set.unions
    [ Map.keysSet (patterns sem)
    , Map.keysSet (applications sem)
    ]

places :: Semantics -> Set Place
places sem = Set.unions
    [ Map.keysSet (marking sem)
    , Set.unions . map Map.keysSet . Map.elems $ patterns sem
    , Set.unions . map Map.keysSet . Map.elems $ applications sem
    ]

symbols :: Semantics -> Set Symbol
symbols sem = Set.unions $ concat
    [ map collect . concatMap (\(lhs, rhs) -> [lhs, rhs]) . rules $ sem
    , map collect . Map.elems . marking $ sem
    , map collect . concatMap Map.elems . Map.elems . patterns $ sem
    , map collect . concatMap Map.elems . Map.elems . applications $ sem
    ]


--------------------------------------------------------------------------------
-- Ordering of Rules
--------------------------------------------------------------------------------

orderRule :: Rule -> Rule -> Ordering
orderRule = orderTerm `on` fst

orderTerm :: Term -> Term -> Ordering
orderTerm term1 term2 = case orderTermBySpecifity term1 term2 of
    Incomparable -> compare term1 term2
    Less -> LT
    Equally -> compare term1 term2
    More -> GT

data SpecifityOrdering
    = Incomparable
    | Less
    | Equally
    | More
    deriving (Show, Eq)

instance Monoid SpecifityOrdering where
    mempty = Incomparable
    mappend = \case
        Incomparable -> const Incomparable
        Less -> const Less
        Equally -> id
        More -> const More

orderTermBySpecifity :: Term -> Term -> SpecifityOrdering
orderTermBySpecifity term1 term2 = case term1 of
    Function symbol1 args1 -> case term2 of
        Function symbol2 args2
            | symbol1 /= symbol2 -> Incomparable
            | length args1 /= length args2 -> Incomparable
            | otherwise -> mconcat (zipWith orderTermBySpecifity (reverse args1) (reverse args2))
        Variable _ -> More
        IntTerm _ -> Incomparable
    Variable _ -> case term2 of
        Variable _ -> Equally
        _ -> Less
    IntTerm x -> case term2 of
        IntTerm y | x == y -> Equally
        _ -> Incomparable
