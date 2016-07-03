{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Csili.Backend.TreeAutomaton
(
) where

import Data.Function (on)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Csili.Types

--------------------------------------------------------------------------------
-- Rule Conversion
--------------------------------------------------------------------------------

fromRuleSet :: [Rule] -> DeterministicTreeAutomaton Int
fromRuleSet = normalize . determinize . unions . map fromRule

fromRule :: Rule -> NonDeterministicTreeAutomaton Int
fromRule (lhs, rhs) = undefined

fromTerm :: Term -> NonDeterministicTreeAutomaton Int
fromTerm = Delta . \case
    Function symbol args
        | null args -> undefined
        | otherwise -> undefined
    Variable _ -> undefined
    Promise _ _ -> undefined
    Future _ -> undefined

--------------------------------------------------------------------------------
-- Tree Automata
--------------------------------------------------------------------------------

newtype Delta match action = Delta (Map Symbol (Map match action))
    deriving (Show, Eq)

delta :: Delta match action -> Map Symbol (Map match action)
delta (Delta relation) = relation

alphabet :: Delta match action -> Set Symbol
alphabet = Map.keysSet . delta

states :: Ord state => (action -> Set state) -> Delta match action -> Set state
states extractor = Set.unions . concatMap (map extractor . Map.elems) . Map.elems . delta

data Action a
    = NextState a
    | Apply Rule
    | Reset
    deriving (Show, Eq, Ord)

fromAction :: Action a -> Maybe a
fromAction = \case
    NextState s -> Just s
    Apply _ -> Nothing
    Reset -> Nothing

data Match a
    = State a
    | AnythingDifferent
    deriving (Show, Eq, Ord)

renameAction :: Ord a => Map a b -> Action a -> Action b
renameAction newNames = \case
    NextState a -> case Map.lookup a newNames of
        Nothing -> undefined
        Just b -> NextState b
    Apply rule -> Apply rule
    Reset -> Reset

renameMatch :: Ord a => Map a b -> Match a -> Match b
renameMatch newNames = \case
    State a -> case Map.lookup a newNames of
        Nothing -> undefined
        Just b -> State b
    AnythingDifferent -> AnythingDifferent

--------------------------------------------------------------------------------
-- Deterministic Tree Automata
--------------------------------------------------------------------------------

type DeterministicAction = Action
type DeterministicTreeAutomaton state = Delta [Match state] (DeterministicAction state)

fromDeterministicAction :: Ord state => DeterministicAction state -> Set state
fromDeterministicAction = maybe Set.empty Set.singleton . fromAction

normalize :: Ord a => DeterministicTreeAutomaton a -> DeterministicTreeAutomaton Int
normalize = flip renameStates <*> Map.fromList . flip zip [0..] . Set.toList . states fromDeterministicAction

renameStates :: (Ord a, Ord b) => Map a b -> DeterministicTreeAutomaton a -> DeterministicTreeAutomaton b
renameStates newNames = Delta . Map.map (renameDelta newNames) . delta

renameDelta :: (Ord a, Ord b) => Map a b -> Map [Match a] (Action a) -> Map [Match b] (Action b)
renameDelta newNames = Map.mapKeys (map (renameMatch newNames)) . Map.map (renameAction newNames)

--------------------------------------------------------------------------------
-- Non-deterministic Tree Automata
--------------------------------------------------------------------------------

type NonDeterministicAction state = [Action state]
type NonDeterministicTreeAutomaton state = Delta [Match state] (NonDeterministicAction state)

fromNonDeterministicAction :: Ord state => NonDeterministicAction state -> Set state
fromNonDeterministicAction = Set.unions . map (maybe Set.empty Set.singleton . fromAction)

unions :: Ord a => [NonDeterministicTreeAutomaton a] -> NonDeterministicTreeAutomaton [a]
unions = undefined

determinize :: NonDeterministicTreeAutomaton a -> DeterministicTreeAutomaton [a]
determinize = undefined
