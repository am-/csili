{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Csili.Backend.TreeAutomaton
(
) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set

import Csili.Types

data Action a
    = NextState a
    | Apply Rule
    deriving (Show, Eq, Ord)

data Match a
    = State a
    | AnythingDifferent
    deriving (Show, Eq, Ord)

data TreeAutomaton a = TreeAutomaton
    { delta :: Map Symbol (Map [Match a] (Action a))
    } deriving (Show, Eq)

fromRule :: Rule -> TreeAutomaton a
fromRule (lhs, rhs) = undefined

alphabet :: TreeAutomaton a -> Set Symbol
alphabet = Map.keysSet . delta

states :: Ord a => TreeAutomaton a -> Set a
states = Set.fromList . concatMap (catMaybes . map fromAction . Map.elems) . Map.elems . delta
  where
    fromAction :: Action a -> Maybe a
    fromAction = \case
        NextState s -> Just s
        Apply _ -> Nothing

unions :: Ord a => [TreeAutomaton a] -> TreeAutomaton [a]
unions = TreeAutomaton . Map.unionsWith undefined . map delta

--------------------------------------------------------------------------------
-- Normalization
--------------------------------------------------------------------------------

normalize :: Ord a => TreeAutomaton a -> TreeAutomaton Int
normalize = flip renameStates <*> Map.fromList . flip zip [0..] . Set.toList . states

renameStates :: (Ord a, Ord b) => Map a b -> TreeAutomaton a -> TreeAutomaton b
renameStates newNames = TreeAutomaton . Map.map (renameDelta newNames) . delta

renameDelta :: (Ord a, Ord b) => Map a b -> Map [Match a] (Action a) -> Map [Match b] (Action b)
renameDelta newNames = Map.mapKeys (map (renameMatch newNames)) . Map.map (renameAction newNames)

renameAction :: Ord a => Map a b -> Action a -> Action b
renameAction newNames = \case
    NextState a -> case Map.lookup a newNames of
        Nothing -> undefined
        Just b -> NextState b
    Apply rule -> Apply rule

renameMatch :: Ord a => Map a b -> Match a -> Match b
renameMatch newNames = \case
    State a -> case Map.lookup a newNames of
        Nothing -> undefined
        Just b -> State b
    AnythingDifferent -> AnythingDifferent
