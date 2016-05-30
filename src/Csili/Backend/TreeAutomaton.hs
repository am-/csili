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
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Csili.Types

data Action a
    = NextState a
    | Apply Rule
    | Reset
    deriving (Show, Eq, Ord)

data Match a
    = State a
    | AnythingDifferent
    deriving (Show, Eq, Ord)

type Delta a = Map Symbol (Map [Match a] (Action a))

data TreeAutomaton a = TreeAutomaton
    { delta :: Map Symbol (Map [Match a] (Action a))
    } deriving (Show, Eq)

fromRule :: Rule -> TreeAutomaton a
fromRule (lhs, rhs) = undefined

fromTerm :: Term -> TreeAutomaton Int
fromTerm = TreeAutomaton . \case
    Function symbol args -> case args of
        [] -> Map.singleton symbol (Map.singleton [] (NextState 1))
        [argument] ->
              let childDelta = delta . fromTerm $ argument
                  largestState = findLargestState childDelta
              in  Map.insertWith Map.union symbol (Map.singleton [State largestState] (NextState $ largestState + 1)) childDelta
        _ -> undefined
    Variable _ -> undefined
    Promise _ _ -> undefined
    Future _ -> undefined

findLargestState :: Ord a => Delta a -> a
findLargestState = maximum . mapMaybe (fromAction . snd)
                 . concatMap (Map.toList . snd) . Map.toList

alphabet :: TreeAutomaton a -> Set Symbol
alphabet = Map.keysSet . delta

states :: Ord a => TreeAutomaton a -> Set a
states = Set.fromList . concatMap (catMaybes . map fromAction . Map.elems) . Map.elems . delta

fromAction :: Action a -> Maybe a
fromAction = \case
    NextState s -> Just s
    Apply _ -> Nothing
    Reset -> Nothing

unions :: Ord a => [TreeAutomaton a] -> TreeAutomaton [a]
unions = TreeAutomaton . undefined . map delta

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
    Reset -> Reset

renameMatch :: Ord a => Map a b -> Match a -> Match b
renameMatch newNames = \case
    State a -> case Map.lookup a newNames of
        Nothing -> undefined
        Just b -> State b
    AnythingDifferent -> AnythingDifferent
