{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

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

data Action
    = NextState Int
    | Apply Rule
    | Reset
    deriving (Show, Ord, Eq)

data Match
    = State Int
    | Everything
    deriving (Show, Ord, Eq)

type Delta = Map Symbol (Map [Match] Action)

data TreeAutomaton = TreeAutomaton
    { delta :: Map Symbol (Map [Match] Action)
    } deriving (Show, Eq)

fromRule :: Rule -> TreeAutomaton
fromRule (lhs, rhs) = undefined

fromTerm :: Term -> TreeAutomaton
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

findLargestState :: Delta -> Int
findLargestState = maximum . mapMaybe (fromAction . snd)
                 . concatMap (Map.toList . snd) . Map.toList


alphabet :: TreeAutomaton -> Set Symbol
alphabet = Map.keysSet . delta

states :: TreeAutomaton -> IntSet
states = IntSet.fromList . concatMap (catMaybes . map fromAction . Map.elems) . Map.elems . delta


fromAction :: Action -> Maybe Int
fromAction = \case
    NextState s -> Just s
    Apply _ -> Nothing
    Reset -> Nothing
