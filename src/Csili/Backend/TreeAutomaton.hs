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
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set

import Csili.Types

data Action
    = NextState Int
    | Apply Rule
    deriving (Show, Eq)

data Match
    = State Int
    | Everything
    deriving (Show, Eq)

data TreeAutomaton = TreeAutomaton
    { delta :: Map Symbol (Map [Match] Action)
    } deriving (Show, Eq)

fromRule :: Rule -> TreeAutomaton
fromRule (lhs, rhs) = undefined

alphabet :: TreeAutomaton -> Set Symbol
alphabet = Map.keysSet . delta

states :: TreeAutomaton -> IntSet
states = IntSet.fromList . concatMap (catMaybes . map fromAction . Map.elems) . Map.elems . delta
  where
    fromAction :: Action -> Maybe Int
    fromAction = \case
        NextState s -> Just s
        Apply _ -> Nothing
    
