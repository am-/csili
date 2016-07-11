{-# LANGUAGE OverloadedStrings #-}

module Csili.Normalization.Capacity
( normalize
) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import Csili.Normalization.Utility
import Csili.Semantics

normalize :: Semantics -> Semantics
normalize sem = sem
    { marking = Map.union newMarking (marking sem)
    , patterns = Map.mapWithKey (addConversePlaces (Pattern unit) (applications sem)) (patterns sem)
    , applications = Map.mapWithKey (addConversePlaces (EffectFree unit) (patterns sem)) (applications sem)
    }
  where
    newMarking = Map.fromSet (const unit) . Set.map prefixPlace
               . Set.difference (places sem) . Map.keysSet . marking $ sem

addConversePlaces :: a -> Map Transition (Map Place b) -> Transition -> Map Place a -> Map Place a
addConversePlaces noop converseMap transition oldMap
    = Map.union oldMap
    . Map.mapKeys prefixPlace . Map.map (const noop)
    . Map.filterWithKey (const . not . flip Map.member oldMap)
    $ Map.findWithDefault Map.empty transition converseMap

prefixPlace :: Place -> Place
prefixPlace (Place name) = Place (T.cons '_' name)

