{-# LANGUAGE OverloadedStrings #-}

module Csili.Normalization.Capacity
( normalize
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

import Csili.Semantics

normalize :: Semantics -> Semantics
normalize sem = sem
    { marking = Map.union newMarking (marking sem)
    , patterns = Map.mapWithKey (addConversePlaces unit (applications sem)) (patterns sem)
    , applications = Map.mapWithKey (addConversePlaces (EffectFree unit) (patterns sem)) (applications sem)
    }
  where
    unit = Function (Symbol "unit") []
    placeSet = places sem
    newMarking = Map.fromSet (const $ Function (Symbol "unit") []) . prefixPlaces
               . Set.difference placeSet . Map.keysSet . marking $ sem

addConversePlaces :: a -> Map Transition (Map Place b) -> Transition -> Map Place a -> Map Place a
addConversePlaces unit converseMap transition oldMap
    = Map.union oldMap      
    . Map.mapKeys prefixPlace . Map.map (const unit)
    . Map.filterWithKey (const . not . flip Map.member oldMap)
    $ Map.findWithDefault Map.empty transition converseMap

prefixPlaces :: Set Place -> Set Place
prefixPlaces = Set.map prefixPlace

prefixPlace :: Place -> Place
prefixPlace (Place name) = Place (T.cons '_' name)


