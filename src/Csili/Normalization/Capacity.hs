{-# LANGUAGE OverloadedStrings #-}

module Csili.Normalization.Capacity
( normalize
) where

import Data.Map as Map
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

import Csili.Semantics

normalize :: Semantics -> Semantics
normalize sem = sem
    { marking = Map.union newMarking (marking sem)
    , patterns = Map.mapWithKey (addConversePlaces (applications sem)) (patterns sem)
    , applications = Map.mapWithKey (addConversePlaces (patterns sem)) (applications sem)
    }
  where
    placeSet = places sem
    newPlaces = prefixPlaces placeSet
    newMarking = Map.fromSet (const $ Function (Symbol "unit") []) . prefixPlaces
               . Set.difference placeSet . Map.keysSet . marking $ sem

addConversePlaces :: Map Transition (Map Place Term) -> Transition -> Map Place Term -> Map Place Term
addConversePlaces converseMap transition oldMap
    = Map.union oldMap      
    . Map.mapKeys prefixPlace . Map.map (const (Function (Symbol "unit") []))
    . Map.filterWithKey (const . not . flip Map.member oldMap)
    $ Map.findWithDefault Map.empty transition converseMap

prefixPlaces :: Set Place -> Set Place
prefixPlaces = Set.map prefixPlace

prefixPlace :: Place -> Place
prefixPlace (Place name) = Place (T.cons '_' name)


