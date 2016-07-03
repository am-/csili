{-# LANGUAGE OverloadedStrings #-}

module Csili.Backend.PetriNet
( generateScheduler
) where

import Data.Bifunctor (first)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Csili.Backend.TermRewriting
import Csili.Semantics
  
generateScheduler :: Semantics -> Text
generateScheduler sem = T.intercalate "\n"
    [ T.concat ["#define PLACES ", T.pack . show . Set.size . transitions $ sem]
    , T.concat ["#define TRANSITIONS ", T.pack . show . Set.size . places $ sem]
    , ""
    , T.intercalate "\n" (generatePlaceDefinitions sem)
    , ""
    , "bool schedule() {"
    , T.intercalate "\n" . map (T.append "  ") . generateTransitions $ sem
    , ""
    , "  return false;"
    , "}"
    ]

generateTransitions :: Semantics -> [Text]
generateTransitions sem
    = concatMap (generateTransition <$> getMap (patterns sem) <*> getMap (applications sem))
    . Set.toList . transitions $ sem
  where
    getMap :: Ord a => Map a (Map b c) -> a -> Map b c
    getMap = flip (Map.findWithDefault Map.empty)

generateTransition :: Map Place Term -> Map Place Term -> [Text]
generateTransition preset postset = concat
    [ [T.concat ["if(", generatePresetMatch preset, ") {"]]
    , map (T.append "  ") $ generateAllocations preset postset
    , map (T.append "  ") $ generateNormalizations postset
    , map (T.append "  ") $ generatePresetConsumption preset
    , map (T.append "  ") $ generatePostsetProduction postset
    , ["  return true;"]
    , ["}"]
    ]

generatePresetMatch :: Map Place Term -> Text
generatePresetMatch
    = T.intercalate " && " . map (uncurry generatePlaceMatch) . Map.toList

generatePlaceMatch :: Place -> Term -> Text
generatePlaceMatch place term
    = T.concat [variable, " != NULL && ", generateMatch variable term]
  where
    variable = buildPlaceVariable place

buildPlaceVariable :: Place -> Text
buildPlaceVariable (Place place) = T.concat ["places[PLACE_", place, "]"]

generateAllocations :: Map Place Term -> Map Place Term -> [Text]
generateAllocations preset
    = concatMap (uncurry (generateRewrite variablesMap) . first (T.cons '_' . unpackPlace))
    . Map.toList
  where
    variablesMap = collectVariablesFromPreset preset

collectVariablesFromPreset :: Map Place Term -> Map Var Text
collectVariablesFromPreset
    = Map.unions . map (uncurry collectVariables . first buildPlaceVariable)
    . Map.toList

generateNormalizations :: Map Place Term -> [Text]
generateNormalizations
    = map (generateNormalization . T.append "new_term_" . unpackPlace . fst)
    . Map.toList

generateNormalization :: Text -> Text
generateNormalization variable = T.concat [variable, " = normalize(", variable, ");"]

generatePresetConsumption :: Map Place Term -> [Text]
generatePresetConsumption
    = map (flip T.append " = NULL;" . buildPlaceVariable . fst)
    . Map.toList

generatePostsetProduction :: Map Place Term -> [Text]
generatePostsetProduction
    = map (\place -> T.concat [buildPlaceVariable place, " = new_term_", unpackPlace place, ";"])
    . map fst . Map.toAscList

--------------------------------------------------------------------------------
-- Symbol generation
--------------------------------------------------------------------------------

generatePlaceDefinitions :: Semantics -> [Text]
generatePlaceDefinitions
    = zipWith (generateDefinition "PLACE") [0..]
    . map unpackPlace . Set.toAscList . places

generateDefinition :: Text -> Int -> Text -> Text
generateDefinition prefix index name
    = T.concat ["#define ", prefix, "_", name, " ", T.pack (show index)]
