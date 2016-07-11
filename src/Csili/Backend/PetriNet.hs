{-# LANGUAGE LambdaCase #-}
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
    [ T.concat ["#define PLACES ", T.pack . show . Set.size . places $ sem]
    , T.concat ["#define TRANSITIONS ", T.pack . show . Set.size . transitions $ sem]
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

generateTransition :: Map Place Matcher -> Map Place Computation -> [Text]
generateTransition preset postset = concat
    [ [T.concat ["if(", generatePresetMatcher preset, ") {"]]
    , map (T.append "  ") $ generateAllocations preset postset
    , map (T.append "  ") $ generateNormalizations postset
    , map (T.append "  ") $ generatePresetConsumption preset
    , map (T.append "  ") $ generatePostsetProduction postset
    , ["  return true;"]
    , ["}"]
    ]

generatePresetMatcher :: Map Place Matcher -> Text
generatePresetMatcher
    = T.intercalate " && " . map (uncurry generatePlaceMatcher) . Map.toList

generatePlaceMatcher :: Place -> Matcher -> Text
generatePlaceMatcher place = \case
    Pattern term -> T.concat [variable, " != NULL && ", generateMatch variable term]
    PromisePending _ -> undefined
    PromiseBroken _ -> undefined
    PromiseKept _ -> undefined
  where
    variable = buildPlaceVariable place

buildPlaceVariable :: Place -> Text
buildPlaceVariable (Place place) = T.concat ["places[PLACE_", place, "]"]

generateAllocations :: Map Place Matcher -> Map Place Computation -> [Text]
generateAllocations preset
    = concatMap (uncurry (generateComputation preset) . first (T.cons '_' . unpackPlace))
    . Map.toList

generateComputation :: Map Place Matcher -> Text -> Computation -> [Text]
generateComputation preset variable = \case
    EffectFree term -> generateRewrite variablesMap variable term
    Effectful _ _ -> error "TODO!"
  where
    variablesMap = collectVariablesFromPreset preset

collectVariablesFromPreset :: Map Place Matcher -> Map Var Text
collectVariablesFromPreset
    = Map.unions . map (uncurry collectVariablesFromMatcher . first buildPlaceVariable)
    . Map.toList
  where
    collectVariablesFromMatcher :: Text -> Matcher -> Map Var Text
    collectVariablesFromMatcher variable = \case
        Pattern term -> collectVariables variable term
        PromisePending _ -> undefined
        PromiseBroken _ -> undefined
        PromiseKept _ -> undefined

generateNormalizations :: Map Place Computation -> [Text]
generateNormalizations
    = map (generateNormalization . T.append "new_term_" . unpackPlace . fst)
    . Map.toList

generateNormalization :: Text -> Text
generateNormalization variable = T.concat [variable, " = normalize(", variable, ");"]

generatePresetConsumption :: Map Place Matcher -> [Text]
generatePresetConsumption
    = map (flip T.append " = NULL;" . buildPlaceVariable . fst)
    . Map.toList

generatePostsetProduction :: Map Place Computation -> [Text]
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
