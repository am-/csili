{-# LANGUAGE RecordWildCards #-}

module Csili.Validation
( ValidationError(..)
, validateProgram
) where

import Data.Containers.ListUtils (nubOrd)
import Data.List ((\\))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Validation (Validation(..))
import Data.Maybe (mapMaybe)

import Csili.Program

data ValidationError
    = OverlappingInputAndOutput Place
    | InternalPlaceInsideInterface Place
    | TokenOnInexistentPlace Place
    | TokenOnInterfacePlace Place
    | DuplicateVariableInPattern TransitionName Place Var
    | DuplicateVariableInDifferentPatterns TransitionName Var (Set Place)
    | ConsumingFromOutputPlace TransitionName Place
    | ConsumingFromInexistentPlace TransitionName Place
    | UnknownVariableInSubstitution TransitionName Place (Set Var)
    | ConstructingOnInputPlace TransitionName Place
    | ConstructingOnInexistentPlace TransitionName Place
    | EffectOnInputPlace TransitionName Place
    | EffectOnInexistentPlace TransitionName Place
    | StructuralConflictWithEffectfulTransition TransitionName (Set TransitionName)
    deriving (Show, Eq, Ord)

validateProgram :: Program -> Validation [ValidationError] Program
validateProgram program@(Program {..})= toValidation $ concat
    [ validateInterface interface
    , validateInternalPlacesAgainstInterface interface internalPlaces
    , validateInitialMarkingAgainstInterfacePlaces interface initialMarking
    , validateInitialMarkingAgainstInternalPlaces interface internalPlaces initialMarking
    , validateTransitions transitions
    , concatMap (validateTransitionAgainstInterface interface) . Set.toList $ transitions
    , concatMap (validateTransitionAgainstPlaces (places program)) . Set.toList $ transitions
    ]
  where
    toValidation = \case
        [] -> pure program
        errors -> Failure errors


validateTransitions :: Set Transition -> [ValidationError]
validateTransitions transitions = concatMap ($ transitions)
    [ concatMap validateTransition . Set.toList
    , validateEffectfulTransitions
    ]

validateTransition :: Transition -> [ValidationError]
validateTransition transition = concatMap ($ transition)
    [ validateVariablesInPatterns
    , validateVariablesInDifferentPatterns
    , validateVariablesInProductions
    ]

validateEffectfulTransitions :: Set Transition -> [ValidationError]
validateEffectfulTransitions transitions = mapMaybe toError . Set.toAscList $ Set.filter isEffectful transitions
  where
    toError transition
        | Set.null conflictingTransitions = Nothing
        | otherwise = Just . StructuralConflictWithEffectfulTransition (name transition) $ Set.map name conflictingTransitions
      where
        conflictingTransitions = Set.filter (areStructurallyConflicting transition) transitions

validateTransitionAgainstInterface :: Interface -> Transition -> [ValidationError]
validateTransitionAgainstInterface interface transition = concat
    [ toError ConsumingFromOutputPlace . Set.intersection preset $ output interface
    , toError ConstructingOnInputPlace . Set.intersection constructionPlaces $ input interface
    , toError EffectOnInputPlace . Set.intersection effectPlaces $ input interface
    ]
  where
    preset = Map.keysSet $ patterns transition
    constructionPlaces = Map.keysSet $ constructions transition
    effectPlaces = Map.keysSet $ effects transition
    toError mkError = map (mkError $ name transition) . Set.toAscList

validateInitialMarkingAgainstInterfacePlaces :: Interface -> Marking -> [ValidationError]
validateInitialMarkingAgainstInterfacePlaces interface marking = concatMap (toError . Set.intersection markedPlaces)
    [ output interface
    , input interface
    ]
  where
    toError = map TokenOnInterfacePlace . Set.toAscList
    markedPlaces = Map.keysSet marking

validateInitialMarkingAgainstInternalPlaces :: Interface -> Set Place -> Marking -> [ValidationError]
validateInitialMarkingAgainstInternalPlaces interface internalPlaces marking = toError (Set.difference markedPlaces definedPlaces)
  where
    toError = map TokenOnInexistentPlace . Set.toAscList
    markedPlaces = Map.keysSet marking
    definedPlaces = Set.unions [internalPlaces, input interface, output interface]

validateTransitionAgainstPlaces :: Set Place -> Transition -> [ValidationError]
validateTransitionAgainstPlaces definedPlaces transition = concat
    [ toError ConsumingFromInexistentPlace $ Set.difference preset definedPlaces
    , toError ConstructingOnInexistentPlace $ Set.difference constructionPlaces definedPlaces
    , toError EffectOnInexistentPlace $ Set.difference effectPlaces definedPlaces
    ]
  where
    preset = Map.keysSet $ patterns transition
    constructionPlaces = Map.keysSet $ constructions transition
    effectPlaces = Map.keysSet $ effects transition
    toError mkError = map (mkError $ name transition) . Set.toAscList

validateVariablesInPatterns :: Transition -> [ValidationError]
validateVariablesInPatterns transition = concatMap toError . Map.assocs $ patterns transition
  where
    toError (place, pattern) = map (mkError place) . findDuplicates $ findVariables pattern
    mkError = DuplicateVariableInPattern (name transition)
    findVariables = \case
        FunctionPattern _ patterns -> concatMap findVariables patterns
        VariablePattern var -> [var]
        WildcardPattern -> []

validateVariablesInDifferentPatterns :: Transition -> [ValidationError]
validateVariablesInDifferentPatterns Transition{..} = mapMaybe toError variables
  where
    toError var
        | Set.size containingPatterns == 1 = Nothing
        | otherwise = Just $ mkError var containingPatterns
      where
        containingPatterns = Map.keysSet $ Map.filter (Set.member var . collect) patterns
    mkError var = DuplicateVariableInDifferentPatterns name var
    variables = Set.toAscList . Set.unions . map collect $ Map.elems patterns

validateVariablesInProductions :: Transition -> [ValidationError]
validateVariablesInProductions transition = mapMaybe toError . Map.assocs $ productions transition
  where
    definedVariables = Set.unions . map collect . Map.elems $ patterns transition
    toError (place, production)
        | Set.null unknownVariables = Nothing
        | otherwise = Just $ UnknownVariableInSubstitution (name transition) place unknownVariables
      where
        unknownVariables = Set.difference (collect production) definedVariables

validateInternalPlacesAgainstInterface :: Interface -> Set Place -> [ValidationError]
validateInternalPlacesAgainstInterface interface internalPlaces = concatMap (map InternalPlaceInsideInterface . Set.toAscList)
    [ Set.intersection internalPlaces $ input interface
    , Set.intersection internalPlaces $ output interface
    ]

validateInterface :: Interface -> [ValidationError]
validateInterface interface
    | Set.null overlappingPlaces = []
    | otherwise = map OverlappingInputAndOutput $ Set.toAscList overlappingPlaces
  where
    overlappingPlaces = Set.intersection (input interface) (output interface)

findDuplicates :: Ord a => [a] -> [a]
findDuplicates elements = elements \\ nubOrd elements
