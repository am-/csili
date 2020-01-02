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
import Data.Map.Strict (Map)

import Csili.Program

data ValidationError
    = InstanceOfUnknownTemplate TemplateInstance TemplateName
    | OverlappingInputAndOutput Place
    | RedeclarationOfPlace Place
    | InternalPlaceInsideInterface Place
    | TokenOnInexistentPlace Place
    | TokenOnHiddenPlace Place
    | TokenOnConsumeOnlyPlace Place
    | DuplicateVariableInPattern TransitionName Place Var
    | DuplicateVariableInDifferentPatterns TransitionName Var (Set Place)
    | ConsumingFromProduceOnlyPlace TransitionName Place
    | ConsumingFromInexistentPlace TransitionName Place
    | ConsumingFromHiddenPlace TransitionName Place
    | UnknownVariableInSubstitution TransitionName Place (Set Var)
    | ConstructingOnConsumeOnlyPlace TransitionName Place
    | ConstructingOnInexistentPlace TransitionName Place
    | ConstructingOnHiddenPlace TransitionName Place
    | EffectOnConsumeOnlyPlace TransitionName Place
    | EffectOnInexistentPlace TransitionName Place
    | EffectOnHiddenPlace TransitionName Place
    | StructuralConflictWithEffectfulTransition TransitionName (Set TransitionName)
    deriving (Show, Eq, Ord)

validateProgram :: Program -> Validation [ValidationError] Program
validateProgram program = toValidation $ concat
    [ validateNet program (mainNet program)
    , concatMap (validateNet program) (templates program)
    ]
  where
    toValidation = \case
        [] -> pure program
        errors -> Failure errors

validateNet :: Program -> Net -> [ValidationError]
validateNet program net@(Net {..}) = concat
    [ validateInstances (Map.keysSet $ templates program) instances
    , validateInterface interface
    , validateInternalPlaces interface internalPlaces
    , validateInitialMarking existing adressable consumeOnly initialMarking
    , validateTransitions transitions
    , concatMap (validateTransitionAgainstPlaces existing adressable consumeOnly produceOnly) $ transitions
    ]
  where
    adressable = adressablePlaces program net
    existing = existingPlaces program net
    consumeOnly = consumeOnlyPlaces program net
    produceOnly = produceOnlyPlaces program net

validateInstances :: Set TemplateName -> Map TemplateInstance TemplateName -> [ValidationError]
validateInstances definedTemplates = map (uncurry InstanceOfUnknownTemplate) . Map.assocs . Map.filter (flip Set.notMember definedTemplates)

validateInterface :: Interface -> [ValidationError]
validateInterface interface = concat
    [ map OverlappingInputAndOutput . Set.toAscList $ Set.intersection (input interface) (output interface)
    , map RedeclarationOfPlace . Set.toAscList . Set.filter isQualified $ Set.union (input interface) (output interface)
    ]

validateInternalPlaces :: Interface -> Set Place -> [ValidationError]
validateInternalPlaces interface internalPlaces = concat
    [ map InternalPlaceInsideInterface . Set.toAscList . Set.intersection internalPlaces $ Set.union (input interface) (output interface)
    , map RedeclarationOfPlace . Set.toAscList $ Set.filter isQualified internalPlaces
    ]

validateInitialMarking :: Set Place -> Set Place -> Set Place -> Marking -> [ValidationError]
validateInitialMarking existing adressable consumeOnly marking = concat
    [ toError TokenOnInexistentPlace $ Set.difference marked existing
    , toError TokenOnHiddenPlace $ Set.intersection hidden marked
    , toError TokenOnConsumeOnlyPlace $ Set.intersection marked consumeOnly
    ]
  where
    toError mkError = map mkError . Set.toAscList
    marked = Map.keysSet marking
    hidden = Set.difference existing adressable

validateTransitions :: Set Transition -> [ValidationError]
validateTransitions transitions = concatMap ($ transitions)
    [ concatMap validateTransition
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

validateTransitionAgainstPlaces :: Set Place -> Set Place -> Set Place -> Set Place -> Transition -> [ValidationError]
validateTransitionAgainstPlaces existing adressable consumeOnly produceOnly transition = concat
    [ toError ConsumingFromInexistentPlace $ Set.difference preset existing
    , toError ConsumingFromHiddenPlace $ Set.intersection (Set.difference preset adressable) existing
    , toError ConsumingFromProduceOnlyPlace $ Set.intersection preset produceOnly
    , toError ConstructingOnInexistentPlace $ Set.difference constructionPlaces existing
    , toError ConstructingOnHiddenPlace $ Set.intersection (Set.difference constructionPlaces adressable) existing
    , toError ConstructingOnConsumeOnlyPlace $ Set.intersection constructionPlaces consumeOnly
    , toError EffectOnInexistentPlace $ Set.difference effectPlaces existing
    , toError EffectOnHiddenPlace $ Set.intersection (Set.difference effectPlaces adressable) existing
    , toError EffectOnConsumeOnlyPlace $ Set.intersection effectPlaces consumeOnly
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

findDuplicates :: Ord a => [a] -> [a]
findDuplicates elements = elements \\ nubOrd elements
