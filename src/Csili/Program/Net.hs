module Csili.Program.Net
( Net(..)
, emptyNet
, netPlaces

, TemplateName(..)
, TemplateInstance(..)
, flatten
, instantiateTemplates

, Place(..)
, isQualified
, emptyInternalPlaces
, Interface(..)
, emptyInterface

, Marking
, emptyMarking
, calculateMarking

, Transition(..)
, TransitionName(..)
, Production(..)
, Effect(..)
, emptyTransitions
, mkTransition
, prePlaces
, postPlaces
, constructions
, effects
, isEnabled
, isEffectful
, areStructurallyConflicting
, bindVariables
) where

import Data.Function (on)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (mapMaybe, isJust)

import Csili.Program.Token
import Csili.Program.Pattern

data Net = Net
    { instances :: Map TemplateInstance TemplateName
    , interface :: Interface
    , internalPlaces :: Set Place
    , initialMarking :: Marking
    , transitions :: Set Transition
    } deriving (Show, Eq)

emptyNet :: Net
emptyNet = Net
    { instances = emptyInstances
    , interface = emptyInterface
    , internalPlaces = emptyInternalPlaces
    , initialMarking = emptyMarking
    , transitions = emptyTransitions
    }

netPlaces :: Net -> Set Place
netPlaces net = Set.unions [input $ interface net, output $ interface net, internalPlaces net]

data Place
    = LocalPlace Text
    | TemplatePlace TemplateInstance Text
    deriving (Show, Eq, Ord)

qualifyPlace :: TemplateInstance -> Place -> Place
qualifyPlace templateInstance = \case
    LocalPlace placeName -> TemplatePlace templateInstance placeName
    TemplatePlace existingInstance placeName -> TemplatePlace (templateInstance <> existingInstance) placeName

isQualified :: Place -> Bool
isQualified = \case
    LocalPlace _ -> False
    TemplatePlace _ _ -> True

newtype TemplateName = TemplateName Text
    deriving (Show, Eq, Ord)

newtype TemplateInstance = TemplateInstance Text
    deriving (Show, Eq, Ord)

instance Semigroup TemplateInstance where
    (TemplateInstance name1) <> (TemplateInstance name2) = TemplateInstance (T.concat [name1 , ".", name2])

emptyInstances :: Map TemplateInstance TemplateName
emptyInstances = Map.empty

flatten :: Map TemplateName Net -> Net -> Maybe Net
flatten templateMap net = embed <$> instantiatedTemplates
  where
    instantiatedTemplates = traverse (uncurry (instantiateTemplate templateMap)) . Map.toList $ instances net
    embed templateInstances = net
        { instances = Map.empty
        , interface = interface net
        , internalPlaces = Set.union (internalPlaces net) . Set.unions $ map netPlaces templateInstances
        , initialMarking = Map.union (initialMarking net) . Map.unions $ map initialMarking templateInstances
        , transitions = Set.union (transitions net) . Set.unions $ map transitions templateInstances
        }

instantiateTemplate :: Map TemplateName Net -> TemplateInstance -> TemplateName -> Maybe Net
instantiateTemplate templateMap templateInstance templateName = do
    net <- Map.lookup templateName templateMap
    qualifyNet templateInstance <$> flatten templateMap net

instantiateTemplates :: Map TemplateName Net -> Map TemplateInstance TemplateName -> [Net]
instantiateTemplates templateMap = mapMaybe (uncurry (instantiateTemplate templateMap)) . Map.toList

data Interface = Interface
    { input :: Set Place
    , output :: Set Place
    } deriving (Show, Eq)

emptyInterface :: Interface
emptyInterface = Interface
    { input = Set.empty
    , output = Set.empty
    }

emptyInternalPlaces :: Set Place
emptyInternalPlaces = Set.empty

type Marking = Map Place Token

emptyMarking :: Marking
emptyMarking = Map.empty

data Transition = Transition
    { name :: TransitionName
    , patterns :: Map Place Pattern
    , productions :: Map Place Production
    } deriving (Show, Eq, Ord)

data TransitionName
    = LocalTransition Text
    | TemplateTransition TemplateInstance Text
    deriving (Show, Eq, Ord)

data Production
    = Construct ConstructionRule
    | Evaluate Effect
    deriving (Show, Eq, Ord)

data Effect
    = WriteWord8 ConstructionRule ConstructionRule
    | ReadWord8 ConstructionRule
    deriving (Show, Eq, Ord)

mkTransition :: Text -> Transition
mkTransition transitionName = Transition
    (LocalTransition transitionName)
    Map.empty
    Map.empty

emptyTransitions :: Set Transition
emptyTransitions = Set.empty

qualifyNet :: TemplateInstance -> Net -> Net
qualifyNet templateInstance net = net
    { instances = Map.mapKeys (templateInstance <>) $ instances net
    , interface = (interface net)
        { input = Set.map (qualifyPlace templateInstance) . input $ interface net
        , output = Set.map (qualifyPlace templateInstance) . output $ interface net
        }
    , internalPlaces = Set.map (qualifyPlace templateInstance) $ internalPlaces net
    , initialMarking = Map.mapKeys (qualifyPlace templateInstance) $ initialMarking net
    , transitions = Set.map (qualifyTransition templateInstance) $ transitions net
    }

qualifyTransition :: TemplateInstance -> Transition -> Transition
qualifyTransition templateInstance transition = transition
    { name = qualifyTransitionName $ name transition
    , patterns = Map.mapKeys (qualifyPlace templateInstance) $ patterns transition
    , productions = Map.mapKeys (qualifyPlace templateInstance) $ productions transition
    }
  where
    qualifyTransitionName = \case
        LocalTransition transitionName -> TemplateTransition templateInstance transitionName
        TemplateTransition existingInstance transitionName -> TemplateTransition (templateInstance <> existingInstance) transitionName

isEffectful :: Transition -> Bool
isEffectful = not . Map.null . effects

constructions :: Transition -> Map Place ConstructionRule
constructions = Map.mapMaybe extractConstruction . productions
  where
    extractConstruction = \case
        Construct construction -> Just construction
        Evaluate _ -> Nothing

effects :: Transition -> Map Place Effect
effects = Map.mapMaybe extractEffect . productions
  where
    extractEffect = \case
        Construct _ -> Nothing
        Evaluate effect -> Just effect

prePlaces :: Transition -> Set Place
prePlaces = Map.keysSet . patterns

postPlaces :: Transition -> Set Place
postPlaces = Map.keysSet . productions

areStructurallyConflicting :: Transition -> Transition -> Bool
areStructurallyConflicting transition1 transition2
    | name transition1 == name transition2 = False
    | haveDisjointPreset && canDeactivateByProducing = True
    | not haveDisjointPreset && haveOverlappingPatterns = True
    | otherwise = False
  where
    haveDisjointPreset = Set.null commonPreset
    haveOverlappingPatterns = and (on comparePatterns restrictToCommonPreset transition1 transition2)
    commonPreset = on Set.intersection prePlaces transition1 transition2
    restrictToCommonPreset = flip Map.restrictKeys commonPreset . patterns
    comparePatterns = Map.intersectionWith areOverlapping
    canDeactivateByProducing = not $ Set.disjoint
        (Set.difference (postPlaces transition1) (prePlaces transition2))
        (Set.difference (postPlaces transition2) (prePlaces transition1))

isEnabled :: Transition -> Marking -> Bool
isEnabled transition = isJust . bindVariables transition

bindVariables :: Transition -> Marking -> Maybe (Map Var Token)
bindVariables transition marking
    | not isPresetMarked = Nothing
    | isPostsetBlocked = Nothing
    | otherwise = matchPatterns
  where
    markedPlaces = Map.keysSet marking
    isPresetMarked = Set.null $ Set.difference (prePlaces transition) markedPlaces
    isPostsetBlocked = not . Set.null . Set.intersection (postPlaces transition) . Set.difference markedPlaces $ prePlaces transition
    matchPatterns = fmap Map.unions . sequence . Map.elems $ Map.intersectionWith match (patterns transition) marking

calculateMarking :: Marking -> Set Place -> Map Place Token -> Marking
calculateMarking marking preset postset = Map.union postset (Map.withoutKeys marking preset)
