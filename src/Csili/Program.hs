{-# LANGUAGE MultiParamTypeClasses #-}

module Csili.Program
( Program(..)
, emptyProgram

, Net(..)
, TemplateName(..)
, TemplateInstance(..)
, emptyNet
, emptyInternalPlaces
, emptyTransitions
, emptyMarking
, adressablePlaces
, produceOnlyPlaces
, consumeOnlyPlaces
, existingPlaces
, netPlaces
, isQualified
, flatten

, Place(..)
, Transition(..)
, mkTransition
, isEffectful
, constructions
, effects
, areStructurallyConflicting
, prePlaces
, postPlaces
, TransitionName(..)
, Pattern(..)
, areOverlapping
, Production(..)
, ConstructionRule(..)
, Effect(..)
, Token(..)
, blackToken
, zero
, one
, false
, true
, nil
, cons
, Var(..)
, Symbol(..)

, Marking

, Interface(..)
, emptyInterface

, Collectible(..)
) where

import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import System.IO (Handle)
import Data.Maybe (mapMaybe)

data Program = Program
    { mainNet :: Net
    , templates :: Map TemplateName Net
    } deriving (Show, Eq)

newtype TemplateName = TemplateName Text
    deriving (Show, Eq, Ord)

newtype TemplateInstance = TemplateInstance Text
    deriving (Show, Eq, Ord)

instance Semigroup TemplateInstance where
    (TemplateInstance name1) <> (TemplateInstance name2) = TemplateInstance (T.concat [name1 , ".", name2])

emptyProgram :: Program
emptyProgram = Program
    { mainNet = emptyNet
    , templates = Map.empty
    }

data Net = Net
    { instances :: Map TemplateInstance TemplateName
    , interface :: Interface
    , internalPlaces :: Set Place
    , initialMarking :: Marking
    , transitions :: Set Transition
    } deriving (Show, Eq)

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

existingPlaces :: Program -> Net -> Set Place
existingPlaces program net = Set.union (netPlaces net) (Set.unions placeSets)
  where
    placeSets = map netPlaces $ instantiateTemplates (templates program) (instances net)

adressablePlaces :: Program -> Net -> Set Place
adressablePlaces program net = Set.union (netPlaces net) (Set.unions interfaces)
  where
    interfaces = map extractInterface $ instantiateTemplates (templates program) (instances net)
    extractInterface = (Set.union <$> input <*> output) . interface

consumeOnlyPlaces :: Program -> Net -> Set Place
consumeOnlyPlaces program net = Set.union
    (input $ interface net)
    (Set.unions . map (output . interface) $ instantiateTemplates (templates program) (instances net))

produceOnlyPlaces :: Program -> Net -> Set Place
produceOnlyPlaces program net = Set.union
    (output $ interface net)
    (Set.unions . map (input . interface) $ instantiateTemplates (templates program) (instances net))

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

qualifyPlace :: TemplateInstance -> Place -> Place
qualifyPlace templateInstance = \case
    LocalPlace placeName -> TemplatePlace templateInstance placeName
    TemplatePlace existingInstance placeName -> TemplatePlace (templateInstance <> existingInstance) placeName

isQualified :: Place -> Bool
isQualified = \case
    LocalPlace _ -> False
    TemplatePlace _ _ -> True

netPlaces :: Net -> Set Place
netPlaces net = Set.unions [input $ interface net, output $ interface net, internalPlaces net]

data TransitionName
    = LocalTransition Text
    | TemplateTransition TemplateInstance Text
    deriving (Show, Eq, Ord)

data Transition = Transition
    { name :: TransitionName
    , patterns :: Map Place Pattern
    , productions :: Map Place Production
    } deriving (Show, Eq, Ord)

mkTransition :: Text -> Transition
mkTransition transitionName = Transition
    (LocalTransition transitionName)
    Map.empty
    Map.empty

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

newtype Symbol = Symbol Text
    deriving (Show, Eq, Ord)

newtype Var = Var Text
    deriving (Show, Eq, Ord)

data Place
    = LocalPlace Text
    | TemplatePlace TemplateInstance Text
    deriving (Show, Eq, Ord)

data Token
    = FunctionToken Symbol [Token]
    | Resource Handle
    deriving (Show, Eq)

blackToken :: Token
blackToken = FunctionToken (Symbol "blackToken") []

zero :: Token
zero = FunctionToken (Symbol "zero") []

one :: Token
one = FunctionToken (Symbol "one") []

false :: Token
false = FunctionToken (Symbol "false") []

true :: Token
true = FunctionToken (Symbol "true") []

nil :: Token
nil = FunctionToken (Symbol "nil") []

cons :: Token -> Token -> Token
cons x xs = FunctionToken (Symbol "cons") [x, xs]

data Pattern
    = FunctionPattern Symbol [Pattern]
    | VariablePattern Var
    | WildcardPattern
    deriving (Show, Eq, Ord)

areOverlapping :: Pattern -> Pattern -> Bool
areOverlapping pattern1 pattern2 = case pattern1 of
    FunctionPattern symbol1 patterns1 -> case pattern2 of
        FunctionPattern symbol2 patterns2 -> symbol1 == symbol2 && and (zipWith areOverlapping patterns1 patterns2)
        _ -> False
    VariablePattern _ -> True
    WildcardPattern -> True

data Production
    = Construct ConstructionRule
    | Evaluate Effect
    deriving (Show, Eq, Ord)

data ConstructionRule
    = FunctionConstruction Symbol [ConstructionRule]
    | Substitution Var
    deriving (Show, Eq, Ord)

data Effect
    = WriteWord8 ConstructionRule ConstructionRule
    | ReadWord8 ConstructionRule
    deriving (Show, Eq, Ord)

type Marking = Map Place Token

emptyNet :: Net
emptyNet = Net
    { instances = emptyInstances
    , interface = emptyInterface
    , internalPlaces = emptyInternalPlaces
    , initialMarking = emptyMarking
    , transitions = emptyTransitions
    }

emptyInstances :: Map TemplateInstance TemplateName
emptyInstances = Map.empty

emptyInternalPlaces :: Set Place
emptyInternalPlaces = Set.empty

emptyMarking :: Marking
emptyMarking = Map.empty

emptyTransitions :: Set Transition
emptyTransitions = Set.empty

data Interface = Interface
    { input :: Set Place
    , output :: Set Place
    } deriving (Show, Eq)

emptyInterface :: Interface
emptyInterface = Interface
    { input = Set.empty
    , output = Set.empty
    }

class Ord b => Collectible a b where
    collect :: a -> Set b

instance Collectible Token Symbol where
    collect = \case
        FunctionToken symbol terms -> Set.insert symbol (Set.unions (map collect terms))
        Resource _ -> Set.empty

instance Collectible Pattern Symbol where
    collect = \case
        FunctionPattern symbol terms -> Set.insert symbol (Set.unions (map collect terms))
        VariablePattern _ -> Set.empty
        WildcardPattern -> Set.empty

instance Collectible Production Symbol where
    collect = \case
        Construct construction -> collect construction
        Evaluate effect -> collect effect

instance Collectible Effect Symbol where
    collect = \case
        WriteWord8 stream word -> Set.union (collect stream) (collect word)
        ReadWord8 stream -> collect stream

instance Collectible ConstructionRule Symbol where
    collect = \case
        FunctionConstruction symbol terms ->  Set.insert symbol (Set.unions (map collect terms))
        Substitution _ -> Set.empty

instance Collectible Token Var where
    collect = const Set.empty

instance Collectible Pattern Var where
    collect = \case
        FunctionPattern _ terms -> Set.unions (map collect terms)
        VariablePattern var -> Set.singleton var
        WildcardPattern -> Set.empty

instance Collectible Production Var where
    collect = \case
        Construct construction -> collect construction
        Evaluate effect -> collect effect

instance Collectible ConstructionRule Var where
    collect = \case
        FunctionConstruction _ terms -> Set.unions (map collect terms)
        Substitution var -> Set.singleton var

instance Collectible Effect Var where
    collect = \case
      WriteWord8 stream word -> Set.union (collect stream) (collect word)
      ReadWord8 stream -> collect stream
