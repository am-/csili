{-# LANGUAGE MultiParamTypeClasses #-}

module Csili.Program
( Program(..)
, empty
, emptyInternalPlaces
, emptyTransitions
, emptyMarking
, places

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
, replaceMarking

, Interface(..)
, emptyInterface

, Collectible(..)
) where

import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import System.IO (Handle)

data Program = Program
    { interface :: Interface
    , internalPlaces :: Set Place
    , initialMarking :: Marking
    , transitions :: Set Transition
    } deriving (Show, Eq)

places :: Program -> Set Place
places program = Set.unions [inputPlaces, outputPlaces, internalPlaces program]
  where
    inputPlaces = input $ interface program
    outputPlaces = output $ interface program

newtype TransitionName = TransitionName Text
    deriving (Show, Eq, Ord)

data Transition = Transition
    { name :: TransitionName
    , patterns :: Map Place Pattern
    , productions :: Map Place Production
    } deriving (Show, Eq, Ord)

mkTransition :: Text -> Transition
mkTransition transitionName = Transition
    (TransitionName transitionName)
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

newtype Place = Place Text
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

empty :: Program
empty = Program
    { interface = emptyInterface
    , internalPlaces = emptyInternalPlaces
    , initialMarking = emptyMarking
    , transitions = emptyTransitions
    }

emptyInternalPlaces :: Set Place
emptyInternalPlaces = Set.empty

emptyMarking :: Marking
emptyMarking = Map.empty

replaceMarking :: Program -> Marking -> Program
replaceMarking program marking = program { initialMarking = marking }

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
