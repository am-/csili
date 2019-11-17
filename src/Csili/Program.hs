{-# LANGUAGE MultiParamTypeClasses #-}

module Csili.Program
( Program(..)
, empty
, transitions
, places
, symbols

, Place(..)
, Transition(..)
, Pattern(..)
, Production(..)
, Token(..)
, Var(..)
, Symbol(..)

, Marking
, replaceMarking

, Interface(..)
, emptyInterface
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

data Program = Program
    { interface :: Interface
    , initialMarking :: Marking
    , patterns :: Map Transition (Map Place Pattern)
    , productions :: Map Transition (Map Place Production)
    } deriving (Show, Eq)

newtype Symbol = Symbol Text
    deriving (Show, Eq, Ord)

newtype Var = Var Text
    deriving (Show, Eq, Ord)

newtype Place = Place Text
    deriving (Show, Eq, Ord)

newtype Transition = Transition Text
    deriving (Show, Eq, Ord)

data Token
    = FunctionToken Symbol [Token]
    | IntToken Int
    deriving (Show, Eq, Ord)

data Pattern
    = FunctionPattern Symbol [Pattern]
    | IntPattern Int
    | VariablePattern Var
    | WildcardPattern
    deriving (Show, Eq, Ord)

data Production
    = FunctionProduction Symbol [Production]
    | IntProduction Int
    | Substitution Var
    deriving (Show, Eq, Ord)

type Marking = Map Place Token

empty :: Program
empty = Program
      { interface = emptyInterface
      , initialMarking = Map.empty
      , patterns = Map.empty
      , productions = Map.empty
      }

transitions :: Program -> Set Transition
transitions program = Set.unions
    [ Map.keysSet (patterns program)
    , Map.keysSet (productions program)
    ]

places :: Program -> Set Place
places program = Set.unions
    [ Map.keysSet . initialMarking $ program
    , Set.unions . map Map.keysSet . Map.elems $ patterns program
    , Set.unions . map Map.keysSet . Map.elems $ productions program
    ]

symbols :: Program -> Set Symbol
symbols program = Set.unions $ concat
    [ map collect . Map.elems . initialMarking $ program
    , map collect . concatMap Map.elems . Map.elems . patterns $ program
    , map collect . concatMap Map.elems . Map.elems . productions $ program
    ]

replaceMarking :: Program -> Marking -> Program
replaceMarking program marking = program { initialMarking = marking }

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
        IntToken _ -> Set.empty

instance Collectible Pattern Symbol where
    collect = \case
        FunctionPattern symbol terms -> Set.insert symbol (Set.unions (map collect terms))
        IntPattern _ -> Set.empty
        VariablePattern _ -> Set.empty
        WildcardPattern -> Set.empty

instance Collectible Production Symbol where
    collect = \case
        FunctionProduction symbol terms ->  Set.insert symbol (Set.unions (map collect terms))
        IntProduction _ -> Set.empty
        Substitution _ -> Set.empty

instance Collectible Token Var where
    collect = const Set.empty

instance Collectible Pattern Var where
    collect = \case
        FunctionPattern _ terms -> Set.unions (map collect terms)
        IntPattern _ -> Set.empty
        VariablePattern var -> Set.singleton var
        WildcardPattern -> Set.empty

instance Collectible Production Var where
    collect = \case
        FunctionProduction _ terms -> Set.unions (map collect terms)
        IntProduction _ -> Set.empty
        Substitution var -> Set.singleton var
