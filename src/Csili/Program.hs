{-# LANGUAGE MultiParamTypeClasses #-}

module Csili.Program
( Program(..)
, empty
, emptyTransitions
, emptyMarking

, Place(..)
, Transition(..)
, TransitionName(..)
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
    , transitions :: Set Transition
    } deriving (Show, Eq)

newtype TransitionName = TransitionName Text
    deriving (Show, Eq, Ord)

data Transition = Transition
    { name :: TransitionName
    , patterns :: Map Place Pattern
    , productions :: Map Place Production
    } deriving (Show, Eq, Ord)

newtype Symbol = Symbol Text
    deriving (Show, Eq, Ord)

newtype Var = Var Text
    deriving (Show, Eq, Ord)

newtype Place = Place Text
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
    , initialMarking = emptyMarking
    , transitions = emptyTransitions
    }

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
