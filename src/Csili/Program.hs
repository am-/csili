module Csili.Program
( module Csili.Syntax

, Program(..)
, empty
, transitions
, places
, symbols
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Csili.Syntax

data Program = Program
    { initialMarking :: Map Place Term
    , patterns :: Map Transition (Map Place Term)
    , productions :: Map Transition (Map Place Term)
    } deriving (Show, Eq)

empty :: Program
empty = Program
      { initialMarking = Map.empty
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
