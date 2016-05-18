{-# LANGUAGE LambdaCase #-}

module Csili.Semantics
( module Csili.Types
, Semantics(..)
, empty
, transitions
, places
) where

import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)

import Csili.Types

--------------------------------------------------------------------------------
-- Petri Net Semantics
--------------------------------------------------------------------------------

data Semantics = Semantics
    { rules :: [Rule]
    , marking :: Map Place Term
    , patterns :: Map Transition (Map Place Term)
    , applications :: Map Transition (Map Place Term)
    } deriving (Show, Eq)

empty :: Semantics
empty = Semantics
      { rules = []
      , marking = Map.empty
      , patterns = Map.empty
      , applications = Map.empty
      }

isValid :: Rule -> Bool
isValid = uncurry (Set.isSubsetOf `on` collectVariables) . swap

collectVariables :: Term -> Set Var
collectVariables = \case
    Function _ terms -> Set.unions (map collectVariables terms)
    Variable var -> Set.singleton var
    Promise _ _ -> Set.empty

transitions :: Semantics -> Set Transition
transitions sem = Set.unions
                [ Map.keysSet (patterns sem)
                , Map.keysSet (applications sem)
                ]

places :: Semantics -> Set Place
places sem = Set.unions
           [ Map.keysSet (marking sem)
           , Set.unions . map Map.keysSet . Map.elems $ patterns sem
           , Set.unions . map Map.keysSet . Map.elems $ applications sem
           ]
