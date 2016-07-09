{-# LANGUAGE OverloadedStrings #-}

module Csili.Normalization.Marking
( normalize
) where

import qualified Data.Map as Map

import Csili.Normalization.Utility
import Csili.Semantics

normalize :: Semantics -> Semantics
normalize sem = sem
    { marking = newMarking
    , patterns = Map.insert initializer newMarking (patterns sem)
    , applications = Map.insert initializer (Map.map EffectFree . marking $ sem) (applications sem)
    }
  where
    initializer = Transition "__initialize"
    newMarking = Map.singleton (Place "__uninitialized") unit
