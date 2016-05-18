
module Csili.Backend.Scheduler
( Program(..)
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Csili.Semantics

data DecisionTree
    = Branch (Term, Place) [DecisionTree]
    | Leaf [Transition]

activation :: Semantics -> [Transition] -> DecisionTree
activation = undefined
