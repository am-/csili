module Csili.Interpreter
( Marking
, run
, isEnabled
, match
, fire
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)

import Csili.Program

run :: Program -> Marking -> Marking
run = undefined

isEnabled :: Program -> Transition -> Bool
isEnabled program transition = and [isPresetMarked, patternsMatch, isPostsetUnmarked]
  where
    marking = initialMarking program
    preset = Map.findWithDefault Map.empty transition (patterns program)
    postset = Map.findWithDefault Map.empty transition (productions program)
    isPresetMarked = Map.null $ Map.difference preset marking
    patternsMatch = isJust . sequence . Map.elems $ Map.intersectionWith match preset marking
    isPostsetUnmarked = Map.null $ Map.intersection marking postset

match :: Term -> Term -> Maybe (Map Var Term)
match pattern term = case pattern of
    Wildcard -> Just Map.empty
    Variable var -> Just $ Map.singleton var term
    IntTerm patternInt -> case term of
        IntTerm termInt
            | patternInt == termInt -> Just Map.empty
            | otherwise -> Nothing
        Function _ _ -> Nothing
    Function patternSymbol patternTerms -> case term of
        IntTerm _ -> Nothing
        Function termSymbol termTerms
            | patternSymbol /= termSymbol -> Nothing
            | length patternTerms /= length termTerms -> Nothing
            | otherwise -> fmap Map.unions . sequence $ zipWith match patternTerms termTerms

fire :: Program -> Transition -> Marking
fire = undefined

type Marking = Map Place Term
