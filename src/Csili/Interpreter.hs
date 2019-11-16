module Csili.Interpreter
( Marking
, run
, evaluate
, isEnabled
, fire
, match
, substitute
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (isJust, mapMaybe)

import Csili.Program

run :: Program -> Marking -> Marking
run program = calculateFinalMarking . evaluate . replaceMarking program . calculateInitialMarking
  where
    calculateInitialMarking = Map.union (initialMarking program) . flip Map.restrictKeys (input $ interface program)
    calculateFinalMarking = flip Map.restrictKeys (output $ interface program)

evaluate :: Program -> Marking
evaluate program = go (initialMarking program)
  where
   go marking = case mapMaybe (fire $ replaceMarking program marking) (findEnabledTransitions marking) of
       [] -> marking
       newMarking:_ -> go newMarking
   findEnabledTransitions marking = filter (isEnabled $ replaceMarking program marking) allTransitions
   allTransitions = Set.toList $ transitions program

isEnabled :: Program -> Transition -> Bool
isEnabled program = isJust . bindVariables program

fire :: Program -> Transition -> Maybe Marking
fire program transition = bindVariables program transition >>= applyBinding program transition

bindVariables :: Program -> Transition -> Maybe (Map Var Term)
bindVariables program transition
    | not isPresetMarked = Nothing
    | isPostsetBlocked = Nothing
    | otherwise = matchPatterns
  where
    marking = initialMarking program
    preset = Map.findWithDefault Map.empty transition (patterns program)
    postset = Map.findWithDefault Map.empty transition (productions program)
    isPresetMarked = Map.null $ Map.difference preset marking
    isPostsetBlocked = not . Map.null . Map.intersection marking $ Map.difference postset preset
    matchPatterns = fmap Map.unions . sequence . Map.elems $ Map.intersectionWith match preset marking

applyBinding :: Program -> Transition -> Map Var Term -> Maybe Marking
applyBinding program transition binding = calculateMarking <$> mapM (substitute binding) postset
  where
    marking = initialMarking program
    preset = Map.findWithDefault Map.empty transition (patterns program)
    postset = Map.findWithDefault Map.empty transition (productions program)
    calculateMarking = Map.unionWith (const id) (Map.difference marking preset)

match :: Term -> Term -> Maybe (Map Var Term)
match pattern term = case pattern of
    Function patternSymbol patternTerms -> case term of
        Function termSymbol termTerms
            | patternSymbol /= termSymbol -> Nothing
            | length patternTerms /= length termTerms -> Nothing
            | otherwise -> fmap Map.unions . sequence $ zipWith match patternTerms termTerms
        IntTerm _ -> Nothing
        Variable _ -> Nothing
        Wildcard -> Nothing
    IntTerm patternInt -> case term of
        Function _ _ -> Nothing
        IntTerm termInt
            | patternInt == termInt -> Just Map.empty
            | otherwise -> Nothing
        Variable _ -> Nothing
        Wildcard -> Nothing
    Variable var -> Just $ Map.singleton var term
    Wildcard -> Just Map.empty

substitute :: Map Var Term -> Term -> Maybe Term
substitute binding = \case
    Function symbol terms -> Function symbol <$> mapM (substitute binding) terms
    term@(IntTerm _) -> Just term
    Variable var -> Map.lookup var binding
    Wildcard -> Nothing
