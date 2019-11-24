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
   go marking = case mapMaybe (fire marking) (findEnabledTransitions marking) of
       [] -> marking
       newMarking:_ -> go newMarking
   findEnabledTransitions marking = filter (isEnabled marking) allTransitions
   allTransitions = Set.elems $ transitions program

isEnabled :: Marking -> Transition -> Bool
isEnabled marking = isJust . bindVariables marking

fire :: Marking -> Transition -> Maybe Marking
fire marking transition = bindVariables marking transition >>= applyBinding marking transition

bindVariables :: Marking -> Transition -> Maybe (Map Var Token)
bindVariables marking transition
    | not isPresetMarked = Nothing
    | isPostsetBlocked = Nothing
    | otherwise = matchPatterns
  where
    preset = patterns transition
    postset = productions transition
    isPresetMarked = Map.null $ Map.difference preset marking
    isPostsetBlocked = not . Map.null . Map.intersection marking $ Map.difference postset preset
    matchPatterns = fmap Map.unions . sequence . Map.elems $ Map.intersectionWith match preset marking

applyBinding :: Marking -> Transition -> Map Var Token -> Maybe Marking
applyBinding marking transition binding = calculateMarking <$> mapM (substitute binding) postset
  where
    preset = patterns transition
    postset = productions transition
    calculateMarking = flip Map.union (Map.difference marking preset)

match :: Pattern -> Token -> Maybe (Map Var Token)
match pattern token = case pattern of
    FunctionPattern patternSymbol patternTerms -> case token of
        FunctionToken termSymbol termTerms
            | patternSymbol /= termSymbol -> Nothing
            | length patternTerms /= length termTerms -> Nothing
            | otherwise -> fmap Map.unions . sequence $ zipWith match patternTerms termTerms
        IntToken _ -> Nothing
    IntPattern patternInt -> case token of
        FunctionToken _ _ -> Nothing
        IntToken termInt
            | patternInt == termInt -> Just Map.empty
            | otherwise -> Nothing
    VariablePattern var -> Just $ Map.singleton var token
    WildcardPattern -> Just Map.empty

substitute :: Map Var Token -> Production -> Maybe Token
substitute binding = \case
    FunctionProduction symbol terms -> FunctionToken symbol <$> mapM (substitute binding) terms
    IntProduction n -> Just (IntToken n)
    Substitution var -> Map.lookup var binding
