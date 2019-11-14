module Csili.Interpreter
( Marking
, run
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
run program additionalMarking = go (Map.union (initialMarking program) additionalMarking)
  where
    go marking = case mapMaybe (fire (program { initialMarking = marking })) (findEnabledTransitions marking) of
        [] -> marking
        newMarking:_ -> go newMarking
    findEnabledTransitions marking = filter (isEnabled (program { initialMarking = marking })) transitions
    transitions = Set.toList $ Set.union (Map.keysSet (patterns program)) (Map.keysSet (productions program))

isEnabled :: Program -> Transition -> Bool
isEnabled program = isJust . bindVariables program

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

substitute :: Map Var Term -> Term -> Maybe Term
substitute binding = \case
    Variable var -> Map.lookup var binding
    Function symbol terms -> Function symbol <$> mapM (substitute binding) terms
    term@(IntTerm _) -> Just term

fire :: Program -> Transition -> Maybe Marking
fire program transition = bindVariables program transition >>= newMarking
  where
    marking = initialMarking program
    preset = Map.findWithDefault Map.empty transition (patterns program)
    postset = Map.findWithDefault Map.empty transition (productions program)
    newMarking :: Map Var Term -> Maybe Marking
    newMarking binding = case sequence $ Map.map (substitute binding) postset of
        Just tokens -> Just . flip (Map.unionWith (const id)) tokens . Map.difference marking $ preset
        Nothing -> Nothing

type Marking = Map Place Term
