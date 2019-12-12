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
import Data.Maybe (isJust)
import System.IO (hPutChar, hGetChar)

import Csili.Interpreter.Word
import Csili.Program

run :: Program -> Marking -> IO Marking
run program = fmap calculateFinalMarking . evaluate . replaceMarking program . calculateInitialMarking
  where
    calculateInitialMarking = Map.union (initialMarking program) . flip Map.restrictKeys (input $ interface program)
    calculateFinalMarking = flip Map.restrictKeys (output $ interface program)

evaluate :: Program -> IO Marking
evaluate program = go (initialMarking program) allTransitions
  where
   go marking = \case
       [] -> return marking
       transition:remainingTransitions -> fire marking transition >>= \case
           Nothing -> go marking remainingTransitions
           Just newMarking -> go newMarking allTransitions
   allTransitions = Set.elems $ transitions program

isEnabled :: Marking -> Transition -> Bool
isEnabled marking = isJust . bindVariables marking

fire :: Marking -> Transition -> IO (Maybe Marking)
fire marking transition = maybe (return Nothing) (applyBinding marking transition) $ bindVariables marking transition

bindVariables :: Marking -> Transition -> Maybe (Map Var Token)
bindVariables marking transition
    | not isPresetMarked = Nothing
    | isPostsetBlocked = Nothing
    | otherwise = matchPatterns
  where
    markedPlaces = Map.keysSet marking
    isPresetMarked = Set.null $ Set.difference (prePlaces transition) markedPlaces
    isPostsetBlocked = not . Set.null . Set.intersection (postPlaces transition) . Set.difference markedPlaces $ prePlaces transition
    matchPatterns = fmap Map.unions . sequence . Map.elems $ Map.intersectionWith match (patterns transition) marking

applyBinding :: Marking -> Transition -> Map Var Token -> IO (Maybe Marking)
applyBinding marking transition binding = fmap calculateMarking . sequence <$> mapM (produce binding) postset
  where
    preset = patterns transition
    postset = productions transition
    calculateMarking = flip Map.union (Map.difference marking preset)

produce :: Map Var Token -> Production -> IO (Maybe Token)
produce binding = \case
    Construct rule -> return $ substitute binding rule
    Evaluate effect -> case effect of
        WriteWord8 stream word -> case (,) <$> substitute binding stream <*> substitute binding word of
            Just (Resource handle, word8) -> case fromWord8 word8 of
                Nothing -> return Nothing
                Just char -> do
                    hPutChar handle char
                    return . Just $ FunctionToken (Symbol "signal") []
            _ -> return Nothing
        ReadWord8 stream -> case substitute binding stream of
            Just (Resource handle) -> Just . toWord8 <$> hGetChar handle
            _ -> return Nothing

match :: Pattern -> Token -> Maybe (Map Var Token)
match pattern token = case pattern of
    FunctionPattern patternSymbol patternTerms -> case token of
        FunctionToken termSymbol termTerms
            | patternSymbol /= termSymbol -> Nothing
            | length patternTerms /= length termTerms -> Nothing
            | otherwise -> fmap Map.unions . sequence $ zipWith match patternTerms termTerms
        Resource _ -> Nothing
    VariablePattern var -> Just $ Map.singleton var token
    WildcardPattern -> Just Map.empty

substitute :: Map Var Token -> ConstructionRule -> Maybe Token
substitute binding = \case
    FunctionConstruction symbol terms -> FunctionToken symbol <$> mapM (substitute binding) terms
    Substitution var -> Map.lookup var binding
