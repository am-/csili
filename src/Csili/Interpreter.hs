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
import Data.Set (Set)
import Data.Maybe (isJust)
import System.IO (hPutChar, hGetChar)
import Control.Concurrent.MVar (newEmptyMVar, tryTakeMVar, MVar, putMVar, takeMVar)
import Control.Concurrent (threadDelay, forkIO)
import qualified Data.DList as DList

import Csili.Interpreter.Word
import Csili.Program

run :: Program -> Marking -> IO Marking
run program = fmap calculateFinalMarking . evaluate . replaceMarking . calculateInitialMarking
  where
    calculateInitialMarking = Map.union (initialMarking net) . flip Map.restrictKeys (input $ interface net)
    calculateFinalMarking = flip Map.restrictKeys (output $ interface net)
    net = mainNet program
    replaceMarking newMarking = program { mainNet = net { initialMarking = newMarking }}

evaluate :: Program -> IO Marking
evaluate program = case flatten (templates program) (mainNet program) of
    Nothing -> return . initialMarking $ mainNet program
    Just net -> go Map.empty (initialMarking net) DList.empty (Set.elems $ transitions net)
  where
   go promises marking nextCandidates = \case
       []  | Map.null promises -> return marking
           | otherwise -> threadDelay 100 >> go promises marking DList.empty (DList.toList nextCandidates)
       transition:remainingTransitions -> fireWithPromises transition promises marking >>= \case
           NotFired -> go promises marking (DList.snoc nextCandidates transition) remainingTransitions
           TriggeredEffects promisedTokens -> go
               (Map.insert (name transition) promisedTokens promises)
               marking
               (DList.cons transition nextCandidates)
               remainingTransitions
           FiredTransition tokens -> go
               (Map.delete (name transition) promises)
               (calculateMarking marking (Map.keysSet $ patterns transition) tokens)
               DList.empty
               (DList.toList . DList.append nextCandidates . flip DList.snoc transition $ DList.fromList remainingTransitions)

isEnabled :: Transition -> Marking -> Bool
isEnabled transition = isJust . bindVariables transition

fire :: Transition -> Marking -> IO (Maybe Marking)
fire transition marking = fireWithPromises transition Map.empty marking >>= \case
    NotFired -> return Nothing
    TriggeredEffects promisedTokens -> toNewMarking <$> takeMVar promisedTokens
    FiredTransition tokens -> return $ toNewMarking tokens
  where
    toNewMarking = Just . calculateMarking marking (Map.keysSet $ patterns transition)

calculateMarking :: Marking -> Set Place -> Map Place Token -> Marking
calculateMarking marking preset postset = Map.union postset (Map.withoutKeys marking preset)

data FiringResult
    = NotFired
    | TriggeredEffects (MVar (Map Place Token))
    | FiredTransition (Map Place Token)

fireWithPromises :: Transition -> Map TransitionName (MVar (Map Place Token)) -> Marking -> IO FiringResult
fireWithPromises transition promises marking = case bindVariables transition marking >>= produce transition of
    Nothing -> return NotFired
    Just (tokens, actions)
        | Map.null actions -> return $ FiredTransition tokens
        | otherwise -> case Map.lookup (name transition) promises of
            Nothing -> TriggeredEffects <$> promiseTokens actions
            Just promise -> tryTakeMVar promise >>= \case
                Nothing -> return NotFired
                Just promisedTokens -> return . FiredTransition $ Map.union tokens promisedTokens

promiseTokens :: Map Place (IO Token) -> IO (MVar (Map Place Token))
promiseTokens actions = do
    promisedTokens <- newEmptyMVar
    _ <- forkIO $ do
        effectedMVars <- mapM defer actions
        effectedTokens <- mapM takeMVar effectedMVars
        putMVar promisedTokens effectedTokens
    return promisedTokens
  where
    defer action = do
        mvar <- newEmptyMVar
        _ <- forkIO $ action >>= putMVar mvar
        return mvar

produce :: Transition -> Map Var Token -> Maybe (Map Place Token, Map Place (IO Token))
produce transition binding = (,)
    <$> sequence (Map.mapMaybe extractToken postset)
    <*> sequence (Map.mapMaybe extractEffect postset)
  where
    postset = productions transition
    extractToken = \case
        Construct rule -> Just $ substitute binding rule
        Evaluate _ -> Nothing
    extractEffect = \case
        Construct _ -> Nothing
        Evaluate effect -> Just $ toEffect binding effect

toEffect :: Map Var Token -> Effect -> Maybe (IO Token)
toEffect binding = \case
    WriteWord8 stream word -> case (,) <$> substitute binding stream <*> substitute binding word of
        Just (Resource handle, word8) -> fmap (const blackToken) . hPutChar handle <$> fromWord8 word8
        _ -> Nothing
    ReadWord8 stream -> case substitute binding stream of
        Just (Resource handle) -> Just $ toWord8 <$> hGetChar handle
        _ -> Nothing

substitute :: Map Var Token -> ConstructionRule -> Maybe Token
substitute binding = \case
    FunctionConstruction symbol terms -> FunctionToken symbol <$> mapM (substitute binding) terms
    Substitution var -> Map.lookup var binding

bindVariables :: Transition -> Marking -> Maybe (Map Var Token)
bindVariables transition marking
    | not isPresetMarked = Nothing
    | isPostsetBlocked = Nothing
    | otherwise = matchPatterns
  where
    markedPlaces = Map.keysSet marking
    isPresetMarked = Set.null $ Set.difference (prePlaces transition) markedPlaces
    isPostsetBlocked = not . Set.null . Set.intersection (postPlaces transition) . Set.difference markedPlaces $ prePlaces transition
    matchPatterns = fmap Map.unions . sequence . Map.elems $ Map.intersectionWith match (patterns transition) marking

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
