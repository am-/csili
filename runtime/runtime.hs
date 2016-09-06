
module Main
( main
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forever, when)
import Foreign.StablePtr

main :: IO ()
main = do
    promiseQueue <- atomically newTQueue
    installNet promiseQueue
    let shutdownGracefully :: BlockedIndefinitelyOnSTM -> IO ()
        shutdownGracefully = const (return ())
    handle shutdownGracefully $ forever $ do
        Promise place action <- atomically (readTQueue promiseQueue)
        case action of
            PutStrLn StdOut term -> do
                putStrLn (show term)
                atomically $ produce place TermUnit
            ReadInt StdIn -> do
                n <- read <$> getLine
                let n' = foldr ($) TermZero (replicate n TermSucc)
                atomically $ produce place n'

data Promise = Promise (TVar Place) Action

data Resource
    = StdIn
    | StdOut

data Action
    = PutStrLn Resource Term
    | ReadInt Resource

data Place
    = EmptyPlace
    | PlaceWithTerm Term
    | PlaceWithFuture

installPlace :: IO (TVar Place)
installPlace = atomically (newTVar EmptyPlace)

installTransition :: String -> STM a -> IO ThreadId
installTransition name transaction = forkIO $ forever $ do
    atomically transaction

consume :: TVar Place -> STM Term
consume place = do
    content <- readTVar place
    case content of
        EmptyPlace -> retry
        PlaceWithFuture -> retry
        PlaceWithTerm term -> do
            writeTVar place EmptyPlace
            return term

produce :: TVar Place -> Term -> STM ()
produce place = writeTVar place . PlaceWithTerm . normalize

promise :: TQueue Promise -> TVar Place -> Action -> STM ()
promise queue place action = do
    writeTQueue queue (Promise place action)
    writeTVar place PlaceWithFuture

--------------------------------------------------------------------------------
-- Automatically generated code
--------------------------------------------------------------------------------

data Term
    = TermUnit
    | TermFalse
    | TermTrue
    | TermNot Term
    | TermAnd Term Term
    | TermOr Term Term
    | TermPlus Term Term
    | TermEquals Term Term
    | TermZero
    | TermSucc Term
    | TermGuess
    | TermLost
    | TermWon
    deriving (Show)

normalize :: Term -> Term
normalize term = maybe term normalize . rewrite $ case term of
    TermUnit -> TermUnit
    TermFalse -> TermFalse
    TermTrue -> TermTrue
    TermNot term1 -> TermNot (normalize term1)
    TermAnd term1 term2 -> TermAnd (normalize term1) (normalize term2)
    TermOr term1 term2 -> TermOr (normalize term1) (normalize term2)
    TermPlus term1 term2 -> TermPlus (normalize term1) (normalize term2)
    TermEquals term1 term2 -> TermEquals (normalize term1) (normalize term2)
    TermZero -> TermZero
    TermSucc term1 -> TermSucc (normalize term1)
    TermGuess -> TermGuess
    TermLost -> TermLost
    TermWon -> TermWon

rewrite :: Term -> Maybe Term
rewrite term = case term of
    TermNot TermTrue -> Just $ TermFalse
    TermNot TermFalse -> Just $ TermTrue
    TermAnd TermTrue bool -> Just $ bool
    TermAnd TermFalse bool -> Just $ TermFalse
    TermOr TermTrue bool -> Just $ TermTrue
    TermOr TermFalse bool -> Just $ bool
    TermPlus TermZero y -> Just $ y
    TermPlus (TermSucc x) y -> Just $ TermSucc (TermPlus x y)
    TermEquals TermZero TermZero -> Just $ TermTrue
    TermEquals TermZero (TermSucc y) -> Just $ TermFalse
    TermEquals (TermSucc x) TermZero -> Just $ TermFalse
    TermEquals (TermSucc x) (TermSucc y) -> Just $ TermEquals x y
    _ -> Nothing

installNet :: TQueue Promise -> IO ()
installNet promiseQueue = do
    placeNumber <- installPlace
    placeTries <- installPlace
    placeRetry <- installPlace
    placeWaiting <- installPlace
    placeGuess <- installPlace
    placeGuessed <- installPlace
    placeOutcome <- installPlace
    
    _ <- installTransition "ask" $ do
        tokenRetry <- consume placeRetry
        () <- case tokenRetry of
            TermTrue -> return ()
            _ -> retry
        promise promiseQueue placeWaiting (PutStrLn StdOut TermGuess)
    
    _ <- installTransition "guess" $ do
        tokenWaiting <- consume placeWaiting
        () <- case tokenWaiting of
            TermUnit -> return ()
            _ -> retry
        promise promiseQueue placeGuess (ReadInt StdIn)
    
    _ <- installTransition "check" $ do
        tokenNumber <- consume placeNumber
        (x) <- case tokenNumber of
            x -> return (x)
            _ -> retry
        tokenGuess <- consume placeGuess
        (y) <- case tokenGuess of
            y -> return (y)
            _ -> retry
        produce placeNumber x
        produce placeGuessed (TermEquals x y)
    
    _ <- installTransition "repeat" $ do
        tokenGuessed <- consume placeGuessed
        () <- case tokenGuessed of
            TermFalse -> return ()
            _ -> retry
        tokenTries <- consume placeTries
        (x) <- case tokenTries of
            TermSucc x -> return (x)
            _ -> retry
        produce placeTries x
        produce placeRetry (TermNot (TermEquals TermZero x))
    
    _ <- installTransition "lose" $ do
        tokenRetry <- consume placeRetry
        () <- case tokenRetry of
            TermFalse -> return ()
            _ -> retry
        promise promiseQueue placeOutcome (PutStrLn StdOut TermLost)
    
    _ <- installTransition "win" $ do
        tokenGuessed <- consume placeGuessed
        () <- case tokenGuessed of
            TermTrue -> return ()
            _ -> retry
        promise promiseQueue placeOutcome (PutStrLn StdOut TermWon)

    atomically $ do
        produce placeNumber . TermSucc . TermSucc . TermSucc . TermSucc . TermSucc $ TermZero
        produce placeTries . TermSucc . TermSucc . TermSucc $ TermZero
        produce placeRetry TermTrue
