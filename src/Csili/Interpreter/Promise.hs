{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Csili.Interpreter.Promise
( fetchFutures
, substituteFutures
, processPromises
) where

import Control.Concurrent
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO.Unsafe (unsafePerformIO)

import Csili.Frontend.Unparser
import Csili.Semantics
import Csili.Interpreter.Promise.IO

--------------------------------------------------------------------------------
-- MVars internal to this module
--------------------------------------------------------------------------------

counter :: MVar Int
counter = unsafePerformIO (newMVar 0)

queue :: MVar [(Int, Term)]
queue = unsafePerformIO (newMVar [])

threads :: MVar (Map ThreadId Int)
threads = unsafePerformIO (newMVar Map.empty)

signal :: MVar ()
signal = unsafePerformIO (newEmptyMVar)

--------------------------------------------------------------------------------
-- Promise/Future handling
--------------------------------------------------------------------------------

fetchFutures :: IO (IntMap Term)
fetchFutures = do
    futures <- IntMap.fromList <$> takeMVar queue
    putMVar queue []
    if not . IntMap.null $ futures then return futures else do
        threadIds <- readMVar threads
        if Map.null threadIds then return futures else do
            takeMVar signal
            fetchFutures

substituteFutures :: IntMap Term -> Term -> Term
substituteFutures termMap = \case
    Function symbol terms -> Function symbol (map (substituteFutures termMap) terms)
    var@(Variable _) -> var
    promise@(Promise _ _) -> promise
    future@(Future n) -> IntMap.findWithDefault future n termMap

generateFuture :: IO Int
generateFuture = do
    futureNo <- takeMVar counter
    putMVar counter $! futureNo + 1
    return futureNo

processPromises :: Term -> IO Term
processPromises = \case
    Function symbol terms -> Function symbol <$> mapM processPromises terms
    var@(Variable _) -> return var
    Promise resource arguments -> Future <$> promise resource arguments
    future@(Future _) -> return future

promise :: Resource -> [Term] -> IO Int
promise resource arguments = do
    futureId <- generateFuture
    threadIds <- takeMVar threads
    threadId <- forkIO $ evaluatePromise futureId (dispatchPromise resource arguments)
    putMVar threads $ Map.insert threadId futureId threadIds
    return futureId

evaluatePromise :: Int -> IO Term -> IO ()
evaluatePromise futureId promisedTerm = do
    term <- promisedTerm
    
    futures <- takeMVar queue
    putMVar queue $ futures ++ [(futureId, term)]
    
    newThreadIds <- Map.delete <$> myThreadId <*> takeMVar threads
    putMVar threads newThreadIds
    
    tryPutMVar signal ()
    return ()

dispatchPromise :: Resource -> [Term] -> IO Term
dispatchPromise (Resource name) = case name of
    "stdin" -> dispatchStdin
    "stdout" -> dispatchStdout
    _ -> error $ "Unknown resource `" ++ T.unpack name ++ "`"
