
module Csili.Interpreter
( normalize
, interpret
, fromSemantics
) where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text.IO as T

import Csili.Interpreter.Promise
import Csili.Interpreter.TermRewriting
import Csili.Frontend.Unparser
import Csili.Semantics

interpret :: [Rule] -> Term -> IO Term
interpret ruleList term = do
    term' <- processPromises (normalize ruleList term)
    T.putStrLn $ unparseTerm term'
    futures <- fetchFutures
    if IntMap.null futures then return term'
        else interpret ruleList (substituteFutures futures term')
