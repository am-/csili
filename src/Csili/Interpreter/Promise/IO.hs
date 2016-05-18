{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Csili.Interpreter.Promise.IO
( dispatchStdin
, dispatchStdout  
) where

import qualified Data.Text.IO as T

import Csili.Types
import Csili.Frontend.Unparser
import Csili.Interpreter.Util

dispatchStdin :: [Term] -> IO Term
dispatchStdin = \case
    Function (Symbol "readInt") []:_ ->
        nat2term . read <$> getLine
    terms -> error $ "Unknown terms: " ++ show terms

dispatchStdout :: [Term] -> IO Term
dispatchStdout = \case
    [Function (Symbol "putStrLn") [], term] -> do
        T.putStrLn (unparseTerm term)
        return $ Function (Symbol "unit") []
    terms -> error $ "Unknown terms: " ++ show terms
