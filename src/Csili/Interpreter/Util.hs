{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Csili.Interpreter.Util
( nat2term
, term2nat
) where

import Control.Monad ((<$!>))

import Csili.Types

nat2term :: Int -> Term
nat2term n
    | n > 0  = Function (Symbol "succ") [nat2term (n-1)]
    | n == 0 = Function (Symbol "zero") []

term2nat :: Term -> Maybe Int
term2nat = \case
    Function (Symbol "zero") [] -> Just 0
    Function (Symbol "succ") [term] -> (+1) <$!> term2nat term
    _ -> Nothing
