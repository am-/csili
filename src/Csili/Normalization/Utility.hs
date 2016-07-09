{-# LANGUAGE OverloadedStrings #-}

module Csili.Normalization.Utility
( unit
) where

import Csili.Semantics

unit :: Term
unit = Function (Symbol "unit") []
