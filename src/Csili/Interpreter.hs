module Csili.Interpreter
( run
) where

import Data.Map.Strict (Map)

import Csili.Program

run :: Program -> Marking -> Marking
run = undefined

type Marking = Map Place Term
