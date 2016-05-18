
module Csili.Backend.AbstractRewritingMachine.Types
( Instruction(..)
) where

import Csili.Types

data Instruction
    = Continuation Symbol Symbol [Var] [Var] Term
    | Return Symbol Var [Var] 
    | Match Symbol Symbol [Var] [Var] Term
    | Add Symbol Symbol [Var] [Var] Var
    | Delete Symbol Symbol [Var] [Var] [Var]
    deriving (Show, Eq, Ord)
