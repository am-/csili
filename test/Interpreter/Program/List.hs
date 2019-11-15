module Interpreter.Program.List where

import qualified Data.Map.Strict as Map
import Test.Tasty

import Csili.Program

import Interpreter.Program.Test

tests :: TestTree
tests = testGroup "List"
    [ reverseEmptyList
    , reverseNonEmptyList
    ]

reverseProgram :: FilePath
reverseProgram = "examples/list/reverse.csl"

reverseEmptyList :: TestTree
reverseEmptyList = testProgram "Reverse (empty)" reverseProgram marking expectation
  where
    marking = Map.fromList [(Place "input", nil)]
    expectation = Map.fromList [(Place "accumulator", nil), (Place "output", nil)]


reverseNonEmptyList :: TestTree
reverseNonEmptyList = testProgram "Reverse (non-empty)" reverseProgram marking expectation
  where
    list = cons (IntTerm 1) $ cons (IntTerm 2) $ cons (IntTerm 3) $ nil
    reversedList = cons (IntTerm 3) $ cons (IntTerm 2) $ cons (IntTerm 1) $ nil
    marking = Map.fromList [(Place "input", list)]
    expectation = Map.fromList [(Place "accumulator", nil), (Place "output", reversedList)]

nil :: Term
nil = Function (Symbol "nil") []

cons :: Term -> Term -> Term
cons x xs =  Function (Symbol "cons") [x, xs]
