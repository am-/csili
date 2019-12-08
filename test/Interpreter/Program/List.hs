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
reverseEmptyList = testProgramAgainstMarking "Reverse (empty)" reverseProgram marking expectation
  where
    marking = Map.fromList [(Place "input", nil)]
    expectation = Map.fromList [(Place "output", nil)]

reverseNonEmptyList :: TestTree
reverseNonEmptyList = testProgramAgainstMarking "Reverse (non-empty)" reverseProgram marking expectation
  where
    list = cons (IntToken 1) $ cons (IntToken 2) $ cons (IntToken 3) $ nil
    reversedList = cons (IntToken 3) $ cons (IntToken 2) $ cons (IntToken 1) $ nil
    marking = Map.fromList [(Place "input", list)]
    expectation = Map.fromList [(Place "output", reversedList)]

nil :: Token
nil = FunctionToken (Symbol "nil") []

cons :: Token -> Token -> Token
cons x xs =  FunctionToken (Symbol "cons") [x, xs]
