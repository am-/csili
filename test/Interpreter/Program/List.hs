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
    x = FunctionToken (Symbol "x") []
    y = FunctionToken (Symbol "y") []
    z = FunctionToken (Symbol "z") []
    list = cons x . cons y . cons z $ nil
    reversedList = cons z . cons y . cons x $ nil
    marking = Map.fromList [(Place "input", list)]
    expectation = Map.fromList [(Place "output", reversedList)]
