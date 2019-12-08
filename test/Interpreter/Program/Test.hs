module Interpreter.Program.Test where

import Test.Tasty
import Test.Tasty.HUnit

import Csili.Program
import Csili.Frontend (loadCsl)
import Csili.Interpreter

testProgram :: String -> FilePath -> (Program -> Assertion) -> TestTree
testProgram label programFile assertion = testCase label $ loadCsl programFile >>= \case
    Left errors -> assertFailure $ show errors
    Right program -> assertion program

testProgramAgainstMarking :: String -> FilePath -> Marking -> Marking -> TestTree
testProgramAgainstMarking label programFile marking expectation = testProgram label programFile assertion
  where
    assertion program = run program marking >>= (expectation @=?)
