module Interpreter.Program.Test where

import Test.Tasty
import Test.Tasty.HUnit

import Csili.Frontend (loadCsl)
import Csili.Interpreter

testProgram :: String -> FilePath -> Marking -> Marking -> TestTree
testProgram label programFile marking expectation = testCase label $ loadCsl programFile >>= \case
    Left reason -> assertFailure reason
    Right program -> expectation @=? run program marking
