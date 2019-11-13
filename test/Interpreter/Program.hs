module Interpreter.Program where

import Test.Tasty

import Interpreter.Program.Bool

tests :: TestTree
tests = testGroup "Program"
    [ Interpreter.Program.Bool.tests
    ]