module Interpreter where

import Test.Tasty

import qualified Interpreter.Program
import qualified Interpreter.PatternMatching
import qualified Interpreter.FiringRule
import qualified Interpreter.General

tests :: TestTree
tests = testGroup "Interpreter"
    [ Interpreter.PatternMatching.tests
    , Interpreter.FiringRule.tests
    , Interpreter.General.tests
    , Interpreter.Program.tests
    ]
