module Interpreter where

import Test.Tasty

import qualified Interpreter.Program
import qualified Interpreter.Enablement
import qualified Interpreter.Matching
import qualified Interpreter.FiringRule

tests :: TestTree
tests = testGroup "Interpreter"
    [ Interpreter.Matching.tests
    , Interpreter.Enablement.tests
    , Interpreter.FiringRule.tests
    , Interpreter.Program.tests
    ]
