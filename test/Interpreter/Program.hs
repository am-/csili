module Interpreter.Program where

import Test.Tasty

import qualified Interpreter.Program.Bool
import qualified Interpreter.Program.List

tests :: TestTree
tests = testGroup "Program"
    [ Interpreter.Program.Bool.tests
    , Interpreter.Program.List.tests
    ]