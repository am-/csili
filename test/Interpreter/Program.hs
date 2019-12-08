module Interpreter.Program where

import Test.Tasty

import qualified Interpreter.Program.Bool
import qualified Interpreter.Program.List
import qualified Interpreter.Program.IO

tests :: TestTree
tests = testGroup "Program"
    [ Interpreter.Program.Bool.tests
    , Interpreter.Program.List.tests
    , Interpreter.Program.IO.tests
    ]