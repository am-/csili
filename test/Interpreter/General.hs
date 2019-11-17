module Interpreter.General where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Tasty
import Test.Tasty.HUnit

import Csili.Program
import Csili.Interpreter

import Interpreter.Program.Test

tests :: TestTree
tests = testGroup "General"
    [ runRestrictsInputToInterface
    , runRestrictsOutputToInterface
    , evaluateIgnoresInterface
    ]

reverseProgram :: FilePath
reverseProgram = "examples/list/reverse.csl"

runRestrictsInputToInterface :: TestTree
runRestrictsInputToInterface = testProgram "Input Restriction to Interface" reverseProgram marking expectation
  where
    marking = Map.fromList [(Place "accumulator", nil), (Place "input", nil)]
    expectation = Map.fromList [(Place "output", nil)]

runRestrictsOutputToInterface :: TestTree
runRestrictsOutputToInterface = testProgram "Output Restriction to Interface" reverseProgram marking expectation
  where
    marking = Map.fromList [(Place "input", nil)]
    expectation = Map.fromList [(Place "output", nil)]

evaluateIgnoresInterface :: TestTree
evaluateIgnoresInterface = testCase "Evaluate Ignores Interface" $ expectation @=? evaluate program
  where
    program = empty
        { initialMarking = Map.fromList [(Place "input", IntToken 1), (Place "store", IntToken 0)]
        , interface = Interface
            { input = Set.fromList [Place "input"]
            , output = Set.fromList [Place "output"]
            }
        , patterns = Map.fromList [(Transition "store", storePattern)]
        , productions = Map.fromList [(Transition "store", storeProduction)]
        }
    storePattern = Map.fromList [(Place "input", VariablePattern (Var "New")), (Place "store", VariablePattern (Var "Old"))]
    storeProduction = Map.fromList [(Place "output", Substitution (Var "Old")), (Place "store", Substitution (Var "New"))]
    expectation = Map.fromList [(Place "output", IntToken 0), (Place "store", IntToken 1)]

nil :: Token
nil = FunctionToken (Symbol "nil") []

