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
        { initialMarking = Map.fromList [(Place "input", IntTerm 1), (Place "store", IntTerm 0)]
        , interface = Interface
            { input = Set.fromList [Place "input"]
            , output = Set.fromList [Place "output"]
            }
        , patterns = Map.fromList [(Transition "store", storePattern)]
        , productions = Map.fromList [(Transition "store", storeProduction)]
        }
    storePattern = Map.fromList [(Place "input", Variable (Var "New")), (Place "store", Variable (Var "Old"))]
    storeProduction = Map.fromList [(Place "output", Variable (Var "Old")), (Place "store", Variable (Var "New"))]
    expectation = Map.fromList [(Place "output", IntTerm 0), (Place "store", IntTerm 1)]

nil :: Term
nil = Function (Symbol "nil") []

