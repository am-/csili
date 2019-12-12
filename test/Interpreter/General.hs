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
runRestrictsInputToInterface = testProgramAgainstMarking "Input Restriction to Interface" reverseProgram marking expectation
  where
    marking = Map.fromList [(Place "accumulator", nil), (Place "input", nil)]
    expectation = Map.fromList [(Place "output", nil)]

runRestrictsOutputToInterface :: TestTree
runRestrictsOutputToInterface = testProgramAgainstMarking "Output Restriction to Interface" reverseProgram marking expectation
  where
    marking = Map.fromList [(Place "input", nil)]
    expectation = Map.fromList [(Place "output", nil)]

evaluateIgnoresInterface :: TestTree
evaluateIgnoresInterface = testCase "Evaluate Ignores Interface" $ evaluate program >>= (expectation @=?) 
  where
    program = empty
        { initialMarking = Map.fromList [(Place "input", true), (Place "store", false)]
        , interface = Interface
            { input = Set.fromList [Place "input"]
            , output = Set.fromList [Place "output"]
            }
        , transitions = Set.singleton transition
        }
    transition = (mkTransition "store")
        { patterns = Map.fromList [(Place "input", VariablePattern (Var "New")), (Place "store", VariablePattern (Var "Old"))]
        , productions = Map.fromList [(Place "output", Construct $ Substitution (Var "Old")), (Place "store", Construct $ Substitution (Var "New"))]
        }
    expectation = Map.fromList [(Place "output", false), (Place "store", true)]
