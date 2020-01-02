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
    marking = Map.fromList [(LocalPlace "accumulator", nil), (LocalPlace "input", nil)]
    expectation = Map.fromList [(LocalPlace "output", nil)]

runRestrictsOutputToInterface :: TestTree
runRestrictsOutputToInterface = testProgramAgainstMarking "Output Restriction to Interface" reverseProgram marking expectation
  where
    marking = Map.fromList [(LocalPlace "input", nil)]
    expectation = Map.fromList [(LocalPlace "output", nil)]

evaluateIgnoresInterface :: TestTree
evaluateIgnoresInterface = testCase "Evaluate Ignores Interface" $ evaluate program >>= (expectation @=?) 
  where
    program = emptyProgram
        { mainNet = emptyNet
            { initialMarking = Map.fromList [(LocalPlace "input", true), (LocalPlace "store", false)]
            , interface = Interface
                { input = Set.fromList [LocalPlace "input"]
                , output = Set.fromList [LocalPlace "output"]
                }
            , transitions = Set.singleton transition
            }        
        }
    transition = (mkTransition "store")
        { patterns = Map.fromList [(LocalPlace "input", VariablePattern (Var "New")), (LocalPlace "store", VariablePattern (Var "Old"))]
        , productions = Map.fromList [(LocalPlace "output", Construct $ Substitution (Var "Old")), (LocalPlace "store", Construct $ Substitution (Var "New"))]
        }
    expectation = Map.fromList [(LocalPlace "output", false), (LocalPlace "store", true)]
