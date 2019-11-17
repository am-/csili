module Frontend where

import qualified Data.Map.Strict as Map (empty, fromList)
import qualified Data.Set as Set (fromList)
import Test.Tasty
import Test.Tasty.HUnit

import Csili.Program
import Csili.Frontend

import qualified Frontend.Parser

tests :: TestTree
tests = testGroup "Frontend"
    [ Frontend.Parser.tests
    , programs
    ]

programs :: TestTree
programs = testGroup "Program"
    [ testCase "and.csl" andProgram
    ]

andProgram :: Assertion
andProgram = loadCsl "examples/bool/and.csl" >>= \case
    Left reason -> assertFailure reason
    Right program -> do
        expectedInterface @=? interface program
        expectedMarking @=? initialMarking program
        expectedPatterns @=? patterns program
        expectedApplications @=? productions program
  where
    expectedInterface = Interface
        { input = Set.fromList [Place "input1", Place "input2"]
        , output = Set.fromList [Place "output"]
        }
    expectedMarking = Map.empty
    expectedPatterns = Map.fromList
        [ (Transition "firstTrue", Map.fromList [(Place "input1", FunctionPattern (Symbol "true") []), (Place "input2", VariablePattern (Var "B"))])
        , (Transition "firstFalse", Map.fromList [(Place "input1", FunctionPattern (Symbol "false") []), (Place "input2", VariablePattern (Var "B"))])
        ]
    expectedApplications = Map.fromList
        [ (Transition "firstTrue", Map.fromList [(Place "output", Substitution (Var "B"))])
        , (Transition "firstFalse", Map.fromList [(Place "output", FunctionProduction (Symbol "false") [])])
        ]