module Frontend.Program where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Test.Tasty
import Test.Tasty.HUnit

import Csili.Program
import Csili.Frontend

import qualified Frontend.Program.TokenType as TokenType
import qualified Frontend.Program.Places as Places
import qualified Frontend.Program.Marking as Marking
import qualified Frontend.Program.Transitions as Transitions
import qualified Frontend.Program.Templates as Templates

tests :: TestTree
tests = testGroup "Program"
    [ programTests
    , TokenType.tests
    , Templates.tests
    , Places.tests
    , Marking.tests
    , Transitions.tests
    ]

programTests :: TestTree
programTests = testGroup "Program"
    [ testCase "reverse.csl" reverseProgram
    , testCase "Unparseable Program" unparseableProgram
    ]

reverseProgram :: Assertion
reverseProgram = loadCsl "examples/list/reverse.csl" >>= \case
    Left _ -> assertFailure "Couldn't load program."
    Right program -> do
        let net = mainNet program
        expectedInterface @=? interface net
        expectedInternalPlaces @=? internalPlaces net
        expectedMarking @=? initialMarking net
        expectedTransitions @=? transitions net
  where
    expectedInterface = Interface
        { input = Set.fromList [LocalPlace "input"]
        , output = Set.fromList [LocalPlace "output"]
        }
    expectedInternalPlaces = Set.fromList [LocalPlace "original", LocalPlace "reversed"]
    expectedMarking = Map.fromList [(LocalPlace "reversed", nil)]
    expectedTransitions = Set.fromList [startTransition, addElementTransition, returnTransition]
    startTransition = (mkTransition "start")
        { patterns = Map.fromList [(LocalPlace "input", VariablePattern (Var "List"))]
        , productions = Map.fromList [(LocalPlace "original", Construct $ Substitution (Var "List"))]
        }
    addElementTransition = (mkTransition "addElement")
        { patterns = Map.fromList [(LocalPlace "original", FunctionPattern (Symbol "cons") [VariablePattern (Var "Head"), VariablePattern (Var "Tail")]), (LocalPlace "reversed", VariablePattern (Var "ReversedList"))]
        , productions = Map.fromList [(LocalPlace "original", Construct $ Substitution (Var "Tail")), (LocalPlace "reversed", Construct $ FunctionConstruction (Symbol "cons") [Substitution (Var "Head"), Substitution (Var "ReversedList")])]
        }
    returnTransition = (mkTransition "return")
        { patterns = Map.fromList [(LocalPlace "original", FunctionPattern (Symbol "nil") []), (LocalPlace "reversed", VariablePattern (Var "ReversedList"))]
        , productions = Map.fromList [(LocalPlace "output", Construct $ Substitution (Var "ReversedList")), (LocalPlace "reversed", Construct $ FunctionConstruction (Symbol "nil") [])]
        }

unparseableProgram :: Assertion
unparseableProgram = Left expected @=? parseCsl "MAKRING"
  where
    expected = [DuringParsing "endOfInput"]
