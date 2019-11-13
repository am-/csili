module Frontend.Parser where

import Data.Text ()
import qualified Data.Text.IO as T (readFile)
import Data.Attoparsec.Text (parseOnly)
import qualified Data.Map.Strict as Map (empty, fromList)
import Test.Tasty
import Test.Tasty.HUnit

import Csili.Frontend.Parser
import Csili.Program

tests :: TestTree
tests = testGroup "Parser"
    [ terms
    , markingBlocks
    , transitionBlocks
    , programs
    ]

terms :: TestTree
terms = testGroup "Terms"
    [ testCase "Single Letter Variable" singleLetterVariable
    , testCase "Multi Letter Variable" multiLetterVariable
    , testCase "Constant (without parentheses)" constantWithoutParentheses
    , testCase "Constant (with parentheses)" constantWithParentheses
    , testCase "Function" function
    , testCase "Wildcard (without name)" wildcardWithoutName
    , testCase "Wildcard (with name)" wildcardWithName
    , testCase "Wildcard (within function)" wildcardWithinFunction
    , testGroup "Integers"
      [ testCase "Zero" $ Right (IntTerm 0) @=? parseOnly term "0"
      , testCase "Zero (positive)" $ Right (IntTerm 0) @=? parseOnly term "+0"
      , testCase "Zero (negative)" $ Right (IntTerm 0) @=? parseOnly term "-0"
      , testCase "Positive" $ Right (IntTerm 42) @=? parseOnly term "42"
      , testCase "Positive (signed)" $ Right (IntTerm 23) @=? parseOnly term "+23"
      , testCase "Negative" $ Right (IntTerm (-273)) @=? parseOnly term "-273"
      ]
    , testGroup "Hexadecimals"
      [ testCase "Zero" $ Right (IntTerm 0) @=? parseOnly term "0x0"
      , testCase "Zero (positive)" $ Right (IntTerm 0) @=? parseOnly term "+0x0"
      , testCase "Zero (negative)" $ Right (IntTerm 0) @=? parseOnly term "-0x0"
      , testCase "Positive" $ Right (IntTerm 42) @=? parseOnly term "0x2A"
      , testCase "Positive (signed)" $ Right (IntTerm 23) @=? parseOnly term "+0x17"
      , testCase "Negative" $ Right (IntTerm (-273)) @=? parseOnly term "-0x0111"
      , testCase "Uppercase and Lowercase Equivalence" $ parseOnly term "0xAB" @=? parseOnly term "0xab"
      ]
    ]

singleLetterVariable :: Assertion
singleLetterVariable = Right (Variable (Var "A")) @=? parseOnly term "A"

multiLetterVariable :: Assertion
multiLetterVariable = Right (Variable (Var "Foobar")) @=? parseOnly term "Foobar"

constantWithoutParentheses :: Assertion
constantWithoutParentheses = Right (Function (Symbol "foobar") []) @=? parseOnly term "foobar"

constantWithParentheses :: Assertion
constantWithParentheses = Right (Function (Symbol "foobar") []) @=? parseOnly term "foobar()"

function :: Assertion
function = Right (Function (Symbol "cons") [headVar, tailVar]) @=? parseOnly term "cons(Head, Tail)"
  where
    headVar = Variable (Var "Head")
    tailVar = Variable (Var "Tail")

wildcardWithoutName :: Assertion
wildcardWithoutName = Right Wildcard @=? parseOnly term "_"

wildcardWithName :: Assertion
wildcardWithName = Right Wildcard @=? parseOnly term "_foobar"

wildcardWithinFunction :: Assertion
wildcardWithinFunction = Right (Function (Symbol "cons") [Wildcard, Wildcard]) @=? parseOnly term "cons(_Head, _)"

markingBlocks :: TestTree
markingBlocks = testGroup "Marking"
    [ testCase "Empty" emptyMarking
    , testCase "Non-Empty" marking
    ]

emptyMarking :: Assertion
emptyMarking = Right expectation @=? parseOnly markingBlock "MARKING {}"
  where
    expectation = Map.fromList []

marking :: Assertion
marking = Right expectation @=? parseOnly markingBlock "MARKING { input1: true input2: false }"
  where
    expectation = Map.fromList [(Place "input1", Function (Symbol "true") []), (Place "input2", Function (Symbol "false") [])]

transitionBlocks :: TestTree
transitionBlocks = testGroup "Transition"
    [ testCase "Isolated" isolatedTransition
    , testCase "Infinite Producer" infiniteProducer
    , testCase "Unconditional Eater" unconditionalEater
    , testCase "Common" commonTransition
    ]

isolatedTransition :: Assertion
isolatedTransition = Right expectation @=? parseOnly transitionBlock "TRANSITION doNothing {}"
  where
    expectation = (Transition "doNothing", (match, produce))
    match = Map.fromList []
    produce = Map.fromList []

infiniteProducer :: Assertion
infiniteProducer = Right expectation @=? parseOnly transitionBlock "TRANSITION InfiniteProducer { PRODUCE { output: true } }"
  where
    expectation = (Transition "InfiniteProducer", (match, produce))
    match = Map.fromList []
    produce = Map.fromList [(Place "output", Function (Symbol "true") [])]

unconditionalEater :: Assertion
unconditionalEater = Right expectation @=? parseOnly transitionBlock "TRANSITION UnconditionalEater { MATCH { input: _ } }"
  where
    expectation = (Transition "UnconditionalEater", (match, produce))
    match = Map.fromList [(Place "input", Wildcard)]
    produce = Map.fromList []

commonTransition :: Assertion
commonTransition = Right expectation @=? parseOnly transitionBlock "TRANSITION firstTrue { MATCH { input1: true input2: B } PRODUCE { output: B } }"
  where
    expectation = (Transition "firstTrue", (match, produce))
    match = Map.fromList [(Place "input1", Function (Symbol "true") []), (Place "input2", Variable (Var "B"))]
    produce = Map.fromList [(Place "output", Variable (Var "B"))]

programs :: TestTree
programs = testGroup "Program"
    [ testCase "and.csl" andProgram
    ]

andProgram :: Assertion
andProgram = parseProgram <$> T.readFile "examples/bool/and.csl" >>= \case
    Left reason -> assertFailure reason
    Right program -> do
        expectedMarking @=? initialMarking program
        expectedPatterns @=? patterns program
        expectedApplications @=? productions program
  where
    expectedMarking = Map.empty
    expectedPatterns = Map.fromList
        [ (Transition "firstTrue", Map.fromList [(Place "input1", Function (Symbol "true") []), (Place "input2", Variable (Var "B"))])
        , (Transition "firstFalse", Map.fromList [(Place "input1", Function (Symbol "false") []), (Place "input2", Variable (Var "B"))])
        ]
    expectedApplications = Map.fromList
        [ (Transition "firstTrue", Map.fromList [(Place "output", Variable (Var "B"))])
        , (Transition "firstFalse", Map.fromList [(Place "output", Function (Symbol "false") [])])
        ]