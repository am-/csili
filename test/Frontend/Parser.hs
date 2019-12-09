module Frontend.Parser where

import Data.Text ()
import Data.Attoparsec.Text (parseOnly, endOfInput)
import Test.Tasty
import Test.Tasty.HUnit

import Csili.Frontend.Parser

tests :: TestTree
tests = testGroup "Parser"
    [ terms
    , interfaceBlocks
    , placeBlocks
    , markingBlocks
    , transitionBlocks
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
    , testGroup "word8"
      [ testCase "4 Bit (too short)" $ Left "endOfInput" @=? parseOnly (term <* endOfInput) "0xA"
      , testCase "8 bit (minimum bound)" $ Right (Function "word8" [zero, zero, zero, zero, zero, zero, zero, zero]) @=? parseOnly (term <* endOfInput) "0x00"
      , testCase "8 bit (in between)" $ Right (Function "word8" [zero, zero, one, zero, one, zero, one, zero]) @=? parseOnly (term <* endOfInput) "0x2A"
      , testCase "8 bit (maximum bound)" $ Right (Function "word8" [one, one, one, one, one, one, one, one]) @=? parseOnly (term <* endOfInput) "0xFF"
      , testCase "8 bit (signed, positive)" $ Left "endOfInput" @=? parseOnly (term <* endOfInput) "+0x17"
      , testCase "8 bit (signed, negative)" $ Left "endOfInput" @=? parseOnly (term <* endOfInput) "-0x17"
      , testCase "12 Bit (too long)" $ Left "endOfInput" @=? parseOnly (term <* endOfInput) "0xABC"
      , testCase "Uppercase and Lowercase Equivalence" $ parseOnly (term <* endOfInput) "0xAB" @=? parseOnly (term <* endOfInput) "0xab"
      ]
    ]
  where
    zero = Function "zero" []
    one = Function "one" []

singleLetterVariable :: Assertion
singleLetterVariable = Right (Variable "A") @=? parseOnly term "A"

multiLetterVariable :: Assertion
multiLetterVariable = Right (Variable "Foobar") @=? parseOnly term "Foobar"

constantWithoutParentheses :: Assertion
constantWithoutParentheses = Right (Function "foobar" []) @=? parseOnly term "foobar"

constantWithParentheses :: Assertion
constantWithParentheses = Right (Function "foobar" []) @=? parseOnly term "foobar()"

function :: Assertion
function = Right (Function "cons" [headVar, tailVar]) @=? parseOnly term "cons(Head, Tail)"
  where
    headVar = Variable "Head"
    tailVar = Variable "Tail"

wildcardWithoutName :: Assertion
wildcardWithoutName = Right Wildcard @=? parseOnly term "_"

wildcardWithName :: Assertion
wildcardWithName = Right Wildcard @=? parseOnly term "_foobar"

wildcardWithinFunction :: Assertion
wildcardWithinFunction = Right (Function "cons" [Wildcard, Wildcard]) @=? parseOnly term "cons(_Head, _)"

interfaceBlocks :: TestTree
interfaceBlocks = testGroup "Interface"
    [ testCase "Empty" emptyInputAndOutputInterface
    , testCase "Empty Input" emptyInputInterface
    , testCase "Empty Output" emptyOutputInterface
    , testCase "1-to-1" oneToOneInterface
    , testCase "2-to-2" twoToTwoInterface
    ]

emptyInputAndOutputInterface :: Assertion
emptyInputAndOutputInterface = Right expectation @=? parseOnly interfaceBlock "INTERFACE {}"
  where
    expectation = ([], [])

emptyInputInterface :: Assertion
emptyInputInterface = Right expectation @=? parseOnly interfaceBlock "INTERFACE { OUTPUT { output } }"
  where
    expectation = ([], ["output"])

emptyOutputInterface :: Assertion
emptyOutputInterface = Right expectation @=? parseOnly interfaceBlock "INTERFACE { INPUT { input } }"
  where
    expectation = (["input"], [])

oneToOneInterface :: Assertion
oneToOneInterface = Right expectation @=? parseOnly interfaceBlock "INTERFACE { INPUT { input } OUTPUT { output } }"
  where
    expectation = (["input"], ["output"])

twoToTwoInterface :: Assertion
twoToTwoInterface = Right expectation @=? parseOnly interfaceBlock "INTERFACE { INPUT { input1 input2 } OUTPUT { output1 output2 } }"
  where
    expectation = (["input1", "input2"], ["output1", "output2"])

placeBlocks :: TestTree
placeBlocks = testGroup "Places"
    [ testCase "Empty" emptyPlaces
    , testCase "Non-Empty" nonEmptyPlaces
    ]

emptyPlaces :: Assertion
emptyPlaces = Right expectation @=? parseOnly placesBlock "PLACES {}"
  where
    expectation = []

nonEmptyPlaces :: Assertion
nonEmptyPlaces = Right expectation @=? parseOnly placesBlock "PLACES { p q }"
  where
    expectation = ["p", "q"]

markingBlocks :: TestTree
markingBlocks = testGroup "Marking"
    [ testCase "Empty" emptyMarking
    , testCase "Non-Empty" nonEmptyMarking
    ]

emptyMarking :: Assertion
emptyMarking = Right expectation @=? parseOnly markingBlock "MARKING {}"
  where
    expectation = []

nonEmptyMarking :: Assertion
nonEmptyMarking = Right expectation @=? parseOnly markingBlock "MARKING { input1: true input2: false }"
  where
    expectation = [("input1", Function "true" []), ("input2", Function "false" [])]

transitionBlocks :: TestTree
transitionBlocks = testGroup "Transition"
    [ testCase "Isolated" isolatedTransition
    , testCase "Infinite Producer" infiniteProducer
    , testCase "Unconditional Eater" unconditionalEater
    , testCase "Common" commonTransition
    , testCase "Writing Transition" writingTransition
    ]

isolatedTransition :: Assertion
isolatedTransition = Right expectation @=? parseOnly transitionBlock "TRANSITION doNothing {}"
  where
    expectation = ("doNothing", (match, produce, effects))
    match = []
    produce = []
    effects = []

infiniteProducer :: Assertion
infiniteProducer = Right expectation @=? parseOnly transitionBlock "TRANSITION produce { PRODUCE { output: true } }"
  where
    expectation = ("produce", (match, produce, effects))
    match = []
    produce = [("output", Function "true" [])]
    effects = []

unconditionalEater :: Assertion
unconditionalEater = Right expectation @=? parseOnly transitionBlock "TRANSITION eat { MATCH { input: _ } }"
  where
    expectation = ("eat", (match, produce, effects))
    match = [("input", Wildcard)]
    produce = []
    effects = []

commonTransition :: Assertion
commonTransition = Right expectation @=? parseOnly transitionBlock "TRANSITION firstTrue { MATCH { input1: true input2: B } PRODUCE { output: B } }"
  where
    expectation = ("firstTrue", (match, produce, effects))
    match = [("input1", Function "true" []), ("input2", Variable "B")]
    produce = [("output", Variable "B")]
    effects = []

writingTransition :: Assertion
writingTransition = Right expectation @=? parseOnly transitionBlock "TRANSITION write { MATCH { input: B } EFFECTS { output: writeByte(B) } }"
  where
    expectation = ("write", (match, produce, effects))
    match = [("input", Variable "B")]
    produce = []
    effects = [("output", Function "writeByte" [Variable "B"])]
