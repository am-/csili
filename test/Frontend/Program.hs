module Frontend.Program where

import qualified Data.Map.Strict as Map (empty, fromList)
import qualified Data.Set as Set (fromList, singleton)

import Test.Tasty
import Test.Tasty.HUnit

import Csili.Program
import Csili.Frontend
import Csili.Frontend.Parser (Term(..))

tests :: TestTree
tests = testGroup "Program"
    [ programTests
    , interfaceTests
    , markingTests
    , transitionTests
    ]

programTests :: TestTree
programTests = testGroup "Program"
    [ testCase "and.csl" andProgram
    , testCase "Unparseable Program" unparseableProgram
    , testCase "Consuming from Output Place" consumingFromOutputPlace
    , testCase "Producing on Input Place" producingOnInputPlace
    ]

andProgram :: Assertion
andProgram = loadCsl "examples/bool/and.csl" >>= \case
    Left _ -> assertFailure "Couldn't load program."
    Right program -> do
        expectedInterface @=? interface program
        expectedMarking @=? initialMarking program
        expectedTransitions @=? transitions program
  where
    expectedInterface = Interface
        { input = Set.fromList [Place "input1", Place "input2"]
        , output = Set.fromList [Place "output"]
        }
    expectedMarking = Map.empty
    expectedTransitions = Set.fromList [firstTrue, firstFalse]
    firstTrue = Transition
        (TransitionName "firstTrue")
        (Map.fromList [(Place "input1", FunctionPattern (Symbol "true") []), (Place "input2", VariablePattern (Var "B"))])
        (Map.fromList [(Place "output", Substitution (Var "B"))])
    firstFalse = Transition
        (TransitionName "firstFalse")
        (Map.fromList [(Place "input1", FunctionPattern (Symbol "false") []), (Place "input2", VariablePattern (Var "B"))])
        (Map.fromList [(Place "output", FunctionProduction (Symbol "false") [])])

consumingFromOutputPlace :: Assertion
consumingFromOutputPlace = Left expected @=? parseCsl "INTERFACE { OUTPUT { p } } TRANSITION t { MATCH { p: _ } }"
  where
    expected = [ConsumingFromOutputPlace (TransitionName "t") (Place "p")]

producingOnInputPlace :: Assertion
producingOnInputPlace = Left expected @=? parseCsl "INTERFACE { INPUT { p } } TRANSITION t { PRODUCE { p: 42 } }"
  where
    expected = [ProducingOnInputPlace (TransitionName "t") (Place "p")]

unparseableProgram :: Assertion
unparseableProgram = Left expected @=? parseCsl "MAKRING"
  where
    expected = [ParseError "endOfInput"]

interfaceTests :: TestTree
interfaceTests = testGroup "Interface"
    [ testCase "Duplicate Input Place" duplicateInputPlace
    , testCase "Duplicate Output Place" duplicateOutputPlace
    , testCase "Duplicate Interface Place" duplicateInterfacePlace
    ]

duplicateInputPlace :: Assertion
duplicateInputPlace = Left expected @=? interface <$> parseCsl "INTERFACE { INPUT { p q p q } OUTPUT {} }"
  where
    expected = [DuplicateInputPlace (Place "p"), DuplicateInputPlace (Place "q")]

duplicateOutputPlace :: Assertion
duplicateOutputPlace = Left expected @=? interface <$> parseCsl "INTERFACE { INPUT {} OUTPUT { p p } }"
  where
    expected = [DuplicateOutputPlace (Place "p")]

duplicateInterfacePlace :: Assertion
duplicateInterfacePlace = Left expected @=? interface <$> parseCsl "INTERFACE { INPUT { p } OUTPUT { p } }"
  where
    expected = [OverlappingInputAndOutput (Place "p")]

markingTests :: TestTree
markingTests = testGroup "Initial Marking"
    [ testCase "Duplicate Token" initialMarkingWithDuplicateToken
    , testCase "Function" initialMarkingWithFunction
    , testCase "Int"  initialMarkingWithInt
    , testCase "Wildcard" initialMarkingWithWildcard
    , testCase "Variable" initialMarkingWithVariable
    ]

initialMarkingWithDuplicateToken :: Assertion
initialMarkingWithDuplicateToken = Left expected @=? parseCsl "MARKING { p: nil p: 42 }"
  where
    expected = [DuplicateToken (Place "p")]

initialMarkingWithFunction :: Assertion
initialMarkingWithFunction = Right expected @=? initialMarking <$> parseCsl "MARKING { p: cons(42, nil) }"
  where
    expected = Map.fromList [(Place "p", FunctionToken (Symbol "cons") [IntToken 42, FunctionToken (Symbol "nil") []])]

initialMarkingWithInt :: Assertion
initialMarkingWithInt = Right expected @=? initialMarking <$> parseCsl "MARKING { p: 42 }"
  where
    expected = Map.fromList [(Place "p", IntToken 42)]

initialMarkingWithWildcard :: Assertion
initialMarkingWithWildcard = Left [InvalidToken (Place "p") Wildcard] @=? parseCsl "MARKING { p: _ }"

initialMarkingWithVariable :: Assertion
initialMarkingWithVariable = Left [InvalidToken (Place "p") (Variable "V")] @=? parseCsl "MARKING { p: cons(V, nil) }"

transitionTests :: TestTree
transitionTests = testGroup "Transitions"
    [ testCase "Duplicate Transitions" duplicateTransitions
    , patternsTests
    , productionsTests
    ]

duplicateTransitions :: Assertion
duplicateTransitions = Left expected @=? parseCsl "TRANSITION t {} TRANSITION t {}"
  where
    expected = [DuplicateTransition (TransitionName "t")]

patternsTests :: TestTree
patternsTests = testGroup "Patterns"
    [ testCase "Duplicate Pattern" duplicatePattern
    , testCase "Function" patternWithFunction
    , testCase "Int" patternWithInt
    , testCase "Wildcard" patternWithWildcard
    , testCase "Variable" patternWithVariable
    ]

duplicatePattern :: Assertion
duplicatePattern = Left expected @=? parseCsl "TRANSITION t { MATCH { p: cons(Head, Tail) p: _ } }"
  where
    expected = [DuplicatePattern (TransitionName "t") (Place "p")]

patternWithFunction :: Assertion
patternWithFunction = Right expected @=? transitions <$> parseCsl "TRANSITION t { MATCH { p: cons(Head, Tail) } }"
  where
    expected = Set.singleton t
    t = Transition
        (TransitionName "t")
        (Map.fromList [(Place "p", FunctionPattern (Symbol "cons") [VariablePattern (Var "Head"), VariablePattern (Var "Tail")])])
        Map.empty

patternWithInt :: Assertion
patternWithInt = Right expected @=? transitions <$> parseCsl "TRANSITION t { MATCH { p: 42 } }"
  where
    expected = Set.singleton t
    t = Transition
        (TransitionName "t")
        (Map.fromList [(Place "p", IntPattern 42)])
        Map.empty

patternWithWildcard :: Assertion
patternWithWildcard = Right expected @=? transitions <$> parseCsl "TRANSITION t { MATCH { p: _ } }"
  where
    expected = Set.singleton t
    t = Transition
        (TransitionName "t")
        (Map.fromList [(Place "p", WildcardPattern)])
        Map.empty

patternWithVariable :: Assertion
patternWithVariable = Right expected @=? transitions <$> parseCsl "TRANSITION t { MATCH { p: V } }"
  where
    expected = Set.singleton t
    t = Transition
        (TransitionName "t")
        (Map.fromList [(Place "p", VariablePattern (Var "V"))])
        Map.empty

productionsTests :: TestTree
productionsTests = testGroup "Productions"
    [ testCase "Duplicate Production" duplicateProduction
    , testCase "Function" productionWithFunction
    , testCase "Int" productionWithInt
    , testCase "Wildcard" productionWithWildcard
    , testCase "Variable" productionWithVariable
    ]

duplicateProduction :: Assertion
duplicateProduction = Left expected @=? parseCsl "TRANSITION t { PRODUCE { p: 42 p: nil } }"
  where
    expected = [DuplicateProduction (TransitionName "t") (Place "p")]

productionWithFunction :: Assertion
productionWithFunction = Right expected @=? transitions <$> parseCsl "TRANSITION t { PRODUCE { p: cons(true, nil) } }"
  where
    expected = Set.singleton t
    t = Transition
        (TransitionName "t")
        Map.empty
        (Map.fromList [(Place "p", FunctionProduction (Symbol "cons") [FunctionProduction (Symbol "true") [], FunctionProduction (Symbol "nil") []])])

productionWithInt :: Assertion
productionWithInt = Right expected @=? transitions <$> parseCsl "TRANSITION t { PRODUCE { p: 42 } }"
  where
    expected = Set.singleton t
    t = Transition
        (TransitionName "t")
        Map.empty
        (Map.fromList [(Place "p", IntProduction 42)])

productionWithWildcard :: Assertion
productionWithWildcard = Left expected @=? transitions <$> parseCsl "TRANSITION t { PRODUCE { p: cons(_, nil) } }"
  where
    expected = [InvalidProduction (TransitionName "t") (Place "p") Wildcard]

productionWithVariable :: Assertion
productionWithVariable = Right expected @=? transitions <$> parseCsl "TRANSITION t { MATCH { p: V } PRODUCE { p: V } }"
  where
    expected = Set.singleton t
    t = Transition
        (TransitionName "t")
        (Map.fromList [(Place "p", VariablePattern (Var "V"))])
        (Map.fromList [(Place "p", Substitution (Var "V"))])
