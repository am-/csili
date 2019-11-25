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
    , internalPlacesTests
    , markingTests
    , transitionTests
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
        expectedInterface @=? interface program
        expectedInternalPlaces @=? internalPlaces program
        expectedMarking @=? initialMarking program
        expectedTransitions @=? transitions program
  where
    expectedInterface = Interface
        { input = Set.fromList [Place "input"]
        , output = Set.fromList [Place "output"]
        }
    expectedInternalPlaces = Set.fromList [Place "original", Place "reversed"]
    expectedMarking = Map.fromList [(Place "reversed", FunctionToken (Symbol "nil") [])]
    expectedTransitions = Set.fromList [startTransition, addElementTransition, returnTransition]
    startTransition = Transition
        (TransitionName "start")
        (Map.fromList [(Place "input", VariablePattern (Var "List"))])
        (Map.fromList [(Place "original", Substitution (Var "List"))])
    addElementTransition = Transition
        (TransitionName "addElement")
        (Map.fromList [(Place "original", FunctionPattern (Symbol "cons") [VariablePattern (Var "Head"), VariablePattern (Var "Tail")]), (Place "reversed", VariablePattern (Var "ReversedList"))])
        (Map.fromList [(Place "original", Substitution (Var "Tail")), (Place "reversed", FunctionProduction (Symbol "cons") [Substitution (Var "Head"), Substitution (Var "ReversedList")])])
    returnTransition = Transition
        (TransitionName "return")
        (Map.fromList [(Place "original", FunctionPattern (Symbol "nil") []), (Place "reversed", VariablePattern (Var "ReversedList"))])
        (Map.fromList [(Place "output", Substitution (Var "ReversedList")), (Place "reversed", FunctionProduction (Symbol "nil") [])])

consumingFromOutputPlace :: Assertion
consumingFromOutputPlace = Left expected @=? parseCsl "INTERFACE { OUTPUT { p } } TRANSITION t { MATCH { p: _ } }"
  where
    expected = [ConsumingFromOutputPlace (TransitionName "t") (Place "p")]

producingOnInputPlace :: Assertion
producingOnInputPlace = Left expected @=? parseCsl "INTERFACE { INPUT { p } } TRANSITION t { PRODUCE { p: 42 } }"
  where
    expected = [ProducingOnInputPlace (TransitionName "t") (Place "p")]

consumingFromInexistentPlace :: Assertion
consumingFromInexistentPlace = Left expected @=? parseCsl "TRANSITION t { MATCH { p: _ } }"
  where
    expected = [ConsumingFromInexistentPlace (TransitionName "t") (Place "p")]

producingOnInexistentPlace :: Assertion
producingOnInexistentPlace = Left expected @=? parseCsl "TRANSITION t { PRODUCE { p: 42 } }"
  where
    expected = [ProducingOnInexistentPlace (TransitionName "t") (Place "p")]

internalPlaceInsideInterface :: Assertion
internalPlaceInsideInterface = Left expected @=? parseCsl "INTERFACE { INPUT { p } } PLACES { p }"
  where
    expected = [InternalPlaceInsideInterface (Place "p")]

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

internalPlacesTests :: TestTree
internalPlacesTests = testGroup "Internal Places"
    [ testCase "Duplicate Internal Place" duplicateInternalPlace
    , testCase "Internal Place inside Interface" internalPlaceInsideInterface
    ]

duplicateInternalPlace :: Assertion
duplicateInternalPlace = Left expected @=? parseCsl "PLACES { p p }"
  where
    expected = [DuplicateInternalPlace (Place "p")]

markingTests :: TestTree
markingTests = testGroup "Initial Marking"
    [ testCase "Duplicate Token" initialMarkingWithDuplicateToken
    , testCase "Token on Inexistent Place" initialMarkingContainingInexistentPlace
    , testCase "Token on Interface Place" initialMarkingContainingInterfacePlace
    , testCase "Function" initialMarkingWithFunction
    , testCase "Int"  initialMarkingWithInt
    , testCase "Wildcard" initialMarkingWithWildcard
    , testCase "Variable" initialMarkingWithVariable
    ]

initialMarkingWithDuplicateToken :: Assertion
initialMarkingWithDuplicateToken = Left expected @=? parseCsl "MARKING { p: nil p: 42 }"
  where
    expected = [DuplicateToken (Place "p")]

initialMarkingContainingInexistentPlace :: Assertion
initialMarkingContainingInexistentPlace = Left expected @=? parseCsl "MARKING { p: nil }"
  where
    expected = [TokenOnInexistentPlace (Place "p")]

initialMarkingContainingInterfacePlace :: Assertion
initialMarkingContainingInterfacePlace = Left expected @=? parseCsl "INTERFACE { INPUT { p } } MARKING { p: nil }"
  where
    expected = [TokenOnInterfacePlace (Place "p")]

initialMarkingWithFunction :: Assertion
initialMarkingWithFunction = Right expected @=? initialMarking <$> parseCsl "PLACES { p } MARKING { p: cons(42, nil) }"
  where
    expected = Map.fromList [(Place "p", FunctionToken (Symbol "cons") [IntToken 42, FunctionToken (Symbol "nil") []])]

initialMarkingWithInt :: Assertion
initialMarkingWithInt = Right expected @=? initialMarking <$> parseCsl "PLACES { p } MARKING { p: 42 }"
  where
    expected = Map.fromList [(Place "p", IntToken 42)]

initialMarkingWithWildcard :: Assertion
initialMarkingWithWildcard = Left [InvalidToken (Place "p") Wildcard] @=? parseCsl "PLACES { p } MARKING { p: _ }"

initialMarkingWithVariable :: Assertion
initialMarkingWithVariable = Left [InvalidToken (Place "p") (Variable "V")] @=? parseCsl "PLACES { p } MARKING { p: cons(V, nil) }"

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
    [ testCase "Consuming from Output Place" consumingFromOutputPlace
    , testCase "Consuming from Inexistent Place" consumingFromInexistentPlace
    , testCase "Duplicate Pattern" duplicatePattern
    , testCase "Duplicate Variable in Pattern" duplicateVariableInPattern
    , testCase "Duplicate Variable in Different Patterns" duplicateVariableInPatterns
    , testCase "Function" patternWithFunction
    , testCase "Int" patternWithInt
    , testCase "Wildcard" patternWithWildcard
    , testCase "Variable" patternWithVariable
    ]

duplicatePattern :: Assertion
duplicatePattern = Left expected @=? parseCsl "PLACES { p } TRANSITION t { MATCH { p: cons(Head, Tail) p: _ } }"
  where
    expected = [DuplicatePattern (TransitionName "t") (Place "p")]

duplicateVariableInPattern :: Assertion
duplicateVariableInPattern = Left expected @=? parseCsl "PLACES { p } TRANSITION t { MATCH { p: cons(X, X) } }"
  where
    expected = [DuplicateVariableInPattern (TransitionName "t") (Place "p") (Var "X")]

duplicateVariableInPatterns :: Assertion
duplicateVariableInPatterns = Left expected @=? parseCsl "PLACES { p q } TRANSITION t { MATCH { p: X q: cons(X, Y) } }"
  where
    expected = [DuplicateVariableInDifferentPatterns (TransitionName "t") (Var "X") (Set.fromList [Place "p", Place "q"])]

patternWithFunction :: Assertion
patternWithFunction = Right expected @=? transitions <$> parseCsl "PLACES { p } TRANSITION t { MATCH { p: cons(Head, Tail) } }"
  where
    expected = Set.singleton t
    t = Transition
        (TransitionName "t")
        (Map.fromList [(Place "p", FunctionPattern (Symbol "cons") [VariablePattern (Var "Head"), VariablePattern (Var "Tail")])])
        Map.empty

patternWithInt :: Assertion
patternWithInt = Right expected @=? transitions <$> parseCsl "PLACES { p } TRANSITION t { MATCH { p: 42 } }"
  where
    expected = Set.singleton t
    t = Transition
        (TransitionName "t")
        (Map.fromList [(Place "p", IntPattern 42)])
        Map.empty

patternWithWildcard :: Assertion
patternWithWildcard = Right expected @=? transitions <$> parseCsl "PLACES { p } TRANSITION t { MATCH { p: _ } }"
  where
    expected = Set.singleton t
    t = Transition
        (TransitionName "t")
        (Map.fromList [(Place "p", WildcardPattern)])
        Map.empty

patternWithVariable :: Assertion
patternWithVariable = Right expected @=? transitions <$> parseCsl "PLACES { p } TRANSITION t { MATCH { p: V } }"
  where
    expected = Set.singleton t
    t = Transition
        (TransitionName "t")
        (Map.fromList [(Place "p", VariablePattern (Var "V"))])
        Map.empty

productionsTests :: TestTree
productionsTests = testGroup "Productions"
    [ testCase "Producing on Input Place" producingOnInputPlace
    , testCase "Producing on Inexistent Place" producingOnInexistentPlace
    , testCase "Duplicate Production" duplicateProduction
    , testCase "Substitution of Unknown Variable" substitutionOfUnknownVariable
    , testCase "Function" productionWithFunction
    , testCase "Int" productionWithInt
    , testCase "Wildcard" productionWithWildcard
    , testCase "Variable" productionWithVariable
    ]

duplicateProduction :: Assertion
duplicateProduction = Left expected @=? parseCsl "PLACES { p } TRANSITION t { PRODUCE { p: 42 p: nil } }"
  where
    expected = [DuplicateProduction (TransitionName "t") (Place "p")]

substitutionOfUnknownVariable :: Assertion
substitutionOfUnknownVariable = Left expected @=? parseCsl "PLACES { p } TRANSITION t { PRODUCE { p: cons(X, Y) } }"
  where
    expected = [UnknownVariableInSubstitution (TransitionName "t") (Place "p") (Set.fromList [Var "X", Var "Y"])]

productionWithFunction :: Assertion
productionWithFunction = Right expected @=? transitions <$> parseCsl "PLACES { p } TRANSITION t { PRODUCE { p: cons(true, nil) } }"
  where
    expected = Set.singleton t
    t = Transition
        (TransitionName "t")
        Map.empty
        (Map.fromList [(Place "p", FunctionProduction (Symbol "cons") [FunctionProduction (Symbol "true") [], FunctionProduction (Symbol "nil") []])])

productionWithInt :: Assertion
productionWithInt = Right expected @=? transitions <$> parseCsl "PLACES { p } TRANSITION t { PRODUCE { p: 42 } }"
  where
    expected = Set.singleton t
    t = Transition
        (TransitionName "t")
        Map.empty
        (Map.fromList [(Place "p", IntProduction 42)])

productionWithWildcard :: Assertion
productionWithWildcard = Left expected @=? transitions <$> parseCsl "PLACES { p } TRANSITION t { PRODUCE { p: cons(_, nil) } }"
  where
    expected = [InvalidProduction (TransitionName "t") (Place "p") Wildcard]

productionWithVariable :: Assertion
productionWithVariable = Right expected @=? transitions <$> parseCsl "PLACES { p } TRANSITION t { MATCH { p: V } PRODUCE { p: V } }"
  where
    expected = Set.singleton t
    t = Transition
        (TransitionName "t")
        (Map.fromList [(Place "p", VariablePattern (Var "V"))])
        (Map.fromList [(Place "p", Substitution (Var "V"))])
