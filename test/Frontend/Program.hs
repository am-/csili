module Frontend.Program where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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
    startTransition = (mkTransition "start")
        { patterns = Map.fromList [(Place "input", VariablePattern (Var "List"))]
        , productions = Map.fromList [(Place "original", Construct $ Substitution (Var "List"))]
        }
    addElementTransition = (mkTransition "addElement")
        { patterns = Map.fromList [(Place "original", FunctionPattern (Symbol "cons") [VariablePattern (Var "Head"), VariablePattern (Var "Tail")]), (Place "reversed", VariablePattern (Var "ReversedList"))]
        , productions = Map.fromList [(Place "original", Construct $ Substitution (Var "Tail")), (Place "reversed", Construct $ FunctionConstruction (Symbol "cons") [Substitution (Var "Head"), Substitution (Var "ReversedList")])]
        }
    returnTransition = (mkTransition "return")
        { patterns = Map.fromList [(Place "original", FunctionPattern (Symbol "nil") []), (Place "reversed", VariablePattern (Var "ReversedList"))]
        , productions = Map.fromList [(Place "output", Construct $ Substitution (Var "ReversedList")), (Place "reversed", Construct $ FunctionConstruction (Symbol "nil") [])]
        }

internalPlaceInsideInterface :: Assertion
internalPlaceInsideInterface = Left expected @=? parseCsl "INTERFACE { INPUT { p } } PLACES { p }"
  where
    expected = [DuringValidation $ InternalPlaceInsideInterface (Place "p")]

unparseableProgram :: Assertion
unparseableProgram = Left expected @=? parseCsl "MAKRING"
  where
    expected = [DuringParsing "endOfInput"]

interfaceTests :: TestTree
interfaceTests = testGroup "Interface"
    [ testCase "Duplicate Input Place" duplicateInputPlace
    , testCase "Duplicate Output Place" duplicateOutputPlace
    , testCase "Duplicate Interface Place" duplicateInterfacePlace
    ]

duplicateInputPlace :: Assertion
duplicateInputPlace = Left expected @=? interface <$> parseCsl "INTERFACE { INPUT { p q p q } OUTPUT {} }"
  where
    expected = map DuringConversion [DuplicateInputPlace (Place "p"), DuplicateInputPlace (Place "q")]

duplicateOutputPlace :: Assertion
duplicateOutputPlace = Left expected @=? interface <$> parseCsl "INTERFACE { INPUT {} OUTPUT { p p } }"
  where
    expected = map DuringConversion [DuplicateOutputPlace (Place "p")]

duplicateInterfacePlace :: Assertion
duplicateInterfacePlace = Left expected @=? interface <$> parseCsl "INTERFACE { INPUT { p } OUTPUT { p } }"
  where
    expected = map DuringValidation [OverlappingInputAndOutput (Place "p")]

internalPlacesTests :: TestTree
internalPlacesTests = testGroup "Internal Places"
    [ testCase "Duplicate Internal Place" duplicateInternalPlace
    , testCase "Internal Place inside Interface" internalPlaceInsideInterface
    ]

duplicateInternalPlace :: Assertion
duplicateInternalPlace = Left expected @=? parseCsl "PLACES { p p }"
  where
    expected = map DuringConversion [DuplicateInternalPlace (Place "p")]

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
    expected = map DuringConversion [DuplicateToken (Place "p")]

initialMarkingContainingInexistentPlace :: Assertion
initialMarkingContainingInexistentPlace = Left expected @=? parseCsl "MARKING { p: nil }"
  where
    expected = map DuringValidation [TokenOnInexistentPlace (Place "p")]

initialMarkingContainingInterfacePlace :: Assertion
initialMarkingContainingInterfacePlace = Left expected @=? parseCsl "INTERFACE { INPUT { p } } MARKING { p: nil }"
  where
    expected = map DuringValidation [TokenOnInterfacePlace (Place "p")]

initialMarkingWithFunction :: Assertion
initialMarkingWithFunction = Right expected @=? initialMarking <$> parseCsl "PLACES { p } MARKING { p: cons(42, nil) }"
  where
    expected = Map.fromList [(Place "p", FunctionToken (Symbol "cons") [IntToken 42, FunctionToken (Symbol "nil") []])]

initialMarkingWithInt :: Assertion
initialMarkingWithInt = Right expected @=? initialMarking <$> parseCsl "PLACES { p } MARKING { p: 42 }"
  where
    expected = Map.fromList [(Place "p", IntToken 42)]

initialMarkingWithWildcard :: Assertion
initialMarkingWithWildcard = Left expected @=? parseCsl "PLACES { p } MARKING { p: _ }"
  where
    expected = map DuringConversion [InvalidToken (Place "p") Wildcard]

initialMarkingWithVariable :: Assertion
initialMarkingWithVariable = Left expected @=? parseCsl "PLACES { p } MARKING { p: cons(V, nil) }"
  where
    expected = map DuringConversion [InvalidToken (Place "p") (Variable "V")]

transitionTests :: TestTree
transitionTests = testGroup "Transitions"
    [ testCase "Duplicate Transitions" duplicateTransitions
    , patternsTests
    , productionsTests
    , effectsTests
    , effectfulTransitionTests
    ]

duplicateTransitions :: Assertion
duplicateTransitions = Left expected @=? parseCsl "TRANSITION t {} TRANSITION t {}"
  where
    expected = map DuringConversion [DuplicateTransition (TransitionName "t")]

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

consumingFromOutputPlace :: Assertion
consumingFromOutputPlace = Left expected @=? parseCsl "INTERFACE { OUTPUT { p } } TRANSITION t { MATCH { p: _ } }"
  where
    expected = map DuringValidation [ConsumingFromOutputPlace (TransitionName "t") (Place "p")]

consumingFromInexistentPlace :: Assertion
consumingFromInexistentPlace = Left expected @=? parseCsl "TRANSITION t { MATCH { p: _ } }"
  where
    expected = map DuringValidation [ConsumingFromInexistentPlace (TransitionName "t") (Place "p")]

duplicatePattern :: Assertion
duplicatePattern = Left expected @=? parseCsl "PLACES { p } TRANSITION t { MATCH { p: cons(Head, Tail) p: _ } }"
  where
    expected = map DuringConversion [DuplicatePattern (TransitionName "t") (Place "p")]

duplicateVariableInPattern :: Assertion
duplicateVariableInPattern = Left expected @=? parseCsl "PLACES { p } TRANSITION t { MATCH { p: cons(X, X) } }"
  where
    expected = map DuringValidation [DuplicateVariableInPattern (TransitionName "t") (Place "p") (Var "X")]

duplicateVariableInPatterns :: Assertion
duplicateVariableInPatterns = Left expected @=? parseCsl "PLACES { p q } TRANSITION t { MATCH { p: X q: cons(X, Y) } }"
  where
    expected = map DuringValidation [DuplicateVariableInDifferentPatterns (TransitionName "t") (Var "X") (Set.fromList [Place "p", Place "q"])]

patternWithFunction :: Assertion
patternWithFunction = Right expected @=? transitions <$> parseCsl "PLACES { p } TRANSITION t { MATCH { p: cons(Head, Tail) } }"
  where
    expected = Set.singleton t
    t = (mkTransition "t")
        { patterns = Map.fromList [(Place "p", FunctionPattern (Symbol "cons") [VariablePattern (Var "Head"), VariablePattern (Var "Tail")])]
        }

patternWithInt :: Assertion
patternWithInt = Right expected @=? transitions <$> parseCsl "PLACES { p } TRANSITION t { MATCH { p: 42 } }"
  where
    expected = Set.singleton t
    t = (mkTransition "t")
        { patterns = Map.fromList [(Place "p", IntPattern 42)]
        }

patternWithWildcard :: Assertion
patternWithWildcard = Right expected @=? transitions <$> parseCsl "PLACES { p } TRANSITION t { MATCH { p: _ } }"
  where
    expected = Set.singleton t
    t = (mkTransition "t")
        { patterns = Map.fromList [(Place "p", WildcardPattern)]
        }

patternWithVariable :: Assertion
patternWithVariable = Right expected @=? transitions <$> parseCsl "PLACES { p } TRANSITION t { MATCH { p: V } }"
  where
    expected = Set.singleton t
    t = (mkTransition "t")
        { patterns = Map.fromList [(Place "p", VariablePattern (Var "V"))]
        }

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

producingOnInputPlace :: Assertion
producingOnInputPlace = Left expected @=? parseCsl "INTERFACE { INPUT { p } } TRANSITION t { PRODUCE { p: 42 } }"
  where
    expected = map DuringValidation [ConstructingOnInputPlace (TransitionName "t") (Place "p")]

producingOnInexistentPlace :: Assertion
producingOnInexistentPlace = Left expected @=? parseCsl "TRANSITION t { PRODUCE { p: 42 } }"
  where
    expected = map DuringValidation [ConstructingOnInexistentPlace (TransitionName "t") (Place "p")]

duplicateProduction :: Assertion
duplicateProduction = Left expected @=? parseCsl "PLACES { p } TRANSITION t { PRODUCE { p: 42 p: nil } }"
  where
    expected = map DuringConversion [DuplicateProduction (TransitionName "t") (Place "p")]

substitutionOfUnknownVariable :: Assertion
substitutionOfUnknownVariable = Left expected @=? parseCsl "PLACES { p } TRANSITION t { PRODUCE { p: cons(X, Y) } }"
  where
    expected = map DuringValidation [UnknownVariableInSubstitution (TransitionName "t") (Place "p") (Set.fromList [Var "X", Var "Y"])]

productionWithFunction :: Assertion
productionWithFunction = Right expected @=? transitions <$> parseCsl "PLACES { p } TRANSITION t { PRODUCE { p: cons(true, nil) } }"
  where
    expected = Set.singleton t
    t = (mkTransition "t")
        { productions = Map.fromList [(Place "p", Construct $ FunctionConstruction (Symbol "cons") [FunctionConstruction (Symbol "true") [], FunctionConstruction (Symbol "nil") []])]
        }

productionWithInt :: Assertion
productionWithInt = Right expected @=? transitions <$> parseCsl "PLACES { p } TRANSITION t { PRODUCE { p: 42 } }"
  where
    expected = Set.singleton t
    t = (mkTransition "t")
        { productions = Map.fromList [(Place "p", Construct $ IntConstruction 42)]
        }

productionWithWildcard :: Assertion
productionWithWildcard = Left expected @=? transitions <$> parseCsl "PLACES { p } TRANSITION t { PRODUCE { p: cons(_, nil) } }"
  where
    expected = map DuringConversion [InvalidProduction (TransitionName "t") (Place "p") Wildcard]

productionWithVariable :: Assertion
productionWithVariable = Right expected @=? transitions <$> parseCsl "PLACES { p } TRANSITION t { MATCH { p: V } PRODUCE { p: V } }"
  where
    expected = Set.singleton t
    t = (mkTransition "t")
        { patterns = Map.fromList [(Place "p", VariablePattern (Var "V"))]
        , productions = Map.fromList [(Place "p", Construct $ Substitution (Var "V"))]
        }

effectsTests :: TestTree
effectsTests = testGroup "Effects"
    [ testCase "Duplicate Effect" duplicateEffect
    , testCase "Effect on Input Place" effectOnInputPlace
    , testCase "Effect on Inexistent Place" effectOnInexistentPlace
    , testCase "Effect on Production Place" effectOnProductionPlace
    , testCase "Inexistent Effect" inexistentEffect
    , testCase "writeByte (with variable)" writeByteWithVariable
    , testCase "writeByte (too few arguments)" writeByteWithTooFewArguments
    , testCase "writeByte (too many arguments)" writeByteWithTooManyArguments
    , testCase "writeByte (invalid argument)" writeByteWithInvalidArgument
    , testCase "Int" intEffect
    , testCase "Wildcard" wildcardEffect
    , testCase "Variable" substitutionEffect
    ]

duplicateEffect :: Assertion
duplicateEffect = Left expected @=? parseCsl "PLACES { p } TRANSITION t { EFFECTS { p: writeByte(4) p: writeByte(2) } }"
  where
    expected = map DuringConversion [DuplicateEffect (TransitionName "t") (Place "p")]

effectOnInputPlace :: Assertion
effectOnInputPlace = Left expected @=? parseCsl "INTERFACE { INPUT { s p } } TRANSITION t { MATCH { s: S } EFFECTS { p: writeByte(S, 42) } }"
  where
    expected = map DuringValidation [EffectOnInputPlace (TransitionName "t") (Place "p")]

effectOnInexistentPlace :: Assertion
effectOnInexistentPlace = Left expected @=? parseCsl "PLACES { s } TRANSITION t { MATCH { s: S } EFFECTS { p: writeByte(S, 42) } }"
  where
    expected = map DuringValidation [EffectOnInexistentPlace (TransitionName "t") (Place "p")]

effectOnProductionPlace :: Assertion
effectOnProductionPlace = Left expected @=? transitions <$> parseCsl "PLACES { p s } TRANSITION t { MATCH { s: S } PRODUCE { p: 42 } EFFECTS { p: writeByte(S, 42) } }"
  where
    expected = map DuringConversion [EffectOnProductionPlace (TransitionName "t") (Place "p")]

inexistentEffect :: Assertion
inexistentEffect = Left expected @=? transitions <$> parseCsl "PLACES { p } TRANSITION t { EFFECTS { p: launchMissiles(42) } }"
  where
    expected = map DuringConversion [InexistentEffect (TransitionName "t") (Place "p") (Symbol "launchMissiles")]

writeByteWithVariable :: Assertion
writeByteWithVariable = Right expected @=? transitions <$> parseCsl "PLACES { s p } TRANSITION t { MATCH { s: S p: V } EFFECTS { p: writeByte(S, V) } }"
  where
    expected = Set.singleton t
    t = (mkTransition "t")
        { patterns = Map.fromList [(Place "p", VariablePattern (Var "V")), (Place "s", VariablePattern (Var "S"))]
        , productions = Map.fromList [(Place "p", Evaluate $ WriteByte (Substitution $ Var "S") (Substitution $ Var "V"))]
        }

writeByteWithTooFewArguments :: Assertion
writeByteWithTooFewArguments = Left expected @=? transitions <$> parseCsl "PLACES { p } TRANSITION t { EFFECTS { p: writeByte } }"
  where
    expected = map DuringConversion [ArityMismatchForEffect (TransitionName "t") (Place "p") (Symbol "writeByte") 2 0]

writeByteWithTooManyArguments :: Assertion
writeByteWithTooManyArguments = Left expected @=? transitions <$> parseCsl "PLACES { p } TRANSITION t { EFFECTS { p: writeByte(3, 1, 4) } }"
  where
    expected = map DuringConversion [ArityMismatchForEffect (TransitionName "t") (Place "p") (Symbol "writeByte") 2 3]

writeByteWithInvalidArgument :: Assertion
writeByteWithInvalidArgument = Left expected @=? transitions <$> parseCsl "PLACES { p s } TRANSITION t { MATCH { s: S } EFFECTS { p: writeByte(S, _) } }"
  where
    expected = map DuringConversion [InvalidProduction (TransitionName "t") (Place "p") Wildcard]

intEffect :: Assertion
intEffect = Left expected @=? transitions <$> parseCsl "PLACES { p } TRANSITION t { EFFECTS { p: 42 } }"
  where
    expected = map DuringConversion [InvalidEffect (TransitionName "t") (Place "p") (IntTerm 42)]

wildcardEffect :: Assertion
wildcardEffect = Left expected @=? transitions <$> parseCsl "PLACES { p } TRANSITION t { EFFECTS { p: _ } }"
  where
    expected = map DuringConversion [InvalidEffect (TransitionName "t") (Place "p") Wildcard]

substitutionEffect :: Assertion
substitutionEffect = Left expected @=? transitions <$> parseCsl "PLACES { p } TRANSITION t { EFFECTS { p: V } }"
  where
    expected = map DuringConversion [InvalidEffect (TransitionName "t") (Place "p") (Variable "V")]

effectfulTransitionTests :: TestTree
effectfulTransitionTests = testGroup "Effectful Transitions"
    [ testCase "Non-Overlapping Patterns" nonOverlappingPatterns
    , testCase "Sling Activation" slingActivation
    , testCase "Structural Conflict with Sling" structuralConflictWithSling
    , testCase "Structural Conflict in Preset" structuralConflictInPreset
    , testCase "Structural Conflict in Postset" structuralConflictInPostset
    , testCase "Structural Conflict between Multiple Transitions" structuralConflictBetweenMultipleTransitions
    , testCase "Multiple Structural Conflicts" multipleStructuralConflicts
    ]

nonOverlappingPatterns :: Assertion
nonOverlappingPatterns = case parseCsl "PLACES { s p r } TRANSITION e1 { MATCH { s: S p: nil } EFFECTS { r: writeByte(S, 0x45) } } TRANSITION e2 { MATCH { s: S p: cons(_, _) } EFFECTS { p: writeByte(S, 0x4E) } }" of
    Left errors -> assertFailure $ show errors
    Right _ -> return ()

slingActivation :: Assertion
slingActivation = case parseCsl "PLACES { s p r } TRANSITION t { PRODUCE { p: cons(0x34, cons(0x32, nil)) } } TRANSITION e { MATCH { s: S p: cons(Byte, Bytes) } PRODUCE { s: S p: Bytes } EFFECTS { r: writeByte(S, Byte) } }" of
    Left errors -> assertFailure $ show errors
    Right _ -> return ()

structuralConflictWithSling :: Assertion
structuralConflictWithSling = Left expected @=? parseCsl "PLACES { s p } TRANSITION t { MATCH { p: X } PRODUCE { p: nil } } TRANSITION e { MATCH { s: S p: X } EFFECTS { p: writeByte(S, X) } }"
  where
    expected = map DuringValidation [StructuralConflictWithEffectfulTransition (TransitionName "e") $ Set.singleton (TransitionName "t")]

structuralConflictInPreset :: Assertion
structuralConflictInPreset = Left expected @=? parseCsl "PLACES { s p q } TRANSITION t { MATCH { p: _ } } TRANSITION e { MATCH { s: S p: X } EFFECTS { q: writeByte(S, X) } }"
  where
    expected = map DuringValidation [StructuralConflictWithEffectfulTransition (TransitionName "e") $ Set.singleton (TransitionName "t")]

structuralConflictInPostset :: Assertion
structuralConflictInPostset = Left expected @=? parseCsl "PLACES { p q r s1 s2 } TRANSITION e1 { MATCH { s1: S p: X } EFFECTS { r: writeByte(S, X) } } TRANSITION e2 { MATCH { s2: S q: X } EFFECTS { r: writeByte(S, X) } }"
  where
    expected = map DuringValidation
        [ StructuralConflictWithEffectfulTransition (TransitionName "e1") $ Set.singleton (TransitionName "e2")
        , StructuralConflictWithEffectfulTransition (TransitionName "e2") $ Set.singleton (TransitionName "e1")
        ]

structuralConflictBetweenMultipleTransitions :: Assertion
structuralConflictBetweenMultipleTransitions = Left expected @=? parseCsl "PLACES { p q r s1 s2 } TRANSITION t { PRODUCE { r: nil } } TRANSITION e1 { MATCH { s1: S p: X } EFFECTS { r: writeByte(S, X) } } TRANSITION e2 { MATCH { s2: S q: X } EFFECTS { r: writeByte(S, X) } }"
  where
    expected = map DuringValidation
        [ StructuralConflictWithEffectfulTransition (TransitionName "e1") $ Set.fromList [TransitionName "e2", TransitionName "t"]
        , StructuralConflictWithEffectfulTransition (TransitionName "e2") $ Set.fromList [TransitionName "e1", TransitionName "t"]
        ]

multipleStructuralConflicts :: Assertion
multipleStructuralConflicts = Left expected @=? parseCsl "PLACES { p q r s1 s2 } TRANSITION t { MATCH { p: _ } } TRANSITION e1 { MATCH { s1: S p: X } EFFECTS { r: writeByte(S, X) } } TRANSITION e2 { MATCH { s2: S q: X } EFFECTS { r: writeByte(S, X) } }"
  where
    expected = map DuringValidation
        [ StructuralConflictWithEffectfulTransition (TransitionName "e1") $ Set.fromList [TransitionName "e2", TransitionName "t"]
        , StructuralConflictWithEffectfulTransition (TransitionName "e2") $ Set.fromList [TransitionName "e1"]
        ]
