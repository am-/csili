module Frontend.Program.Transitions
( tests
) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Test.Tasty
import Test.Tasty.HUnit

import Csili.Program
import Csili.Frontend
import Csili.Frontend.Parser (Term(..))

tests :: TestTree
tests = testGroup "Transitions"
    [ testCase "Duplicate Transitions" duplicateTransitions
    , patternsTests
    , productionsTests
    , effectsTests
    , effectfulTransitionTests
    ]

duplicateTransitions :: Assertion
duplicateTransitions = Left expected @=? parseCsl "TRANSITION t {} TRANSITION t {}"
  where
    expected = map DuringConversion [DuplicateTransition (LocalTransition "t")]

patternsTests :: TestTree
patternsTests = testGroup "Patterns"
    [ testCase "Consuming from Output Place" consumingFromOutputPlace
    , testCase "Consuming from Inexistent Place" consumingFromInexistentPlace
    , testCase "Duplicate Pattern" duplicatePattern
    , testCase "Duplicate Variable in Pattern" duplicateVariableInPattern
    , testCase "Duplicate Variable in Different Patterns" duplicateVariableInPatterns
    , testCase "Function" patternWithFunction
    , testCase "Wildcard" patternWithWildcard
    , testCase "Variable" patternWithVariable
    ]

consumingFromOutputPlace :: Assertion
consumingFromOutputPlace = Left expected @=? parseCsl "INTERFACE { OUTPUT { p } } TRANSITION t { MATCH { p: _ } }"
  where
    expected = map DuringValidation [ConsumingFromProduceOnlyPlace (LocalTransition "t") (LocalPlace "p")]

consumingFromInexistentPlace :: Assertion
consumingFromInexistentPlace = Left expected @=? parseCsl "TRANSITION t { MATCH { p: _ } }"
  where
    expected = map DuringValidation [ConsumingFromInexistentPlace (LocalTransition "t") (LocalPlace "p")]

duplicatePattern :: Assertion
duplicatePattern = Left expected @=? parseCsl "PLACES { p } TRANSITION t { MATCH { p: cons(Head, Tail) p: _ } }"
  where
    expected = map DuringConversion [DuplicatePattern (LocalTransition "t") (LocalPlace "p")]

duplicateVariableInPattern :: Assertion
duplicateVariableInPattern = Left expected @=? parseCsl "PLACES { p } TRANSITION t { MATCH { p: cons(X, X) } }"
  where
    expected = map DuringValidation [DuplicateVariableInPattern (LocalTransition "t") (LocalPlace "p") (Var "X")]

duplicateVariableInPatterns :: Assertion
duplicateVariableInPatterns = Left expected @=? parseCsl "PLACES { p q } TRANSITION t { MATCH { p: X q: cons(X, Y) } }"
  where
    expected = map DuringValidation [DuplicateVariableInDifferentPatterns (LocalTransition "t") (Var "X") (Set.fromList [LocalPlace "p", LocalPlace "q"])]

patternWithFunction :: Assertion
patternWithFunction = Right expected @=? transitions . mainNet <$> parseCsl "PLACES { p } TRANSITION t { MATCH { p: cons(Head, Tail) } }"
  where
    expected = Set.singleton t
    t = (mkTransition "t")
        { patterns = Map.fromList [(LocalPlace "p", FunctionPattern (Symbol "cons") [VariablePattern (Var "Head"), VariablePattern (Var "Tail")])]
        }

patternWithWildcard :: Assertion
patternWithWildcard = Right expected @=? transitions . mainNet <$> parseCsl "PLACES { p } TRANSITION t { MATCH { p: _ } }"
  where
    expected = Set.singleton t
    t = (mkTransition "t")
        { patterns = Map.fromList [(LocalPlace "p", WildcardPattern)]
        }

patternWithVariable :: Assertion
patternWithVariable = Right expected @=? transitions . mainNet <$> parseCsl "PLACES { p } TRANSITION t { MATCH { p: V } }"
  where
    expected = Set.singleton t
    t = (mkTransition "t")
        { patterns = Map.fromList [(LocalPlace "p", VariablePattern (Var "V"))]
        }

productionsTests :: TestTree
productionsTests = testGroup "Productions"
    [ testCase "Producing on Input Place" producingOnInputPlace
    , testCase "Producing on Inexistent Place" producingOnInexistentPlace
    , testCase "Duplicate Production" duplicateProduction
    , testCase "Substitution of Unknown Variable" substitutionOfUnknownVariable
    , testCase "Function" productionWithFunction
    , testCase "Wildcard" productionWithWildcard
    , testCase "Variable" productionWithVariable
    ]

producingOnInputPlace :: Assertion
producingOnInputPlace = Left expected @=? parseCsl "INTERFACE { INPUT { p } } TRANSITION t { PRODUCE { p: 42 } }"
  where
    expected = map DuringValidation [ConstructingOnConsumeOnlyPlace (LocalTransition "t") (LocalPlace "p")]

producingOnInexistentPlace :: Assertion
producingOnInexistentPlace = Left expected @=? parseCsl "TRANSITION t { PRODUCE { p: 42 } }"
  where
    expected = map DuringValidation [ConstructingOnInexistentPlace (LocalTransition "t") (LocalPlace "p")]

duplicateProduction :: Assertion
duplicateProduction = Left expected @=? parseCsl "PLACES { p } TRANSITION t { PRODUCE { p: 42 p: nil } }"
  where
    expected = map DuringConversion [DuplicateProduction (LocalTransition "t") (LocalPlace "p")]

substitutionOfUnknownVariable :: Assertion
substitutionOfUnknownVariable = Left expected @=? parseCsl "PLACES { p } TRANSITION t { PRODUCE { p: cons(X, Y) } }"
  where
    expected = map DuringValidation [UnknownVariableInSubstitution (LocalTransition "t") (LocalPlace "p") (Set.fromList [Var "X", Var "Y"])]

productionWithFunction :: Assertion
productionWithFunction = Right expected @=? transitions . mainNet <$> parseCsl "PLACES { p } TRANSITION t { PRODUCE { p: cons(true, nil) } }"
  where
    expected = Set.singleton t
    t = (mkTransition "t")
        { productions = Map.fromList [(LocalPlace "p", Construct $ FunctionConstruction (Symbol "cons") [FunctionConstruction (Symbol "true") [], FunctionConstruction (Symbol "nil") []])]
        }

productionWithWildcard :: Assertion
productionWithWildcard = Left expected @=? transitions . mainNet <$> parseCsl "PLACES { p } TRANSITION t { PRODUCE { p: cons(_, nil) } }"
  where
    expected = map DuringConversion [InvalidProduction (LocalTransition "t") (LocalPlace "p") Wildcard]

productionWithVariable :: Assertion
productionWithVariable = Right expected @=? transitions . mainNet <$> parseCsl "PLACES { p } TRANSITION t { MATCH { p: V } PRODUCE { p: V } }"
  where
    expected = Set.singleton t
    t = (mkTransition "t")
        { patterns = Map.fromList [(LocalPlace "p", VariablePattern (Var "V"))]
        , productions = Map.fromList [(LocalPlace "p", Construct $ Substitution (Var "V"))]
        }

effectsTests :: TestTree
effectsTests = testGroup "Effects"
    [ testCase "Duplicate Effect" duplicateEffect
    , testCase "Effect on Input Place" effectOnInputPlace
    , testCase "Effect on Inexistent Place" effectOnInexistentPlace
    , testCase "Effect on Production Place" effectOnProductionPlace
    , testCase "Inexistent Effect" inexistentEffect
    , testCase "writeWord8 (with variable)" writeWord8WithVariable
    , testCase "writeWord8 (too few arguments)" writeWord8WithTooFewArguments
    , testCase "writeWord8 (too many arguments)" writeWord8WithTooManyArguments
    , testCase "writeWord8 (invalid argument)" writeWord8WithInvalidArgument
    , testCase "Wildcard" wildcardEffect
    , testCase "Variable" substitutionEffect
    ]

duplicateEffect :: Assertion
duplicateEffect = Left expected @=? parseCsl "PLACES { p } TRANSITION t { EFFECTS { p: writeWord8(4) p: writeWord8(2) } }"
  where
    expected = map DuringConversion [DuplicateEffect (LocalTransition "t") (LocalPlace "p")]

effectOnInputPlace :: Assertion
effectOnInputPlace = Left expected @=? parseCsl "INTERFACE { INPUT { s p } } TRANSITION t { MATCH { s: S } EFFECTS { p: writeWord8(S, 42) } }"
  where
    expected = map DuringValidation [EffectOnConsumeOnlyPlace (LocalTransition "t") (LocalPlace "p")]

effectOnInexistentPlace :: Assertion
effectOnInexistentPlace = Left expected @=? parseCsl "PLACES { s } TRANSITION t { MATCH { s: S } EFFECTS { p: writeWord8(S, 42) } }"
  where
    expected = map DuringValidation [EffectOnInexistentPlace (LocalTransition "t") (LocalPlace "p")]

effectOnProductionPlace :: Assertion
effectOnProductionPlace = Left expected @=? transitions . mainNet <$> parseCsl "PLACES { p s } TRANSITION t { MATCH { s: S } PRODUCE { p: 42 } EFFECTS { p: writeWord8(S, 42) } }"
  where
    expected = map DuringConversion [EffectOnProductionPlace (LocalTransition "t") (LocalPlace "p")]

inexistentEffect :: Assertion
inexistentEffect = Left expected @=? transitions . mainNet <$> parseCsl "PLACES { p } TRANSITION t { EFFECTS { p: launchMissiles(42) } }"
  where
    expected = map DuringConversion [InexistentEffect (LocalTransition "t") (LocalPlace "p") (Symbol "launchMissiles")]

writeWord8WithVariable :: Assertion
writeWord8WithVariable = Right expected @=? transitions . mainNet <$> parseCsl "PLACES { s p } TRANSITION t { MATCH { s: S p: V } EFFECTS { p: writeWord8(S, V) } }"
  where
    expected = Set.singleton t
    t = (mkTransition "t")
        { patterns = Map.fromList [(LocalPlace "p", VariablePattern (Var "V")), (LocalPlace "s", VariablePattern (Var "S"))]
        , productions = Map.fromList [(LocalPlace "p", Evaluate $ WriteWord8 (Substitution $ Var "S") (Substitution $ Var "V"))]
        }

writeWord8WithTooFewArguments :: Assertion
writeWord8WithTooFewArguments = Left expected @=? transitions . mainNet <$> parseCsl "PLACES { p } TRANSITION t { EFFECTS { p: writeWord8 } }"
  where
    expected = map DuringConversion [ArityMismatchForEffect (LocalTransition "t") (LocalPlace "p") (Symbol "writeWord8") 2 0]

writeWord8WithTooManyArguments :: Assertion
writeWord8WithTooManyArguments = Left expected @=? transitions . mainNet <$> parseCsl "PLACES { p } TRANSITION t { EFFECTS { p: writeWord8(3, 1, 4) } }"
  where
    expected = map DuringConversion [ArityMismatchForEffect (LocalTransition "t") (LocalPlace "p") (Symbol "writeWord8") 2 3]

writeWord8WithInvalidArgument :: Assertion
writeWord8WithInvalidArgument = Left expected @=? transitions . mainNet <$> parseCsl "PLACES { p s } TRANSITION t { MATCH { s: S } EFFECTS { p: writeWord8(S, _) } }"
  where
    expected = map DuringConversion [InvalidProduction (LocalTransition "t") (LocalPlace "p") Wildcard]

wildcardEffect :: Assertion
wildcardEffect = Left expected @=? transitions . mainNet <$> parseCsl "PLACES { p } TRANSITION t { EFFECTS { p: _ } }"
  where
    expected = map DuringConversion [InvalidEffect (LocalTransition "t") (LocalPlace "p") Wildcard]

substitutionEffect :: Assertion
substitutionEffect = Left expected @=? transitions . mainNet <$> parseCsl "PLACES { p } TRANSITION t { EFFECTS { p: V } }"
  where
    expected = map DuringConversion [InvalidEffect (LocalTransition "t") (LocalPlace "p") (Variable "V")]

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
nonOverlappingPatterns = case parseCsl "PLACES { s p r } TRANSITION e1 { MATCH { s: S p: nil } EFFECTS { r: writeWord8(S, 0x45) } } TRANSITION e2 { MATCH { s: S p: cons(_, _) } EFFECTS { r: writeWord8(S, 0x4E) } }" of
    Left errors -> assertFailure $ show errors
    Right _ -> return ()

slingActivation :: Assertion
slingActivation = case parseCsl "PLACES { s p r } TRANSITION t { PRODUCE { p: cons(0x34, cons(0x32, nil)) } } TRANSITION e { MATCH { s: S p: cons(Byte, Bytes) } PRODUCE { s: S p: Bytes } EFFECTS { r: writeWord8(S, Byte) } }" of
    Left errors -> assertFailure $ show errors
    Right _ -> return ()

structuralConflictWithSling :: Assertion
structuralConflictWithSling = Left expected @=? parseCsl "PLACES { s p } TRANSITION t { MATCH { p: X } PRODUCE { p: nil } } TRANSITION e { MATCH { s: S p: X } EFFECTS { p: writeWord8(S, X) } }"
  where
    expected = map DuringValidation [StructuralConflictWithEffectfulTransition (LocalTransition "e") $ Set.singleton (LocalTransition "t")]

structuralConflictInPreset :: Assertion
structuralConflictInPreset = Left expected @=? parseCsl "PLACES { s p q } TRANSITION t { MATCH { p: _ } } TRANSITION e { MATCH { s: S p: X } EFFECTS { q: writeWord8(S, X) } }"
  where
    expected = map DuringValidation [StructuralConflictWithEffectfulTransition (LocalTransition "e") $ Set.singleton (LocalTransition "t")]

structuralConflictInPostset :: Assertion
structuralConflictInPostset = Left expected @=? parseCsl "PLACES { p q r s1 s2 } TRANSITION e1 { MATCH { s1: S p: X } EFFECTS { r: writeWord8(S, X) } } TRANSITION e2 { MATCH { s2: S q: X } EFFECTS { r: writeWord8(S, X) } }"
  where
    expected = map DuringValidation
        [ StructuralConflictWithEffectfulTransition (LocalTransition "e1") $ Set.singleton (LocalTransition "e2")
        , StructuralConflictWithEffectfulTransition (LocalTransition "e2") $ Set.singleton (LocalTransition "e1")
        ]

structuralConflictBetweenMultipleTransitions :: Assertion
structuralConflictBetweenMultipleTransitions = Left expected @=? parseCsl "PLACES { p q r s1 s2 } TRANSITION t { PRODUCE { r: nil } } TRANSITION e1 { MATCH { s1: S p: X } EFFECTS { r: writeWord8(S, X) } } TRANSITION e2 { MATCH { s2: S q: X } EFFECTS { r: writeWord8(S, X) } }"
  where
    expected = map DuringValidation
        [ StructuralConflictWithEffectfulTransition (LocalTransition "e1") $ Set.fromList [LocalTransition "e2", LocalTransition "t"]
        , StructuralConflictWithEffectfulTransition (LocalTransition "e2") $ Set.fromList [LocalTransition "e1", LocalTransition "t"]
        ]

multipleStructuralConflicts :: Assertion
multipleStructuralConflicts = Left expected @=? parseCsl "PLACES { p q r s1 s2 } TRANSITION t { MATCH { p: _ } } TRANSITION e1 { MATCH { s1: S p: X } EFFECTS { r: writeWord8(S, X) } } TRANSITION e2 { MATCH { s2: S q: X } EFFECTS { r: writeWord8(S, X) } }"
  where
    expected = map DuringValidation
        [ StructuralConflictWithEffectfulTransition (LocalTransition "e1") $ Set.fromList [LocalTransition "e2", LocalTransition "t"]
        , StructuralConflictWithEffectfulTransition (LocalTransition "e2") $ Set.fromList [LocalTransition "e1"]
        ]
