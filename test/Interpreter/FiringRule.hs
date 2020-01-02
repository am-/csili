module Interpreter.FiringRule where

import qualified Data.Map.Strict as Map (empty, fromList)

import Test.Tasty
import Test.Tasty.HUnit

import Csili.Interpreter
import Csili.Program

tests :: TestTree
tests = testGroup "Firing Rule"
    [ testCase "Disabled (unmarked preset)" unmarkedPreset
    , testCase "Disabled (marked production place)" markedProductionPlace
    , testCase "Disabled (marked effect place)" markedEffectPlace
    , testCase "Disabled (no match)" noMatch
    , testCase "Enabled (no preset)" noPreset
    , testCase "Enabled (no postset)" noPostset
    , testCase "Enabled (sling)" sling
    , testCase "Enabled (match all)" matchAll
    , testCase "Enabled (missing variable)" missingVariable
    ]

unmarkedPreset :: Assertion
unmarkedPreset = do
    False @=? isEnabled t emptyMarking
    fire t emptyMarking >>= (Nothing @=?)
  where
    t = (mkTransition "t")
        { patterns = Map.fromList [(LocalPlace "p1", WildcardPattern)]
        , productions = Map.fromList [(LocalPlace "p2", Construct $ FunctionConstruction (Symbol "nil") [])]
        }

markedProductionPlace :: Assertion
markedProductionPlace = do
    False @=? isEnabled t marking
    fire t marking >>= (Nothing @=?)
  where
    marking = Map.fromList [(LocalPlace "p1", nil), (LocalPlace "p2", nil)]
    t = (mkTransition "t")
        { patterns = Map.fromList [(LocalPlace "p1", WildcardPattern)]
        , productions = Map.fromList [(LocalPlace "p2", Construct $ FunctionConstruction (Symbol "nil") [])]
        }

markedEffectPlace :: Assertion
markedEffectPlace = do
    False @=? isEnabled t marking
    fire t marking >>= (Nothing @=?)
  where
    marking = Map.fromList [(LocalPlace "p1", nil), (LocalPlace "p2", nil)]
    t = (mkTransition "t")
        { patterns = Map.fromList [(LocalPlace "p1", VariablePattern (Var "S"))]
        , productions = Map.fromList [(LocalPlace "p2", Evaluate $ WriteWord8 (Substitution (Var "S")) nullWord8)]
        }
    nullWord8 = FunctionConstruction (Symbol "word8") $ replicate 8 (FunctionConstruction (Symbol "zero") [])

noPreset :: Assertion
noPreset = do
    True @=? isEnabled t emptyMarking
    fire t emptyMarking >>= (Just expectedMarking @=?)
  where
    place = LocalPlace "p"
    t = (mkTransition "t")
        { productions = Map.fromList [(place, Construct $ FunctionConstruction (Symbol "nil") [])]
        }
    expectedMarking = Map.fromList [(place, nil)]

noPostset :: Assertion
noPostset = do
    True @=? isEnabled t marking
    fire t marking >>= (Just expectedMarking @=?)
  where
    marking = Map.fromList [(LocalPlace "p", nil)]
    t = (mkTransition "t")
        { patterns = Map.fromList [(LocalPlace "p", VariablePattern (Var "V"))]
        }
    expectedMarking = Map.empty

noMatch :: Assertion
noMatch = do
    False @=? isEnabled t marking
    fire t marking >>= (Nothing @=?)
  where
    marking = Map.fromList [(LocalPlace "p1", nil)]
    t = (mkTransition "t")
        { patterns = Map.fromList [(LocalPlace "p1", FunctionPattern (Symbol "cons") [WildcardPattern, WildcardPattern])]
        , productions = Map.fromList [(LocalPlace "p2", Construct $ FunctionConstruction (Symbol "nil") [])]
        }

sling :: Assertion
sling = do
    True @=? isEnabled t marking
    fire t marking >>= (Just expectedMarking @=?)
  where
    var = Var "V"
    marking = Map.fromList [(LocalPlace "p", nil)]
    t = (mkTransition "t")
        { patterns = Map.fromList [(LocalPlace "p", VariablePattern var)]
        , productions = Map.fromList [(LocalPlace "p", Construct $ FunctionConstruction (Symbol "cons") [FunctionConstruction (Symbol "blackToken") [], Substitution var])]
        }
    expectedMarking = Map.fromList [(LocalPlace "p", cons blackToken nil)]

matchAll :: Assertion
matchAll = do
    True @=? isEnabled t marking
    fire t marking >>= (Just expectedMarking @=?)
  where
    p2 = LocalPlace "p2"
    var = Var "V"
    marking = Map.fromList [(LocalPlace "p1", nil)]
    t = (mkTransition "t")
        { patterns = Map.fromList [(LocalPlace "p1", VariablePattern var)]
        , productions = Map.fromList [(p2, Construct $ Substitution var)]
        }
    expectedMarking = Map.fromList [(p2, nil)]


missingVariable :: Assertion
missingVariable = do
    True @=? isEnabled t marking
    fire t marking >>= (Nothing @=?)
  where
    p2 = LocalPlace "p2"
    marking = Map.fromList [(LocalPlace "p1", nil)]
    t = (mkTransition "t")
        { patterns = Map.fromList [(LocalPlace "p1", WildcardPattern)]
        , productions = Map.fromList [(p2, Construct $ Substitution (Var "V"))]
        }
