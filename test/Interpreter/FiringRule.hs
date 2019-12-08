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
    False @=? isEnabled emptyMarking t
    fire emptyMarking t >>= (Nothing @=?)
  where
    t = (mkTransition "t")
        { patterns = Map.fromList [(Place "p1", WildcardPattern)]
        , productions = Map.fromList [(Place "p2", Construct $ FunctionConstruction (Symbol "nil") [])]
        }

markedProductionPlace :: Assertion
markedProductionPlace = do
    False @=? isEnabled marking t
    fire marking t >>= (Nothing @=?)
  where
    marking = Map.fromList [(Place "p1", FunctionToken (Symbol "nil") []), (Place "p2", FunctionToken (Symbol "nil") [])]
    t = (mkTransition "t")
        { patterns = Map.fromList [(Place "p1", WildcardPattern)]
        , productions = Map.fromList [(Place "p2", Construct $ FunctionConstruction (Symbol "nil") [])]
        }

markedEffectPlace :: Assertion
markedEffectPlace = do
    False @=? isEnabled marking t
    fire marking t >>= (Nothing @=?)
  where
    marking = Map.fromList [(Place "p1", FunctionToken (Symbol "nil") []), (Place "p2", FunctionToken (Symbol "nil") [])]
    t = (mkTransition "t")
        { patterns = Map.fromList [(Place "p1", VariablePattern (Var "S"))]
        , productions = Map.fromList [(Place "p2", Evaluate $ WriteByte (Substitution (Var "S")) (IntConstruction 32))]
        }

noPreset :: Assertion
noPreset = do
    True @=? isEnabled emptyMarking t
    fire emptyMarking t >>= (Just expectedMarking @=?)
  where
    place = Place "p"
    t = (mkTransition "t")
        { productions = Map.fromList [(place, Construct $ FunctionConstruction (Symbol "nil") [])]
        }
    expectedMarking = Map.fromList [(place, FunctionToken (Symbol "nil") [])]

noPostset :: Assertion
noPostset = do
    True @=? isEnabled marking t
    fire marking t >>= (Just expectedMarking @=?)
  where
    marking = Map.fromList [(Place "p", FunctionToken (Symbol "nil") [])]
    t = (mkTransition "t")
        { patterns = Map.fromList [(Place "p", VariablePattern (Var "V"))]
        }
    expectedMarking = Map.empty

noMatch :: Assertion
noMatch = do
    False @=? isEnabled marking t
    fire marking t >>= (Nothing @=?)
  where
    marking = Map.fromList [(Place "p1", FunctionToken (Symbol "nil") [])]
    t = (mkTransition "t")
        { patterns = Map.fromList [(Place "p1", FunctionPattern (Symbol "cons") [WildcardPattern, WildcardPattern])]
        , productions = Map.fromList [(Place "p2", Construct $ FunctionConstruction (Symbol "nil") [])]
        }

sling :: Assertion
sling = do
    True @=? isEnabled marking t
    fire marking t >>= (Just expectedMarking @=?)
  where
    var = Var "V"
    marking = Map.fromList [(Place "p", FunctionToken (Symbol "nil") [])]
    t = (mkTransition "t")
        { patterns = Map.fromList [(Place "p", VariablePattern var)]
        , productions = Map.fromList [(Place "p", Construct $ FunctionConstruction (Symbol "cons") [IntConstruction 1, Substitution var])]
        }
    expectedMarking = Map.fromList [(Place "p", FunctionToken (Symbol "cons") [IntToken 1, FunctionToken (Symbol "nil") []])]

matchAll :: Assertion
matchAll = do
    True @=? isEnabled marking t
    fire marking t >>= (Just expectedMarking @=?)
  where
    p2 = Place "p2"
    nil = FunctionToken (Symbol "nil") []
    var = Var "V"
    marking = Map.fromList [(Place "p1", nil)]
    t = (mkTransition "t")
        { patterns = Map.fromList [(Place "p1", VariablePattern var)]
        , productions = Map.fromList [(p2, Construct $ Substitution var)]
        }
    expectedMarking = Map.fromList [(p2, nil)]


missingVariable :: Assertion
missingVariable = do
    True @=? isEnabled marking t
    fire marking t >>= (Nothing @=?)
  where
    p2 = Place "p2"
    marking = Map.fromList [(Place "p1", FunctionToken (Symbol "nil") [])]
    t = (mkTransition "t")
        { patterns = Map.fromList [(Place "p1", WildcardPattern)]
        , productions = Map.fromList [(p2, Construct $ Substitution (Var "V"))]
        }
