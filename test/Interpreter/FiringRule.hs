module Interpreter.FiringRule where

import qualified Data.Map.Strict as Map (empty, fromList)

import Test.Tasty
import Test.Tasty.HUnit

import Csili.Interpreter
import Csili.Program

tests :: TestTree
tests = testGroup "Firing Rule"
    [ testCase "Disabled (unmarked preset)" unmarkedPreset
    , testCase "Disabled (marked postset)" markedPostset
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
    Nothing @=? fire emptyMarking t
  where
    t = Transition
        (TransitionName "t")
        (Map.fromList [(Place "p1", WildcardPattern)])
        (Map.fromList [(Place "p2", FunctionProduction (Symbol "nil") [])])

markedPostset :: Assertion
markedPostset = do
    False @=? isEnabled marking t
    Nothing @=? fire marking t
  where
    marking = Map.fromList [(Place "p1", FunctionToken (Symbol "nil") []), (Place "p2", FunctionToken (Symbol "nil") [])]
    t = Transition
        (TransitionName "t")
        (Map.fromList [(Place "p1", WildcardPattern)])
        (Map.fromList [(Place "p2", FunctionProduction (Symbol "nil") [])])

noPreset :: Assertion
noPreset = do
    True @=? isEnabled emptyMarking t
    Just expectedMarking @=? fire emptyMarking t
  where
    place = Place "p"
    t = Transition
        (TransitionName "t")
        Map.empty
        (Map.fromList [(place, FunctionProduction (Symbol "nil") [])])
    expectedMarking = Map.fromList [(place, FunctionToken (Symbol "nil") [])]

noPostset :: Assertion
noPostset = do
    True @=? isEnabled marking t
    Just expectedMarking @=? fire marking t
  where
    marking = Map.fromList [(Place "p", FunctionToken (Symbol "nil") [])]
    t = Transition
        (TransitionName "t")
        (Map.fromList [(Place "p", VariablePattern (Var "V"))])
        Map.empty
    expectedMarking = Map.empty

noMatch :: Assertion
noMatch = do
    False @=? isEnabled marking t
    Nothing @=? fire marking t
  where
    marking = Map.fromList [(Place "p1", FunctionToken (Symbol "nil") [])]
    t = Transition
        (TransitionName "t")
        (Map.fromList [(Place "p1", FunctionPattern (Symbol "cons") [WildcardPattern, WildcardPattern])])
        (Map.fromList [(Place "p2", FunctionProduction (Symbol "nil") [])])

sling :: Assertion
sling = do
    True @=? isEnabled marking t
    Just expectedMarking @=? fire marking t
  where
    var = Var "V"
    marking = Map.fromList [(Place "p", FunctionToken (Symbol "nil") [])]
    t = Transition
        (TransitionName "t")
        (Map.fromList [(Place "p", VariablePattern var)])
        (Map.fromList [(Place "p", FunctionProduction (Symbol "cons") [IntProduction 1, Substitution var])])
    expectedMarking = Map.fromList [(Place "p", FunctionToken (Symbol "cons") [IntToken 1, FunctionToken (Symbol "nil") []])]

matchAll :: Assertion
matchAll = do
    True @=? isEnabled marking t
    Just expectedMarking @=? fire marking t
  where
    p2 = Place "p2"
    nil = FunctionToken (Symbol "nil") []
    var = Var "V"
    marking = Map.fromList [(Place "p1", nil)]
    t = Transition
        (TransitionName "t")
        (Map.fromList [(Place "p1", VariablePattern var)])
        (Map.fromList [(p2, Substitution var)])
    expectedMarking = Map.fromList [(p2, nil)]


missingVariable :: Assertion
missingVariable = do
    True @=? isEnabled marking t
    Nothing @=? fire marking t
  where
    p2 = Place "p2"
    marking = Map.fromList [(Place "p1", FunctionToken (Symbol "nil") [])]
    t = Transition
        (TransitionName "t")
        (Map.fromList [(Place "p1", WildcardPattern)])
        (Map.fromList [(p2, Substitution (Var "V"))])
