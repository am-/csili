module Interpreter.FiringRule where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map.Strict as Map (empty, fromList)

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
    False @=? isEnabled program (Transition "t")
    Nothing @=? fire program (Transition "t")
  where
    program = empty
        { patterns = Map.fromList [(Transition "t", Map.fromList [(Place "p1", WildcardPattern)])]
        , productions = Map.fromList [(Transition "t", Map.fromList [(Place "p2", FunctionProduction (Symbol "nil") [])])]
        }

markedPostset :: Assertion
markedPostset = do
    False @=? isEnabled program (Transition "t")
    Nothing @=? fire program (Transition "t")
  where
    program = empty
        { initialMarking = Map.fromList [(Place "p1", FunctionToken (Symbol "nil") []), (Place "p2", FunctionToken (Symbol "nil") [])]
        , patterns = Map.fromList [(Transition "t", Map.fromList [(Place "p1", WildcardPattern)])]
        , productions = Map.fromList [(Transition "t", Map.fromList [(Place "p2", FunctionProduction (Symbol "nil") [])])]
        }

noPreset :: Assertion
noPreset = do
    True @=? isEnabled program (Transition "t")
    Just marking @=? fire program (Transition "t")
  where
    place = Place "p"
    program = empty
        { productions = Map.fromList [(Transition "t", Map.fromList [(place, FunctionProduction (Symbol "nil") [])])]
        }
    marking = Map.fromList [(place, FunctionToken (Symbol "nil") [])]

noPostset :: Assertion
noPostset = do
    True @=? isEnabled program (Transition "t")
    Just marking @=? fire program (Transition "t")
  where
    program = empty
        { initialMarking = Map.fromList [(Place "p", FunctionToken (Symbol "nil") [])]
        , patterns = Map.fromList [(Transition "t", Map.fromList [(Place "p", VariablePattern (Var "V"))])]
        }
    marking = Map.empty

noMatch :: Assertion
noMatch = do
    False @=? isEnabled program (Transition "t")
    Nothing @=? fire program (Transition "t")
  where
    program = empty
        { initialMarking = Map.fromList [(Place "p1", FunctionToken (Symbol "nil") [])]
        , patterns = Map.fromList [(Transition "t", Map.fromList [(Place "p1", FunctionPattern (Symbol "cons") [WildcardPattern, WildcardPattern])])]
        , productions = Map.fromList [(Transition "t", Map.fromList [(Place "p2", FunctionProduction (Symbol "nil") [])])]
        }

sling :: Assertion
sling = do
    True @=? isEnabled program (Transition "t")
    Just marking @=? fire program (Transition "t")
  where
    var = Var "V"
    program = empty
        { initialMarking = Map.fromList [(Place "p", FunctionToken (Symbol "nil") [])]
        , patterns = Map.fromList [(Transition "t", Map.fromList [(Place "p", VariablePattern var)])]
        , productions = Map.fromList [(Transition "t", Map.fromList [(Place "p", FunctionProduction (Symbol "cons") [IntProduction 1, Substitution var])])]
        }
    marking = Map.fromList [(Place "p", FunctionToken (Symbol "cons") [IntToken 1, FunctionToken (Symbol "nil") []])]

matchAll :: Assertion
matchAll = do
    True @=? isEnabled program (Transition "t")
    Just marking @=? fire program (Transition "t")
  where
    p2 = Place "p2"
    nil = FunctionToken (Symbol "nil") []
    var = Var "V"
    program = empty
        { initialMarking = Map.fromList [(Place "p1", nil)]
        , patterns = Map.fromList [(Transition "t", Map.fromList [(Place "p1", VariablePattern var)])]
        , productions = Map.fromList [(Transition "t", Map.fromList [(p2, Substitution var)])]
        }
    marking = Map.fromList [(p2, nil)]


missingVariable :: Assertion
missingVariable = do
    True @=? isEnabled program (Transition "t")
    Nothing @=? fire program (Transition "t")
  where
    p2 = Place "p2"
    program = empty
        { initialMarking = Map.fromList [(Place "p1", FunctionToken (Symbol "nil") [])]
        , patterns = Map.fromList [(Transition "t", Map.fromList [(Place "p1", WildcardPattern)])]
        , productions = Map.fromList [(Transition "t", Map.fromList [(p2, Substitution (Var "V"))])]
        }