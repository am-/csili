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
        { patterns = Map.fromList [(Transition "t", Map.fromList [(Place "p1", Wildcard)])]
        , productions = Map.fromList [(Transition "t", Map.fromList [(Place "p2", Function (Symbol "nil") [])])]
        }

markedPostset :: Assertion
markedPostset = do
    False @=? isEnabled program (Transition "t")
    Nothing @=? fire program (Transition "t")
  where
    program = empty
        { initialMarking = Map.fromList [(Place "p1", Function (Symbol "nil") []), (Place "p2", Function (Symbol "nil") [])]
        , patterns = Map.fromList [(Transition "t", Map.fromList [(Place "p1", Wildcard)])]
        , productions = Map.fromList [(Transition "t", Map.fromList [(Place "p2", Function (Symbol "nil") [])])]
        }

noPreset :: Assertion
noPreset = do
    True @=? isEnabled program (Transition "t")
    Just marking @=? fire program (Transition "t")
  where
    place = Place "p"
    function = Function (Symbol "nil") []
    program = empty
        { productions = Map.fromList [(Transition "t", Map.fromList [(place, function)])]
        }
    marking = Map.fromList [(place, function)]

noPostset :: Assertion
noPostset = do
    True @=? isEnabled program (Transition "t")
    Just marking @=? fire program (Transition "t")
  where
    program = empty
        { initialMarking = Map.fromList [(Place "p", Function (Symbol "nil") [])]
        , patterns = Map.fromList [(Transition "t", Map.fromList [(Place "p", Variable (Var "V"))])]
        }
    marking = Map.empty

noMatch :: Assertion
noMatch = do
    False @=? isEnabled program (Transition "t")
    Nothing @=? fire program (Transition "t")
  where
    program = empty
        { initialMarking = Map.fromList [(Place "p1", Function (Symbol "nil") [])]
        , patterns = Map.fromList [(Transition "t", Map.fromList [(Place "p1", Function (Symbol "cons") [Wildcard, Wildcard])])]
        , productions = Map.fromList [(Transition "t", Map.fromList [(Place "p2", Function (Symbol "nil") [])])]
        }

sling :: Assertion
sling = do
    True @=? isEnabled program (Transition "t")
    Just marking @=? fire program (Transition "t")
  where
    nil = Function (Symbol "nil") []
    var = Var "V"
    program = empty
        { initialMarking = Map.fromList [(Place "p", nil)]
        , patterns = Map.fromList [(Transition "t", Map.fromList [(Place "p", Variable var)])]
        , productions = Map.fromList [(Transition "t", Map.fromList [(Place "p", Function (Symbol "cons") [IntTerm 1, Variable var])])]
        }
    marking = Map.fromList [(Place "p", Function (Symbol "cons") [IntTerm 1, nil])]

matchAll :: Assertion
matchAll = do
    True @=? isEnabled program (Transition "t")
    Just marking @=? fire program (Transition "t")
  where
    p2 = Place "p2"
    nil = Function (Symbol "nil") []
    var = Var "V"
    program = empty
        { initialMarking = Map.fromList [(Place "p1", nil)]
        , patterns = Map.fromList [(Transition "t", Map.fromList [(Place "p1", Variable var)])]
        , productions = Map.fromList [(Transition "t", Map.fromList [(p2, Variable var)])]
        }
    marking = Map.fromList [(p2, nil)]


missingVariable :: Assertion
missingVariable = do
    True @=? isEnabled program (Transition "t")
    Nothing @=? fire program (Transition "t")
  where
    p2 = Place "p2"
    program = empty
        { initialMarking = Map.fromList [(Place "p1", Function (Symbol "nil") [])]
        , patterns = Map.fromList [(Transition "t", Map.fromList [(Place "p1", Wildcard)])]
        , productions = Map.fromList [(Transition "t", Map.fromList [(p2, Variable (Var "V"))])]
        }