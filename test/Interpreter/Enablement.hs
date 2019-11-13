module Interpreter.Enablement where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map.Strict as Map (fromList)

import Csili.Interpreter
import Csili.Program

tests :: TestTree
tests = testGroup "Enablement"
    [ testCase "Disabled (unmarked preset)" unmarkedPreset
    , testCase "Disabled (marked postset)" markedPostset
    , testCase "Disabled (no match)" noMatch
    , testCase "Enabled (no preset)" noPreset
    , testCase "Enabled (no postset)" noPostset
    , testCase "Enabled (match all)" matchAll
    ]

unmarkedPreset :: Assertion
unmarkedPreset = False @=? isEnabled program (Transition "t")
  where
    program = empty
        { patterns = Map.fromList [(Transition "t", Map.fromList [(Place "p1", Wildcard)])]
        , productions = Map.fromList [(Transition "t", Map.fromList [(Place "p2", Function (Symbol "nil") [])])]
        }

markedPostset :: Assertion
markedPostset = False @=? isEnabled program (Transition "t")
  where
    program = empty
        { initialMarking = Map.fromList [(Place "p1", Function (Symbol "nil") []), (Place "p2", Function (Symbol "nil") [])]
        , patterns = Map.fromList [(Transition "t", Map.fromList [(Place "p1", Wildcard)])]
        , productions = Map.fromList [(Transition "t", Map.fromList [(Place "p2", Function (Symbol "nil") [])])]
        }

noPreset :: Assertion
noPreset = True @=? isEnabled program (Transition "t")
  where
    program = empty
        { productions = Map.fromList [(Transition "t", Map.fromList [(Place "p", Function (Symbol "nil") [])])]
        }

noPostset :: Assertion
noPostset = True @=? isEnabled program (Transition "t")
  where
    program = empty
        { initialMarking = Map.fromList [(Place "p", Function (Symbol "nil") [])]
        , patterns = Map.fromList [(Transition "t", Map.fromList [(Place "p", Variable (Var "V"))])]
        }

noMatch :: Assertion
noMatch = False @=? isEnabled program (Transition "t")
  where
    program = empty
        { initialMarking = Map.fromList [(Place "p1", Function (Symbol "nil") [])]
        , patterns = Map.fromList [(Transition "t", Map.fromList [(Place "p1", Function (Symbol "cons") [Wildcard, Wildcard])])]
        , productions = Map.fromList [(Transition "t", Map.fromList [(Place "p2", Function (Symbol "nil") [])])]
        }

matchAll :: Assertion
matchAll = True @=? isEnabled program (Transition "t")
  where
    program = empty
        { initialMarking = Map.fromList [(Place "p1", Function (Symbol "nil") [])]
        , patterns = Map.fromList [(Transition "t", Map.fromList [(Place "p1", Wildcard)])]
        , productions = Map.fromList [(Transition "t", Map.fromList [(Place "p2", Function (Symbol "nil") [])])]
        }
