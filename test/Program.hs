module Program where

import qualified Data.Set as Set

import Test.Tasty
import Test.Tasty.HUnit

import Csili.Program

tests :: TestTree
tests = testGroup "Program"
    [ testCase "Places (empty interface, without internal places)" emptyInterfaceWithoutInternalPlaces
    , testCase "Places (non-empty interface, without internal places)" nonEmptyInterfaceWithoutInternalPlaces
    , testCase "Places (empty interface, with internal places)" emptyInterfaceWithInternalPlaces
    , testCase "Places (non-empty interface, with internal places)" nonEmptyInterfaceWithInternalPlaces
    ]

emptyInterfaceWithoutInternalPlaces :: Assertion
emptyInterfaceWithoutInternalPlaces = Set.empty @=? places empty

nonEmptyInterfaceWithoutInternalPlaces :: Assertion
nonEmptyInterfaceWithoutInternalPlaces = Set.union inputPlaces outputPlaces @=? places program
  where
    program = empty { interface = Interface inputPlaces outputPlaces }
    inputPlaces = Set.fromList [Place "input1", Place "input2"]
    outputPlaces = Set.fromList [Place "output"]

emptyInterfaceWithInternalPlaces :: Assertion
emptyInterfaceWithInternalPlaces = internal @=? places program
  where
    program = empty { internalPlaces = internal }
    internal = Set.fromList [Place "x", Place "y", Place "z"]

nonEmptyInterfaceWithInternalPlaces :: Assertion
nonEmptyInterfaceWithInternalPlaces = Set.unions [internal, inputPlaces, outputPlaces] @=? places program
  where
    program = empty { interface = Interface inputPlaces outputPlaces, internalPlaces = internal }
    inputPlaces = Set.fromList [Place "input1", Place "input2"]
    outputPlaces = Set.fromList [Place "output"]
    internal = Set.fromList [Place "x", Place "y", Place "z"]
