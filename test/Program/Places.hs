module Program.Places where

import qualified Data.Set as Set

import Test.Tasty
import Test.Tasty.HUnit

import Csili.Program

tests :: TestTree
tests = testGroup "Places"
    [ testCase "Places (empty interface, without internal places)" emptyInterfaceWithoutInternalPlaces
    , testCase "Places (non-empty interface, without internal places)" nonEmptyInterfaceWithoutInternalPlaces
    , testCase "Places (empty interface, with internal places)" emptyInterfaceWithInternalPlaces
    , testCase "Places (non-empty interface, with internal places)" nonEmptyInterfaceWithInternalPlaces
    ]

emptyInterfaceWithoutInternalPlaces :: Assertion
emptyInterfaceWithoutInternalPlaces = Set.empty @=? netPlaces emptyNet

nonEmptyInterfaceWithoutInternalPlaces :: Assertion
nonEmptyInterfaceWithoutInternalPlaces = Set.union inputPlaces outputPlaces @=? netPlaces net
  where
    net = emptyNet { interface = Interface inputPlaces outputPlaces }
    inputPlaces = Set.fromList [LocalPlace "input1", LocalPlace "input2"]
    outputPlaces = Set.fromList [LocalPlace "output"]

emptyInterfaceWithInternalPlaces :: Assertion
emptyInterfaceWithInternalPlaces = internal @=? netPlaces net
  where
    net = emptyNet { internalPlaces = internal }
    internal = Set.fromList [LocalPlace "x", LocalPlace "y", LocalPlace "z"]

nonEmptyInterfaceWithInternalPlaces :: Assertion
nonEmptyInterfaceWithInternalPlaces = Set.unions [internal, inputPlaces, outputPlaces] @=? netPlaces net
  where
    net = emptyNet { interface = Interface inputPlaces outputPlaces, internalPlaces = internal }
    inputPlaces = Set.fromList [LocalPlace "input1", LocalPlace "input2"]
    outputPlaces = Set.fromList [LocalPlace "output"]
    internal = Set.fromList [LocalPlace "x", LocalPlace "y", LocalPlace "z"]
