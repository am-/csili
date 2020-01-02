module Frontend.Program.Places
( tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import Csili.Program
import Csili.Frontend

tests :: TestTree
tests = testGroup "Places"
    [ interfaceTests
    , internalPlacesTests
    ]

interfaceTests :: TestTree
interfaceTests = testGroup "Interface"
    [ testCase "Duplicate Input Place" duplicateInputPlace
    , testCase "Duplicate Output Place" duplicateOutputPlace
    , testCase "Duplicate Interface Place" duplicateInterfacePlace
    ]

duplicateInputPlace :: Assertion
duplicateInputPlace = Left expected @=? interface . mainNet <$> parseCsl "INTERFACE { INPUT { p q p q } OUTPUT {} }"
  where
    expected = map DuringConversion [DuplicateInputPlace (LocalPlace "p"), DuplicateInputPlace (LocalPlace "q")]

duplicateOutputPlace :: Assertion
duplicateOutputPlace = Left expected @=? interface . mainNet <$> parseCsl "INTERFACE { INPUT {} OUTPUT { p p } }"
  where
    expected = map DuringConversion [DuplicateOutputPlace (LocalPlace "p")]

duplicateInterfacePlace :: Assertion
duplicateInterfacePlace = Left expected @=? interface . mainNet <$> parseCsl "INTERFACE { INPUT { p } OUTPUT { p } }"
  where
    expected = map DuringValidation [OverlappingInputAndOutput (LocalPlace "p")]

internalPlacesTests :: TestTree
internalPlacesTests = testGroup "Internal Places"
    [ testCase "Duplicate Internal Place" duplicateInternalPlace
    , testCase "Internal Place inside Interface" internalPlaceInsideInterface
    ]

duplicateInternalPlace :: Assertion
duplicateInternalPlace = Left expected @=? parseCsl "PLACES { p p }"
  where
    expected = map DuringConversion [DuplicateInternalPlace (LocalPlace "p")]

internalPlaceInsideInterface :: Assertion
internalPlaceInsideInterface = Left expected @=? parseCsl "INTERFACE { INPUT { p } } PLACES { p }"
  where
    expected = [DuringValidation $ InternalPlaceInsideInterface (LocalPlace "p")]
