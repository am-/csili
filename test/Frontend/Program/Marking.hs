module Frontend.Program.Marking
( tests
) where

import qualified Data.Map.Strict as Map

import Test.Tasty
import Test.Tasty.HUnit

import Csili.Program
import Csili.Frontend
import Csili.Frontend.Parser (Term(..))

tests :: TestTree
tests = testGroup "Initial Marking"
    [ testCase "Duplicate Token" initialMarkingWithDuplicateToken
    , testCase "Token on Inexistent Place" initialMarkingContainingInexistentPlace
    , testCase "Token on Input Place" initialMarkingOnInputPlace
    , testCase "Token on Output Place" initialMarkingOnOutputPlace
    , testCase "Function" initialMarkingWithFunction
    , testCase "Wildcard" initialMarkingWithWildcard
    , testCase "Variable" initialMarkingWithVariable
    ]

initialMarkingWithDuplicateToken :: Assertion
initialMarkingWithDuplicateToken = Left expected @=? parseCsl "MARKING { p: nil p: 42 }"
  where
    expected = map DuringConversion [DuplicateToken (LocalPlace "p")]

initialMarkingContainingInexistentPlace :: Assertion
initialMarkingContainingInexistentPlace = Left expected @=? parseCsl "MARKING { p: nil }"
  where
    expected = map DuringValidation [TokenOnInexistentPlace (LocalPlace "p")]

initialMarkingOnInputPlace :: Assertion
initialMarkingOnInputPlace = Left expected @=? parseCsl "INTERFACE { INPUT { p } } MARKING { p: nil }"
  where
    expected = map DuringValidation [TokenOnConsumeOnlyPlace (LocalPlace "p")]

initialMarkingOnOutputPlace :: Assertion
initialMarkingOnOutputPlace = Right expected @=? initialMarking . mainNet <$> parseCsl "INTERFACE { OUTPUT { p } } MARKING { p: nil }"
  where
    expected = Map.singleton (LocalPlace "p") nil

initialMarkingWithFunction :: Assertion
initialMarkingWithFunction = Right expected @=? initialMarking . mainNet <$> parseCsl "PLACES { p } MARKING { p: cons(@, nil) }"
  where
    expected = Map.fromList [(LocalPlace "p", cons blackToken nil)]

initialMarkingWithWildcard :: Assertion
initialMarkingWithWildcard = Left expected @=? parseCsl "PLACES { p } MARKING { p: _ }"
  where
    expected = map DuringConversion [InvalidToken (LocalPlace "p") Wildcard]

initialMarkingWithVariable :: Assertion
initialMarkingWithVariable = Left expected @=? parseCsl "PLACES { p } MARKING { p: cons(V, nil) }"
  where
    expected = map DuringConversion [InvalidToken (LocalPlace "p") (Variable "V")]
