module Program where

import Test.Tasty

import qualified Program.Places
import qualified Program.Pattern

tests :: TestTree
tests = testGroup "Program"
    [ Program.Places.tests
    , Program.Pattern.tests
    ]
