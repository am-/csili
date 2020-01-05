module Frontend.Program.TokenType
( tests
) where

import qualified Data.Map.Strict as Map

import Test.Tasty
import Test.Tasty.HUnit

import Csili.Program
import Csili.Frontend

tests :: TestTree
tests = testGroup "Token Type"
    [ testCase "Duplicate Token Type" duplicateTokenType
    , testCase "Single Constructor" singleConstructor
    ]

duplicateTokenType :: Assertion
duplicateTokenType = Left expectation @=? parseCsl program
  where
    expectation = [DuringConversion $ DuplicateTokenType (TokenTypeName "void")] 
    program = "TOKEN void {} TOKEN void {}"

singleConstructor :: Assertion
singleConstructor = Right expectation @=? tokenTypes <$> parseCsl program
  where
    expectation = Map.singleton (TokenTypeName "blackToken") (TokenTypeConstructors constructors)
    constructors = Map.singleton (TokenConstructorName "blackToken") []
    program = "TOKEN blackToken { blackToken }"
