module Frontend.Program.TokenType
( tests
) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Test.Tasty
import Test.Tasty.HUnit

import Csili.Program
import Csili.Frontend

tests :: TestTree
tests = testGroup "Token Type"
    [ testCase "Duplicate Token Type" duplicateTokenType
    , testCase "Single Constructor" singleConstructor
    , testCase "Duplicate Constructor" duplicateConstructor
    , testCase "Variable as Constructor" variableAsConstructor
    , testCase "Wildcard as Constructor" wildcardConstructor
    , testCase "Recursive Constructor" recursiveConstructor
    , testCase "Variable in Constructor Argument" variableInArgument
    , testCase "Wildcard in Constructor Argument" wildcardInArgument
    , testCase "Constructor with Multiple Parameters" constructorWithMultipleParameters
    ]

duplicateTokenType :: Assertion
duplicateTokenType = Left expectation @=? parseCsl program
  where
    expectation = [DuringConversion . DuplicateTokenType $ TokenType "void"]
    program = "TOKEN void {} TOKEN void {}"

singleConstructor :: Assertion
singleConstructor = Right expectation @=? tokenTypes <$> parseCsl program
  where
    expectation = Map.singleton (TokenType "blackToken") (TokenTypeConstructors constructors)
    constructors = Map.singleton (Symbol "blackToken") []
    program = "TOKEN blackToken { blackToken }"

duplicateConstructor :: Assertion
duplicateConstructor = Left expectation @=? tokenTypes <$> parseCsl program
  where
    expectation = [DuringConversion $ DuplicateTokenConstructor (TokenType "coloredToken") (Symbol "blackToken")]
    program = "TOKEN coloredToken { blackToken blackToken }"

variableAsConstructor :: Assertion
variableAsConstructor = Left expectation @=? tokenTypes <$> parseCsl program
  where
    expectation = [DuringConversion $ VariableInTokenConstructor (TokenType "blackToken") (Var "X")]
    program = "TOKEN blackToken { X }"

wildcardConstructor :: Assertion
wildcardConstructor = Left expectation @=? tokenTypes <$> parseCsl program
  where
    expectation = [DuringConversion $ WildcardInTokenConstructor (TokenType "blackToken")]
    program = "TOKEN blackToken { _ }"

recursiveConstructor :: Assertion
recursiveConstructor = Right expectation @=? tokenTypes <$> parseCsl program
  where
    expectation = Map.singleton (TokenType "nat") (TokenTypeConstructors constructors)
    constructors = Map.fromList
        [ (Symbol "z", [])
        , (Symbol "s", [TokenTypeArgument (TokenType "nat")])
        ]
    program = T.concat
        [ "TOKEN nat { z s(nat) }"
        ]

variableInArgument :: Assertion
variableInArgument = Left expectation @=? tokenTypes <$> parseCsl program
  where
    expectation = [DuringConversion $ VariableInTokenTypeArgument (TokenType "maybe") (Symbol "just") (Var "X")]
    program = "TOKEN maybe { nothing just(X) }"

wildcardInArgument :: Assertion
wildcardInArgument = Left expectation @=? tokenTypes <$> parseCsl program
  where
    expectation = [DuringConversion $ WildcardInTokenTypeArgument (TokenType "maybe") (Symbol "just")]
    program = "TOKEN maybe { nothing just(_) }"

constructorWithMultipleParameters :: Assertion
constructorWithMultipleParameters = Right expectation @=? tokenTypes <$> parseCsl program
  where
    expectation = Map.singleton (TokenType "binaryTree") (TokenTypeConstructors constructors)
    constructors = Map.fromList
        [ (Symbol "leaf", [])
        , (Symbol "internal", [TokenTypeArgument (TokenType "binaryTree"), TokenTypeArgument (TokenType "binaryTree")])
        ]
    program = T.concat
        [ "TOKEN binaryTree { leaf internal(binaryTree, binaryTree) }"
        ]
