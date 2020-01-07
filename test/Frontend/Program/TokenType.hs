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
    , testCase "Undefined Variable in Constructor Argument" undefinedVariableInArgument
    , testCase "Wildcard in Constructor Argument" wildcardInArgument
    , testCase "Constructor with Multiple Parameters" constructorWithMultipleParameters
    , testCase "Single Parameter Token Type" singleParameterTokenType
    , testCase "Multi Parameter Token Type" multiParameterTokenType
    ]

duplicateTokenType :: Assertion
duplicateTokenType = Left expectation @=? parseCsl program
  where
    expectation = [DuringConversion . DuplicateTokenType $ TokenTypeName "void"]
    program = "TOKEN void {} TOKEN void {}"

singleConstructor :: Assertion
singleConstructor = Right expectation @=? tokenTypes <$> parseCsl program
  where
    expectation = Map.singleton (TokenTypeDefinition (TokenTypeName "blackToken") []) (TokenTypeConstructors constructors)
    constructors = Map.singleton (Symbol "blackToken") []
    program = "TOKEN blackToken { blackToken }"

duplicateConstructor :: Assertion
duplicateConstructor = Left expectation @=? tokenTypes <$> parseCsl program
  where
    expectation = [DuringConversion $ DuplicateTokenConstructor (TokenTypeName "coloredToken") (Symbol "blackToken")]
    program = "TOKEN coloredToken { blackToken blackToken }"

variableAsConstructor :: Assertion
variableAsConstructor = Left expectation @=? tokenTypes <$> parseCsl program
  where
    expectation = [DuringConversion $ VariableInTokenConstructor (TokenTypeName "blackToken") (TokenTypeVariable "X")]
    program = "TOKEN blackToken { X }"

wildcardConstructor :: Assertion
wildcardConstructor = Left expectation @=? tokenTypes <$> parseCsl program
  where
    expectation = [DuringConversion $ WildcardInTokenConstructor (TokenTypeName "blackToken")]
    program = "TOKEN blackToken { _ }"

recursiveConstructor :: Assertion
recursiveConstructor = Right expectation @=? tokenTypes <$> parseCsl program
  where
    expectation = Map.singleton (TokenTypeDefinition (TokenTypeName "nat") []) (TokenTypeConstructors constructors)
    constructors = Map.fromList
        [ (Symbol "z", [])
        , (Symbol "s", [TokenType (TokenTypeName "nat") []])
        ]
    program = T.concat
        [ "TOKEN nat { z s(nat) }"
        ]

undefinedVariableInArgument :: Assertion
undefinedVariableInArgument = Left expectation @=? tokenTypes <$> parseCsl program
  where
    expectation = [DuringConversion $ UndefinedVariableInTokenTypeArgument (TokenTypeName "maybe") (Symbol "just") (TokenTypeVariable "X")]
    program = "TOKEN maybe { nothing just(X) }"

wildcardInArgument :: Assertion
wildcardInArgument = Left expectation @=? tokenTypes <$> parseCsl program
  where
    expectation = [DuringConversion $ WildcardInTokenTypeArgument (TokenTypeName "maybe") (Symbol "just")]
    program = "TOKEN maybe { nothing just(_) }"

constructorWithMultipleParameters :: Assertion
constructorWithMultipleParameters = Right expectation @=? tokenTypes <$> parseCsl program
  where
    expectation = Map.singleton (TokenTypeDefinition (TokenTypeName "binaryTree") []) (TokenTypeConstructors constructors)
    constructors = Map.fromList
        [ (Symbol "leaf", [])
        , (Symbol "internal", [TokenType (TokenTypeName "binaryTree") [], TokenType (TokenTypeName "binaryTree") []])
        ]
    program = T.concat
        [ "TOKEN binaryTree { leaf internal(binaryTree, binaryTree) }"
        ]

singleParameterTokenType :: Assertion
singleParameterTokenType = Right expectation @=? tokenTypes <$> parseCsl program
  where
    expectation = Map.singleton (TokenTypeDefinition (TokenTypeName "list") [TokenTypeVariable "A"]) (TokenTypeConstructors constructors)
    constructors = Map.fromList
        [ (Symbol "nil", [])
        , (Symbol "cons", [TokenTypeSubstitution (TokenTypeVariable "A"), TokenType (TokenTypeName "list") [TokenTypeSubstitution (TokenTypeVariable "A")]])
        ]
    program = T.concat
        [ "TOKEN list(A) { nil cons(A, list(A)) }"
        ]

multiParameterTokenType :: Assertion
multiParameterTokenType = Right expectation @=? tokenTypes <$> parseCsl program
  where
    expectation = Map.singleton
        (TokenTypeDefinition (TokenTypeName "either") [TokenTypeVariable "A", TokenTypeVariable "B"])
        (TokenTypeConstructors constructors)
    constructors = Map.fromList
        [ (Symbol "left", [TokenTypeSubstitution (TokenTypeVariable "A")])
        , (Symbol "right", [TokenTypeSubstitution (TokenTypeVariable "B")])
        ]
    program = T.concat
        [ "TOKEN either(A, B) { left(A) right(B) }"
        ]
