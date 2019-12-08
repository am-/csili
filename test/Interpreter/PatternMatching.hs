module Interpreter.PatternMatching where

import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit

import Csili.Program
import Csili.Interpreter

tests :: TestTree
tests = testGroup "Pattern Matching"
    [ matching
    , substitution
    ]

matching :: TestTree
matching = testGroup "Matching"
    [ testCase "WildcardPattern Pattern (top level)" topLevelWildcardPattern
    , testCase "WildcardPattern Pattern (inside)" wildcardInside
    , testCase "Variable (top level)" topLevelVariable
    , testCase "Variable (inside)" variableInside
    , testCase "Same Int" sameInt
    , testCase "Same Function" sameFunction
    , testCase "Different Term Types" differentTermTypes
    , testCase "Different Integer Values" differentIntegerValues
    , testCase "Different Top Level Symbol" differentTopLevelSymbol
    , testCase "Different Arity" differentArity
    , testCase "Different Sub Term" differentSubterm
    ]

topLevelWildcardPattern :: Assertion
topLevelWildcardPattern = do
    Just Map.empty @=? match WildcardPattern (IntToken 42)
    Just Map.empty @=? match WildcardPattern (FunctionToken (Symbol "cons") [IntToken 1, FunctionToken (Symbol "nil") []])

wildcardInside :: Assertion
wildcardInside = Just Map.empty @=? match (FunctionPattern (Symbol "x") [WildcardPattern]) (FunctionToken (Symbol "x") [IntToken 1])

topLevelVariable :: Assertion
topLevelVariable = do
    Just (Map.fromList [(var, int)]) @=? match (VariablePattern var) int
    Just (Map.fromList [(var, function)]) @=? match (VariablePattern var) function
  where
    var = Var "V"
    int = IntToken 42
    function = FunctionToken (Symbol "cons") [IntToken 1, FunctionToken (Symbol "nil") []]

variableInside :: Assertion
variableInside = Just (Map.fromList [(var, int)]) @=? match (FunctionPattern (Symbol "x") [VariablePattern var]) function
  where
    var = Var "V"
    int = IntToken 1
    function = FunctionToken (Symbol "x") [int]

sameFunction :: Assertion
sameFunction = Just Map.empty @=? match (FunctionPattern (Symbol "nil") []) (FunctionToken (Symbol "nil") [])

sameInt :: Assertion
sameInt = Just Map.empty @=? match (IntPattern 1) (IntToken 1)

differentTermTypes :: Assertion
differentTermTypes = do
    Nothing @=? match (IntPattern 15) (FunctionToken (Symbol "nil") [])
    Nothing @=? match (FunctionPattern (Symbol "nil") []) (IntToken 15)

differentIntegerValues :: Assertion
differentIntegerValues = Nothing @=? match (IntPattern 15) (IntToken 16)

differentTopLevelSymbol :: Assertion
differentTopLevelSymbol = Nothing @=? match (FunctionPattern (Symbol "x") []) (FunctionToken (Symbol "y") [])

differentArity :: Assertion
differentArity = Nothing @=? match (FunctionPattern (Symbol "x") [IntPattern 1, IntPattern 1]) (FunctionToken (Symbol "x") [IntToken 1])

differentSubterm :: Assertion
differentSubterm = Nothing @=? match (FunctionPattern (Symbol "x") [IntPattern 1]) (FunctionToken (Symbol "x") [IntToken 2])

substitution :: TestTree
substitution = testGroup "Substitution"
    [ testCase "Identity without Variables" identityWithoutVariables
    , testCase "Complete Substitution" completeSubstitution
    , testCase "Missing Variable" missingVariable
    , testCase "Inside Substitution" insideSubstitution
    , testCase "Missing Variable Inside Substitution" missingVariableInsideSubstitution
    ]

identityWithoutVariables :: Assertion
identityWithoutVariables = do
    Just (IntToken 42) @=? substitute Map.empty (IntConstruction 42)
    Just (FunctionToken (Symbol "x") []) @=? substitute Map.empty (FunctionConstruction (Symbol "x") [])

completeSubstitution :: Assertion
completeSubstitution = Just int @=? substitute binding (Substitution var)
  where
    int = IntToken 42
    var = Var "V"
    binding = Map.fromList [(var, int)]

missingVariable :: Assertion
missingVariable = Nothing  @=? substitute binding (Substitution var)
  where
    binding = Map.empty
    var = Var "V"

insideSubstitution :: Assertion
insideSubstitution = Just expectation @=? substitute binding production
  where
    expectation = FunctionToken (Symbol "cons") [int, FunctionToken (Symbol "nil") []]
    production = FunctionConstruction (Symbol "cons") [Substitution var, FunctionConstruction (Symbol "nil") []]
    binding = Map.fromList [(var, int)]
    int = IntToken 42
    var = Var "V"

missingVariableInsideSubstitution :: Assertion
missingVariableInsideSubstitution = Nothing @=? substitute binding (FunctionConstruction (Symbol "cons") [IntConstruction 42, Substitution var])
  where
    var = Var "V"
    binding = Map.empty
