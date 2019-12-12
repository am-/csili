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
    , testCase "Same Function" sameFunction
    , testCase "Different Top Level Symbol" differentTopLevelSymbol
    , testCase "Different Arity" differentArity
    , testCase "Different Sub Term" differentSubterm
    ]

topLevelWildcardPattern :: Assertion
topLevelWildcardPattern = do
    Just Map.empty @=? match WildcardPattern (cons blackToken nil)

wildcardInside :: Assertion
wildcardInside = Just Map.empty @=? match (FunctionPattern (Symbol "x") [WildcardPattern]) (FunctionToken (Symbol "x") [FunctionToken (Symbol "token") []])

topLevelVariable :: Assertion
topLevelVariable = Just (Map.fromList [(var, function)]) @=? match (VariablePattern var) function
  where
    var = Var "V"
    function = cons blackToken nil

variableInside :: Assertion
variableInside = Just (Map.fromList [(var, token)]) @=? match (FunctionPattern (Symbol "x") [VariablePattern var]) function
  where
    var = Var "V"
    token = FunctionToken (Symbol "token") []
    function = FunctionToken (Symbol "x") [token]

sameFunction :: Assertion
sameFunction = Just Map.empty @=? match (FunctionPattern (Symbol "nil") []) (nil)

differentTopLevelSymbol :: Assertion
differentTopLevelSymbol = Nothing @=? match (FunctionPattern (Symbol "x") []) (FunctionToken (Symbol "y") [])

differentArity :: Assertion
differentArity = Nothing @=? match (FunctionPattern (Symbol "x") [tokenPattern, tokenPattern]) (FunctionToken (Symbol "x") [blackToken])
  where
    tokenPattern = FunctionPattern (Symbol "token") []

differentSubterm :: Assertion
differentSubterm = Nothing @=? match (FunctionPattern (Symbol "x") [FunctionPattern (Symbol "true") []]) (FunctionToken (Symbol "x") [false])

substitution :: TestTree
substitution = testGroup "Substitution"
    [ testCase "Identity without Variables" identityWithoutVariables
    , testCase "Complete Substitution" completeSubstitution
    , testCase "Missing Variable" missingVariable
    , testCase "Inside Substitution" insideSubstitution
    , testCase "Missing Variable Inside Substitution" missingVariableInsideSubstitution
    ]

identityWithoutVariables :: Assertion
identityWithoutVariables = Just (FunctionToken (Symbol "x") []) @=? substitute Map.empty (FunctionConstruction (Symbol "x") [])

completeSubstitution :: Assertion
completeSubstitution = Just blackToken @=? substitute binding (Substitution var)
  where
    var = Var "V"
    binding = Map.fromList [(var, blackToken)]

missingVariable :: Assertion
missingVariable = Nothing  @=? substitute binding (Substitution var)
  where
    binding = Map.empty
    var = Var "V"

insideSubstitution :: Assertion
insideSubstitution = Just expectation @=? substitute binding production
  where
    expectation = cons blackToken nil
    production = FunctionConstruction (Symbol "cons") [Substitution var, FunctionConstruction (Symbol "nil") []]
    binding = Map.fromList [(var, blackToken)]
    var = Var "V"

missingVariableInsideSubstitution :: Assertion
missingVariableInsideSubstitution = Nothing @=? substitute binding (FunctionConstruction (Symbol "cons") [FunctionConstruction (Symbol "token") [], Substitution var])
  where
    var = Var "V"
    binding = Map.empty
