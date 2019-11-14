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
    [ testCase "Wildcard Pattern (top level)" topLevelWildcard
    , testCase "Wildcard Pattern (inside)" wildcardInside
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

topLevelWildcard :: Assertion
topLevelWildcard = do
    Just Map.empty @=? match Wildcard (IntTerm 42)
    Just Map.empty @=? match Wildcard (Function (Symbol "cons") [IntTerm 1, Function (Symbol "nil") []])

wildcardInside :: Assertion
wildcardInside = Just Map.empty @=? match (Function (Symbol "x") [Wildcard]) (Function (Symbol "x") [IntTerm 1])

topLevelVariable :: Assertion
topLevelVariable = do
    Just (Map.fromList [(var, int)]) @=? match (Variable var) int
    Just (Map.fromList [(var, function)]) @=? match (Variable var) function
  where
    var = Var "V"
    int = IntTerm 42
    function = Function (Symbol "cons") [IntTerm 1, Function (Symbol "nil") []]

variableInside :: Assertion
variableInside = Just (Map.fromList [(var, int)]) @=? match (Function (Symbol "x") [Variable var]) function
  where
    var = Var "V"
    int = IntTerm 1
    function = Function (Symbol "x") [int]

sameFunction :: Assertion
sameFunction = Just Map.empty @=? match (Function (Symbol "nil") []) (Function (Symbol "nil") [])

sameInt :: Assertion
sameInt = Just Map.empty @=? match (IntTerm 1) (IntTerm 1)

differentTermTypes :: Assertion
differentTermTypes = do
    Nothing @=? match (IntTerm 15) (Function (Symbol "nil") [])
    Nothing @=? match (Function (Symbol "nil") []) (IntTerm 15)

differentIntegerValues :: Assertion
differentIntegerValues = Nothing @=? match (IntTerm 15) (IntTerm 16)

differentTopLevelSymbol :: Assertion
differentTopLevelSymbol = Nothing @=? match (Function (Symbol "x") []) (Function (Symbol "y") [])

differentArity :: Assertion
differentArity = Nothing @=? match (Function (Symbol "x") [IntTerm 1, IntTerm 1]) (Function (Symbol "x") [IntTerm 1])

differentSubterm :: Assertion
differentSubterm = Nothing @=? match (Function (Symbol "x") [IntTerm 1]) (Function (Symbol "x") [IntTerm 2])

substitution :: TestTree
substitution = testGroup "Substitution"
    [ testCase "Identity without Variables" identityWithoutVariables
    , testCase "Complete Substitution" completeSubstitution
    , testCase "Missing Variable" missingVariable
    , testCase "Inside Substitution" insideSubstitution
    , testCase "Missing Variable Inside Substitution" missingVariableInsideSubstitution
    ]

identityWithoutVariables :: Assertion
identityWithoutVariables = mapM_ (\term -> Just term @=? substitute Map.empty term) terms
  where
    terms = [IntTerm 42, Function (Symbol "x") []]

completeSubstitution :: Assertion
completeSubstitution = Just term @=? substitute binding (Variable var)
  where
    term = IntTerm 42
    var = Var "V"
    binding = Map.fromList [(var, term)]

missingVariable :: Assertion
missingVariable = Nothing  @=? substitute binding (Variable var)
  where
    binding = Map.empty
    var = Var "V"

insideSubstitution :: Assertion
insideSubstitution = Just (Function (Symbol "cons") [term, Function (Symbol "nil") []]) @=? substitute binding (Function (Symbol "cons") [Variable var, Function (Symbol "nil") []])
  where
    term = IntTerm 42
    var = Var "V"
    binding = Map.fromList [(var, term)]

missingVariableInsideSubstitution :: Assertion
missingVariableInsideSubstitution = Nothing @=? substitute binding (Function (Symbol "cons") [IntTerm 42, Variable var])
  where
    var = Var "V"
    binding = Map.empty
