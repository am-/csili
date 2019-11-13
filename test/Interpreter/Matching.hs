module Interpreter.Matching where

import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit

import Csili.Program
import Csili.Interpreter

tests :: TestTree
tests = testGroup "Matching"
    [ testCase "Wildcard Pattern (top level)" topLevelWildcard
    , testCase "Wildcard Pattern (inside)" wildcardInside
    , testCase "Variable (top level)" topLevelVariable
    , testCase "Variable (inside)" variableInside
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