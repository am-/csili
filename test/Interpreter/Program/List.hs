module Interpreter.Program.List where

import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit

import Csili.Program
import Csili.Interpreter

import Interpreter.Program.Test

tests :: TestTree
tests = testGroup "List"
    [ reverseEmptyList
    , reverseNonEmptyList
    , boolsGenerator
    ]

reverseProgram :: FilePath
reverseProgram = "examples/list/reverse.csl"

reverseEmptyList :: TestTree
reverseEmptyList = testProgramAgainstMarking "Reverse (empty)" reverseProgram marking expectation
  where
    marking = Map.fromList [(LocalPlace "input", nil)]
    expectation = Map.fromList [(LocalPlace "output", nil)]

reverseNonEmptyList :: TestTree
reverseNonEmptyList = testProgramAgainstMarking "Reverse (non-empty)" reverseProgram marking expectation
  where
    x = FunctionToken (Symbol "x") []
    y = FunctionToken (Symbol "y") []
    z = FunctionToken (Symbol "z") []
    list = cons x . cons y . cons z $ nil
    reversedList = cons z . cons y . cons x $ nil
    marking = Map.fromList [(LocalPlace "input", list)]
    expectation = Map.fromList [(LocalPlace "output", reversedList)]

boolsGeneratorProgram :: FilePath
boolsGeneratorProgram = "examples/list/bools-generator.csl"

boolsGenerator :: TestTree
boolsGenerator = testProgram "Bools Generator" boolsGeneratorProgram $ \program -> do
    Just (falses, trues) <- fmap (count (0, 0)) . Map.lookup (LocalPlace "bools") <$> run program marking
    assertIsBelowThreshold "falses" falses
    assertIsBelowThreshold "trues" trues
  where
    assertIsBelowThreshold label n = assertBool
        (concat [label, " is above threshold: ", show n, "/", show size])
        (n < threshold)
    size = 100
    threshold = 7 * (size `div` 10)
    marking = Map.fromList [(LocalPlace "tokens", foldl (flip cons) nil (replicate size blackToken))]
    count :: (Int, Int) -> Token -> (Int, Int)
    count (falses, trues) = \case
        FunctionToken (Symbol "nil") [] -> (falses, trues)
        FunctionToken (Symbol "cons") [bool, list] -> case bool of
            FunctionToken (Symbol "false") [] -> count (falses + 1, trues) list
            FunctionToken (Symbol "true") [] -> count (falses, trues + 1) list
            _ -> count (falses, trues) list
        _ -> (falses, trues)
