module Interpreter.Program.Test where

import qualified Data.Text.IO as T
import Test.Tasty
import Test.Tasty.HUnit

import Csili.Frontend.Parser (parseProgram)
import Csili.Interpreter

testProgram :: String -> FilePath -> Marking -> Marking -> TestTree
testProgram label programFile marking expectation = testCase label $ parseProgram <$> T.readFile programFile >>= \case
    Left reason -> assertFailure reason
    Right program -> expectation @=? run program marking
