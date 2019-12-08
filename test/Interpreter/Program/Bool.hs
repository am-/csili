module Interpreter.Program.Bool where

import qualified Data.Map.Strict as Map
import Test.Tasty

import Csili.Program

import Interpreter.Program.Test

tests :: TestTree
tests = testGroup "Bool"
    [ andTests
    , orTests
    , notTests
    ]

andTests :: TestTree
andTests = testGroup "and"
    [ falseAndFalse
    , falseAndTrue
    , trueAndFalse
    , trueAndTrue
    ]

andProgram :: FilePath
andProgram = "examples/bool/and.csl"

falseAndFalse :: TestTree
falseAndFalse = testProgramAgainstMarking "false /\\ false" andProgram marking expectation
  where
    marking :: Marking
    marking = Map.fromList [(Place "input1", false), (Place "input2", false)]

    expectation :: Marking
    expectation = Map.fromList [(Place "output", false)]

falseAndTrue :: TestTree
falseAndTrue = testProgramAgainstMarking "false /\\ true" andProgram marking expectation
  where
    marking :: Marking
    marking = Map.fromList [(Place "input1", false), (Place "input2", true)]

    expectation :: Marking
    expectation = Map.fromList [(Place "output", false)]

trueAndFalse :: TestTree
trueAndFalse = testProgramAgainstMarking "true /\\ false" andProgram marking expectation
  where
    marking :: Marking
    marking = Map.fromList [(Place "input1", true), (Place "input2", false)]

    expectation :: Marking
    expectation = Map.fromList [(Place "output", false)]

trueAndTrue :: TestTree
trueAndTrue = testProgramAgainstMarking "true /\\ true" andProgram marking expectation
  where
    marking :: Marking
    marking = Map.fromList [(Place "input1", true), (Place "input2", true)]

    expectation :: Marking
    expectation = Map.fromList [(Place "output", true)]


orTests :: TestTree
orTests = testGroup "or"
    [ falseOrFalse
    , falseOrTrue
    , trueOrFalse
    , trueOrTrue
    ]

orProgram :: FilePath
orProgram = "examples/bool/or.csl"

falseOrFalse :: TestTree
falseOrFalse = testProgramAgainstMarking "false \\/ false" orProgram marking expectation
  where
    marking :: Marking
    marking = Map.fromList [(Place "input1", false), (Place "input2", false)]

    expectation :: Marking
    expectation = Map.fromList [(Place "output", false)]

falseOrTrue :: TestTree
falseOrTrue = testProgramAgainstMarking "false \\/ true" orProgram marking expectation
  where
    marking :: Marking
    marking = Map.fromList [(Place "input1", false), (Place "input2", true)]

    expectation :: Marking
    expectation = Map.fromList [(Place "output", true)]

trueOrFalse :: TestTree
trueOrFalse = testProgramAgainstMarking "true \\/ false" orProgram marking expectation
  where
    marking :: Marking
    marking = Map.fromList [(Place "input1", true), (Place "input2", false)]

    expectation :: Marking
    expectation = Map.fromList [(Place "output", true)]

trueOrTrue :: TestTree
trueOrTrue = testProgramAgainstMarking "true \\/ true" orProgram marking expectation
  where
    marking :: Marking
    marking = Map.fromList [(Place "input1", true), (Place "input2", true)]

    expectation :: Marking
    expectation = Map.fromList [(Place "output", true)]


notTests :: TestTree
notTests = testGroup "not"
    [ notFalse
    , notTrue
    ]

notProgram :: FilePath
notProgram = "examples/bool/not.csl"

notFalse :: TestTree
notFalse = testProgramAgainstMarking "!false" notProgram marking expectation
  where
    marking :: Marking
    marking = Map.fromList [(Place "input", false)]

    expectation :: Marking
    expectation = Map.fromList [(Place "output", true)]

notTrue :: TestTree
notTrue = testProgramAgainstMarking "!true" notProgram marking expectation
  where
    marking :: Marking
    marking = Map.fromList [(Place "input", true)]

    expectation :: Marking
    expectation = Map.fromList [(Place "output", false)]

false :: Token
false = FunctionToken (Symbol "false") []

true :: Token
true = FunctionToken (Symbol "true") []
