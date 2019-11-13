module Interpreter.Program.Bool where

import qualified Data.Map.Strict as Map
import Test.Tasty

import Csili.Program
import Csili.Interpreter

import Interpreter.Program.Test

tests :: TestTree
tests = testGroup "bool"
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
falseAndFalse = testProgram "false /\\ false" andProgram marking expectation
  where
    marking :: Marking
    marking = Map.fromList [(Place "input1", false), (Place "input2", false)]

    expectation :: Marking
    expectation = Map.fromList [(Place "output", false)]

falseAndTrue :: TestTree
falseAndTrue = testProgram "false /\\ true" andProgram marking expectation
  where
    marking :: Marking
    marking = Map.fromList [(Place "input1", false), (Place "input2", true)]

    expectation :: Marking
    expectation = Map.fromList [(Place "output", false)]

trueAndFalse :: TestTree
trueAndFalse = testProgram "true /\\ false" andProgram marking expectation
  where
    marking :: Marking
    marking = Map.fromList [(Place "input1", true), (Place "input2", false)]

    expectation :: Marking
    expectation = Map.fromList [(Place "output", false)]

trueAndTrue :: TestTree
trueAndTrue = testProgram "true /\\ true" andProgram marking expectation
  where
    marking :: Marking
    marking = Map.fromList [(Place "input1", true), (Place "input2", true)]

    expectation :: Marking
    expectation = Map.fromList [(Place "output", false)]


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
falseOrFalse = testProgram "false \\/ false" orProgram marking expectation
  where
    marking :: Marking
    marking = Map.fromList [(Place "input1", false), (Place "input2", false)]

    expectation :: Marking
    expectation = Map.fromList [(Place "output", false)]

falseOrTrue :: TestTree
falseOrTrue = testProgram "false \\/ true" orProgram marking expectation
  where
    marking :: Marking
    marking = Map.fromList [(Place "input1", false), (Place "input2", true)]

    expectation :: Marking
    expectation = Map.fromList [(Place "output", true)]

trueOrFalse :: TestTree
trueOrFalse = testProgram "true \\/ false" orProgram marking expectation
  where
    marking :: Marking
    marking = Map.fromList [(Place "input1", true), (Place "input2", false)]

    expectation :: Marking
    expectation = Map.fromList [(Place "output", true)]

trueOrTrue :: TestTree
trueOrTrue = testProgram "true \\/ true" orProgram marking expectation
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

notFalse :: TestTree
notFalse = testProgram "!false" orProgram marking expectation
  where
    marking :: Marking
    marking = Map.fromList [(Place "input", false)]

    expectation :: Marking
    expectation = Map.fromList [(Place "output", true)]

notTrue :: TestTree
notTrue = testProgram "!true" orProgram marking expectation
  where
    marking :: Marking
    marking = Map.fromList [(Place "input", true)]

    expectation :: Marking
    expectation = Map.fromList [(Place "output", false)]

false :: Term
false = Function (Symbol "false") []

true :: Term
true = Function (Symbol "true") []
