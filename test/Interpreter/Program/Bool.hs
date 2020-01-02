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
    , implicationTests
    , equivalenceTests
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
    marking = Map.fromList [(LocalPlace "input1", false), (LocalPlace "input2", false)]
    expectation = Map.fromList [(LocalPlace "output", false)]

falseAndTrue :: TestTree
falseAndTrue = testProgramAgainstMarking "false /\\ true" andProgram marking expectation
  where
    marking = Map.fromList [(LocalPlace "input1", false), (LocalPlace "input2", true)]
    expectation = Map.fromList [(LocalPlace "output", false)]

trueAndFalse :: TestTree
trueAndFalse = testProgramAgainstMarking "true /\\ false" andProgram marking expectation
  where
    marking = Map.fromList [(LocalPlace "input1", true), (LocalPlace "input2", false)]
    expectation = Map.fromList [(LocalPlace "output", false)]

trueAndTrue :: TestTree
trueAndTrue = testProgramAgainstMarking "true /\\ true" andProgram marking expectation
  where
    marking = Map.fromList [(LocalPlace "input1", true), (LocalPlace "input2", true)]
    expectation = Map.fromList [(LocalPlace "output", true)]


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
    marking = Map.fromList [(LocalPlace "input1", false), (LocalPlace "input2", false)]
    expectation = Map.fromList [(LocalPlace "output", false)]

falseOrTrue :: TestTree
falseOrTrue = testProgramAgainstMarking "false \\/ true" orProgram marking expectation
  where
    marking = Map.fromList [(LocalPlace "input1", false), (LocalPlace "input2", true)]
    expectation = Map.fromList [(LocalPlace "output", true)]

trueOrFalse :: TestTree
trueOrFalse = testProgramAgainstMarking "true \\/ false" orProgram marking expectation
  where
    marking = Map.fromList [(LocalPlace "input1", true), (LocalPlace "input2", false)]
    expectation = Map.fromList [(LocalPlace "output", true)]

trueOrTrue :: TestTree
trueOrTrue = testProgramAgainstMarking "true \\/ true" orProgram marking expectation
  where
    marking = Map.fromList [(LocalPlace "input1", true), (LocalPlace "input2", true)]
    expectation = Map.fromList [(LocalPlace "output", true)]


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
    marking = Map.fromList [(LocalPlace "input", false)]
    expectation = Map.fromList [(LocalPlace "output", true)]

notTrue :: TestTree
notTrue = testProgramAgainstMarking "!true" notProgram marking expectation
  where
    marking = Map.fromList [(LocalPlace "input", true)]
    expectation = Map.fromList [(LocalPlace "output", false)]

implicationTests :: TestTree
implicationTests = testGroup "implies"
    [ falseImpliesFalse
    , falseImpliesTrue
    , trueImpliesFalse
    , trueImpliesTrue
    ]

implicationProgram :: FilePath
implicationProgram = "examples/bool/implication.csl"

falseImpliesFalse :: TestTree
falseImpliesFalse = testProgramAgainstMarking "false => false" implicationProgram marking expectation
  where
    marking = Map.fromList [(LocalPlace "input1", false), (LocalPlace "input2", false)]
    expectation = Map.singleton (LocalPlace "output") true

falseImpliesTrue :: TestTree
falseImpliesTrue = testProgramAgainstMarking "false => true" implicationProgram marking expectation
  where
    marking = Map.fromList [(LocalPlace "input1", false), (LocalPlace "input2", true)]
    expectation = Map.singleton (LocalPlace "output") true

trueImpliesFalse :: TestTree
trueImpliesFalse = testProgramAgainstMarking "true => false" implicationProgram marking expectation
  where
    marking = Map.fromList [(LocalPlace "input1", true), (LocalPlace "input2", false)]
    expectation = Map.singleton (LocalPlace "output") false

trueImpliesTrue :: TestTree
trueImpliesTrue = testProgramAgainstMarking "true => true" implicationProgram marking expectation
  where
    marking = Map.fromList [(LocalPlace "input1", true), (LocalPlace "input2", true)]
    expectation = Map.singleton (LocalPlace "output") true

equivalenceTests :: TestTree
equivalenceTests = testGroup "equals"
    [ falseEqualsFalse
    , falseEqualsTrue
    , trueEqualsFalse
    , trueEqualsTrue
    ]

equivalenceProgram :: FilePath
equivalenceProgram = "examples/bool/equivalence.csl"

falseEqualsFalse :: TestTree
falseEqualsFalse = testProgramAgainstMarking "false <=> false" equivalenceProgram marking expectation
  where
    marking = Map.fromList [(LocalPlace "input1", false), (LocalPlace "input2", false)]
    expectation = Map.singleton (LocalPlace "output") true

falseEqualsTrue :: TestTree
falseEqualsTrue = testProgramAgainstMarking "false <=> true" equivalenceProgram marking expectation
  where
    marking = Map.fromList [(LocalPlace "input1", false), (LocalPlace "input2", true)]
    expectation = Map.singleton (LocalPlace "output") false

trueEqualsFalse :: TestTree
trueEqualsFalse = testProgramAgainstMarking "true <=> false" equivalenceProgram marking expectation
  where
    marking = Map.fromList [(LocalPlace "input1", true), (LocalPlace "input2", false)]
    expectation = Map.singleton (LocalPlace "output") false

trueEqualsTrue :: TestTree
trueEqualsTrue = testProgramAgainstMarking "true <=> true" equivalenceProgram marking expectation
  where
    marking = Map.fromList [(LocalPlace "input1", true), (LocalPlace "input2", true)]
    expectation = Map.singleton (LocalPlace "output") true
