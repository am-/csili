module Interpreter.Program.IO where

import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit
import System.Process (createPipe)

import Csili.Program
import Csili.Interpreter

import Interpreter.Program.Test
import System.IO (hSetBinaryMode, hGetContents)

tests :: TestTree
tests = testGroup "IO"
    [ helloWorld
    ]

helloWorldProgram :: FilePath
helloWorldProgram = "examples/io/hello-world.csl"

helloWorld :: TestTree
helloWorld = testProgram "Hello World" helloWorldProgram $ \program -> do
    (readHandle, writeHandle) <- createPipe
    hSetBinaryMode readHandle True
    hSetBinaryMode writeHandle True
    _ <- run program (Map.singleton (Place "stream") (Resource writeHandle))
    text <- hGetContents readHandle
    "Hello World!" @=? text
