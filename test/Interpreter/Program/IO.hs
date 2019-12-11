module Interpreter.Program.IO where

import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit
import System.Process (createPipe)
import System.IO (hSetBinaryMode, hGetContents, hPutStr)

import Csili.Program
import Csili.Interpreter

import Interpreter.Program.Test

tests :: TestTree
tests = testGroup "IO"
    [ helloWorld
    , reverseEcho
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

reverseEchoProgram :: FilePath
reverseEchoProgram = "examples/io/reverse-echo.csl"

reverseEcho :: TestTree
reverseEcho = testProgram "Reverse Echo" reverseEchoProgram $ \program -> do
    (inputReadHandle, inputWriteHandle) <- createPipe
    hSetBinaryMode inputReadHandle True
    hSetBinaryMode inputWriteHandle True
    hPutStr inputWriteHandle "Hello World!\n"
    (ouputReadHandle, outputWriteHandle) <- createPipe
    hSetBinaryMode ouputReadHandle True
    hSetBinaryMode outputWriteHandle True
    _ <- run program (Map.fromList [(Place "input", Resource inputReadHandle), (Place "output", Resource outputWriteHandle)])
    text <- hGetContents ouputReadHandle
    "!dlroW olleH\n" @=? text
