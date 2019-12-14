module Interpreter.Program.IO where

import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit
import System.Process (createPipe)
import System.IO (hSetBinaryMode, hGetContents, hPutStr, Handle, hGetChar, hPutChar, BufferMode(..), hSetBuffering)

import Csili.Program
import Csili.Interpreter

import Interpreter.Program.Test

tests :: TestTree
tests = testGroup "IO"
    [ helloWorld
    , reverseEcho
    , pipeWord8Twice
    ]

helloWorldProgram :: FilePath
helloWorldProgram = "examples/io/hello-world.csl"

helloWorld :: TestTree
helloWorld = testProgram "Hello World" helloWorldProgram $ \program -> do
    (readHandle, writeHandle) <- createBinaryPipe
    _ <- run program (Map.singleton (Place "stream") (Resource writeHandle))
    text <- hGetContents readHandle
    "Hello World!" @=? text

reverseEchoProgram :: FilePath
reverseEchoProgram = "examples/io/reverse-echo.csl"

reverseEcho :: TestTree
reverseEcho = testProgram "Reverse Echo" reverseEchoProgram $ \program -> do
    (inputReadHandle, inputWriteHandle) <- createBinaryPipe
    hPutStr inputWriteHandle "Hello World!\n"
    (ouputReadHandle, outputWriteHandle) <- createBinaryPipe
    _ <- run program (Map.fromList [(Place "input", Resource inputReadHandle), (Place "output", Resource outputWriteHandle)])
    text <- hGetContents ouputReadHandle
    "!dlroW olleH\n" @=? text

pipeWord8TwiceProgram :: FilePath
pipeWord8TwiceProgram = "examples/io/pipe-word8-twice.csl"

pipeWord8Twice :: TestTree
pipeWord8Twice = localOption (mkTimeout 2000000) $ testGroup "Pipe Word8 (twice)"
    [ testProgram "First to Second" pipeWord8TwiceProgram $ \program -> test program '1'
        $ \sourceReadHandle intermediateWriteHandle intermediateReadHandle sinkWriteHandle -> Map.fromList
        [ (Place "firstInput", Resource sourceReadHandle)
        , (Place "firstOutput", Resource intermediateWriteHandle)
        , (Place "secondInput", Resource intermediateReadHandle)
        , (Place "secondOutput", Resource sinkWriteHandle)
        ]
    , testProgram "Second to First" pipeWord8TwiceProgram $ \program -> test program '2'
        $ \sourceReadHandle intermediateWriteHandle intermediateReadHandle sinkWriteHandle -> Map.fromList
        [ (Place "firstInput", Resource intermediateReadHandle)
        , (Place "firstOutput", Resource sinkWriteHandle)
        , (Place "secondInput", Resource sourceReadHandle)
        , (Place "secondOutput", Resource intermediateWriteHandle)
        ]
    ]
  where
    test program character mkMarking = do
        (sourceReadHandle, sourceWriteHandle) <- createBinaryPipe
        (intermediateReadHandle, intermediateWriteHandle) <- createBinaryPipe
        (sinkReadHandle, sinkWriteHandle) <- createBinaryPipe
        hPutChar sourceWriteHandle character
        _ <- run program (mkMarking sourceReadHandle intermediateWriteHandle intermediateReadHandle sinkWriteHandle)
        actualCharacter <- hGetChar sinkReadHandle
        character @=? actualCharacter

createBinaryPipe :: IO (Handle, Handle)
createBinaryPipe = do
    (readHandle, writeHandle) <- createPipe
    hSetBinaryMode readHandle True
    hSetBuffering readHandle NoBuffering
    hSetBinaryMode writeHandle True
    hSetBuffering writeHandle NoBuffering
    return (readHandle, writeHandle)
