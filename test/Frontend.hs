module Frontend where

import Test.Tasty

import qualified Frontend.Parser
import qualified Frontend.Program

tests :: TestTree
tests = testGroup "Frontend"
    [ Frontend.Parser.tests
    , Frontend.Program.tests
    ]
