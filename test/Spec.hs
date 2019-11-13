import Test.Tasty

import qualified Frontend.Parser
import qualified Interpreter

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ Frontend.Parser.tests
    , Interpreter.tests
    ]
