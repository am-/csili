import Test.Tasty

import qualified Frontend
import qualified Interpreter
import qualified Program

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ Frontend.tests
    , Interpreter.tests
    , Program.tests
    ]
