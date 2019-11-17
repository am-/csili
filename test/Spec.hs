import Test.Tasty

import qualified Frontend
import qualified Interpreter

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ Frontend.tests
    , Interpreter.tests
    ]
