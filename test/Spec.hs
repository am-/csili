import Test.Tasty

import qualified Frontend.Parser as Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ Parser.tests
    ]
