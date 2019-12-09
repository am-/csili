module Csili.Interpreter.Word
( word8ToChar
) where

import Data.Char (chr)
import Control.Monad (zipWithM)

import Csili.Program

word8ToChar :: Token -> Maybe Char
word8ToChar = \case
    FunctionToken (Symbol "word8") bits -> chr . (`mod` 256) <$> bitsToInt bits
    _ -> Nothing

bitsToInt :: [Token] -> Maybe Int
bitsToInt tokens = sum <$> zipWithM calculate powersOfTwo (reverse tokens)
  where
    calculate power = \case
        FunctionToken (Symbol "zero") [] -> Just 0
        FunctionToken (Symbol "one") [] -> Just power
        _ -> Nothing
    powersOfTwo = iterate (*2) 1
