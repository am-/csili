module Csili.Interpreter.Word
( fromWord8
, toWord8
) where

import Data.Bits
import Data.Char (chr, ord)
import Control.Monad (zipWithM)

import Csili.Program

fromWord8 :: Token -> Maybe Char
fromWord8 = \case
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

toWord8 :: Char -> Token
toWord8 = FunctionToken (Symbol "word8") . intToBits . ord

intToBits :: Int -> [Token]
intToBits n = reverse . take 8 . map toBit . enumFromTo 0 $ finiteBitSize n
  where
    toBit k
        | testBit n k = one
        | otherwise = zero
