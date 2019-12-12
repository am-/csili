module Csili.Frontend.Parser
( SyntaxTree(..)
, Term(..)
, file
, term
, interfaceBlock
, placesBlock
, markingBlock
, transitionBlock
) where

import Control.Applicative ((<|>), many)
import Data.Attoparsec.Text
import Data.Char (isAlpha, isAlphaNum, isLower, isUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (takeWhile)

import Csili.Frontend.SyntaxTree

file :: Parser SyntaxTree
file = clean *> syntaxTree <* endOfInput

syntaxTree :: Parser SyntaxTree
syntaxTree = SyntaxTree
    <$> option ([], []) (rightClean interfaceBlock)
    <*> option [] (rightClean placesBlock)
    <*> option [] (rightClean markingBlock)
    <*> many (rightClean transitionBlock)

--------------------------------------------------------------------------------
-- Terms
--------------------------------------------------------------------------------

term :: Parser Term
term = fullClean (variable <|> function <|> word8 <|> int64 <|> wildcard)

variable :: Parser Term
variable = Variable <$> upperCaseIdentifier

function :: Parser Term
function = uncurry Function <$> functionTerm (lowerCaseIdentifier) term

int64 :: Parser Term
int64 = toInt64 <$> signed decimal
  where
    toInt64 :: Integer -> Term
    toInt64 n
      | n < 0 = Function "int64" . (one:) $ toInt63 ((9223372036854775808 :: Integer) + n)
      | otherwise = Function "int64" . (zero:) $ toInt63 n

    toInt63 :: Integer -> [Term]
    toInt63 n = extractBits n (reverse powersOfTwo)

    extractBits :: Integer -> [Integer] -> [Term]
    extractBits n = \case
        [] -> []
        power:powers
            | power <= n -> one : extractBits (n - power) powers
            | otherwise -> zero : extractBits n powers

    powersOfTwo :: [Integer]
    powersOfTwo = Prelude.take 63 $ iterate (2*) 1

word8 :: Parser Term
word8 = char '0' *> char 'x' *> (toWord8 <$> hexDigitInBits <*> hexDigitInBits)
  where
    toWord8 bits1 bits0 = Function "word8" $ concat [bits1, bits0]

hexDigitInBits :: Parser [Term]
hexDigitInBits = flip satisfyWith (not . null) $ \case
    '0' -> [zero, zero, zero, zero]
    '1' -> [zero, zero, zero, one]
    '2' -> [zero, zero, one, zero]
    '3' -> [zero, zero, one, one]
    '4' -> [zero, one, zero, zero]
    '5' -> [zero, one, zero, one]
    '6' -> [zero, one, one, zero]
    '7' -> [zero, one, one, one]
    '8' -> [one, zero, zero, zero]
    '9' -> [one, zero, zero, one]
    'A' -> [one, zero, one, zero]
    'a' -> [one, zero, one, zero]
    'B' -> [one, zero, one, one]
    'b' -> [one, zero, one, one]
    'C' -> [one, one, zero, zero]
    'c' -> [one, one, zero, zero]
    'D' -> [one, one, zero, one]
    'd' -> [one, one, zero, one]
    'E' -> [one, one, one, zero]
    'e' -> [one, one, one, zero]
    'F' -> [one, one, one, one]
    'f' -> [one, one, one, one]
    _ -> []

wildcard :: Parser Term
wildcard = const Wildcard <$> (char '_' *> takeWhile isAlphaNum)

--------------------------------------------------------------------------------
-- Net Structure
--------------------------------------------------------------------------------

interfaceBlock :: Parser ([Text], [Text])
interfaceBlock = unnamedBlock "INTERFACE" $ (,)
    <$> option [] inputBlock
    <*> option [] outputBlock

inputBlock :: Parser [Text]
inputBlock = unnamedBlock "INPUT" placeSet

outputBlock :: Parser [Text]
outputBlock = unnamedBlock "OUTPUT" placeSet

placesBlock :: Parser [Text]
placesBlock = unnamedBlock "PLACES" placeSet

markingBlock :: Parser [(Text, Term)]
markingBlock = unnamedBlock "MARKING" (placeMap term)

transitionBlock :: Parser (Text, ([(Text, Term)], [(Text, Term)], [(Text, Term)]))
transitionBlock = namedBlock "TRANSITION" identifier $ (,,)
    <$> option [] (unnamedBlock "MATCH" (placeMap term))
    <*> option [] (unnamedBlock "PRODUCE" (placeMap term))
    <*> option [] (unnamedBlock "EFFECTS" (placeMap term))

placeSet :: Parser [Text]
placeSet = many (fullClean identifier)

placeMap :: Parser a -> Parser [(Text, a)]
placeMap p = many (placeAssignment p)

placeAssignment :: Parser a -> Parser (Text, a)
placeAssignment p = (,) <$> fullClean identifier <*> (char ':' *> p)

--------------------------------------------------------------------------------
-- Basic parser
--------------------------------------------------------------------------------

identifier :: Parser Text
identifier = T.cons <$> satisfy isAlpha <*> takeWhile isAlphaNum

upperCaseIdentifier :: Parser Text
upperCaseIdentifier = T.cons <$> satisfy isUpper <*> takeWhile isAlphaNum

lowerCaseIdentifier :: Parser Text
lowerCaseIdentifier = T.cons <$> satisfy isLower <*> takeWhile isAlphaNum

namedBlock :: Text -> Parser a -> Parser b -> Parser (a, b)
namedBlock keyword ident content = (,)
    <$> (string keyword *> leftClean ident)
    <*> enclose '{' '}' content

unnamedBlock :: Text -> Parser a -> Parser a
unnamedBlock keyword content = leftClean (string keyword) *> enclose '{' '}' content

enclose :: Char -> Char -> Parser a -> Parser a
enclose opener closer parser = leftClean (char opener) *> parser <* leftClean (char closer)

functionTerm :: Parser a -> Parser b -> Parser (a, [b])
functionTerm symbol argument = (,)
    <$> rightClean symbol
    <*> option [] (char '(' *> sepBy1 argument (char ',')  <* char ')')

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

clean :: Parser ()
clean = skipMany space

leftClean :: Parser a -> Parser a
leftClean p = clean *> p

rightClean :: Parser a -> Parser a
rightClean p = p <* clean

fullClean :: Parser a -> Parser a
fullClean = leftClean . rightClean
