{-# LANGUAGE ApplicativeDo #-}

module Csili.Frontend.Parser
( SyntaxTree(..)
, Term(..)
, file
, term
, interfaceBlock
, markingBlock
, transitionBlock
) where

import Control.Applicative ((<|>), many)
import Data.Attoparsec.Text
import Data.Char (isAlpha, isAlphaNum, isLower, isUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (takeWhile)

data SyntaxTree = SyntaxTree
    { interface :: ([Text], [Text])
    , marking :: [(Text, Term)]
    , transitions :: [(Text, ([Pattern], [Production]))]
    }

type Symbol = Text
type Var = Text
type Pattern = (Text, Term)
type Production = (Text, Term)

data Term
    = Function Symbol [Term]
    | IntTerm Int
    | Variable Var
    | Wildcard
    deriving (Show, Eq, Ord)

file :: Parser SyntaxTree
file = clean *> syntaxTree <* endOfInput

syntaxTree :: Parser SyntaxTree
syntaxTree = SyntaxTree
    <$> option ([], []) (rightClean interfaceBlock)
    <*> option [] (rightClean markingBlock)
    <*> many (rightClean transitionBlock)

--------------------------------------------------------------------------------
-- Terms
--------------------------------------------------------------------------------

term :: Parser Term
term = fullClean (variable <|> function <|> intTerm <|> wildcard)

variable :: Parser Term
variable = Variable <$> upperCaseIdentifier

function :: Parser Term
function = uncurry Function <$> functionTerm (lowerCaseIdentifier) term

intTerm :: Parser Term
intTerm = IntTerm <$> signed (choice [char '0' *> char 'x' *> hexadecimal, decimal])

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

markingBlock :: Parser [(Text, Term)]
markingBlock = unnamedBlock "MARKING" (placeMap term)

transitionBlock :: Parser (Text, ([(Text, Term)], [(Text, Term)]))
transitionBlock = namedBlock "TRANSITION" identifier blocks
  where
    blocks :: Parser ([(Text, Term)], [(Text, Term)])
    blocks = (,)
        <$> option [] (unnamedBlock "MATCH" (placeMap term))
        <*> option [] (unnamedBlock "PRODUCE" (placeMap term))

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
