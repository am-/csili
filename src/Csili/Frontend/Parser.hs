{-# LANGUAGE ApplicativeDo #-}

module Csili.Frontend.Parser
( parseProgram

, term
, markingBlock
, transitionBlock
) where

import Control.Applicative ((<|>), many)
import Data.Attoparsec.Text
import Data.Bifunctor (second)
import Data.Char (isAlpha, isAlphaNum, isLower, isUpper)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (takeWhile)

import Csili.Program (Program, Symbol(..), Var(..), Term(..))
import Csili.Program (Place(..), Transition(..))
import qualified Csili.Program as Program

parseProgram :: Text -> Either String Program
parseProgram = parseOnly $ do
    clean
    marking <- option Map.empty (rightClean markingBlock)
    transitions <- many (rightClean transitionBlock)
    endOfInput
    return $ Program.empty
      { Program.initialMarking = marking
      , Program.patterns = Map.fromList . map (second fst) $ transitions
      , Program.productions = Map.fromList . map (second snd) $ transitions
      }

--------------------------------------------------------------------------------
-- Terms
--------------------------------------------------------------------------------

term :: Parser Term
term = fullClean (variable <|> function <|> intTerm <|> wildcard)

variable :: Parser Term
variable = Variable . Var <$> upperCaseIdentifier

function :: Parser Term
function = uncurry Function <$> functionTerm (Symbol <$> lowerCaseIdentifier) term

intTerm :: Parser Term
intTerm = IntTerm <$> signed (choice [char '0' *> char 'x' *> hexadecimal, decimal])

wildcard :: Parser Term
wildcard = const Wildcard <$> (char '_' *> takeWhile isAlphaNum)

--------------------------------------------------------------------------------
-- Net Structure
--------------------------------------------------------------------------------

placeIdentifier :: Parser Place
placeIdentifier = Place <$> identifier

transitionIdentifier :: Parser Transition
transitionIdentifier = Transition <$> identifier

markingBlock :: Parser (Map Place Term)
markingBlock = unnamedBlock "MARKING" (placeMap term)

transitionBlock :: Parser (Transition, (Map Place Term, Map Place Term))
transitionBlock = namedBlock "TRANSITION" transitionIdentifier blocks
  where
    blocks :: Parser (Map Place Term, Map Place Term)
    blocks = (,)
        <$> option Map.empty (unnamedBlock "MATCH" (placeMap term))
        <*> option Map.empty (unnamedBlock "PRODUCE" (placeMap term))

placeMap :: Parser a -> Parser (Map Place a)
placeMap p = Map.fromList <$> many (placeAssignment p)

placeAssignment :: Parser a -> Parser (Place, a)
placeAssignment p = (,) <$> fullClean placeIdentifier <*> (char ':' *> p)

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
