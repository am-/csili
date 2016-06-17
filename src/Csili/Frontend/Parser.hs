{-# LANGUAGE OverloadedStrings #-}

module Csili.Frontend.Parser
( parseCsl
) where

import Control.Applicative ((<|>), many)
import Data.Attoparsec.Text
import Data.Char (isAlpha, isAlphaNum, isLower, isUpper)
import Data.Function (on)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (takeWhile)

import Csili.Semantics (Semantics(..), Symbol(..), Var(..), Term(..), Rule, Resource(..))
import Csili.Semantics (Place(..), Transition(..))
import qualified Csili.Semantics as Sem
import Csili.Normalization (normalize)

parseCsl :: Text -> Either String Semantics
parseCsl = fmap normalize .  parseOnly (parseInput Sem.empty)

parseInput :: Semantics -> Parser Semantics
parseInput sem =  (parseBlockOrRule sem >>= parseInput)
              <|> sem <$ endOfInput

parseBlockOrRule :: Semantics -> Parser Semantics
parseBlockOrRule sem =  fullClean
                     $  integrateRule <$> rule
                    <|> integrateMarking <$> markingBlock
                    <|> uncurry integrateTransition <$> transitionBlock
  where
    integrateRule rule = sem { rules = Sem.rules sem ++ [rule] }
    integrateMarking marking = sem { Sem.marking = marking }
    integrateTransition name (patterns, applications) = sem
        { patterns = Map.insert name patterns (Sem.patterns sem)
        , applications = Map.insert name applications (Sem.applications sem)
        }


--------------------------------------------------------------------------------
-- Term Rewriting
--------------------------------------------------------------------------------

rule :: Parser Rule
rule = (,) <$> term  <*> (string "->" *> term)

term :: Parser Term
term = fullClean (variable <|> function <|> promise)

variable :: Parser Term
variable = Variable . Var <$> upperCaseIdentifier

promise :: Parser Term
promise = uncurry Promise <$> generalTerm (char '@' *> (Resource <$> lowerCaseIdentifier)) term

function :: Parser Term
function = uncurry Function <$> generalTerm (Symbol <$> lowerCaseIdentifier) term

--------------------------------------------------------------------------------
-- Petri net parser
--------------------------------------------------------------------------------

place :: Parser Place
place = Place <$> identifier

transition :: Parser Transition
transition = Transition <$> identifier

markingBlock :: Parser (Map Place Term)
markingBlock = placeTermMap "MARKING"

matchBlock :: Parser (Map Place Term)
matchBlock = placeTermMap "MATCH"

produceBlock :: Parser (Map Place Term)
produceBlock = placeTermMap "PRODUCE"

placeTermMap :: Text -> Parser (Map Place Term)
placeTermMap kind = Map.fromList . snd <$> block (string kind) (many placeTerm)

placeTerm :: Parser (Place, Term)
placeTerm = (,) <$> fullClean place <*> (char ':' *> term)

transitionBlock :: Parser (Transition, (Map Place Term, Map Place Term))
transitionBlock =  block (string "TRANSITION" *> leftClean transition) innerBlocks

innerBlocks :: Parser (Map Place Term, Map Place Term)
innerBlocks =  foldl' merge (Map.empty, Map.empty) <$> many (choice blocks)
  where
    blocks = [ flip (,) Map.empty <$> matchBlock
             , (,) Map.empty <$> produceBlock
             ]

    merge (match, produce) = (,)
        <$> Map.union match . fst
        <*> Map.union produce . snd

--------------------------------------------------------------------------------
-- Basic parser
--------------------------------------------------------------------------------

identifier :: Parser Text
identifier = T.cons <$> satisfy isAlpha <*> takeWhile isAlphaNum

upperCaseIdentifier :: Parser Text
upperCaseIdentifier = T.cons <$> satisfy isUpper <*> takeWhile isAlphaNum

lowerCaseIdentifier :: Parser Text
lowerCaseIdentifier = T.cons <$> satisfy isLower <*> takeWhile isAlphaNum

block :: Parser a -> Parser b -> Parser (a, b)
block header content
    =   (,)
    <$> leftClean header
    <*> (leftClean (char '{') *> content <* leftClean (char '}'))

generalTerm :: Parser a -> Parser b -> Parser (a, [b])
generalTerm symbol argument = (,)
    <$> rightClean symbol
    <*> option [] (char '(' *> sepBy1 argument (char ',')  <* char ')')

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

clean :: Parser ()
clean = skipMany space

leftClean :: Parser a -> Parser a
leftClean = (*>) clean

rightClean :: Parser a -> Parser a
rightClean = flip (<*) clean

fullClean :: Parser a -> Parser a
fullClean = leftClean . rightClean
