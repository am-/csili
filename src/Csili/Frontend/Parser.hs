{-# LANGUAGE OverloadedStrings #-}

module Csili.Frontend.Parser
( parseCsl
) where

import Control.Applicative ((<|>), many)
import Data.Attoparsec.Text
import Data.Char (isAlpha, isAlphaNum, isLower, isUpper)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (takeWhile)

import Csili.Semantics (Semantics, Symbol(..), Var(..), Term(..), Matcher(..), Computation(..), Rule, Effect(..))
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
    integrateRule newRule = sem { Sem.rules = Sem.rules sem ++ [newRule] }
    integrateMarking marking = sem { Sem.marking = marking }
    integrateTransition name (patterns, applications) = sem
        { Sem.patterns = Map.insert name patterns (Sem.patterns sem)
        , Sem.applications = Map.insert name applications (Sem.applications sem)
        }


--------------------------------------------------------------------------------
-- Term Rewriting
--------------------------------------------------------------------------------

rule :: Parser Rule
rule = (,) <$> term <*> (string "->" *> term)

term :: Parser Term
term = fullClean (variable <|> function)

variable :: Parser Term
variable = Variable . Var <$> upperCaseIdentifier

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
markingBlock = placeTermMap "MARKING" term

matchBlock :: Parser (Map Place Matcher)
matchBlock = placeTermMap "MATCH" matcher

matcher :: Parser Matcher
matcher = choice
    [ Pattern <$> term
    , PromisePending . Var <$> (char '~' *> upperCaseIdentifier)
    , PromiseBroken <$> (char '!' *> term)
    , PromiseKept <$> (char '?' *> term)
    ]

produceBlock :: Parser (Map Place Computation)
produceBlock = placeTermMap "PRODUCE" computation

computation :: Parser Computation
computation = choice
    [ EffectFree <$> term
    , uncurry Effectful <$> generalTerm (char '#' *> (Effect <$> lowerCaseIdentifier)) term
    ]

placeTermMap :: Text -> Parser a -> Parser (Map Place a)
placeTermMap kind p = Map.fromList . snd <$> block (string kind) (many (placeTerm p))

placeTerm :: Parser a -> Parser (Place, a)
placeTerm p = (,) <$> fullClean place <*> (char ':' *> p)

transitionBlock :: Parser (Transition, (Map Place Matcher, Map Place Computation))
transitionBlock = block (string "TRANSITION" *> leftClean transition) innerBlocks

innerBlocks :: Parser (Map Place Matcher, Map Place Computation)
innerBlocks = foldl' merge (Map.empty, Map.empty) <$> many (choice blocks)
  where
    blocks = [ flip (,) Map.empty <$> matchBlock
             , (,) Map.empty <$> produceBlock
             ]

    merge (matching, producing) = (,)
        <$> Map.union matching . fst
        <*> Map.union producing . snd

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
