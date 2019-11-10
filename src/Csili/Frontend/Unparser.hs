{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Csili.Frontend.Unparser
( unparseProgram
, unparseMarking
, unparseTerm
) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Csili.Program

unparseProgram :: Program -> Text
unparseProgram program = T.intercalate "\n\n"
    $ unparseMarking (initialMarking $ program)
    : unparseTransitions program

unparseMarking :: Map Place Term -> Text
unparseMarking = T.intercalate "\n" . unparseBlock "MARKING" unparseTerm

unparseTransitions :: Program -> [Text]
unparseTransitions = map <$> unparseTransition <*> Set.toAscList . transitions

unparseTransition :: Program -> Transition -> Text
unparseTransition program transition@(Transition name)
    = T.intercalate "\n"
    . (:) (T.append (T.append "TRANSITION " name) " {") . (++ ["}"]) 
    $ filter (not . T.null)
    [ maybe T.empty unparseMatch . Map.lookup transition . patterns $ program
    , maybe T.empty unparseProduce . Map.lookup transition . productions $ program
    ]

unparseMatch :: Map Place Term -> Text
unparseMatch = T.intercalate "\n" . map (T.append "  ")
    . unparseBlock "MATCH" unparseTerm

unparseProduce :: Map Place Term -> Text
unparseProduce = T.intercalate "\n" . map (T.append "  ")
    . unparseBlock "PRODUCE" unparseTerm

unparseTerm :: Term -> Text
unparseTerm = \case
    Function (Symbol symbol) terms ->
        unparseTextTerm symbol (map unparseTerm terms)
    Variable (Var var) -> var
    IntTerm n -> T.pack (show n)
    Wildcard -> "_"

unparseBlock :: Text -> (a -> Text) -> Map Place a -> [Text]
unparseBlock caption convert
    = (:) (T.append caption " {") . flip (++) ["}"]
    . map (T.append "  " . uncurry (unparsePlaceAndValue convert))
    . Map.toAscList

unparsePlaceAndValue :: (a -> Text) -> Place -> a -> Text
unparsePlaceAndValue convert (Place name) value
    = T.concat [name, ": ", convert value]

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

unparseTextTerm :: Text -> [Text] -> Text
unparseTextTerm symbol terms
    | null terms = symbol
    | otherwise = T.concat [symbol, "(", T.intercalate ", " terms, ")"]
