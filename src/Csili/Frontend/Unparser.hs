{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Csili.Frontend.Unparser
( unparseCsl  
, unparseStructure
, unparseRules
, unparseMarking
, unparseTerm
) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Csili.Semantics

unparseCsl :: Semantics -> Text
unparseCsl sem = T.intercalate "\n\n"
    $ unparseRules (rules sem)
    : unparseMarking (marking sem)
    : unparseTransitions sem

unparseStructure :: Semantics -> Text
unparseStructure = T.intercalate "\n\n"
    . ((:) <$> unparseRules . rules <*> unparseTransitions)

unparseRules :: [Rule] -> Text
unparseRules = T.intercalate "\n" . map unparseRule

unparseRule :: Rule -> Text
unparseRule (lhs, rhs) = T.append
    (T.append (unparseTerm lhs) " -> ")
    (unparseTerm rhs)

unparseMarking :: Map Place Term -> Text
unparseMarking = T.intercalate "\n" . unparseBlock "MARKING" unparseTerm

unparseTransitions :: Semantics -> [Text]
unparseTransitions = map <$> unparseTransition <*> Set.toAscList . transitions

unparseTransition :: Semantics -> Transition -> Text
unparseTransition sem transition@(Transition name)
    = T.intercalate "\n"
    . (:) (T.append (T.append "TRANSITION " name) " {") . (++ ["}"]) 
    $ filter (not . T.null)
    [ maybe T.empty unparseMatch . Map.lookup transition . patterns $ sem
    , maybe T.empty unparseProduce . Map.lookup transition . applications $ sem
    ]

unparseMatch :: Map Place Matcher -> Text
unparseMatch = T.intercalate "\n" . map (T.append "  ")
    . unparseBlock "MATCH" unparseMatcher

unparseProduce :: Map Place Computation -> Text
unparseProduce = T.intercalate "\n" . map (T.append "  ")
    . unparseBlock "PRODUCE" unparseComputation

unparseMatcher :: Matcher -> Text
unparseMatcher = \case
    Pattern term -> unparseTerm term
    PromisePending (Var name) -> T.cons '~' name
    PromiseBroken term -> T.cons '!' (unparseTerm term)
    PromiseKept term -> T.cons '?' (unparseTerm term)

unparseComputation :: Computation -> Text
unparseComputation = \case
    EffectFree term -> unparseTerm term
    Effectful (Effect effect) terms
        | null terms -> T.cons '#' effect
        | otherwise -> T.concat [T.cons '#' effect, "(", T.intercalate ", " (map unparseTerm terms), ")"]

unparseTerm :: Term -> Text
unparseTerm = \case
    Function (Symbol symbol) terms
        | null terms -> symbol
        | otherwise -> T.concat [symbol, "(", T.intercalate ", " (map unparseTerm terms), ")"]
    Variable (Var var) -> var
    IntTerm n -> T.pack (show n)

unparseBlock :: Text -> (a -> Text) -> Map Place a -> [Text]
unparseBlock caption convert
    = (:) (T.append caption " {") . flip (++) ["}"]
    . map (T.append "  " . uncurry (unparsePlaceAndValue convert))
    . Map.toAscList

unparsePlaceAndValue :: (a -> Text) -> Place -> a -> Text
unparsePlaceAndValue convert (Place name) value
    = T.concat [name, ": ", convert value]

