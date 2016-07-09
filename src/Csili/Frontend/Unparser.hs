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
import Data.Set (Set)
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
unparseStructure = T.intercalate "\n\n" . ((:) <$> unparseRules . rules <*> unparseTransitions)

unparseRules :: [Rule] -> Text
unparseRules = T.intercalate "\n" . map unparseRule

unparseRule :: Rule -> Text
unparseRule (lhs, rhs) = T.append (T.append (unparseTerm lhs) " -> ") (unparseTerm rhs)

unparseMarking :: Map Place Term -> Text
unparseMarking = T.intercalate "\n" . (:) "MARKING {" . (++ ["}"])
               . map (T.append "  " . uncurry unparsePlaceTerm) . Map.toAscList

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

unparseMatch :: Map Place Term -> Text
unparseMatch = T.intercalate "\n" . (:) "  MATCH {" . flip (++) ["  }"]
             . map (T.append "    " . uncurry unparsePlaceTerm) . Map.toAscList

unparseProduce :: Map Place Computation -> Text
unparseProduce = T.intercalate "\n" . (:) "  PRODUCE {" . flip (++) ["  }"]
               . map (T.append "    " . uncurry unparsePlaceComputation) . Map.toAscList

unparsePlaceComputation :: Place -> Computation -> Text
unparsePlaceComputation (Place name) computation = T.concat [name, ": ", unparseComputation computation]

unparseComputation :: Computation -> Text
unparseComputation = \case
    EffectFree term -> unparseTerm term
    Effectful (Effect effect) terms
        | null terms -> T.cons '#' effect
        | otherwise -> T.concat [T.cons '#' effect, "(", T.intercalate ", " (map unparseTerm terms), ")"]

unparsePlaceTerm :: Place -> Term -> Text
unparsePlaceTerm (Place name) term = T.concat [name, ": ", unparseTerm term]

unparseTerm :: Term -> Text
unparseTerm = \case
    Function (Symbol symbol) terms
        | null terms -> symbol
        | otherwise -> T.concat [symbol, "(", T.intercalate ", " (map unparseTerm terms), ")"]
    Variable (Var var) -> var
