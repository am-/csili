{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Csili.Interpreter.TermRewriting
( normalize
, fromSemantics
) where

import Control.Monad (zipWithM)
import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

import Csili.Semantics

--------------------------------------------------------------------------------
-- Term Rewriting Engine
--------------------------------------------------------------------------------

normalize :: [Rule] -> Term -> Term
normalize rules = recurse . \case
    Function ident terms -> Function ident (map (normalize rules) terms)
    Variable var -> Variable var
    promise@(Promise _ _) -> promise
    future@(Future _) -> future
  where
    recurse :: Term -> Term
    recurse = flip maybe (normalize rules) <*> rewrite rules

rewrite :: [Rule] -> Term -> Maybe Term
rewrite rules
    = fmap (uncurry substitute) . listToMaybe
    . mapMaybe (\(mbx, y) -> fmap (flip (,) y) mbx)
    . flip map rules . first . matching

matching :: Term -> Term -> Maybe Binding
matching term pattern = case term of
    Function ident1 terms -> case pattern of
        Function ident2 patterns
            | ident1 == ident2 -> Map.unions <$> zipWithM matching terms patterns
            | otherwise -> Nothing
        Variable var -> Just (Map.singleton var term)
        Promise _ _ -> Nothing
        Future _ -> Nothing
    Variable _ -> Nothing
    Promise _ _
        | term == pattern -> Just Map.empty
        | otherwise -> Nothing
    Future _ 
        | term == pattern -> Just Map.empty
        | otherwise -> Nothing

type Binding = Map Var Term

substitute :: Binding -> Term -> Term
substitute binding = \case
    Function ident terms -> Function ident (map (substitute binding) terms)
    Variable var -> Map.findWithDefault (Variable var) var binding
    promise@(Promise _ _) -> promise
    future@(Future _) -> future

--------------------------------------------------------------------------------
-- Transforming a Petri net into a term rewrite system
--------------------------------------------------------------------------------

fromSemantics :: Semantics -> ([Rule], Term)
fromSemantics sem = (,)
    ( (++)
        (map (mkFireRule placeList <$> get (patterns sem) <*> get (applications sem)) transitionList)
        (rules sem)
    )
    (termMapToTerm placeList (const emptyTerm) (marking sem))
  where
    placeList = filter (not . isComplementaryPlace) . Set.toList . places $ sem
    transitionList = Set.toList (transitions sem)
    get x = flip (Map.findWithDefault Map.empty) x

mkFireRule :: [Place] -> Map Place Term -> Map Place Term -> Rule
mkFireRule places preset postset = (,)
    (termMapToTerm places toVariable . Map.mapKeys renameComplementaryPlace . Map.mapWithKey matchComplementaryPlace $ preset)
    (termMapToTerm places toVariable . Map.union postset . Map.map (const emptyTerm) $ preset)
  where
    matchComplementaryPlace :: Place -> Term -> Term
    matchComplementaryPlace place
        | isComplementaryPlace place = const emptyTerm
        | otherwise = id
    
    renameComplementaryPlace :: Place -> Place
    renameComplementaryPlace place@(Place name)
        | isComplementaryPlace place = Place (T.tail name)
        | otherwise = place

isComplementaryPlace :: Place -> Bool
isComplementaryPlace (Place name)
    | T.take 2 name == "__" = False
    | otherwise = T.head name == '_'

emptyTerm :: Term
emptyTerm = Function (Symbol "__empty") []

tokenTerm :: Term -> Term
tokenTerm = Function (Symbol "__term") . (:[])

mkPlaceTerm :: Term -> Term
mkPlaceTerm term
    | term == emptyTerm = term
    | otherwise = tokenTerm term

toVariable :: Place -> Term
toVariable (Place name) = Variable . Var . T.cons '_' . T.toTitle $ name

termMapToTerm :: [Place] -> (Place -> Term) -> Map Place Term -> Term
termMapToTerm places defaultTerm termMap = Function (Symbol "__marking")
    . map (\place -> maybe (defaultTerm place) mkPlaceTerm . Map.lookup  place $ termMap)
    $ places
