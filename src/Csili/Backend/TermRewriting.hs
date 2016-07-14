{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Csili.Backend.TermRewriting
( generateRules
, generateMatch
, generateRewrite
, collectVariables
) where

import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple (swap)

import Csili.Semantics

--------------------------------------------------------------------------------
-- Rewriting
--------------------------------------------------------------------------------

generateRules :: Semantics -> Text
generateRules sem = T.intercalate "\n" $ concat
    [ generateSymbols (symbols sem)
    , [""]
    , ["struct term* rewrite(struct term* term) {"]
    , concatMap (map (T.append "  ") . generateRule) . sortBy orderRule . rules $ sem
    , [ ""
      , "  return NULL;"
      , "}"]
    ]

generateRule :: Rule -> [Text]
generateRule (lhs, rhs) = concat
    [ [T.concat ["if(", generateMatch "term" lhs, ") {"]]
    , map (T.append "  ") $ generateRewrite (collectVariables "term" lhs) "" rhs
    , [ "  return new_term;"
      , "}"
      ]
    ]

generateRewrite :: Map Var Text -> Text -> Term -> [Text]
generateRewrite variablesMap suffix = \case
    Function (Symbol symbol) args ->
        let extendWithNumber :: Text -> Int -> Text
            extendWithNumber current = T.append current . T.cons '_' . T.pack . show
            termVariable = T.append "new_term" suffix
            argumentsVariable = T.append "arguments" suffix
            argumentVariables = T.intercalate ", " (zipWith (const $ extendWithNumber termVariable) args [0..])
            arguments = T.concat ["struct term* ", argumentsVariable, "[] = { ", argumentVariables, " };"]
            symbolConstant = T.append "TERM_" symbol
            arity = T.pack . show . length $ args
            allocationArguments = T.intercalate ", " [symbolConstant, arity, argumentsVariable]
            allocation = T.concat ["struct term* ", termVariable, " = allocate_function(", allocationArguments, ");"]
        in  concat (zipWith (generateRewrite variablesMap . extendWithNumber suffix) [0..] args) ++ [arguments, allocation]
    Variable var -> case Map.lookup var variablesMap of
        Nothing -> undefined
        Just term -> [T.concat ["struct term* new_term", suffix, " = ", term, ";"]]
    IntTerm x -> [T.concat ["struct term* new_term", suffix, " = allocate_int(", T.pack $ show x, ");"]]

collectVariables :: Text -> Term -> Map Var Text
collectVariables variable = Map.fromList . mapMaybe extractVariable . buildLocationsInPreOrder variable
  where
    extractVariable :: (Term, Text) -> Maybe (Var, Text)
    extractVariable (term, location) = flip (,) location <$> case term of
        Function _ _ -> Nothing
        Variable var -> Just var
        IntTerm _ -> Nothing

--------------------------------------------------------------------------------
-- Pattern matching
--------------------------------------------------------------------------------

generateMatch :: Text -> Term -> Text
generateMatch variable
    = conjoin . concatMap (uncurry buildCondition . swap)
    . buildLocationsInPreOrder variable
  where
    conjoin :: [Text] -> Text
    conjoin conditions
        | null conditions = "true"
        | otherwise = T.intercalate " && " conditions

buildCondition :: Text -> Term -> [Text]
buildCondition variable = \case
    Function (Symbol symbol) _ ->
        [ T.concat [variable, "->type == FUNCTION"]
        , T.concat [variable, "->value.function->symbol == TERM_", symbol]
        ]
    Variable _ -> []
    IntTerm x ->
        [ T.concat [variable, "->type == INT"]
        , T.concat [variable, "->value.int_value == ", T.pack $ show x]
        ]


--------------------------------------------------------------------------------
-- Symbol generation
--------------------------------------------------------------------------------

generateSymbols :: Set Symbol -> [Text]
generateSymbols = zipWith (flip generateDefinition) [0..] . Set.toAscList

generateDefinition :: Symbol -> Int -> Text
generateDefinition (Symbol symbol) i = T.concat ["#define TERM_", symbol, " ", T.pack (show i)]

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

buildLocationsInPreOrder :: Text -> Term -> [(Term, Text)]
buildLocationsInPreOrder variable term = (term, variable) : case term of
    Function _ args -> concat (zipWith (buildLocationsInPreOrder . enter) [0..] args)
    Variable _ -> []
    IntTerm _ -> []
  where
    enter :: Int -> Text
    enter index = T.concat [variable, "->value.function->arguments[", T.pack (show index), "]"]
