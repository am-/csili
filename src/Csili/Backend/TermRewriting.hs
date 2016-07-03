{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Csili.Backend.TermRewriting
( generateRules
, generateMatch
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

generateRules :: [Rule] -> Text
generateRules rules = T.intercalate "\n" $ concat
    [ generateSymbols (concatMap (\(lhs, rhs) -> [lhs, rhs]) rules)
    , [""]
    , ["struct term* rewrite(struct term* term) {"]
    , concatMap (map (T.append "  ") . generateRule) . sortBy orderRule $ rules
    , [ ""
      , "  return NULL;"
      , "}"]
    ]

generateRule :: Rule -> [Text]
generateRule (lhs, rhs) = concat
    [ [T.concat ["if(", generateMatch "term" lhs, ") {"]]
    , map (T.append "  ") $ generateRewrite (collectVariables lhs) rhs
    , [ "  return new_term;"
      , "}"
      ]
    ]

generateRewrite :: Map Var Text -> Term -> [Text]
generateRewrite variablesMap = go ""
  where
    go :: Text -> Term -> [Text]
    go suffix = \case
        Function (Symbol symbol) args ->
            let extendWithNumber current = T.append current . T.cons '_' . T.pack . show
                termVariable = T.append "new_term" suffix
                argumentsVariable = T.append "arguments" suffix
                argumentVariables = T.intercalate ", " (zipWith (const $ extendWithNumber termVariable) args [0..])
                arguments = T.concat ["struct term* ", argumentsVariable, "[] = { ", argumentVariables, " };"]
                symbolConstant = T.append "TERM_" symbol
                arity = T.pack . show . length $ args
                allocationArguments = T.intercalate ", " [symbolConstant, arity, argumentsVariable]
                allocation = T.concat ["struct term* ", termVariable, " = allocate_function(", allocationArguments, ");"]
            in  concat (zipWith (go . extendWithNumber suffix) [0..] args) ++ [arguments, allocation]
        Variable var -> case Map.lookup var variablesMap of
            Nothing -> undefined
            Just term -> [T.concat ["new_term", suffix, " = ", term, ";"]]
        Promise _ _ -> undefined
        Future _ -> undefined

collectVariables :: Term -> Map Var Text
collectVariables = Map.fromList . mapMaybe extractVariable . buildLocationsInPreOrder "term"
  where
    extractVariable :: (Term, Text) -> Maybe (Var, Text)
    extractVariable (term, location) = flip (,) location <$> case term of
        Function _ _ -> Nothing
        Variable var -> Just var
        Promise _ _ -> Nothing
        Future _ -> Nothing

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
        Promise _ _ -> undefined
        Future _ -> undefined


--------------------------------------------------------------------------------
-- Symbol generation
--------------------------------------------------------------------------------

generateSymbols :: [Term] -> [Text]
generateSymbols = flip (zipWith generateDefinition) [0..]
    . Set.toAscList . Set.unions . map collectSymbols

generateDefinition :: Symbol -> Int -> Text
generateDefinition (Symbol symbol) i = T.concat ["#define TERM_", symbol, " ", T.pack (show i)]

collectSymbols :: Term -> Set Symbol
collectSymbols = \case
    Function symbol args -> Set.insert symbol (Set.unions (map collectSymbols args))
    Variable _ -> Set.empty
    Promise _ args -> Set.unions (map collectSymbols args)
    Future _ -> Set.empty

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

buildLocationsInPreOrder :: Text -> Term -> [(Term, Text)]
buildLocationsInPreOrder variable term = (term, variable) : case term of
    Function _ args ->
        let enter index = T.concat [variable, "->value.function->arguments[", T.pack (show index), "]"]
        in  concat (zipWith (buildLocationsInPreOrder . enter) [0..] args)
    Variable _ -> []
    Promise _ _ -> []
    Future _ -> []
