{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Csili.Backend.AbstractRewritingMachine
( toInstructions
, segment
) where

import Data.Bifunctor (first, second)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.Tuple (swap)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple (swap)

import Csili.Backend.AbstractRewritingMachine.Types
import Csili.Types

toInstructions :: [Rule] -> ([Rule], [Instruction])
toInstructions = second concat . partitionEithers
               . map (\rule -> maybe (Left rule) Right  (toInstruction rule))

toInstruction :: Rule -> Maybe [Instruction]
toInstruction (lhs, rhs) = case lhs of
    Function leftSymbol leftArgs -> case rhs of
        Function rightSymbol rightArgs -> case segmentWithVariables leftArgs rightArgs of
            (prefix, inner, suffix) -> case inner of
              ([leftTerm@(Function _ leftInnerArgs)], rightInnerArgs)
                  | any (not . isVariable) leftInnerArgs -> Nothing
                  | leftInnerArgs == rightInnerArgs -> Just
                      [Match leftSymbol rightSymbol prefix suffix leftTerm]
                  | otherwise -> Nothing
              (leftInnerArgs, [rightTerm@(Function _ rightInnerArgs)])
                  | any (not . isVariable) leftInnerArgs -> Nothing
                  | leftInnerArgs == rightInnerArgs -> Just
                      [Continuation leftSymbol rightSymbol prefix suffix rightTerm]
              ([], [Variable innerVar])
                  | on (||) (elem innerVar) prefix suffix -> Just
                      [Add leftSymbol rightSymbol prefix suffix innerVar]
                  | otherwise -> Nothing
              (innerArgs, [])
                  | all isVariable innerArgs -> Just
                      [Delete leftSymbol rightSymbol prefix suffix . termsToVars $ innerArgs]
                  | otherwise -> Nothing
              _ -> Nothing
        Variable rightVar
            | all isVariable leftArgs && last leftArgs == rhs ->
                Just [Return leftSymbol rightVar . termsToVars . init $ leftArgs]
            | otherwise -> Nothing
        Promise _ _ -> Nothing
        Future _ -> error "Error: Producing a future."
    Variable _ -> error "Error: Matching a variable."
    Promise _ _ -> error "Error: Matching a promise."
    Future _ -> error "Error: Matching a future."
  where
    go :: Symbol -> [Term] -> Symbol -> [Term] -> Maybe Instruction
    go leftSymbol leftArgs rightSymbol rightArgs = undefined

segmentWithVariables :: [Term] -> [Term] -> ([Var], ([Term], [Term]), [Var])
segmentWithVariables terms1 terms2 = case segment terms1 terms2 of
    (prefix, (inner1, inner2), suffix) ->
        let (prefixVariables, endOfPrefix) = break isVariable prefix
            (suffixVariables, startOfSuffix) = first reverse . second reverse . break isVariable . reverse $ suffix
            encloseInRemains = (++) endOfPrefix . flip (++) startOfSuffix
        in  (termsToVars prefixVariables, on (,) encloseInRemains inner1 inner2, termsToVars suffixVariables)

termsToVars :: [Term] -> [Var]
termsToVars = map (\(Variable var) -> var)
  
canonizeRules :: [Rule] -> [Rule]
canonizeRules = map (uncurry $ on (,) addArityToSymbols)

{-
minimizeRule :: Rule -> Instruction
minimizeRule = \case
    (Function lhsSymbol lhsArguments, Function rhsSymbol rhsArguments)
        | all isVariable lhsArguments && all isVariable rhsArguments && undefined ->
            Delete lhsSymbol rhsSymbol undefined undefined undefined
    (Function lhsSymbol lhsArguments, Variable lastVar)
        | all isVariable lhsArguments && last lhsArguments == Variable lastVar ->
            flip (Return lhsSymbol) lastVar . init . map toVar $ lhsArguments
    _ -> undefined
  where
    toVar :: Term -> Var
    toVar (Variable var) = var


minimizeRules :: [Rule] -> [Rule]
minimizeRules = minimizeLeftHandSides . canonizeRules

-}

addArityToSymbols :: Term -> Term
addArityToSymbols term = case term of
    Function symbol arguments ->
        Function (addArityToSymbol symbol . length $ arguments) (map addArityToSymbols arguments)
    Variable _ -> term
    Promise _ _ -> term
    Future _ -> term
  where
    addArityToSymbol :: Symbol -> Int -> Symbol
    addArityToSymbol (Symbol name) = Symbol . T.append name . T.cons '/' . T.pack . show

{-
minimizeLeftHandSides :: [Rule] -> [Rule]
minimizeLeftHandSides = concatMap minimizeLeftHandSide
  where
    minimizeLeftHandSide :: Rule -> [Rule]
    minimizeLeftHandSide rule@(lhs, rhs) = case lhs of
        Function symbol1 arguments -> case break isFunction arguments of
            (leftVariables, []) -> [rule]
            (leftVariables, functionTerm@(Function symbol2 arguments2) : rest) ->
                let newFunctionTerm = Function (newSymbol symbol1 symbol2)
                    middleVariables = take (length arguments2) newVariables
                    rightVariables = take (length rest) . drop (length middleVariables) $ newVariables
                in  [ ( newFunctionTerm (concat [leftVariables, arguments, rightVariables])
                      , rhs)
                    , ( Function symbol1 (concat [leftVariables, [functionTerm], rightVariables])
                      , newFunctionTerm (concat [leftVariables, middleVariables, rightVariables])
                      )
                    ]
        Variable _ -> []
        Promise _ _ -> []
        Future _ -> []

    newSymbol :: Symbol -> Symbol -> Symbol
    newSymbol (Symbol name1) (Symbol name2) = Symbol (T.concat [name1, "_", name2])

    newVariables :: [Term]
    newVariables = map (Variable . Var . T.append "_Var" . T.pack . show) [1..]
-}

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

segment :: Eq a => [a] -> [a] -> ([a], ([a], [a]), [a])
segment xs ys =
    let prefix = longestCommonPrefix xs ys
        suffix = longestCommonSuffix xs ys
    in  (prefix, on (,) (cut (length prefix) (length suffix)) xs ys, suffix)

cut :: Int -> Int -> [a] -> [a]
cut prefixLength suffixLength = reverse . drop suffixLength . reverse . drop prefixLength

longestCommonPrefix :: Eq a => [a] -> [a] -> [a]
longestCommonPrefix list1 list2 = case (list1, list2) of
    (x:xs, y:ys) | x == y -> x : longestCommonPrefix xs ys
    _ -> []

longestCommonSuffix :: Eq a => [a] -> [a] -> [a]
longestCommonSuffix = (reverse .) .  longestCommonPrefix `on` reverse
