module Csili.Program.Pattern
( Var(..)
, Pattern(..)
, match
, areOverlapping
, ConstructionRule(..)
, substitute
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Csili.Program.Token

newtype Var = Var Text
    deriving (Show, Eq, Ord)

data Pattern
    = FunctionPattern Symbol [Pattern]
    | VariablePattern Var
    | WildcardPattern
    deriving (Show, Eq, Ord)

areOverlapping :: Pattern -> Pattern -> Bool
areOverlapping pattern1 pattern2 = case pattern1 of
    FunctionPattern symbol1 patterns1 -> case pattern2 of
        FunctionPattern symbol2 patterns2 -> symbol1 == symbol2 && and (zipWith areOverlapping patterns1 patterns2)
        _ -> False
    VariablePattern _ -> True
    WildcardPattern -> True

match :: Pattern -> Token -> Maybe (Map Var Token)
match pattern token = case pattern of
    FunctionPattern patternSymbol patternTerms -> case token of
        FunctionToken termSymbol termTerms
            | patternSymbol /= termSymbol -> Nothing
            | length patternTerms /= length termTerms -> Nothing
            | otherwise -> fmap Map.unions . sequence $ zipWith match patternTerms termTerms
        Resource _ -> Nothing
    VariablePattern var -> Just $ Map.singleton var token
    WildcardPattern -> Just Map.empty

data ConstructionRule
    = FunctionConstruction Symbol [ConstructionRule]
    | Substitution Var
    deriving (Show, Eq, Ord)

substitute :: Map Var Token -> ConstructionRule -> Maybe Token
substitute binding = \case
    FunctionConstruction symbol terms -> FunctionToken symbol <$> mapM (substitute binding) terms
    Substitution var -> Map.lookup var binding
