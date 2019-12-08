{-# LANGUAGE RecordWildCards #-}

module Csili.Frontend.Conversion
( ConversionError(..)
, convert
) where

import Data.Containers.ListUtils (nubOrd)
import Data.List ((\\))
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Validation (Validation(..), bindValidation)

import Csili.Frontend.SyntaxTree (SyntaxTree, Term(..))
import qualified Csili.Frontend.SyntaxTree as SyntaxTree
import Csili.Program hiding (effects)

data ConversionError
    = DuplicateInputPlace Place
    | DuplicateOutputPlace Place
    | DuplicateInternalPlace Place
    | DuplicateToken Place
    | InvalidToken Place Term
    | DuplicateTransition TransitionName
    | DuplicatePattern TransitionName Place
    | DuplicateProduction TransitionName Place
    | InvalidProduction TransitionName Place Term
    | DuplicateEffect TransitionName Place
    | EffectOnProductionPlace TransitionName Place
    | InvalidEffect TransitionName Place Term
    | InexistentEffect TransitionName Place Symbol
    | ArityMismatchForEffect TransitionName Place Symbol Expectation Actual
    deriving (Show, Eq, Ord)

type Expectation = Int
type Actual = Int

convert :: SyntaxTree -> Validation [ConversionError] Program
convert tree = Program
    <$> toInterface tree
    <*> toInternalPlaces tree
    <*> toInitialMarking tree
    <*> toTransitions tree

toInterface :: SyntaxTree -> Validation [ConversionError] Interface
toInterface tree = Interface
    <$> (toPlaces DuplicateInputPlace . fst $ SyntaxTree.interface tree)
    <*> (toPlaces DuplicateOutputPlace . snd $ SyntaxTree.interface tree)

toInternalPlaces :: SyntaxTree -> Validation [ConversionError] (Set Place)
toInternalPlaces = toPlaces DuplicateInternalPlace . SyntaxTree.internalPlaces

toPlaces :: (Place -> ConversionError) -> [Text] -> Validation [ConversionError] (Set Place)
toPlaces mkError declarations = case findDuplicates declarations of
    [] -> pure . Set.fromList $ map Place declarations
    duplicates -> Failure $ map (mkError . Place) duplicates

toInitialMarking :: SyntaxTree -> Validation [ConversionError] Marking
toInitialMarking tree = case findDuplicates . map fst $ SyntaxTree.marking tree of
    [] -> Map.fromList <$> traverse convertPair (SyntaxTree.marking tree)
    duplicates -> Failure $ map (DuplicateToken . Place) duplicates
  where
    convertPair (place, term) = (,) <$> pure (Place place) <*> toToken (Place place) term

toToken :: Place -> Term -> Validation [ConversionError] Token
toToken place = \case
    Function symbol terms -> FunctionToken (Symbol symbol) <$> traverse (toToken place) terms
    IntTerm n -> pure $ IntToken n
    term@_ -> Failure [InvalidToken place term]

toTransitions :: SyntaxTree -> Validation [ConversionError] (Set Transition)
toTransitions tree = case findDuplicates . map fst $ SyntaxTree.transitions tree of
    [] -> Set.fromList <$> traverse toTransition (SyntaxTree.transitions tree)
    duplicates -> Failure $ map (DuplicateTransition . TransitionName) duplicates

toTransition :: (Text, ([SyntaxTree.Pattern], [SyntaxTree.ConstructionRule], [SyntaxTree.Effect])) -> Validation [ConversionError] Transition
toTransition (name, (patterns, constructionRules, effects)) = Transition
    <$> pure (TransitionName name)
    <*> processTransitionBlock toPattern DuplicatePattern (TransitionName name) patterns
    <*> toProductions name constructionRules effects

toProductions :: Text -> [SyntaxTree.ConstructionRule] -> [SyntaxTree.Effect] -> Validation [ConversionError] (Map Place Production)
toProductions name constructionRules effects = bindValidation separatedProductions (uncurry combine)
  where
    separatedProductions = (,)
        <$> processTransitionBlock toConstructionRule DuplicateProduction (TransitionName name) constructionRules
        <*> processTransitionBlock toEffect DuplicateEffect (TransitionName name) effects
    combine constructionMap effectMap = case Set.toAscList . Set.intersection (Map.keysSet constructionMap) $ Map.keysSet effectMap of
        [] -> pure $ Map.union (Map.map Construct constructionMap) (Map.map Evaluate effectMap)
        duplicates -> Failure $ map (EffectOnProductionPlace $ TransitionName name) duplicates

processTransitionBlock
    :: (TransitionName -> Place -> Term -> Validation [ConversionError] convertedTerm)
    -> (TransitionName -> Place -> ConversionError)
    -> TransitionName
    -> [(Text, Term)]
    -> Validation [ConversionError] (Map Place convertedTerm)
processTransitionBlock convertTerm mkDuplicateError name mapping = case findDuplicates (map fst mapping) of
      [] -> Map.fromList <$> traverse convertPair mapping
      duplicates -> Failure $ map (mkDuplicateError name . Place) duplicates
    where
      convertPair (place, term) = (,) <$> pure (Place place) <*> convertTerm name (Place place) term

toPattern :: TransitionName -> Place -> Term -> Validation [ConversionError] Pattern
toPattern _name _place = \case
    Function symbol terms -> FunctionPattern (Symbol symbol) <$> traverse (toPattern _name _place) terms
    IntTerm n -> pure $ IntPattern n
    Variable var -> pure $ VariablePattern (Var var)
    Wildcard -> pure $ WildcardPattern

toConstructionRule :: TransitionName -> Place -> Term -> Validation [ConversionError] ConstructionRule
toConstructionRule name place = \case
    Function symbol terms -> FunctionConstruction (Symbol symbol) <$> traverse (toConstructionRule name place) terms
    IntTerm n -> pure $ IntConstruction n
    Variable var -> pure $ Substitution (Var var)
    term@_ -> Failure [InvalidProduction name place term]

toEffect :: TransitionName -> Place -> Term -> Validation [ConversionError] Effect
toEffect name place = \case
    Function effect arguments -> case effect of
        "writeByte" -> case arguments of
            [stream, byte] -> WriteByte <$> toConstructionRule name place stream <*> toConstructionRule name place byte
            _ -> Failure [ArityMismatchForEffect name place (Symbol effect) 2 (length arguments)]
        _ -> Failure [InexistentEffect name place (Symbol effect)]
    term@_ -> Failure [InvalidEffect name place term]

findDuplicates :: Ord a => [a] -> [a]
findDuplicates elements = elements \\ nubOrd elements
