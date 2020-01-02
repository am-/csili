{-# LANGUAGE RecordWildCards #-}

module Csili.Frontend.Conversion
( ConversionError(..)
, convert
) where

import Data.Bifunctor (first)
import Data.Containers.ListUtils (nubOrd)
import Data.List ((\\))
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Validation (Validation(..), bindValidation)
import Data.Maybe (mapMaybe)

import Csili.Frontend.SyntaxTree (SyntaxTree, Term(..))
import qualified Csili.Frontend.SyntaxTree as SyntaxTree
import Csili.Program hiding (effects)

data ConversionError
    = DuplicateTemplate TemplateName
    | CyclicTemplates [TemplateName]
    | DuplicateInstance TemplateInstance
    | DuplicateInputPlace Place
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
    <$> toNet (SyntaxTree.mainNet tree)
    <*> bindValidation (toTemplates $ SyntaxTree.nets tree) ensureAcyclicity

toTemplates :: [(Text, SyntaxTree.Net)] -> Validation [ConversionError] (Map TemplateName Net)
toTemplates nets = case findDuplicates (map fst nets) of
    [] -> traverse toNet . Map.fromList $ map (first TemplateName) nets
    duplicates -> Failure $ map (DuplicateTemplate . TemplateName) duplicates

ensureAcyclicity :: Map TemplateName Net -> Validation [ConversionError] (Map TemplateName Net)
ensureAcyclicity templateMap = case mapMaybe (toCycle Set.empty) . Set.elems $ Map.keysSet templateMap of
    [] -> pure templateMap
    cycles -> Failure $ map CyclicTemplates cycles
  where
    toCycle visited templateName
        | Set.member templateName visited = Just [templateName]
        | otherwise = case instances <$> Map.lookup templateName templateMap of
            Nothing -> Nothing
            Just instanceMap
                | Map.null instanceMap -> Nothing
                | otherwise -> case mapMaybe (toCycle (Set.insert templateName visited)) $ Map.elems instanceMap of
                    [] -> Nothing
                    cyclicTemplates:_ -> Just (templateName : cyclicTemplates)

toNet :: SyntaxTree.Net -> Validation [ConversionError] Net
toNet net = Net
    <$> toInstances net
    <*> toInterface net
    <*> toInternalPlaces net
    <*> toInitialMarking net
    <*> toTransitions net

toInstances :: SyntaxTree.Net -> Validation [ConversionError] (Map TemplateInstance TemplateName)
toInstances net = case findDuplicates . map fst $ SyntaxTree.instances net of
    [] -> pure . Map.fromList . map ((,) <$> TemplateInstance . fst <*> TemplateName . snd) $ SyntaxTree.instances net
    duplicates -> Failure $ map (DuplicateInstance . TemplateInstance) duplicates

toInterface :: SyntaxTree.Net -> Validation [ConversionError] Interface
toInterface net = Interface
    <$> (toPlaces DuplicateInputPlace . fst $ SyntaxTree.interface net)
    <*> (toPlaces DuplicateOutputPlace . snd $ SyntaxTree.interface net)

toInternalPlaces :: SyntaxTree.Net -> Validation [ConversionError] (Set Place)
toInternalPlaces = toPlaces DuplicateInternalPlace . SyntaxTree.internalPlaces

toPlaces :: (Place -> ConversionError) -> [SyntaxTree.Place] -> Validation [ConversionError] (Set Place)
toPlaces mkError declarations = case findDuplicates declarations of
    [] -> Set.fromList <$> traverse toPlace declarations
    duplicates -> bindValidation (traverse (fmap mkError . toPlace) duplicates) Failure

toPlace :: SyntaxTree.Place -> Validation [ConversionError] Place
toPlace components = pure $ case (init components, last components) of
    ([], place) -> LocalPlace place
    (instanceName, place) -> TemplatePlace (TemplateInstance (foldl1 (<>) instanceName)) place

toInitialMarking :: SyntaxTree.Net -> Validation [ConversionError] Marking
toInitialMarking net = case findDuplicates . map fst $ SyntaxTree.marking net of
    [] -> Map.fromList <$> traverse convertPair (SyntaxTree.marking net)
    duplicates -> bindValidation (traverse (fmap DuplicateToken . toPlace) duplicates) Failure
  where
    convertPair (placeName, term) = bindValidation (toPlace placeName) (\place -> (,) place <$> toToken place term)

toToken :: Place -> Term -> Validation [ConversionError] Token
toToken place = \case
    Function symbol terms -> FunctionToken (Symbol symbol) <$> traverse (toToken place) terms
    term@_ -> Failure [InvalidToken place term]

toTransitions :: SyntaxTree.Net -> Validation [ConversionError] (Set Transition)
toTransitions net = case findDuplicates . map fst $ SyntaxTree.transitions net of
    [] -> Set.fromList <$> traverse toTransition (SyntaxTree.transitions net)
    duplicates -> Failure $ map (DuplicateTransition . LocalTransition) duplicates

toTransition :: (Text, ([SyntaxTree.Pattern], [SyntaxTree.ConstructionRule], [SyntaxTree.Effect])) -> Validation [ConversionError] Transition
toTransition (name, (patterns, constructionRules, effects)) = Transition
    <$> pure (LocalTransition name)
    <*> processTransitionBlock toPattern DuplicatePattern (LocalTransition name) patterns
    <*> toProductions name constructionRules effects

toProductions :: Text -> [SyntaxTree.ConstructionRule] -> [SyntaxTree.Effect] -> Validation [ConversionError] (Map Place Production)
toProductions name constructionRules effects = bindValidation separatedProductions (uncurry combine)
  where
    separatedProductions = (,)
        <$> processTransitionBlock toConstructionRule DuplicateProduction (LocalTransition name) constructionRules
        <*> processTransitionBlock toEffect DuplicateEffect (LocalTransition name) effects
    combine constructionMap effectMap = case Set.toAscList . Set.intersection (Map.keysSet constructionMap) $ Map.keysSet effectMap of
        [] -> pure $ Map.union (Map.map Construct constructionMap) (Map.map Evaluate effectMap)
        duplicates -> Failure $ map (EffectOnProductionPlace $ LocalTransition name) duplicates

processTransitionBlock
    :: (TransitionName -> Place -> Term -> Validation [ConversionError] convertedTerm)
    -> (TransitionName -> Place -> ConversionError)
    -> TransitionName
    -> [(SyntaxTree.Place, Term)]
    -> Validation [ConversionError] (Map Place convertedTerm)
processTransitionBlock convertTerm mkDuplicateError name mapping = case findDuplicates (map fst mapping) of
      [] -> Map.fromList <$> traverse convertPair mapping
      duplicates -> bindValidation (traverse (fmap (mkDuplicateError name) . toPlace) duplicates) Failure
    where
      convertPair (placeName, term) = bindValidation (toPlace placeName) (\place -> (,) place <$> convertTerm name place term)

toPattern :: TransitionName -> Place -> Term -> Validation [ConversionError] Pattern
toPattern _name _place = \case
    Function symbol terms -> FunctionPattern (Symbol symbol) <$> traverse (toPattern _name _place) terms
    Variable var -> pure $ VariablePattern (Var var)
    Wildcard -> pure $ WildcardPattern

toConstructionRule :: TransitionName -> Place -> Term -> Validation [ConversionError] ConstructionRule
toConstructionRule name place = \case
    Function symbol terms -> FunctionConstruction (Symbol symbol) <$> traverse (toConstructionRule name place) terms
    Variable var -> pure $ Substitution (Var var)
    term@_ -> Failure [InvalidProduction name place term]

toEffect :: TransitionName -> Place -> Term -> Validation [ConversionError] Effect
toEffect name place = \case
    Function effect arguments -> case effect of
        "writeWord8" -> case arguments of
            [stream, word] -> WriteWord8 <$> toConstructionRule name place stream <*> toConstructionRule name place word
            _ -> Failure [ArityMismatchForEffect name place (Symbol effect) 2 (length arguments)]
        "readWord8" -> case arguments of
            [stream] -> ReadWord8 <$> toConstructionRule name place stream
            _ -> Failure [ArityMismatchForEffect name place (Symbol effect) 1 (length arguments)]
        _ -> Failure [InexistentEffect name place (Symbol effect)]
    term@_ -> Failure [InvalidEffect name place term]

findDuplicates :: Ord a => [a] -> [a]
findDuplicates elements = elements \\ nubOrd elements
