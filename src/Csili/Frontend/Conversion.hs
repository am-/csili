module Csili.Frontend.Conversion
( Error(..)
, convert
) where

import Data.Bifunctor (first, second)
import Data.Containers.ListUtils (nubOrd)
import Data.List ((\\))
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Validation (Validation(..), bindValidation)

import Csili.Frontend.SyntaxTree (SyntaxTree, Term(..))
import qualified Csili.Frontend.SyntaxTree as SyntaxTree (SyntaxTree(..), Pattern, Production)
import Csili.Program (Program(Program), Transition, TransitionName(..), Interface, Token(..), Place(..), Pattern(..), Production(..))
import qualified Csili.Program as Program

data Error
    = ParseError Text
    | ConsumingFromOutputPlace TransitionName Place
    | ProducingOnInputPlace TransitionName Place
    | ConsumingFromInexistentPlace TransitionName Place
    | ProducingOnInexistentPlace TransitionName Place
    | DuplicateInputPlace Place
    | DuplicateOutputPlace Place
    | OverlappingInputAndOutput Place
    | DuplicateInternalPlace Place
    | InternalPlaceInsideInterface Place
    | DuplicateToken Place
    | InvalidToken Place Term
    | DuplicateTransition TransitionName
    | DuplicatePattern TransitionName Place
    | DuplicateProduction TransitionName Place
    | InvalidProduction TransitionName Place Term
    deriving (Show, Eq)

type Errors = [Error]

convert :: SyntaxTree -> Validation Errors Program
convert tree = bindValidation (toProgram tree) validateProgram

toProgram :: SyntaxTree -> Validation Errors Program
toProgram tree = Program
    <$> bindValidation (toInterface tree) validateInterface
    <*> toInternalPlaces tree
    <*> toInitialMarking tree
    <*> toTransitions tree

validateProgram :: Program -> Validation Errors Program
validateProgram program = toValidation $ concat
    [ concatMap (validateTransition program) . Set.toList $ Program.transitions program
    , validateInternalPlacesAgainstInterface (Program.interface program) (Program.internalPlaces program)
    ]
  where
    toValidation = \case
        [] -> pure program
        errors -> Failure errors

validateTransition :: Program -> Transition -> Errors
validateTransition program transition = concatMap ($ transition)
    [ validateTransitionAgainstInterface (Program.interface program)
    , validateTransitionAgainstPlaces (Program.places program)
    ]

validateTransitionAgainstInterface :: Interface -> Transition -> Errors
validateTransitionAgainstInterface interface transition = concat
    [ toErrors ConsumingFromOutputPlace . Set.intersection preset $ Program.output interface
    , toErrors ProducingOnInputPlace . Set.intersection postset $ Program.input interface
    ]
  where
    preset = Map.keysSet $ Program.patterns transition
    postset = Map.keysSet $ Program.productions transition
    toErrors mkError = map (mkError $ Program.name transition) . Set.toAscList

validateTransitionAgainstPlaces :: Set Place -> Transition -> Errors
validateTransitionAgainstPlaces places transition = concat
    [ toErrors ConsumingFromInexistentPlace $ Set.difference preset places
    , toErrors ProducingOnInexistentPlace $ Set.difference postset places
    ]
  where
    preset = Map.keysSet $ Program.patterns transition
    postset = Map.keysSet $ Program.productions transition
    toErrors mkError = map (mkError $ Program.name transition) . Set.toAscList

validateInternalPlacesAgainstInterface :: Interface -> Set Place -> Errors
validateInternalPlacesAgainstInterface interface internalPlaces = concatMap (map InternalPlaceInsideInterface . Set.toAscList)
    [ Set.intersection internalPlaces $ Program.input interface
    , Set.intersection internalPlaces $ Program.output interface
    ]

toInterface :: SyntaxTree -> Validation Errors Program.Interface
toInterface tree = Program.Interface
    <$> (toPlaces DuplicateInputPlace . fst $ SyntaxTree.interface tree)
    <*> (toPlaces DuplicateOutputPlace . snd $ SyntaxTree.interface tree)

validateInterface :: Program.Interface -> Validation Errors Program.Interface
validateInterface interface
    | Set.null overlappingPlaces = pure interface
    | otherwise = Failure . map OverlappingInputAndOutput $ Set.toAscList overlappingPlaces
  where
    overlappingPlaces = Set.intersection (Program.input interface) (Program.output interface)

toInternalPlaces :: SyntaxTree -> Validation Errors (Set Place)
toInternalPlaces = toPlaces DuplicateInternalPlace . SyntaxTree.internalPlaces

toPlaces :: (Place -> Error) -> [Text] -> Validation Errors (Set Place)
toPlaces mkError declarations = case findDuplicates declarations of
    [] -> pure . Set.fromList $ map Place declarations
    duplicates -> Failure $ map (mkError . Place) duplicates

toInitialMarking :: SyntaxTree -> Validation Errors Program.Marking
toInitialMarking tree = case findDuplicates . map fst $ SyntaxTree.marking tree of
    [] -> Map.fromList <$> traverse convertPair (SyntaxTree.marking tree)
    duplicates -> Failure $ map (DuplicateToken . Place) duplicates
  where
    convertPair (place, term) = (,) <$> pure (Place place) <*> toToken (Place place) term

toToken :: Place -> Term -> Validation Errors Token
toToken place = \case
    Function symbol terms -> Program.FunctionToken (Program.Symbol symbol) <$> traverse (toToken place) terms
    IntTerm n -> pure $ Program.IntToken n
    term@_ -> Failure [InvalidToken place term]

toTransitions :: SyntaxTree -> Validation Errors (Set Transition)
toTransitions tree = case findDuplicates . map fst $ SyntaxTree.transitions tree of
    [] -> Set.fromList <$> traverse toTransition (SyntaxTree.transitions tree)
    duplicates -> Failure $ map (DuplicateTransition . TransitionName) duplicates

toTransition :: (Text, ([SyntaxTree.Pattern], [SyntaxTree.Production])) -> Validation Errors Transition
toTransition (name, (patterns, productions)) = Program.Transition
    <$> pure (TransitionName name)
    <*> toPatterns (TransitionName name) patterns
    <*> toProductions (TransitionName name) productions

toPatterns :: TransitionName -> [SyntaxTree.Pattern] -> Validation Errors (Map Place Pattern)
toPatterns name patterns = case findDuplicates (map fst patterns) of
    [] -> pure . Map.fromList $ map (second toPattern . first Place) patterns
    duplicates -> Failure $ map (DuplicatePattern name . Place) duplicates

toPattern :: Term -> Pattern
toPattern = \case
    Function symbol terms -> Program.FunctionPattern (Program.Symbol symbol) (map toPattern terms)
    IntTerm n -> Program.IntPattern n
    Variable var -> Program.VariablePattern (Program.Var var)
    Wildcard -> Program.WildcardPattern

toProductions :: TransitionName -> [SyntaxTree.Production] -> Validation Errors (Map Place Production)
toProductions name productions = case findDuplicates (map fst productions) of
    [] -> Map.fromList <$> traverse convertPair productions
    duplicates -> Failure $ map (DuplicateProduction name . Place) duplicates
  where
    convertPair (place, production) = (,) <$> pure (Place place) <*> toProduction name (Place place) production

toProduction :: TransitionName -> Place -> Term -> Validation Errors Production
toProduction name place = \case
    Function symbol terms -> Program.FunctionProduction (Program.Symbol symbol) <$> traverse (toProduction name place) terms
    IntTerm n -> pure $ Program.IntProduction n
    Variable var -> pure $ Program.Substitution (Program.Var var)
    term@_ -> Failure [InvalidProduction name place term]

findDuplicates :: Ord a => [a] -> [a]
findDuplicates elements = elements \\ nubOrd elements
