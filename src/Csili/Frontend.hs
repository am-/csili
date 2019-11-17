module Csili.Frontend
( loadCsl
, parseCsl
) where

import Data.Bifunctor (first, second)
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Attoparsec.Text

import Csili.Frontend.Parser (SyntaxTree, Term(..), file)
import qualified Csili.Frontend.Parser as SyntaxTree (SyntaxTree(..))
import Csili.Program as Program (Program, Token(..), Pattern(..), Production(..))
import qualified Csili.Program as Program

loadCsl :: FilePath -> IO (Either String Program)
loadCsl = fmap parseCsl . T.readFile

parseCsl :: Text -> Either String Program
parseCsl = fmap toProgram . parseOnly file

toProgram :: SyntaxTree -> Program
toProgram tree = Program.empty
    { Program.interface = Program.Interface
        { Program.input = Set.fromList . map Program.Place . fst $ SyntaxTree.interface tree
        , Program.output = Set.fromList . map Program.Place . snd $ SyntaxTree.interface tree
        }
    , Program.initialMarking = Map.fromList . map (second toToken . first Program.Place) $ SyntaxTree.marking tree
    , Program.patterns = Map.fromList . map (extractPattern . first Program.Transition) $ SyntaxTree.transitions tree
    , Program.productions = Map.fromList . map (extractProduction . first Program.Transition) $ SyntaxTree.transitions tree
    }
  where
    extractPattern = second (Map.fromList . map (second toPattern . first Program.Place) . fst)
    extractProduction = second (Map.fromList . map (second toProduction . first Program.Place) . snd)

toToken :: Term -> Token
toToken = \case
    Function symbol terms -> Program.FunctionToken (Program.Symbol symbol) (map toToken terms)
    IntTerm n -> Program.IntToken n
    term@_ -> error $ "Cannot convert term into token: " ++ show term

toPattern :: Term -> Pattern
toPattern = \case
    Function symbol terms -> Program.FunctionPattern (Program.Symbol symbol) (map toPattern terms)
    IntTerm n -> Program.IntPattern n
    Variable var -> Program.VariablePattern (Program.Var var)
    Wildcard -> Program.WildcardPattern

toProduction :: Term -> Production
toProduction = \case
    Function symbol terms -> Program.FunctionProduction (Program.Symbol symbol) (map toProduction terms)
    IntTerm n -> Program.IntProduction n
    Variable var -> Program.Substitution (Program.Var var)
    term@_ -> error $ "Cannot convert term into production: " ++ show term
