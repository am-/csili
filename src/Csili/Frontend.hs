module Csili.Frontend
( Error(..)
, ConversionError(..)
, ValidationError(..)
, loadCsl
, parseCsl
) where

import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.Text
import Data.Validation (Validation, bindValidation, toEither)

import Csili.Frontend.Parser (SyntaxTree, file)
import Csili.Program (Program)
import Csili.Frontend.Conversion
import Csili.Validation

data Error
    = DuringParsing Text
    | DuringConversion ConversionError
    | DuringValidation ValidationError
    deriving (Show, Eq, Ord)

loadCsl :: FilePath -> IO (Either [Error] Program)
loadCsl = fmap parseCsl . T.readFile

parseCsl :: Text -> Either [Error] Program
parseCsl = either (Left . toParseError) (toEither . toProgram) . parseOnly file
  where
    toParseError = (:[]) . DuringParsing . T.pack

toProgram :: SyntaxTree -> Validation [Error] Program
toProgram tree = bindValidation
    (first (map DuringConversion) (convert tree))
    (first (map DuringValidation) . validateProgram)
