module Csili.Frontend
( loadCsl
, parseCsl
, Error(..)
) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.Text
import Data.Validation (toEither)

import Csili.Frontend.Parser (file)
import Csili.Program (Program)
import Csili.Frontend.Conversion

loadCsl :: FilePath -> IO (Either [Error] Program)
loadCsl = fmap parseCsl . T.readFile

parseCsl :: Text -> Either [Error] Program
parseCsl = either (Left . toParseError) (toEither . convert) . parseOnly file
  where
    toParseError = (:[]) . ParseError . T.pack
