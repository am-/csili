{-# LANGUAGE OverloadedStrings #-}

module Main
( main
) where

import Control.Monad (forM_)
import Data.Either (partitionEithers)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import System.Directory (copyFile)
import System.FilePath ((</>))
  
import Csili.Backend.CodeGenerator
import Csili.Frontend.Parser
import Csili.Frontend.Unparser
import Csili.Semantics

main :: IO ()
main = do
  files <- getArgs
  forM_ files $ \file -> do
      eitherErrorOrSemantics <- parseCsl <$> T.readFile file
      case eitherErrorOrSemantics of
          Left err -> putStrLn err
          Right sem -> do
              copyFile ("runtime" </> "runtime.c") "program.c"
              T.appendFile "program.c" (generate sem)
