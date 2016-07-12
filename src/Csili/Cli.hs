{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main
( main
) where

import Control.Monad (join, when)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory (copyFile, removeFile)
import System.FilePath ((<.>), (</>))
import System.Process (callProcess)
import Options.Applicative
  
import Csili.Backend.CodeGenerator
import Csili.Frontend.Parser

main :: IO ()
main = join . execParser $ info
    (helper <*> versionOr (subparser commands))
    csiliInformation
  where
    commands = command "compile" compilation

versionOr :: Parser (IO ()) -> Parser (IO ())
versionOr = fmap (fromMaybe (putStrLn version))
    . (<|>) (flag' Nothing (long "version")) . fmap Just
  where
    version = intercalate "\n"
        [ "Csili 0.1.0.0"
        ]

csiliInformation :: InfoMod a
csiliInformation = fullDesc
    <> header "Csili - An Intermediate Language For Concurrency-Aware Schedulers"

--------------------------------------------------------------------------------
-- Compilation
--------------------------------------------------------------------------------

data CompilationOptions = CompilationOptions
    { retainIntermediateFiles :: Bool
    , runtime :: FilePath
    , output :: FilePath
    , files :: [FilePath]
    }

compilation :: ParserInfo (IO ())
compilation = info
    (compile <$> (helper <*> compilationParser))
    (csiliInformation <> progDesc "Compilation of a Csili program")

compilationParser :: Parser CompilationOptions
compilationParser = CompilationOptions
    <$> switch (hidden <> short 'i' <> long "retain" <> help "Retain intermediate files")
    <*> (fromMaybe "runtime" <$> optional runtimeOption)
    <*> (fromMaybe "program" <$> optional outputOption)
    <*> some (argument str (metavar "INPUT.."))
  where
    outputOption = strOption
        $ short 'o' <> long "output" <> metavar "FILE"
        <> help "Write program to FILE"
    runtimeOption = strOption
        $ short 'r' <> long "runtime" <> metavar "DIRECTORY"
        <> help "Location of the runtime"
        <> hidden

compile :: CompilationOptions -> IO ()
compile CompilationOptions{..} = do
    eitherErrorOrSemantics <- parseCsl . T.unlines <$> mapM T.readFile files
    case eitherErrorOrSemantics of
        Left err -> putStrLn err
        Right sem -> do
            copyFile (runtime </> "runtime.c") (output <.> "c")
            T.appendFile (output <.> "c") (generate sem)
            callProcess "gcc" ["--std=c99", "-o" ++ output, output <.> "c"]
            when (not retainIntermediateFiles) (removeFile $ output <.> "c")
