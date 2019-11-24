{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main
( main
) where

import Data.Version (showVersion)
import Control.Monad (join)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import Paths_csili (version)

import Csili.Frontend
import Csili.Interpreter (evaluate)

main :: IO ()
main = join . execParser $ info
    (helper <*> versionOption <*> subparser commands)
    csiliInformation
  where
    commands = command "run" run

versionOption :: Parser (a -> a)
versionOption = infoOption ("Csili " ++ showVersion version) (long "version" <> help "Show version")

csiliInformation :: InfoMod a
csiliInformation = fullDesc <> header "Csili - An Intermediate Language For Concurrency-Aware Schedulers"

--------------------------------------------------------------------------------
-- Run
--------------------------------------------------------------------------------

data RunOptions = RunOptions
    { file :: FilePath
    }

run :: ParserInfo (IO ())
run = info
    (interpret <$> (helper <*> runParser))
    (csiliInformation <> progDesc "Run a Csili program")

runParser :: Parser RunOptions
runParser = RunOptions
    <$> argument str (metavar "INPUT")

interpret :: RunOptions -> IO ()
interpret RunOptions{..} = loadCsl file >>= \case
    Left errors -> mapM_ printError errors
    Right program -> evaluate program `seq` return ()

printError :: Error -> IO ()
printError = T.putStrLn . T.pack . show
