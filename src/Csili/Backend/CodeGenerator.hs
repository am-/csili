{-# LANGUAGE OverloadedStrings #-}

module Csili.Backend.CodeGenerator
( generate
) where

import Data.Text (Text)
import qualified Data.Text as T

import Csili.Backend.TermRewriting
import Csili.Backend.PetriNet
import Csili.Semantics

generate :: Semantics -> Text
generate sem = T.intercalate "\n"
    [ "////////////////////////////////////////////////////////////////////////////////"
    , "// Generated code"
    , "////////////////////////////////////////////////////////////////////////////////"
    , ""
    , generateRules sem
    , ""
    , generateScheduler sem
    , ""
    , generateMain
    ]

generateMain :: Text
generateMain = T.intercalate "\n"
    [ "int main() {"
    , "  places = calloc(sizeof *places, PLACES);"
    , "  places[PLACE___uninitialized] = allocate_function(TERM_unit, 0, NULL);"      
    , "  while(schedule());"
    , "  for(int i = 0; i < PLACES; i++) {"
    , "    if(places[i] != NULL) {"
    , "      printf(\"place[%d] = \", i);"
    , "      print_term(places[i]);"
    , "      printf(\"\\n\");"
    , "    }"
    , "  }"      
    , "}"
    ]
