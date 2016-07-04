{-# LANGUAGE OverloadedStrings #-}

module Csili.Backend.CodeGenerator
( generate
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
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
    , generateMain (marking sem)
    ]

generateMain :: Map Place Term -> Text
generateMain initialMarking = T.intercalate "\n"
    [ "int main() {"
    , "  places = calloc(sizeof *places, PLACES);"
    , "  places[PLACE___uninitialized] = allocate_function(TERM_unit, 0, NULL);"      
    , "  while(schedule());"
    , "  for(int i = 0; i < PLACES; i++) {"
    , "    if(places[i] != NULL) {"
    , "      printf(\"place[%d] = \", i);"
    , "      print_term(places[i]);"
    , "    }"
    , "  }"      
    , "}"
    ]