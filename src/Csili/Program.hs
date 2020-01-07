{-# LANGUAGE MultiParamTypeClasses #-}

module Csili.Program
( Program(..)
, emptyProgram
, existingPlaces
, adressablePlaces
, produceOnlyPlaces
, consumeOnlyPlaces

, module Csili.Program.Net
, module Csili.Program.Pattern
, module Csili.Program.Token

, Collectible(..)
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Csili.Program.Net
import Csili.Program.Pattern
import Csili.Program.Token

data Program = Program
    { tokenTypes :: Map TokenTypeDefinition TokenTypeConstructors
    , mainNet :: Net
    , templates :: Map TemplateName Net
    } deriving (Show, Eq)

emptyProgram :: Program
emptyProgram = Program
    { tokenTypes = Map.empty
    , mainNet = emptyNet
    , templates = Map.empty
    }

existingPlaces :: Program -> Net -> Set Place
existingPlaces program net = Set.union (netPlaces net) (Set.unions placeSets)
  where
    placeSets = map netPlaces $ instantiateTemplates (templates program) (instances net)

adressablePlaces :: Program -> Net -> Set Place
adressablePlaces program net = Set.union (netPlaces net) (Set.unions interfaces)
  where
    interfaces = map extractInterface $ instantiateTemplates (templates program) (instances net)
    extractInterface = (Set.union <$> input <*> output) . interface

consumeOnlyPlaces :: Program -> Net -> Set Place
consumeOnlyPlaces program net = Set.union
    (input $ interface net)
    (Set.unions . map (output . interface) $ instantiateTemplates (templates program) (instances net))

produceOnlyPlaces :: Program -> Net -> Set Place
produceOnlyPlaces program net = Set.union
    (output $ interface net)
    (Set.unions . map (input . interface) $ instantiateTemplates (templates program) (instances net))

class Ord b => Collectible a b where
    collect :: a -> Set b

instance Collectible Token Symbol where
    collect = \case
        FunctionToken symbol terms -> Set.insert symbol (Set.unions (map collect terms))
        Resource _ -> Set.empty

instance Collectible Pattern Symbol where
    collect = \case
        FunctionPattern symbol terms -> Set.insert symbol (Set.unions (map collect terms))
        VariablePattern _ -> Set.empty
        WildcardPattern -> Set.empty

instance Collectible Production Symbol where
    collect = \case
        Construct construction -> collect construction
        Evaluate effect -> collect effect

instance Collectible Effect Symbol where
    collect = \case
        WriteWord8 stream word -> Set.union (collect stream) (collect word)
        ReadWord8 stream -> collect stream

instance Collectible ConstructionRule Symbol where
    collect = \case
        FunctionConstruction symbol terms ->  Set.insert symbol (Set.unions (map collect terms))
        Substitution _ -> Set.empty

instance Collectible Token Var where
    collect = const Set.empty

instance Collectible Pattern Var where
    collect = \case
        FunctionPattern _ terms -> Set.unions (map collect terms)
        VariablePattern var -> Set.singleton var
        WildcardPattern -> Set.empty

instance Collectible Production Var where
    collect = \case
        Construct construction -> collect construction
        Evaluate effect -> collect effect

instance Collectible ConstructionRule Var where
    collect = \case
        FunctionConstruction _ terms -> Set.unions (map collect terms)
        Substitution var -> Set.singleton var

instance Collectible Effect Var where
    collect = \case
      WriteWord8 stream word -> Set.union (collect stream) (collect word)
      ReadWord8 stream -> collect stream
