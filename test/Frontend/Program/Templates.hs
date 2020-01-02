module Frontend.Program.Templates
( tests
) where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty, (==>), Property)

import Csili.Program
import Csili.Frontend

tests :: TestTree
tests = testGroup "Templates"
    [ testCase "Single Template" singleEmptyTemplate
    , testCase "Multiple Templates" multipleTemplates
    , testCase "Duplicate Templates" duplicateTemplates
    , testCase "Duplicate Instance" duplicateInstance
    , testCase "Reference of Unknown Template" instanceOfUnknownTemplate
    , testCase "Use Template's Interface Places (properly)" useTemplatesInterfacePlacesProperly
    , testCase "Use Template's Interface Places (improperly)" useTemplatesInterfacePlacesImproperly
    , testCase "Use Template's Internal Place" useTemplatesInternalPlace
    , testCase "Use Template Inside Another Template" useTemplateInsideAnotherTemplate
    , testCase "Declare Template's Input Place inside Interface" declareTemplatesInputPlaceInsideInterface
    , testCase "Declare Template's Output Place inside Interface" declareTemplatesOutputPlaceInsideInterface
    , testCase "Declare Template's Internal Place inside Interface" declareTemplatesInternalPlaceInsideInterface
    , testCase "Declare Template's Input Place as Internal" declareTemplatesInputPlaceAsInternal
    , testCase "Declare Template's Output Place as Internal" declareTemplatesOutputPlaceAsInternal
    , testCase "Declare Template's Internal Place as Internal" declareTemplatesInternalPlaceAsInternal
    , testCase "Declare Another Template's Input Place inside Interface" declareAnotherTemplatesInputPlaceInsideInterface
    , testCase "Declare Another Template's Output Place inside Interface" declareAnotherTemplatesOutputPlaceInsideInterface
    , testCase "Declare Another Template's Internal Place inside Interface" declareAnotherTemplatesInternalPlaceInsideInterface
    , testCase "Declare Another Template's Input Place as Internal" declareAnotherTemplatesInputPlaceAsInternal
    , testCase "Declare Another Template's Output Place as Internal" declareAnotherTemplatesOutputPlaceAsInternal
    , testCase "Declare Another Template's Internal Place as Internal" declareAnotherTemplatesInternalPlaceAsInternal
    , testCase "Marking of Template's Input Place" markingOfTemplatesInputPlace
    , testCase "Marking of Template's Output Place" markingOfTemplatesOutputPlace
    , testCase "Marking of Template's Internal Place" markingOfTemplatesInternalPlace
    , testCase "Marking of Another Template's Input Place" markingOfAnotherTemplatesInputPlace
    , testCase "Marking of Another Template's Output Place" markingOfAnotherTemplatesOutputPlace
    , testCase "Marking of Another Template's Internal Place" markingOfAnotherTemplatesInternalPlace
    , testCase "Self Instantiation" selfInstantiation
    , testProperty "All-Encompassing Template Cycle" allEncompassingTemplateCycle
    , testProperty "Lasso Template Cycle" lassoTemplateCycle
    ]

singleEmptyTemplate :: Assertion
singleEmptyTemplate = Right expectation @=? templates <$> parseCsl program
  where
    expectation = Map.singleton (TemplateName "x") expectedNet
    expectedNet = emptyNet
        { interface = emptyInterface
            { input = Set.singleton (LocalPlace "x")
            , output = Set.singleton (LocalPlace "y")
            }
        , transitions = Set.singleton t
        }
    t = (mkTransition "t")
        { patterns = Map.singleton (LocalPlace "x") WildcardPattern
        , productions = Map.singleton (LocalPlace "y") (Construct (FunctionConstruction (Symbol "blackToken") []))
        }
    program = T.concat
        [ "TEMPLATE x {"
        , "  INTERFACE { INPUT { x } OUTPUT { y } }"
        , "  TRANSITION t { MATCH { x: _ } PRODUCE { y: @ } }"
        , "}"
        ]

multipleTemplates :: Assertion
multipleTemplates = Right expectation @=? templates <$> parseCsl program
  where
    expectation = Map.fromList
        [ (TemplateName "toBlackToken", blackTokenNet)
        , (TemplateName "toFalse", falseNet)
        , (TemplateName "toTrue", trueNet)
        ]
    blackTokenNet = mkNet (FunctionConstruction (Symbol "blackToken") [])
    falseNet = mkNet (FunctionConstruction (Symbol "false") [])
    trueNet = mkNet (FunctionConstruction (Symbol "true") [])
    mkNet construction = emptyNet
        { interface = emptyInterface
            { input = Set.singleton (LocalPlace "input")
            , output = Set.singleton (LocalPlace "output")
            }
        , transitions = Set.singleton $ (mkTransition "t")
            { patterns = Map.singleton (LocalPlace "input") WildcardPattern
            , productions = Map.singleton (LocalPlace "output") (Construct construction)
            }
        }
    program = T.concat
        [ "TEMPLATE toBlackToken {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  TRANSITION t { MATCH { input: _ } PRODUCE { output: @ } }"
        , "}"
        , "TEMPLATE toFalse {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  TRANSITION t { MATCH { input: _ } PRODUCE { output: false } }"
        , "}"
        , "TEMPLATE toTrue {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  TRANSITION t { MATCH { input: _ } PRODUCE { output: true } }"
        , "}"
        ]

duplicateTemplates :: Assertion
duplicateTemplates = Left expectation @=? parseCsl program
  where
    expectation = [DuringConversion $ DuplicateTemplate (TemplateName "x")]
    program = "TEMPLATE x {} TEMPLATE x {}"

duplicateInstance :: Assertion
duplicateInstance = Left expectation @=? parseCsl program
  where
    expectation = [DuringConversion $ DuplicateInstance (TemplateInstance "z")]
    program = T.concat
        [ "INSTANCES { z: x z: y }"
        , "TEMPLATE x {}"
        , "TEMPLATE y {}"
        ]

instanceOfUnknownTemplate :: Assertion
instanceOfUnknownTemplate = Left expectation @=? parseCsl program
  where
    expectation = [DuringValidation $ InstanceOfUnknownTemplate (TemplateInstance "y") (TemplateName "z")]
    program = T.concat
        [ "TEMPLATE x {"
        , "  INSTANCES { y: z }"
        , "}"
        ]

useTemplatesInterfacePlacesProperly :: Assertion
useTemplatesInterfacePlacesProperly = Right expectation @=? mainNet <$> parseCsl program
  where
    expectation = emptyNet
        { instances = Map.singleton templateInstance (TemplateName "toBlackToken")
        , transitions = Set.fromList [produceOntoNet, consumeFromNet]
        }
    produceOntoNet = (mkTransition "produceOntoNet")
        { productions = Map.singleton (TemplatePlace templateInstance "input") (Construct $ FunctionConstruction (Symbol "false") [])
        }
    consumeFromNet = (mkTransition "consumeFromNet")
        { patterns = Map.singleton (TemplatePlace templateInstance "output") (FunctionPattern (Symbol "blackToken") [])
        }
    templateInstance = TemplateInstance "tokenizer"
    program = T.concat
        [ "INSTANCES { tokenizer: toBlackToken }"
        , "TRANSITION produceOntoNet { PRODUCE { tokenizer.input: false } }"
        , "TRANSITION consumeFromNet { MATCH { tokenizer.output: @ } }"
        , "TEMPLATE toBlackToken {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  TRANSITION t { MATCH { input: _ } PRODUCE { output: @ } }"
        , "}"
        ]

useTemplatesInterfacePlacesImproperly :: Assertion
useTemplatesInterfacePlacesImproperly = Left expectation @=? parseCsl program
  where
    expectation = map (DuringValidation $)
        [ ConsumingFromProduceOnlyPlace (LocalTransition "consumeFromNet") (TemplatePlace (TemplateInstance "tokenizer") "input")
        , EffectOnConsumeOnlyPlace (LocalTransition "effectOnNet") (TemplatePlace (TemplateInstance "tokenizer") "effect")
        , ConstructingOnConsumeOnlyPlace (LocalTransition "produceOntoNet") (TemplatePlace (TemplateInstance "tokenizer") "output")
        ]
    program = T.concat
        [ "INSTANCES { tokenizer: toBlackToken }"
        , "PLACES { stream }"
        , "TRANSITION produceOntoNet { PRODUCE { tokenizer.output: false } }"
        , "TRANSITION consumeFromNet { MATCH { tokenizer.input: @ } }"
        , "TRANSITION effectOnNet { MATCH { stream: Stream } EFFECTS { tokenizer.effect: writeWord8(Stream, 0x00) } }"
        , "TEMPLATE toBlackToken {"
        , "  INTERFACE { INPUT { input } OUTPUT { output effect } }"
        , "  TRANSITION t { MATCH { input: _ } PRODUCE { output: @ } }"
        , "}"
        ]

useTemplatesInternalPlace :: Assertion
useTemplatesInternalPlace = Left expectation @=? parseCsl program
  where
    expectation = map (DuringValidation)
        [ TokenOnHiddenPlace (TemplatePlace templateInstance "inner")
        , ConsumingFromHiddenPlace (LocalTransition "reset") (TemplatePlace templateInstance "inner")
        , ConstructingOnHiddenPlace (LocalTransition "reset") (TemplatePlace templateInstance "inner")
        , EffectOnHiddenPlace (LocalTransition "reset") (TemplatePlace templateInstance "effect")
        ]
    templateInstance = TemplateInstance "delay"
    program = T.concat
        [ "INSTANCES { delay: delay }"
        , "PLACES { stream }"
        , "MARKING { delay.inner: @ }"
        , "TRANSITION reset { MATCH { stream: Stream delay.inner: _ } PRODUCE { delay.inner: @ } EFFECTS { delay.effect: writeWord8(Stream, 0x00) } }"
        , "TEMPLATE delay {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  PLACES { inner effect }"
        , "  MARKING { inner: @ }"
        , "  TRANSITION forward { MATCH { input: X inner: Y } PRODUCE { inner: X output: Y } }"
        , "  TRANSITION clearEffect { MATCH { effect: _ } }"
        , "}"
        ]

useTemplateInsideAnotherTemplate :: Assertion
useTemplateInsideAnotherTemplate = Right expectation @=? templates <$> parseCsl program
  where
    expectation = Map.fromList
        [ (TemplateName "toTrue", trueNet)
        , (TemplateName "toFalse", falseNet)
        , (TemplateName "not", notNet)
        ]
    trueNet = emptyNet
        { instances = Map.fromList [(TemplateInstance "toFalse", TemplateName "toFalse"), (TemplateInstance "not", TemplateName "not")]
        , interface = emptyInterface
            { input = Set.singleton (LocalPlace "input")
            , output = Set.singleton (LocalPlace "output")
            }
        , transitions = Set.fromList [toFalse, toNot, toOutput]
        }
    toFalse = (mkTransition "toFalse")
        { patterns = Map.singleton (LocalPlace "input") (VariablePattern $ Var "X")
        , productions = Map.singleton (TemplatePlace (TemplateInstance "toFalse") "input") (Construct . Substitution $ Var "X")
        }
    toNot = (mkTransition "toNot")
        { patterns = Map.singleton (TemplatePlace (TemplateInstance "toFalse") "output") (VariablePattern $ Var "X")
        , productions = Map.singleton (TemplatePlace (TemplateInstance "not") "input") (Construct . Substitution $ Var "X")
        }
    toOutput = (mkTransition "toOutput")
        { patterns = Map.singleton (TemplatePlace (TemplateInstance "not") "output") (VariablePattern $ Var "X")
        , productions = Map.singleton (LocalPlace "output") (Construct . Substitution $ Var "X")
        }
    falseNet = emptyNet
        { interface = emptyInterface
            { input = Set.singleton (LocalPlace "input")
            , output = Set.singleton (LocalPlace "output")
            }
        , transitions = Set.singleton $ (mkTransition "t")
            { patterns = Map.singleton (LocalPlace "input") WildcardPattern
            , productions = Map.singleton (LocalPlace "output") (Construct $ FunctionConstruction (Symbol "false") [])
            }
        }
    notNet = emptyNet
        { interface = emptyInterface
            { input = Set.singleton (LocalPlace "input")
            , output = Set.singleton (LocalPlace "output")
            }
        , transitions = Set.fromList [notTrue, notFalse]
        }
    notTrue = (mkTransition "notTrue")
        { patterns = Map.singleton (LocalPlace "input") (FunctionPattern (Symbol "true") [])
        , productions = Map.singleton (LocalPlace "output") (Construct $ FunctionConstruction (Symbol "false") [])
        }
    notFalse = (mkTransition "notFalse")
        { patterns = Map.singleton (LocalPlace "input") (FunctionPattern (Symbol "false") [])
        , productions = Map.singleton (LocalPlace "output") (Construct $ FunctionConstruction (Symbol "true") [])
        }
    program = T.concat
        [ "TEMPLATE toTrue {"
        , "  INSTANCES { toFalse: toFalse not: not }"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  TRANSITION toFalse { MATCH { input: X } PRODUCE { toFalse.input: X } }"
        , "  TRANSITION toNot { MATCH { toFalse.output: X } PRODUCE { not.input: X } }"
        , "  TRANSITION toOutput { MATCH { not.output: X } PRODUCE { output: X } }"
        , "}"
        , "TEMPLATE toFalse {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  TRANSITION t { MATCH { input: _ } PRODUCE { output: false } }"
        , "}"
        , "TEMPLATE not {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  TRANSITION notTrue  { MATCH { input: true }  PRODUCE { output: false } }"
        , "  TRANSITION notFalse { MATCH { input: false } PRODUCE { output: true  } }"
        , "}"
        ]

declareTemplatesInputPlaceInsideInterface :: Assertion
declareTemplatesInputPlaceInsideInterface = Left expectation @=? parseCsl program
  where
    expectation = [DuringValidation . RedeclarationOfPlace $ TemplatePlace (TemplateInstance "delay") "input"]
    program = T.concat
        [ "INSTANCES { delay: delay }"
        , "INTERFACE { INPUT { delay.input } }"
        , "TEMPLATE delay {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  PLACES { inner }"
        , "  TRANSITION initialize { MATCH { input: X } PRODUCE { inner: X } }"
        , "  TRANSITION forward { MATCH { input: X inner: Y } PRODUCE { inner: X output: Y } }"
        , "}"
        ]

declareTemplatesOutputPlaceInsideInterface :: Assertion
declareTemplatesOutputPlaceInsideInterface = Left expectation @=? parseCsl program
  where
    expectation = [DuringValidation . RedeclarationOfPlace $ TemplatePlace (TemplateInstance "delay") "output"]
    program = T.concat
        [ "INSTANCES { delay: delay }"
        , "INTERFACE { OUTPUT { delay.output } }"
        , "TEMPLATE delay {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  PLACES { inner }"
        , "  TRANSITION initialize { MATCH { input: X } PRODUCE { inner: X } }"
        , "  TRANSITION forward { MATCH { input: X inner: Y } PRODUCE { inner: X output: Y } }"
        , "}"
        ]

declareTemplatesInternalPlaceInsideInterface :: Assertion
declareTemplatesInternalPlaceInsideInterface = Left expectation @=? parseCsl program
  where
    expectation = [DuringValidation . RedeclarationOfPlace $ TemplatePlace (TemplateInstance "delay") "inner"]
    program = T.concat
        [ "INSTANCES { delay: delay }"
        , "INTERFACE { INPUT { delay.inner } }"
        , "TEMPLATE delay {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  PLACES { inner }"
        , "  TRANSITION initialize { MATCH { input: X } PRODUCE { inner: X } }"
        , "  TRANSITION forward { MATCH { input: X inner: Y } PRODUCE { inner: X output: Y } }"
        , "}"
        ]

declareTemplatesInputPlaceAsInternal :: Assertion
declareTemplatesInputPlaceAsInternal = Left expectation @=? parseCsl program
  where
    expectation = [DuringValidation . RedeclarationOfPlace $ TemplatePlace (TemplateInstance "delay") "input"]
    program = T.concat
        [ "INSTANCES { delay: delay }"
        , "PLACES { delay.input }"
        , "TEMPLATE delay {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  PLACES { inner }"
        , "  TRANSITION initialize { MATCH { input: X } PRODUCE { inner: X } }"
        , "  TRANSITION forward { MATCH { input: X inner: Y } PRODUCE { inner: X output: Y } }"
        , "}"
        ]

declareTemplatesOutputPlaceAsInternal :: Assertion
declareTemplatesOutputPlaceAsInternal = Left expectation @=? parseCsl program
  where
    expectation = [DuringValidation . RedeclarationOfPlace $ TemplatePlace (TemplateInstance "delay") "output"]
    program = T.concat
        [ "INSTANCES { delay: delay }"
        , "PLACES { delay.output }"
        , "TEMPLATE delay {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  PLACES { inner }"
        , "  TRANSITION initialize { MATCH { input: X } PRODUCE { inner: X } }"
        , "  TRANSITION forward { MATCH { input: X inner: Y } PRODUCE { inner: X output: Y } }"
        , "}"
        ]

declareTemplatesInternalPlaceAsInternal :: Assertion
declareTemplatesInternalPlaceAsInternal = Left expectation @=? parseCsl program
  where
    expectation = [DuringValidation . RedeclarationOfPlace $ TemplatePlace (TemplateInstance "delay") "inner"]
    program = T.concat
        [ "INSTANCES { delay: delay }"
        , "PLACES { delay.inner }"
        , "TEMPLATE delay {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  PLACES { inner }"
        , "  TRANSITION initialize { MATCH { input: X } PRODUCE { inner: X } }"
        , "  TRANSITION forward { MATCH { input: X inner: Y } PRODUCE { inner: X output: Y } }"
        , "}"
        ]


declareAnotherTemplatesInputPlaceInsideInterface :: Assertion
declareAnotherTemplatesInputPlaceInsideInterface = Left expectation @=? parseCsl program
  where
    expectation = [DuringValidation . RedeclarationOfPlace $ TemplatePlace (TemplateInstance "delay") "input"]
    program = T.concat
        [ "TEMPLATE x {"
        , "  INSTANCES { delay: delay }"
        , "  INTERFACE { INPUT { delay.input } }"
        , "}"
        , "TEMPLATE delay {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  PLACES { inner }"
        , "  TRANSITION initialize { MATCH { input: X } PRODUCE { inner: X } }"
        , "  TRANSITION forward { MATCH { input: X inner: Y } PRODUCE { inner: X output: Y } }"
        , "}"
        ]

declareAnotherTemplatesOutputPlaceInsideInterface :: Assertion
declareAnotherTemplatesOutputPlaceInsideInterface = Left expectation @=? parseCsl program
  where
    expectation = [DuringValidation . RedeclarationOfPlace $ TemplatePlace (TemplateInstance "delay") "output"]
    program = T.concat
        [ "TEMPLATE x {"
        , "  INSTANCES { delay: delay }"
        , "  INTERFACE { OUTPUT { delay.output } }"
        , "}"
        , "TEMPLATE delay {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  PLACES { inner }"
        , "  TRANSITION initialize { MATCH { input: X } PRODUCE { inner: X } }"
        , "  TRANSITION forward { MATCH { input: X inner: Y } PRODUCE { inner: X output: Y } }"
        , "}"
        ]

declareAnotherTemplatesInternalPlaceInsideInterface :: Assertion
declareAnotherTemplatesInternalPlaceInsideInterface = Left expectation @=? parseCsl program
  where
    expectation = [DuringValidation . RedeclarationOfPlace $ TemplatePlace (TemplateInstance "delay") "inner"]
    program = T.concat
        [ "TEMPLATE x {"
        , "  INSTANCES { delay: delay }"
        , "  INTERFACE { INPUT { delay.inner } }"
        , "}"
        , "TEMPLATE delay {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  PLACES { inner }"
        , "  TRANSITION initialize { MATCH { input: X } PRODUCE { inner: X } }"
        , "  TRANSITION forward { MATCH { input: X inner: Y } PRODUCE { inner: X output: Y } }"
        , "}"
        ]

declareAnotherTemplatesInputPlaceAsInternal :: Assertion
declareAnotherTemplatesInputPlaceAsInternal = Left expectation @=? parseCsl program
  where
    expectation = [DuringValidation . RedeclarationOfPlace $ TemplatePlace (TemplateInstance "delay") "input"]
    program = T.concat
        [ "TEMPLATE x {"
        , "  INSTANCES { delay: delay }"
        , "  PLACES { delay.input }"
        , "}"
        , "TEMPLATE delay {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  PLACES { inner }"
        , "  TRANSITION initialize { MATCH { input: X } PRODUCE { inner: X } }"
        , "  TRANSITION forward { MATCH { input: X inner: Y } PRODUCE { inner: X output: Y } }"
        , "}"
        ]

declareAnotherTemplatesOutputPlaceAsInternal :: Assertion
declareAnotherTemplatesOutputPlaceAsInternal = Left expectation @=? parseCsl program
  where
    expectation = [DuringValidation . RedeclarationOfPlace $ TemplatePlace (TemplateInstance "delay") "output"]
    program = T.concat
        [ "TEMPLATE x {"
        , "  INSTANCES { delay: delay }"
        , "  PLACES { delay.output }"
        , "}"
        , "TEMPLATE delay {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  PLACES { inner }"
        , "  TRANSITION initialize { MATCH { input: X } PRODUCE { inner: X } }"
        , "  TRANSITION forward { MATCH { input: X inner: Y } PRODUCE { inner: X output: Y } }"
        , "}"
        ]

declareAnotherTemplatesInternalPlaceAsInternal :: Assertion
declareAnotherTemplatesInternalPlaceAsInternal = Left expectation @=? parseCsl program
  where
    expectation = [DuringValidation . RedeclarationOfPlace $ TemplatePlace (TemplateInstance "delay") "inner"]
    program = T.concat
        [ "TEMPLATE x {"
        , "  INSTANCES { delay: delay }"
        , "  PLACES { delay.inner }"
        , "}"
        , "TEMPLATE delay {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  PLACES { inner }"
        , "  TRANSITION initialize { MATCH { input: X } PRODUCE { inner: X } }"
        , "  TRANSITION forward { MATCH { input: X inner: Y } PRODUCE { inner: X output: Y } }"
        , "}"
        ]


markingOfTemplatesInputPlace :: Assertion
markingOfTemplatesInputPlace = Right expectation @=? mainNet <$> parseCsl program
  where
    expectation = emptyNet
        { instances = Map.singleton (TemplateInstance "toBlackToken") (TemplateName "toBlackToken")
        , initialMarking = Map.singleton (TemplatePlace (TemplateInstance "toBlackToken") "input") blackToken
        }
    program = T.concat
        [ "INSTANCES { toBlackToken: toBlackToken }"
        , "MARKING { toBlackToken.input: @ }"
        , "TEMPLATE toBlackToken {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  TRANSITION t { MATCH { input: _ } PRODUCE { output: @ } }"
        , "}"
        ]

markingOfTemplatesOutputPlace :: Assertion
markingOfTemplatesOutputPlace = Left expectation @=? parseCsl program
  where
    expectation = [DuringValidation . TokenOnConsumeOnlyPlace $ TemplatePlace (TemplateInstance "toBlackToken") "output"]
    program = T.concat
        [ "INSTANCES { toBlackToken: toBlackToken }"
        , "MARKING { toBlackToken.output: @ }"
        , "TEMPLATE toBlackToken {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  TRANSITION t { MATCH { input: _ } PRODUCE { output: @ } }"
        , "}"
        ]


markingOfTemplatesInternalPlace :: Assertion
markingOfTemplatesInternalPlace = Left expectation @=? parseCsl program
  where
    expectation = [DuringValidation . TokenOnHiddenPlace $ TemplatePlace (TemplateInstance "delay") "inner"]
    program = T.concat
        [ "INSTANCES { delay: delay }"
        , "MARKING { delay.inner: @ }"
        , "TEMPLATE delay {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  PLACES { inner }"
        , "  TRANSITION initialize { MATCH { input: X } PRODUCE { inner: X } }"
        , "  TRANSITION forward { MATCH { input: X inner: Y } PRODUCE { inner: X output: Y } }"
        , "}"
        ]

markingOfAnotherTemplatesInputPlace :: Assertion
markingOfAnotherTemplatesInputPlace = Right (Just expectation) @=? fmap initialMarking . Map.lookup (TemplateName "createBlackToken") . templates <$> parseCsl program
  where
    expectation = Map.singleton (TemplatePlace (TemplateInstance "toBlackToken") "input") blackToken
    program = T.concat
        [ "TEMPLATE createBlackToken {"
        , "  INSTANCES { toBlackToken: toBlackToken }"
        , "  INTERFACE { OUTPUT { output } }"
        , "  MARKING { toBlackToken.input: @ }"
        , "  TRANSITION call { PRODUCE { toBlackToken.input: @ } }"
        , "  TRANSITION return { MATCH { toBlackToken.output: Token } PRODUCE { output: Token } }"
        , "}"
        , "TEMPLATE toBlackToken {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  TRANSITION t { MATCH { input: _ } PRODUCE { output: @ } }"
        , "}"
        ]

markingOfAnotherTemplatesOutputPlace :: Assertion
markingOfAnotherTemplatesOutputPlace = Left expectation @=? parseCsl program
  where
    expectation = [DuringValidation . TokenOnConsumeOnlyPlace $ TemplatePlace (TemplateInstance "toBlackToken") "output"]
    program = T.concat
        [ "TEMPLATE createBlackToken {"
        , "  INSTANCES { toBlackToken: toBlackToken }"
        , "  INTERFACE { OUTPUT { output } }"
        , "  MARKING { toBlackToken.output: @ }"
        , "  TRANSITION t { MATCH { toBlackToken.output: @ } PRODUCE { output: @ } }"
        , "}"
        , "TEMPLATE toBlackToken {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  TRANSITION t { MATCH { input: _ } PRODUCE { output: @ } }"
        , "}"
        ]

markingOfAnotherTemplatesInternalPlace :: Assertion
markingOfAnotherTemplatesInternalPlace = Left expectation @=? parseCsl program
  where
    expectation = [DuringValidation . TokenOnHiddenPlace $ TemplatePlace (TemplateInstance "delay") "inner"]
    program = T.concat
        [ "TEMPLATE x {"
        , "  INSTANCES { delay: delay }"
        , "  INTERFACE { OUTPUT { output } }"
        , "  MARKING { delay.inner: @ }"
        , "  TRANSITION call { PRODUCE { delay.input: @ } }"
        , "  TRANSITION return { MATCH { delay.output: Token } PRODUCE { output: Token } }"
        , "}"
        , "TEMPLATE delay {"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  PLACES { inner }"
        , "  TRANSITION initialize { MATCH { input: X } PRODUCE { inner: X } }"
        , "  TRANSITION forward { MATCH { input: X inner: Y } PRODUCE { inner: X output: Y } }"
        , "}"
        ]

selfInstantiation :: Assertion
selfInstantiation = Left expectation @=? parseCsl program
  where
    expectation = map (DuringConversion $)
        [ CyclicTemplates [TemplateName "x", TemplateName "x"]
        ]
    program = T.concat
        [ "TEMPLATE x {"
        , "  INSTANCES { y: x }"
        , "  INTERFACE { INPUT { input } OUTPUT { output } }"
        , "  TRANSITION call { MATCH { input: Token } PRODUCE { y.input: Token } }"
        , "  TRANSITION return { MATCH { y.output: Token } PRODUCE { output: Token } }"
        , "}"
        ]

allEncompassingTemplateCycle :: Int -> Property
allEncompassingTemplateCycle cycleLength = cycleLength > 1 ==> either (expectation `elem`) (const False) (parseCsl program)
  where
    expectation = DuringConversion . CyclicTemplates $ map (TemplateName . T.append "template" . T.pack . show) ([0..cycleLength - 1] ++ [0])
    program = T.concat $ map mkTemplate [0..cycleLength - 1]
    mkTemplate k = T.concat
        [ "TEMPLATE template", T.pack $ show k, " {"
        , "  INSTANCES { dependency: template", T.pack . show $ (k + 1) `mod` cycleLength,  "}"
        , "}"
        ]

lassoTemplateCycle :: Int -> Int -> Property
lassoTemplateCycle pathLength cycleLength = pathLength > 0 && cycleLength > 1 ==> either (expectation `elem`) (const False) (parseCsl program)
  where
    expectation = DuringConversion . CyclicTemplates $ path ++ lasso
    path = map (TemplateName . T.append "template" . T.pack . show) [0..pathLength - 1]
    lasso = map (TemplateName . T.append "template" . T.pack . show . (+ pathLength)) ([0..cycleLength - 1] ++ [0])
    program = T.concat $ map mkTemplateInPath [0..pathLength - 1] ++ map mkTemplateInsideLasso [0..cycleLength - 1]
    mkTemplateInPath k = T.concat
        [ "TEMPLATE template", T.pack $ show k, " {"
        , "  INSTANCES { dependency: template", T.pack . show $ (k + 1),  "}"
        , "}"
        ]
    mkTemplateInsideLasso k = T.concat
        [ "TEMPLATE template", T.pack . show $ pathLength + k, " {"
        , "  INSTANCES { dependency: template", T.pack . show . (+ pathLength) $ (k + 1) `mod` cycleLength, "}"
        , "}"
        ]
