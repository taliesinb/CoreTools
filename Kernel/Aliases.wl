SystemExports[
  "MetaFunction", DefineAliasRules, DefinePatternRules
];

PackageExports[
  "IOFunction",
    ReadRawJSONFile, ReadRawJSONStream, WriteRawJSONFile, WriteRawJSONStream, CellInformation,
  "Function",
    ReadRawJSONString, WriteRawJSONString,
  "ScopingFunction",
    InheritedBlock,
  "ObjectHead",
    NBObject,
  "SpecialVariable",
    $Fail,
  "SpecialFunction",
    ConstructValidExpr, ConstructNoEntryExpr, WithTimestampsPreserved, HoldC,
  "MutatingFunction",
    StuffBag, HoldSetNoEntryExpr, HoldSetValidExpr, SetD,
  "TypeHead",
    Str, Int, Sym,
  "DataHead",
    Bag, Dict, Assoc, UDict, UAssoc, UAssociation, UnorderedAssociation, RuleD, InternalData,
  "ControlFlowFunction",
    Seq, Then, Eval, NoEval, TryEval, FailEval, FastQuietCheck, WithLocalSettings,
  "PatternHead",
    HoldP, Regex, Alt,
    VPattern, VCondition, VPatternTest, VBlank, VBlankSeq, VBlankNullSeq, VAlt, VRepeated, VExcept, VVerbatim, VHoldP,
    BlankSeq2, BlankSeq, BlankNullSeq,
  "Symbol",
    Auto, Inf, Tau,
  "PatternSymbol",
    AtomP, DatumP, BoolP, ZeroP, NonZeroP, NonZeroIntP, Zero2P, NatP, Nat2P, PosIntP, PosInt2P,
    NumP, Num2P, Num3P, UnitNumP,
    ExtNumP, ExtNatP, ExtIntP, ExtPosIntP,
    PosNumP, ExtPosNumP, ColorP, SideP, ExtSideP,
    OnePartSpecP, MultiPartSpecP, ExtPartSpecP,
    SymP, FormalSymP,
    SingleP, PairP, ListVecP, AssocVecP, BoolVecP, SymVecP, StrVecP, PairVecP, IntVecP, NatVecP, PosIntVecP, RealVecP, NumVecP, ExtNumVecP,
    RuleP, RuleLikeP, RuleVecP, RuleLikeVecP, RuleSeqP,
    AssocLikeP, ListDictP, AssocP,
    EmptyP, EmptyDataP, EmptyAssoc, EmptyDict,
    UserSymbolP, InertSymbolP, SystemSymbolP,
  "MessageFunction",
    ThrowMsg, ErrorMsg, ReturnMsg,
  "Function",
    PosIndex, RangeAssoc, AssocRange, UAssocRange, RangeUAssoc,
    SeqLen, Seq1, Seq2, Seq3, SeqN, SeqMost, SeqRest, SeqReverse,
    LenRange, RangeLen, ConstAssoc, ConstRules, ConstList, ConstUAssoc, ConstTrueAssoc, PairsToAssoc, AssocToPairs,
    Rep, RepRep, RepAll, VecRep,
    Vals, ToVals, KeysVals, MapVals, MapValsP,
    DelCases, DelNone, DelNull, DelDups,
    Make, Dist,
    Inter, Compl,
    Occs, OccsPos,
    InvertDict, InvertUDict,
    ListDictParts, DictMap, DictThread, UDictThread, DictRange, RangeDict, UDictRange, RangeUDict,
    ConstDict, ConstUDict, ConstTrueDict, PairsToDict, RulesToDict, DictToPairs, DictToRules,
    LevelDict, OccDict, ArgDict, LeafDict, PartDict,
    AssocMap, UAssocThread, AssocMapThread,
    HoldLen, Len, Len2, Dims, Fn, Id, Rev, IntDigits,
    IntStr, HexStr, Chars, FromCharCode, ToCharCode, CharRange,
    Char1, CharN, StrFirst, StrRest, StrMost, StrLast,
    StrCases, StrDelete, StrDrop, StrExpr,
    StrExtract, StrInsert, StrJoin, StrLen, StrPadLeft, StrPadRight, StrPartition, StrPos, StrPosL, StrPosR,
    StrCaseF, StrCaseL, StrSplitPos, StrSplitL, StrSplitR, StrTake, StrTrim, StrTrimL, StrTrimR, StrTrimLR,
    StrSegment, StrSegmentL, StrSegmentR, StrRepeat, StrRep, StrRepPart, StrRev, StrSplit, StrPre, StrApp,
    DecodeBase64, DecodeBase64ToByteArray, EncodeBase64, ToList, ToPackedArray, ToPacked, FromPackedArray, BagPart, BagLength,
    RepetitionFromMultiplicity, Reciprocal, OutermostToInnermost, PackedArrayForm, PackedArrayType, StringHash,
    MapThreadMin, MapThreadMax,
    Select1, SelectIndex1, VectorIndex1, VectorIndex1Of,
    RandInt, RandBit, RandNat, RandBool, RandReal, RandNorm,
  "Predicate",
    AutoQ, InfQ, NotAutoQ, NotInfQ,
    ZeroIntQ, NonZeroIntQ,
    PosQ, NegQ, NonPosQ, NonNegQ, NonPosIntQ, NonNegIntQ,
    StrQ, StrMatchQ, StrDelimQ, StrStartsQ, StrEndsQ, StrFreeQ, StrContainsQ, ASCIIQ,
    BoolQ, IntQ, PosIntQ, NegIntQ, NatQ, NumQ,
    AssocQ, VecQ, AssocVecQ, StrVecQ, PairVecQ, ListVecQ, BoolVecQ, SymVecQ, NatVecQ, IntVecQ, PosIntVecQ, RealVecQ,
    ExprEntryQ, ExprNoEntryQ, ExprValidQ, ExprInvalidQ, HoldExprEntryQ, HoldExprNoEntryQ, HoldExprValidQ, HoldExprInvalidQ, ExprMDataQ, MightEvaluateQ, MaybeFunctionQ, ExprWillNotEvaluateQ, ExprWillNotEvaluateWhenAppliedQ,
    SymbolAnyCodesQ, SymbolAnyEvaluationsQ, SymbolDelayedValueQ, SymbolDownCodeQ, SymbolDownEvaluationsQ, SymbolImmediateValueQ, SymbolNoCodesQ, SymbolNoEvaluationsQ, SymbolOwnEvaluationsQ, SymbolPrintCodeQ, SymbolSubCodeQ, SymbolSubEvaluationsQ, SymbolUpCodeQ, SymbolUpEvaluationsQ,
    Base64StringQ, IntegerPartitionQ,
    AssociationVectorQ, ListOrAssociationQ, StringOrStringVectorQ, StringVectorQ,
    UnsafeEmptyQ, NonEmptyQ, NotEmptyQ
    SymbolQ, HoldSymbolQ, HoldAtomQ, SymQ, HSymQ, HAtomQ,
    RealQ, MachineRealQ, MachineIntegerQ, MachineComplexQ,
    PackedArrayQ, PackedQ,
    NaturalQ, PositiveIntegerQ, NegativeIntegerQ, NonPositiveIntegerQ, NonNegativeIntegerQ,
    PositiveMachineIntegerQ, NegativeMachineIntegerQ, NonPositiveMachineIntegerQ, NonNegativeMachineIntegerQ,
    SyntacticNegativeQ, ComplexPresentQ, ExceptionFreeQ,
    TensorTypeQ,
    PatternPresentQ, PatternFreeQ, VContainsQ, VFreeQ, ValidSymbolNameQ,
    DupFreeQ,
    AssocScanWhileQ, EmptyComplementQ, EmptyIntersectionQ, SameHashQ, NValueQ, FastNumericIndices,
    HAssocQ, HPackedQ,
    DictQ, ListDictQ,
    NonEmptyDictQ, NonEmptyListDictQ,
    DictOfQ, ListDictOfQ,
    NonEmptyDictOfQ, NonEmptyListDictOfQ,
    ContainsDictQ,
    SameLenQ, AllSameLenQ,
    FailedQ, FailQ, NotFailedQ, NotFailQ,
  "PredicateOperator",
    DictOf, ListDictOf,
    NonEmptyDictOf, NonEmptyListDictOf,
  "Function",
    DRowStr, RiffRowStr, SpaceRowStr, CommaRowStr,
  "BoxFunction",
    DRowBox, DRiffRowBox
];

(*************************************************************************************************)

SetAttributes[{DefineAliasRules, DefinePatternRules, defineAlias, definePattern}, HoldAllComplete];

DefineAliasRules[rules___Rule]   := iDefineRules[defineAlias, Hold @ rules];
DefinePatternRules[rules___Rule] := iDefineRules[definePattern, Hold @ rules];

iDefineRules[fn_, held_] := With[
  {syms = Part[held, All, 1]},
  Prelude`Packages`$SymbolAliasesDirty = True;
  UnprotectClearAll @@ syms;
  Scan[fn, held];
  Protect @@ syms;
  invalidateMacroRules[];
];

defineAlias[aliasSym_Symbol -> targetSym_Symbol] := (
  $SymbolAliases[aliasSym] = targetSym;
  Set[aliasSym, targetSym];
);

definePattern[patternSym_Symbol -> rhs_] := (
  $SymbolAliases[patternSym] = rhs;
  Set[patternSym, rhs];
);

DefineAliasRules::notValidRule = "Expected rule mapping symbol to symbol: ``.";
DefinePatternRules::notValidRule = "Expected rule mapping symbol to pattern: ``.";

defineAlias[e_]      := (Message[DefineAliasRules::notValidRule,   HoldForm @ e]; $Failed)
definePattern[e_]    := (Message[DefinePatternRules::notValidRule, HoldForm @ e]; $Failed)
e_DefineAliasRules   := (Message[DefineAliasRules::notValidRule,   HoldForm @ e]; $Failed)
e_DefinePatternRules := (Message[DefinePatternRules::notValidRule, HoldForm @ e]; $Failed)

(*************************************************************************************************)

DefinePatternRules[
  VPattern       -> Verbatim[Pattern],
  VCondition     -> Verbatim[Condition],
  VPatternTest   -> Verbatim[PatternTest],
  VBlank         -> Verbatim[Blank],
  VBlankSeq      -> Verbatim[BlankSequence],
  VBlankNullSeq  -> Verbatim[BlankNullSequence],
  VAlt           -> Verbatim[Alternatives],
  VRepeated      -> Verbatim[Repeated],
  VExcept        -> Verbatim[Except],
  VVerbatim      -> Verbatim[Verbatim],
  VHoldP         -> Verbatim[HoldPattern],
  BlankSeq2      -> Repeated[_, {2, Infinity}]
];

DefinePatternRules[
  AtomP          -> HoldPattern[Except[_Association] ? Developer`HoldAtomQ],
  DatumP         -> Alternatives[False, True, None, Null, _String, _Integer, _Real, _Rational] ? Developer`HoldAtomQ,
  BoolP          -> False | True,
  ColorP         -> Alternatives[_RGBColor, _GrayLevel, _CMYKColor, _Hue, _XYZColor, _LABColor, _LCHColor, _LUVColor, Opacity[_, _]],
  SideP          -> Left | Right | Bottom | Top,
  ExtSideP       -> Left | Right | Bottom | Top | BottomLeft | BottomRight | TopLeft | TopRight,
  OnePartSpecP   -> _Integer | _Key | _String,
  MultiPartSpecP -> (_Span | All | _List) ? MultiPartSpecQ,
  ExtPartSpecP   -> (_Integer | _Key | _String | _Span | All | _List) ? ExtPartSpecQ,
  SymP           -> _Symbol ? Developer`HoldSymbolQ,
  FormalSymP     -> _Symbol ? HoldFormalSymbolQ,
  AssocP         -> _Association ? Developer`HoldAtomQ,
  UserSymbolP    -> _Symbol ? UserSymbolQ,
  InertSymbolP   -> _Symbol ? InertSymbolQ,
  SystemSymbolP  -> _Symbol ? SystemSymbolQ
];

With[
  {emptyAssoc = Association[], dirInf = DirectedInfinity[1], tau = 2. * Pi},
  DefinePatternRules[
    EmptyP     -> _[],
    EmptyDataP -> Alternatives[{}, emptyAssoc],
    EmptyAssoc -> emptyAssoc,
    EmptyDict  -> emptyAssoc,
    Inf        -> dirInf,
    Tau        -> tau
  ]
];

DefinePatternRules[
  ZeroP          -> 0 | 0.,
  NonZeroP       -> Except[0 | 0., _Integer | _Real | _Rational],
  NonZeroIntP    -> Except[0, _Integer],
  Zero2P         -> {0 | 0., 0 | 0.},
  NatP           -> _Integer ? NonNegative,
  Nat2P          -> {_Integer ? NonNegative, _Integer ? NonNegative},
  PosIntP        -> _Integer ? Positive,
  PosInt2P       -> {_Integer ? Positive, _Integer ? Positive},
  NumP           -> (_Integer | _Real | _Rational),
  PosNumP        -> (_Integer | _Real | _Rational) ? Positive,
  UnitNumP       -> (_Integer | _Real | _Rational) ? UnitNumberQ,
  ExtNumP        -> (_Integer | _Real | _Rational | _DirectedInfinity),
  ExtNatP        -> (_Integer ? NonNegative | _DirectedInfinity),
  ExtIntP        -> _Integer | _DirectedInfinity | _DirectedInfinity,
  ExtPosIntP     -> (_Integer ? Positive | _DirectedInfinity),
  ExtPosNumP     -> (_Integer | _Real | _Rational | _DirectedInfinity) ? Positive,
  Num2P          -> {_ ? NumberQ, _ ? NumberQ},
  Num3P          -> {_ ? NumberQ, _ ? NumberQ, _ ? NumberQ}
];

DefinePatternRules[
  SingleP        -> {_},
  PairP          -> {_, _},
  ListVecP       -> _List ? ListVectorQ,
  AssocVecP      -> _List ? AssociationVectorQ,
  BoolVecP       -> _List ? BooleanVectorQ,
  SymVecP        -> {___Symbol},
  StrVecP        -> _List ? StringVectorQ,
  PairVecP       -> _List ? PairVectorQ,
  IntVecP        -> _List ? IntegerVectorQ,
  NatVecP        -> _List ? NaturalVectorQ,
  PosIntVecP     -> _List ? PositiveIntegerVectorQ,
  RealVecP       -> _List ? RealVectorQ,
  NumVecP        -> _List ? NumberVectorQ,
  ExtNumVecP     -> _List ? ExtendedNumberVectorQ
];

DefinePatternRules[
  RuleP          -> _Rule,
  RuleLikeP      -> (_Rule | _RuleDelayed),
  RuleVecP       -> (_List ? RuleVectorQ),
  RuleLikeVecP   -> {(_Rule | _RuleDelayed)...},
  RuleSeqP       -> Alt[__Rule, {__Rule}],
  AssocLikeP     -> (_Association ? AssociationQ) | {(_Rule | _RuleDelayed)...},
  ListDictP      -> _List | (_Association ? Developer`HoldAtomQ)
];

DefineAliasRules[
  DictQ                         -> AssociationQ,
  ListDictQ                     -> Developer`ListOrAssociationQ,
  NonEmptyDictQ                 -> NonEmptyAssociationQ,
  NonEmptyListDictQ             -> NonEmptyListAssociationQ,
  DictOfQ                       -> AssociationOfQ,
  ListDictOfQ                   -> ListAssociationOfQ,
  NonEmptyDictOfQ               -> NonEmptyAssociationOfQ,
  NonEmptyListDictOfQ           -> NonEmptyListAssociationOfQ,
  ContainsDictQ                 -> ContainsAssociationQ,
  SameLenQ                      -> SameLengthQ,
  AllSameLenQ                   -> AllSameLengthQ,
  FailedQ                       -> FailureQ,
  FailQ                         -> FailureQ,
  NotFailedQ                    -> NotFailureQ,
  NotFailQ                      -> NotFailureQ
];

DefineAliasRules[
  DictOf                        -> AssociationOf,
  ListDictOf                    -> ListAssociationOf,
  NonEmptyDictOf                -> NonEmptyAssociationOf,
  NonEmptyListDictOf            -> NonEmptyListAssociationOf
];

DefineAliasRules[
  AutoQ                         -> AutomaticQ,
  NotAutoQ                      -> NotAutomaticQ,
  InfQ                          -> InfinityQ,
  NotInfQ                       -> NotInfinityQ,
  StrQ                          -> StringQ,
  StrMatchQ                     -> StringMatchQ,
  StrDelimQ                     -> StringStartsEndsQ,
  StrStartsQ                    -> StringStartsQ,
  StrEndsQ                      -> StringEndsQ,
  StrFreeQ                      -> StringFreeQ,
  StrContainsQ                  -> StringContainsQ,
  ASCIIQ                        -> PrintableASCIIQ
];

DefineAliasRules[
  BoolQ                         -> BooleanQ,
  IntQ                          -> IntegerQ,
  ZeroIntQ                      -> ZeroIntegerQ,
  NonZeroIntQ                   -> NonZeroIntegerQ,
  PosQ                          -> Positive,
  NegQ                          -> Negative,
  NonPosQ                       -> NonPositive,
  NonNegQ                       -> NonNegative,
  NonPosIntQ                    -> Internal`NonPositiveIntegerQ,
  NonNegIntQ                    -> Internal`NonNegativeIntegerQ,
  PosIntQ                       -> Internal`PositiveIntegerQ,
  NegIntQ                       -> Internal`NegativeIntegerQ,
  NatQ                          -> Internal`NonNegativeIntegerQ,
  NumQ                          -> RealValuedNumberQ
];

DefineAliasRules[
  AssocQ                        -> AssociationQ,
  VecQ                          -> VectorQ,
  AssocVecQ                     -> Developer`AssociationVectorQ,
  StrVecQ                       -> Developer`StringVectorQ,
  PairVecQ                      -> PairVectorQ,
  ListVecQ                      -> ListVectorQ,
  BoolVecQ                      -> BooleanVectorQ,
  SymVecQ                       -> SymbolVectorQ,
  NatVecQ                       -> NaturalVectorQ,
  IntVecQ                       -> IntegerVectorQ,
  PosIntVecQ                    -> PositiveIntegerVectorQ,
  RealVecQ                      -> RealVectorQ
];

DefineAliasRules[
  DupFreeQ                      -> DuplicateFreeQ,
  AssocScanWhileQ               -> Association`ScanWhile,
  EmptyComplementQ              -> Language`EmptyComplementQ,
  EmptyIntersectionQ            -> Language`EmptyIntersectionQ,
  HAssocQ                       -> HoldAssociationQ,
  HPackedQ                      -> HoldPackedArrayQ
];

(* Developer` predicates *)
DefineAliasRules[
  AssociationVectorQ            -> Developer`AssociationVectorQ,
  ListOrAssociationQ            -> Developer`ListOrAssociationQ,
  StringOrStringVectorQ         -> Developer`StringOrStringVectorQ,
  StringVectorQ                 -> Developer`StringVectorQ,
  UnsafeEmptyQ                  -> Developer`EmptyQ, (* atoms count as not EmptyQ somehow, we define our own EmptyQ*)
  NotEmptyQ                     -> Developer`NotEmptyQ,
  NonEmptyQ                     -> Developer`NotEmptyQ,

  SymbolQ                       -> Developer`SymbolQ,
  HoldSymbolQ                   -> Developer`HoldSymbolQ,
  HoldAtomQ                     -> Developer`HoldAtomQ,
  SymQ                          -> Developer`SymbolQ,
  HSymQ                         -> Developer`HoldSymbolQ,
  HAtomQ                        -> Developer`HoldAtomQ,

  RealQ                         -> Developer`RealQ,
  MachineRealQ                  -> Developer`MachineRealQ,
  MachineIntegerQ               -> Developer`MachineIntegerQ,
  MachineComplexQ               -> Developer`MachineComplexQ
]

(* JoinOrFail issues this message, but it isn't defined *)
Join::nonlist = "`` is not a list.";

(* Internal` predicates *)
DefineAliasRules[
  ListJoin                      -> Internal`JoinOrFail,
  NaturalQ                      -> Internal`NonNegativeIntegerQ,
  PositiveIntegerQ              -> Internal`PositiveIntegerQ,
  NegativeIntegerQ              -> Internal`NegativeIntegerQ,
  NonPositiveIntegerQ           -> Internal`NonPositiveIntegerQ,
  NonNegativeIntegerQ           -> Internal`NonNegativeIntegerQ,
  PositiveMachineIntegerQ       -> Internal`PositiveMachineIntegerQ,
  NegativeMachineIntegerQ       -> Internal`NegativeMachineIntegerQ,
  NonPositiveMachineIntegerQ    -> Internal`NonPositiveMachineIntegerQ,
  NonNegativeMachineIntegerQ    -> Internal`NonNegativeMachineIntegerQ,
  SyntacticNegativeQ            -> Internal`SyntacticNegativeQ,
  ComplexPresentQ               -> Internal`HasComplex,
  ExceptionFreeQ                -> Internal`ExceptionFreeQ,
  IntegerPartitionQ             -> Internal`IntegerPartitionQ,
  TensorTypeQ                   -> Internal`TensorTypeQ,
  PatternPresentQ               -> Internal`PatternPresentQ,
  PatternFreeQ                  -> Internal`PatternFreeQ,
  VFreeQ                        -> Internal`LiterallyAbsentQ,
  VContainsQ                    -> Internal`LiterallyOccurringQ,
  ValidSymbolNameQ              -> Internal`SymbolNameQ,
  SameHashQ                     -> Internal`SameHashQ
];

(* System`Private` expression predicates *)
DefineAliasRules[
  ExprEntryQ                    -> System`Private`EntryQ,
  ExprNoEntryQ                  -> System`Private`NoEntryQ,
  ExprValidQ                    -> System`Private`ValidQ,
  ExprInvalidQ                  -> System`Private`NotValidQ,
  HoldExprEntryQ                -> System`Private`HoldEntryQ,
  HoldExprNoEntryQ              -> System`Private`HoldNoEntryQ,
  HoldExprValidQ                -> System`Private`HoldValidQ,
  HoldExprInvalidQ              -> System`Private`HoldNotValidQ,
  ExprMDataQ                    -> System`Private`MDataQ,
  MightEvaluateQ                -> System`Private`MightEvaluateQ,
  MaybeFunctionQ                -> System`Private`MightEvaluateWhenAppliedQ
];

(* System`Private` symbol predicates *)
DefineAliasRules[
  SymbolAnyCodesQ               -> System`Private`HasAnyCodesQ,
  SymbolAnyEvaluationsQ         -> System`Private`HasAnyEvaluationsQ,
  SymbolDelayedValueQ           -> System`Private`HasDelayedValueQ,
  SymbolDownCodeQ               -> System`Private`HasDownCodeQ,
  SymbolDownEvaluationsQ        -> System`Private`HasDownEvaluationsQ,
  SymbolImmediateValueQ         -> System`Private`HasImmediateValueQ,
  SymbolNoCodesQ                -> System`Private`HasNoCodesQ,
  SymbolNoEvaluationsQ          -> System`Private`HasNoEvaluationsQ,
  SymbolOwnEvaluationsQ         -> System`Private`HasOwnEvaluationsQ,
  SymbolPrintCodeQ              -> System`Private`HasPrintCodeQ,
  SymbolSubCodeQ                -> System`Private`HasSubCodeQ,
  SymbolSubEvaluationsQ         -> System`Private`HasSubEvaluationsQ,
  SymbolUpCodeQ                 -> System`Private`HasUpCodeQ,
  SymbolUpEvaluationsQ          -> System`Private`HasUpEvaluationsQ
];

DefineAliasRules[
  NBObject                      -> NotebookObject
];

(*************************************************************************************************)

DefineAliasRules[
  Sym            -> Symbol,
  Dict           -> Association,
  Assoc          -> Association,
  Str            -> String,
  Int            -> Integer,
  RuleD          -> RuleDelayed
];

(*************************************************************************************************)

DefineAliasRules[
  Regex          -> RegularExpression,
  StrExpr        -> StringExpression
];

(*************************************************************************************************)

DefineAliasRules[
  UnorderedAssociation  -> Data`UnorderedAssociation,
  UAssociation          -> Data`UnorderedAssociation,
  UAssoc                -> Data`UnorderedAssociation,
  UDict                 -> Data`UnorderedAssociation
];

(*************************************************************************************************)

DefineAliasRules[
  Alt            -> Alternatives,
  HoldP          -> HoldPattern,
  BlankSeq       -> BlankSequence,
  BlankNullSeq   -> BlankNullSequence
];

(*************************************************************************************************)

DefineAliasRules[
  Auto           -> Automatic,
  SetD           -> SetDelayed
];

(*************************************************************************************************)

DefineAliasRules[
  Eval           -> Evaluate,
  NoEval         -> Unevaluated,
  TryEval        -> RuleCondition,
  FailEval       -> Fail,
  Seq            -> Sequence,
  Then           -> CompoundExpression,
  HoldC          -> HoldComplete,
  $Fail          -> $Failed
];

(*************************************************************************************************)

DefineAliasRules[
  Inter                         -> Intersection,
  Compl                         -> Complement
];

(*************************************************************************************************)

DefineAliasRules[
  InvertDict     -> InvertAssociation,
  InvertUDict    -> InvertUnorderedAssociation,

  ListDictParts  -> ListAssociationParts,
  DictMap        -> AssociationMap,
  DictThread     -> AssocThread,
  UDictThread    -> UnorderedAssociationThread,

  RangeDict      -> RangeAssociation,
  RangeUDict     -> RangeUnorderedAssociation,
  UDictRange     -> UnorderedAssociationRange,
  DictRange      -> AssociationRange,

  ConstDict      -> ConstantAssociation,
  ConstUDict     -> ConstantUnorderedAssociation,
  ConstTrueDict  -> ConstantTrueAssociation,

  PairsToDict    -> PairsToAssociation,
  RulesToDict    -> RulesToAssociation,
  DictToPairs    -> AssociationToPairs,
  DictToRules    -> AssociationToRules,

  LevelDict      -> LevelAssociation,
  OccDict        -> OccurenceAssociation,
  ArgDict        -> ArgumentAssociation,
  LeafDict       -> LeafAssociation,
  PartDict       -> PartAssociation
];

DefineAliasRules[
  Occs           -> Occurences,
  OccsPos        -> OccurencePositions
];

DefineAliasRules[
  Vals           -> Values,
  ToVals         -> ToValues,
  KeysVals       -> KeysValues,
  MapVals        -> MapValues,
  MapValsP       -> MapValuesP,

  PosIndex       -> PositionIndex,

  AssocMap       -> AssociationMap,
  UAssocThread   -> UnorderedAssociationThread,
  AssocMapThread -> AssociationMapThread,

  RangeAssoc     -> RangeAssociation,

  AssocRange     -> AssociationRange,
  UAssocRange    -> UnorderedAssociationRange,

  ConstAssoc     -> ConstantAssociation,
  ConstUAssoc    -> ConstantUnorderedAssociation,
  ConstTrueAssoc -> ConstantTrueAssociation,

  ConstRules     -> ConstantRules,
  ConstList      -> ConstantArray,

  PairsToAssoc   -> PairsToAssociation,
  AssocToPairs   -> AssociationToPairs
];

(*************************************************************************************************)

DefineAliasRules[
  Rep            -> Replace,
  RepRep         -> ReplaceRepeated,
  RepAll         -> ReplaceAll,
  VecRep         -> VectorReplace
];

DefineAliasRules[
  Select1        -> SelectFirst,
  SelectIndex1   -> SelectFirstIndex,
  VectorIndex1   -> FirstVectorIndex,
  VectorIndex1Of -> FirstVectorIndexOf,
  DelCases       -> DeleteCases,
  DelNone        -> DeleteNone,
  DelNull        -> DeleteNull,
  DelDups        -> DeleteDuplicates,
  LenRange       -> LengthRange,
  RangeLen       -> LengthRange
];

DefineAliasRules[
  Make           -> Construct,
  (* TODO: remove these *)
  HoldLen        -> HoldLength,
  Len            -> Length,
  Len2           -> Length2,
  Dims           -> Dimensions
];

DefineAliasRules[
  SeqLen         -> SequenceLength,
  Seq1           -> SequenceFirst,
  Seq2           -> SequenceSecond,
  Seq3           -> SequenceThird,
  SeqN           -> SequenceLast,
  SeqMost        -> SequenceMost,
  SeqRest        -> SequenceRest,
  SeqReverse     -> SequenceReverse
];

DefineAliasRules[
  Id             -> Identity,
  Rev            -> Reverse,
  IntDigits      -> IntegerDigits,
  IntStr         -> IntegerString,
  HexStr         -> HexString,
  Chars          -> Characters,
  FromCharCode   -> FromCharacterCode,
  ToCharCode     -> ToCharacterCode,
  CharRange      -> CharacterRange,
  Dist           -> EuclideanDistance
];

DefineAliasRules[
  Char1          -> StringFirst,
  CharN          -> StringLast,
  StrFirst       -> StringFirst,
  StrRest        -> StringRest,
  StrMost        -> StringMost,
  StrLast        -> StringLast,
  StrFirstRest   -> StringFirstRest,
  StrMostLast    -> StringMostLast,
  StrCases       -> StringCases,
  StrDelete      -> StringDelete,
  StrDrop        -> StringDrop,
  StrInsert      -> StringInsert,
  StrJoin        -> StringJoin,
  StrLen         -> StringLength,
  StrPadLeft     -> StringPadLeft,
  StrPadRight    -> StringPadRight,
  StrPos         -> StringPosition,
  StrPosL        -> StringPositionLeft,
  StrPosR        -> StringPositionRight,
  StrCaseF       -> StringCaseFirst,
  StrCaseL       -> StringCaseLast,
  StrSplitPos    -> StringSplitPositions,
  StrSplitL      -> StringSplitBefore,
  StrSplitR      -> StringSplitAfter,
  StrTake        -> StringTake,
  StrTrim        -> StringTrim,
  StrTrimL       -> StringTrimLeft,
  StrTrimR       -> StringTrimRight,
  StrTrimLR      -> StringTrimLeftRight,
  StrSegment     -> StringSegment,
  StrSegmentL    -> StringSegmentBefore,
  StrSegmentR    -> StringSegmentAfter,
  (* StrRepeat      -> StringRepeat, *)
  StrRep         -> StringReplace,
  StrRepPart     -> StringReplacePart,
  StrRev         -> StringReverse,
  StrSplit       -> StringSplit,
  StrPre         -> StringPrepend,
  StrApp         -> StringAppend
];

DefineAliasRules[
  RandInt        -> RandomInt,
  RandBit        -> RandomBit,
  RandNat        -> RandomNatural,
  RandBool       -> RandomBoolean,
  RandReal       -> RandomUnitReal,
  RandNorm       -> RandomUnitNormal
];

DefineAliasRules[
  InternalData                  -> System`Private`InternalData
];

(* System`Private` flag manipulation *)
DefineAliasRules[
  ConstructValidExpr            -> System`Private`ConstructValid,
  ConstructNoEntryExpr          -> System`Private`ConstructNoEntry,
  HoldSetNoEntryExpr            -> System`Private`HoldSetNoEntry,
  HoldSetValidExpr              -> System`Private`HoldSetValid
];

ConstructValidExpr[head_, args___] := System`Private`HoldSetValid[head[args]];

(* Developer` base64 utilities *)
DefineAliasRules[
  ReadRawJSONFile               -> Developer`ReadRawJSONFile,
  ReadRawJSONStream             -> Developer`ReadRawJSONStream,
  ReadRawJSONString             -> Developer`ReadRawJSONString,
  WriteRawJSONString            -> Developer`WriteRawJSONString,
  WriteRawJSONFile              -> Developer`WriteRawJSONFile,
  WriteRawJSONStream            -> Developer`WriteRawJSONStream,
  CellInformation               -> Developer`CellInformation,
  DecodeBase64                  -> Developer`DecodeBase64,
  EncodeBase64                  -> Developer`EncodeBase64,
  DecodeBase64ToByteArray       -> Developer`DecodeBase64ToByteArray,
  Base64StringQ                 -> Developer`Base64StringQ
];

(* Developer` PackedArray utilities *)
DefineAliasRules[
  ToList                        -> Developer`ToList,
  ToPackedArray                 -> Developer`ToPackedArray,
  ToPacked                      -> Developer`ToPackedArray,
  FromPackedArray               -> Developer`FromPackedArray,
  PackedQ                       -> Developer`PackedArrayQ,
  PackedArrayQ                  -> Developer`PackedArrayQ,
  PackedArrayType               -> Internal`PackedArrayType
];

(* data structures *)
DefineAliasRules[
  StringHash                    -> Data`StringHash,
  BagPart                       -> Internal`BagPart,
  Bag                           -> Internal`Bag,
  BagLength                     -> Internal`BagLength,
  StuffBag                      -> Internal`StuffBag
];

(* Internal` functions *)
DefineAliasRules[
  FastQuietCheck                -> Internal`UnsafeQuietCheck,
  WithLocalSettings             -> Internal`WithLocalSettings,
  WithTimestampsPreserved       -> Internal`WithTimestampsPreserved,
  InheritedBlock                -> Internal`InheritedBlock,
  RepetitionFromMultiplicity    -> Internal`RepetitionFromMultiplicity,
  Reciprocal                    -> Internal`Reciprocal,
  OutermostToInnermost          -> Internal`OutermostToInnermost
];


(* Internal` functions *)
DefineAliasRules[
  MapThreadMin                  -> Random`Private`MapThreadMin,
  MapThreadMax                  -> Random`Private`MapThreadMax,
  FastNumericIndices            -> Random`Private`PositionsOf
];

DefineAliasRules[
  ThrowMsg                      -> ThrowErrorMessage,
  ErrorMsg                      -> ErrorMessage,
  ReturnMsg                     -> ReturnMessage
];