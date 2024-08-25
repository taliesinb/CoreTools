SystemExports[
  "MetaFunction", DefineAliasRules, DefineLiteralRules, DefinePatternRules
];

PackageExports[

  "IOFunction",
    ReadRawJSONFile, ReadRawJSONStream, WriteRawJSONFile, WriteRawJSONStream, CellInformation,

  "Function",
    ReadRawJSONString, WriteRawJSONString,

  "ScopingFunction",
    InheritedBlock, IBlock,

  "ObjectHead",
    NBObject,

  "OptionSymbol",
    VertexColorFn,

  "SpecialVariable",
    $Fail,

  "SpecialFunction",
    ConstructValidExpr, ConstructNoEntryExpr, WithTimestampsPreserved, HoldC, HoldComp, HoldSetNoEntryExpr, HoldSetValidExpr,
    MakeValid, MakeSealed,

  "MutatingFunction",
    StuffBag, SetD, TagSetD, UpSetD, AssocTo, SubFrom,

  "TypeHead",
    Int, Rat, Sym, Str,

  "DataHead",
    Dict, UDict, ODict,
    UnorderedAssociation,
    RuleD, InternalData,
    DEdge, UEdge,
    DirInf,
    Bag,
    PackedTree,
    USet, OSet, MSet,

  "Function",
    AbsDelta,
    ToPackedTree,
    DimsTree,
    DimsProd,
    FlatProd,

  "Function",
    SplitLengths, Multisets, MultiSubsets, ReplaceSuccessive, SublistPosition,

  "ControlFlowFunction",
    Fn, Seq, Then, Eval, NoEval, RuleEval, MaybeEval, FailEval, FastQuietCheck, WithLocalSettings,
    EvalMap,

  "PatternHead",
    HoldP, Alt,
    VPattern, VCondition, VPatternTest, VBlank, VBlankSeq, VBlankNullSeq, VAlt, VRepeated, VExcept, VVerbatim, VHoldP,
    VFn, VFunction, VSet, VSetD, VTagSetD, VRule, VRuleD,
    Blank1, Blank2, Blank3, Blank01, Blank02, Blank12, Blank13, Blank23, BlankSeq2, BlankSeq3, BlankNullSeq, BlankSeq, Opt,

  "Function",
    SymName, SymContext, SymPath,

  "Symbol",
    EmptyDict, EmptyUDict, EmptyODict,
    Auto, Inherit,
    HInf, Inf, PosInf, NegInf,
    Tau,  Root2,  Root3,
    NTau, NRoot2, NRoot3, NPi,

  "StrPatternSymbol",
    Dots, CDots, VDots, LAssoc, RAssoc, Newline,

  "StrPatternHead",
    StrExpr, Regex,

  "PatternSymbol",
    SingleP, PairP, TripleP,
    List1P, List2P, List3P, List4P, List23P,
    DictP, UDictP, ODictP, ListDictP, EmptyP, NonEmptyP, EmptyDataP, AtomP, BoolP, SymP, SymStrP,
    DatumP, ColorP, SideP, ExtSideP, PosDollarP, DollarP, AnonFnP,
    FormalSymP, UserSymP, InertSymP, SystemSymP,

    PiP, TauP, InfP,
    ZeroIntP, ZeroRealP, ZeroNumP, NumE,
    IntP, NatP, RealP, NumP,
    ZeroIntP, ZeroRealP, ZeroNumP,
    NonZeroIntP, NonZeroNumP, NonZeroRealP,
    PosIntP, NegIntP, NonPosIntP, NonNegIntP,
    PosRealP, NegRealP, NonPosRealP, NonNegRealP,
    PosNumP, NegNumP, NonPosNumP, NonNegNumP,
    ExtIntP, ExtRealP, ExtNumP, ExtNatP, ExtPosIntP, ExtPosRealP, ExtNonNegRealP, ExtPosNumP, ExtNonNegNumP,
    UnitIntP, UnitRealP, UnitNumP,
    ZeroP, NonZeroP,

    Zero2P, PosInt2P, Nat2P, Num2P, Num3P, Num23P,
    Pos2P, Pos2ListP, Pos2ListsP, Pos2PairP,
    Pos3P, Pos3ListP, Pos3ListsP, Pos3PairP,
    PosAP, PosAListP, PosAListsP, PosAPairP,

    ListVecP, DictVecP, BoolVecP, SymVecP, StrVecP, PairVecP, IntVecP, NatVecP, PosIntVecP, RealVecP, NumVecP, ExtNumVecP,
    NEListVecP, NEDictVecP, NEBoolVecP, NESymVecP, NEStrVecP, NEPairVecP, NEIntVecP, NENatVecP, NEPosIntVecP, NERealVecP, NENumVecP,
    NEListP, NEDictP, NEListDictP,

    RuleP,    RuleLP,    ORuleP     SetLP,    DelayP,
    RuleSeqP, RuleLSeqP, ORuleSeqP, SetLSeqP, DelaySeqP,
    RuleVecP, RuleLVecP, ORuleVecP, SetLVecP, DelayVecP,
    RuleLSymP, SetLSymP, DelaySymP,
    DictLikeP, DictLP,

  "SlotSymbol",
    FmA, FmB, FmC, FmD, FmE, FmF, FmG, FmH, FmI, FmJ, FmK, FmL, FmM, FmN, FmO, FmP, FmQ, FmR, FmS, FmT, FmU, FmV, FmW, FmX, FmY, FmZ,

  "SlotVariable",
    $, $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $$, $LHS, $RHS,

  "MessageFunction",
    ThrowMsg, ErrorMsg, ReturnMsg,

  "GraphicsDirective",
    APointSize, AThickness, ADashing,

  "Function",
    PosIndex,
    SeqLen,
    Seq0, Seq1, Seq2, Seq3, SeqN, SeqMost, SeqRest, SeqReverse,
    Seq12, Seq21,

    LenRange, RangeLen, ConstRules, ConstList,
    ToRowVec, ToColVec,

    Rep,  RepRep, RepAll,   VecRep,
    Vals, ToVals, KeysVals, MapVals, MapValsP,
    MapF, MapL, MapM, MapR,

    DelCases, DelNone, DelNull, DelDups, DelDupsBy,
    At, Make,
    MakeSet, MakeSetD, MakeTagSetD, MakeUpSetD,
    Dist,
    Inter, Compl,
    Occs, OccsPos,
    ListDictParts,
    LevelDict, OccDict, ArgDict, LeafDict, PartDict,

     DictSum,  DictPlus,  InvertDict,  DictRange,  RangeDict,  ConstDict,  PairsToDict,  RulesToDict, DictToPairs, DictToRules,
    UDictSum, UDictPlus, InvertUDict, UDictRange, RangeUDict, ConstUDict, PairsToUDict, RulesToUDict, TrueDict, FalseDict,
     DictMap,  DictMapApply,  DictMapThread,  DictThread,
    UDictMap, UDictMapApply, UDictMapThread, UDictThread,
    UCounts,

    HoldLen, HoldSeqLen, Len, Len2, LenN, Dims, DimN, Id, Rev, IntDigits,
    SafeIntegerString,
    IntStr, NatStr, HexStr, Chars, FromCharCode, ToCharCode, CharRange,
    Char1, CharN, StrFirst, StrRest, StrMost, StrLast, StrFirstRest, StrMostLast,
    StrCases, StrDelete, StrDrop,
    StrExtract, StrInsert, StrJoin, StrLen, StrPadLeft, StrPadRight, StrPartition, StrPos, StrPosL, StrPosR,
    StrCaseF, StrCaseL, StrSelect, StrDiscard, StrSelectDiscard,
    StrSplitPos, StrSplitL, StrSplitR, StrTake, StrTrim, StrTrimL, StrTrimR, StrTrimLR,
    StrSegment, StrSegmentL, StrSegmentR, StrRepeat, StrRep, StrRepPart, StrRev, StrSplit, StrPre, StrApp,

    DecodeBase64, DecodeBase64ToByteArray, EncodeBase64,
    ToList, ToPackedArray, ToPacked, FromPackedArray,
    PackedArrayForm, PackedArrayType,

    BagPart, BagLength,
    ContainsListRepresentationQ, RepetitionFromMultiplicity, Reciprocal,
    OutermostToInnermost, InnermostToOutermost, TransposeInPlace, ListJoin,

    StringHash,
    MapThreadMin, MapThreadMax,

    Parts,
    Select1, SelectIndex1, VectorIndex1, VectorIndex1Of,
    RandSym,  RandLet,   RandDec,
    RandAtom, RandDatum, RandBit, RandNat, RandBool, RandSign,
    RandRange, RandReal, RandNorm,

    FastNumericIndices,
    ContainedSymbols,

    ToAltPat, ToLHSPat, PatHeadSym, DefHeadSym,
    ToBlankSeq, ToBlankNullSeq,

  "MutatingFunction",
    UnpackDict, PackDict, SetInherit,

  "Predicate",
    SymbolQ, RealQ, NaturalQ, PositiveIntegerQ, NegativeIntegerQ, NonPositiveIntegerQ, NonNegativeIntegerQ, PositiveMachineIntegerQ, NegativeMachineIntegerQ, NonPositiveMachineIntegerQ, NonNegativeMachineIntegerQ,
    PackedQ, AssociationVectorQ, ListOrAssociationQ, StringOrVectorQ, StringVectorQ, UnsafeEmptyQ, NotEmptyQ, NonEmptyQ,
    HoldSymbolQ, HoldAtomQ,

    StrQ, SymQ, IntQ, NatQ, NumQ, BoolQ, DictQ, UDictQ, ODictQ, ListDictQ,
    HStrQ, HSymQ, HIntQ, HNatQ, HNumQ, HBoolQ, HAtomQ, HEmptyQ, HNotEmptyQ, HDictQ, HUDictQ, HODictQ, HPackedQ,
    AutoQ, InheritQ, NotAutoQ, InfQ, NotInfQ,
    ORuleQ, RuleLQ, RuleDQ,

    ZeroQ, NonZeroQ,
    ZeroIntQ, ZeroNumQ, NonZeroIntQ, NonZeroNumQ,
    PosIntQ, NegIntQ, PosRealQ, NegRealQ, PosNumQ, NegNumQ,
    NonPosIntQ, NonNegIntQ, NonPosRealQ, NonNegRealQ, NonPosNumQ, NonNegNumQ,
    UnitIntQ, UnitNumQ,
    ExtIntQ, ExtNatQ, ExtNumQ,
    ExtPosRealQ, ExtPosNumQ, ExtNonNegRealQ, ExtNonNegNumQ,

    FailedQ, FailQ, NotFailedQ, NotFailQ,
    DictOfQ, ListDictOfQ,
    AllSameLenQ, SameOKeysQ, AllSameOKeysQ,

    NEDictQ, NEDictOfQ, NEListQ, NEListOfQ, NEListDictQ, NEListDictOfQ,
    NEAllSameQ, NEAllSameByQ, NEAllSameLenQ, NEAllSameHeadsQ, NEAllSamePartsQ, NEAllSameSetsQ, NEAllSameKeysQ, NEAllSameOKeysQ,

    RuleVecQ, RuleLVecQ, ORuleVecQ, DictLikeQ,
    VecQ,    DictVecQ, PairVecQ, ListVecQ, BoolVecQ, SymVecQ, ColVecQ, StrVecQ, NatVecQ, IntVecQ, PosIntVecQ, RealVecQ, NumVecQ, ExtNumVecQ,
    AnyMatQ, DictMatQ, PairMatQ, ListMatQ, BoolMatQ, SymMatQ, ColMatQ, StrMatQ, NatMatQ, IntMatQ, PosIntMatQ, RealMatQ, NumMatQ, ExtNumMatQ,
    ArrQ,    DictArrQ, PairArrQ, ListArrQ, BoolArrQ, SymArrQ, ColArrQ, StrArrQ, NatArrQ, IntArrQ, PosIntArrQ, RealArrQ, NumArrQ, ExtNumArrQ,

    StrOrVecQ, BoolOrVecQ, SymOrVecQ, IntOrVecQ, RuleOrVecQ, RuleLOrVecQ, ORuleOrVecQ,
    IntKeysQ, StrKeysQ, DictKeysQ, SymKeysQ, IntValsQ, StrValsQ, ListValsQ, DictValsQ, BoolValsQ, SymValsQ,
    HasLenQ, HasDimsQ, SameLenQ, SameOKeysQ, AllSameLenQ, AllSameOKeysQ,
    HasDupsQ, DupFreeQ, ContainsDictQ,

    StrMatchQ, StrDelimQ, StrStartsQ, StrEndsQ, StrFreeQ, StrContainsQ, StrHasQ, ASCIIQ,
    PackedArrayQ, MachineRealQ, MachineIntegerQ, MachineComplexQ,
    VFreeQ, VContainsQ, ExceptionFreeQ, ComplexPresentQ, ContainsDictQ, PatternPresentQ, PatternFreeQ, CouldContainQ,
    AssocScanWhileQ, EmptyComplementQ, EmptyIntersectionQ, SyntacticNegativeQ, IntegerPartitionQ, TensorTypeQ, ValidSymbolNameQ, HashSameQ, Base64StringQ,

    SealedQ, UnsealedQ,
    ExprEntryQ, ExprNoEntryQ, ExprValidQ, ExprInvalidQ, HoldExprEntryQ, HoldExprNoEntryQ, HoldExprValidQ, HoldExprInvalidQ, ExprMDataQ, MightEvaluateQ, HoldMaybeFnQ, MaybeFnQ,
    HasAnyCodesQ, HasAnyDefsQ, HasDValueQ, HasDownCodeQ, HasDownDefsQ, HasIValueQ, HasNoCodesQ, HasNoDefsQ, HasOwnDefsQ, HasPrintCodeQ, HasSubCodeQ, HasSubDefsQ, HasUpCodeQ, HasUpDefsQ, HasUsageQ,
    HashTableQ, HashTableContainsQ,

  "PredicateOperator",
    LenOf, DimsOf, DictOf, ListDictOf, NEListOf, NEDictOf, NEListDictOf, RuleVecOf,

  "MutatingFunction",
    GlobalWeakTablePut,
    HashTableSet, HashTableAdd, HashTableRemove, HashTableMapAt,

  "Head",
    HashTable,

  "Function",
    GlobalWeakTableGet,
    HashTableGet,
    HashTableClone,
    HashTableKeys, HashTableValues,
    HashTableToAssociation,

  "Function",
    DRowStr, RiffRowStr, SpaceRowStr, CommaRowStr,

  "BoxFunction",
    SubBox, SuperBox, SubsuperBox
];

PrivateExports[
  "PatternSymbol",
    OnePartSpecP, MultiPartSpecP, ExtPartSpecP
];

(*************************************************************************************************)

Protect[$, $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $$, $LHS, $RHS]

SetAttributes[{DefineAliasRules, DefinePatternRules, DefineLiteralRules, defineAlias, definePattern}, HoldAllComplete];

DefineAliasRules[rules___Rule]   := iDefineRules[defineAlias,   Hold @ rules];
DefinePatternRules[rules___Rule] := iDefineRules[definePattern, Hold @ rules];
DefineLiteralRules[rules___Rule] := iDefineRules[definePattern, Hold @ rules];

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

definePattern[patternSym_Symbol -> Evaluate[rhs_]] :=
  With[{rhs2 = rhs}, definePattern[patternSym -> rhs]];

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

DefineAliasRules[
  Dict                 -> Association,
  UnorderedAssociation -> Data`UnorderedAssociation,
  ODict                -> Association,
  UDict                -> Data`UnorderedAssociation,
  DirInf               -> DirectedInfinity
];

(*************************************************************************************************)

DefineAliasRules[
  Sym        -> Symbol,
  Str        -> String,
  Int        -> Integer,
  Rat        -> Rational,
  RuleD      -> RuleDelayed,
  DEdge      -> DirectedEdge,
  UEdge      -> UndirectedEdge
];

(*************************************************************************************************)

DefineAliasRules[
  MSet       -> Multiset,
  USet       -> UnorderedSet,
  OSet       -> OrderedSet
];

DefineAliasRules[
  SetD       -> SetDelayed,
  TagSetD    -> TagSetDelayed,
  UpSetD     -> UpSetDelayed
];

(*************************************************************************************************)

DefineAliasRules[
  Auto       -> Automatic,
  Inherit    -> Inherited,
  HInf       -> Infinity
];

(*************************************************************************************************)

DefineLiteralRules[
  Tau        -> 2 * Pi,
  Root2      -> Sqrt[2],
  Root3      -> Sqrt[3],
  Inf        -> Evaluate @ DirInf[1],
  PosInf     -> Evaluate @ DirInf[1],
  NegInf     -> Evaluate @ DirInf[-1],
  NTau       -> Evaluate @ N[2 * Pi],
  NPi        -> Evaluate @ N[Pi],
  NRoot2     -> Evaluate @ N[Sqrt[2]],
  NRoot3     -> Evaluate @ N[Sqrt[3]]
];

(*************************************************************************************************)

DefineLiteralRules[
  EmptyDict  -> Evaluate @ Dict[],
  EmptyUDict -> Evaluate @ UDict[],
  EmptyODict -> Evaluate @ ODict[]
];

(*************************************************************************************************)

DefineLiteralRules[
  Dots       -> "\[Ellipsis]",
  CDots      -> "\[CenterEllipsis]",
  VDots      -> "\[VerticalEllipsis]",
  LAssoc     -> "\[LeftAssociation]",
  RAssoc     -> "\[RightAssociation]",
  Newline    -> "\n"
];

DefineAliasRules[
  Regex      -> RegularExpression,
  StrExpr    -> StringExpression
];

(*************************************************************************************************)

DefineAliasRules[
  ThrowMsg   -> ThrowMessage,
  ErrorMsg   -> ErrorMessage,
  ReturnMsg  -> ReturnMessage
];

(*************************************************************************************************)

DefineAliasRules[
  Eval       -> Evaluate,
  NoEval     -> Unevaluated,
  MaybeEval  -> RuleCondition,
  RuleEval   -> RuleCondition,
  FailEval   -> Fail,
  Fn         -> Function,
  EvalMap    -> EvaluateMap,
  Seq        -> Sequence,
  Then       -> CompoundExpression,
  HoldC      -> HoldComplete,
  HoldComp   -> HoldComplete,
  $Fail      -> $Failed
];

(*************************************************************************************************)

DefineAliasRules[
  APointSize -> AbsolutePointSize,
  AThickness -> AbsoluteThickness,
  ADashing   -> AbsoluteDashing
];

(*************************************************************************************************)

DefineAliasRules[
  SubBox       -> SubscriptBox,
  SuperBox     -> SuperscriptBox,
  SubsuperBox  -> SubsuperscriptBox
];

(*************************************************************************************************)

(* TODO: Mappable? *)

(* These are defined upfront so we can use them in later pattern aliases *)

DefineAliasRules[
  SymbolQ                    -> Developer`SymbolQ,
  RealQ                      -> Developer`RealQ,
  NaturalQ                   -> Internal`NonNegativeIntegerQ,
  PositiveIntegerQ           -> Internal`PositiveIntegerQ,
  NegativeIntegerQ           -> Internal`NegativeIntegerQ,
  NonPositiveIntegerQ        -> Internal`NonPositiveIntegerQ,
  NonNegativeIntegerQ        -> Internal`NonNegativeIntegerQ,
  PositiveMachineIntegerQ    -> Internal`PositiveMachineIntegerQ,
  NegativeMachineIntegerQ    -> Internal`NegativeMachineIntegerQ,
  NonPositiveMachineIntegerQ -> Internal`NonPositiveMachineIntegerQ,
  NonNegativeMachineIntegerQ -> Internal`NonNegativeMachineIntegerQ
];

DefineAliasRules[
  PackedQ            -> Developer`PackedArrayQ,
  AssociationVectorQ -> Developer`AssociationVectorQ,
  ListOrAssociationQ -> Developer`ListOrAssociationQ,
  StringOrVectorQ    -> Developer`StringOrStringVectorQ,
  StringVectorQ      -> Developer`StringVectorQ,
  UnsafeEmptyQ       -> Developer`EmptyQ,
  NotEmptyQ          -> Developer`NotEmptyQ,
  NonEmptyQ          -> Developer`NotEmptyQ,
  HoldSymbolQ        -> Developer`HoldSymbolQ,
  HoldAtomQ          -> Developer`HoldAtomQ
];

DefineAliasRules[
  StrQ             -> StringQ,
  SymQ             -> SymbolQ,
  IntQ             -> IntegerQ,
  NatQ             -> NaturalQ,
  NumQ             -> RealValuedNumberQ,
  BoolQ            -> BooleanQ,
  DictQ            -> AssociationQ,
  UDictQ           -> UnorderedAssociationQ,
  ODictQ           -> OrderedAssociationQ,
  ListDictQ        -> ListOrAssociationQ
];

DefineAliasRules[
  HStrQ            -> HoldStringQ,
  HSymQ            -> HoldSymbolQ,
  HIntQ            -> HoldIntegerQ,
  HNatQ            -> HoldNaturalQ,
  HNumQ            -> HoldNumberQ,
  HBoolQ           -> HoldBooleanQ,
  HAtomQ           -> HoldAtomQ,
  HEmptyQ          -> HoldEmptyQ,
  HNotEmptyQ       -> HoldNotEmptyQ,
  HDictQ           -> HoldAssociationQ,
  HUDictQ          -> HoldUnorderedAssociationQ,
  HODictQ          -> HoldOrderedAssociationQ,
  HPackedQ         -> HoldPackedArrayQ
];

(**************************************************************************************************)

(* TODO: rename InfinityQ to InfiniteQ *)

DefineAliasRules[
  AutoQ            -> AutomaticQ,
  InheritQ         -> InheritedQ,
  NotAutoQ         -> NotAutomaticQ,
  InfQ             -> InfinityQ,
  NotInfQ          -> NotInfinityQ
];

DefineAliasRules[
  ORuleQ           -> OptionRuleQ,
  RuleLQ           -> RuleLikeQ,
  RuleDQ           -> RuleDelayedQ
];

(**************************************************************************************************)

DefineAliasRules[
  ZeroQ            -> ZeroNumberQ,
  NonZeroQ         -> NonZeroNumberQ
];

DefineAliasRules[
  ZeroIntQ         -> ZeroIntegerQ,
  ZeroNumQ         -> ZeroNumberQ,
  ZeroRealQ        -> ZeroRealQ,
  NonZeroIntQ      -> NonZeroIntegerQ,
  NonZeroRealQ     -> NonZeroRealQ,
  NonZeroNumQ      -> NonZeroNumberQ,

  PosIntQ          -> PositiveIntegerQ,
  NegIntQ          -> NegativeIntegerQ,
  PosRealQ         -> PositiveRealQ,
  NegRealQ         -> NegativeRealQ,
  PosNumQ          -> PositiveNumberQ,
  NegNumQ          -> NegativeNumberQ,

  NonPosIntQ       -> NonPositiveIntegerQ,
  NonNegIntQ       -> NonNegativeIntegerQ,
  NonPosRealQ      -> NonPositiveRealQ,
  NonNegRealQ      -> NonNegativeRealQ,
  NonPosNumQ       -> NonPositiveNumberQ,
  NonNegNumQ       -> NonNegativeNumberQ,

  UnitIntQ         -> UnitIntegerQ,
  UnitNumQ         -> UnitNumberQ,

  ExtIntQ          -> ExtendedIntegerQ,
  ExtNatQ          -> ExtendedNaturalQ,
  ExtNumQ          -> ExtendedNumberQ,
  ExtPosRealQ      -> ExtendedPositiveRealQ,
  ExtPosNumQ       -> ExtendedPositiveNumberQ,
  ExtNonNegRealQ   -> ExtendedNonNegativeRealQ,
  ExtNonNegNumQ    -> ExtendedNonNegativeNumberQ
];

(**************************************************************************************************)

DefineAliasRules[
  FailedQ          -> FailureQ,
  FailQ            -> FailureQ,
  NotFailedQ       -> NotFailureQ,
  NotFailQ         -> NotFailureQ
];

DefineAliasRules[
  DictOfQ          -> AssociationOfQ,
  ListDictOfQ      -> ListAssociationOfQ
];

DefineAliasRules[
  LenOf            -> LengthOf,
  DimsOf           -> DimensionsOf,
  DictOf           -> AssociationOf,
  ListDictOf       -> ListAssociationOf,
  NEListOf         -> NonEmptyListOf,
  NEDictOf         -> NonEmptyAssociationOf,
  NEListDictOf     -> NonEmptyListAssociationOf,
  RuleVecOf        -> RuleVectorOf
];

DefineAliasRules[
  NEDictQ          -> NonEmptyAssociationQ,
  NEDictOfQ        -> NonEmptyAssociationOfQ,
  NEListQ          -> NonEmptyListQ,
  NEListOfQ        -> NonEmptyListOfQ,
  NEListDictQ      -> NonEmptyListAssociationQ,
  NEListDictOfQ    -> NonEmptyListAssociationOfQ,
  NEAllSameQ       -> NonEmptyAllSameQ,
  NEAllSameByQ     -> NonEmptyAllSameByQ,
  NEAllSameLenQ    -> NonEmptyAllSameLengthQ,
  NEAllSameHeadsQ  -> NonEmptyAllSameHeadsQ,
  NEAllSamePartsQ  -> NonEmptyAllSamePartsQ,
  NEAllSameSetsQ   -> NonEmptyAllSameSetsQ,
  NEAllSameKeysQ   -> NonEmptyAllSameKeysQ,
  NEAllSameOKeysQ  -> NonEmptyAllSameOrderedKeysQ
];

(**************************************************************************************************)

DefineAliasRules[
  RuleVecQ         -> RuleVectorQ,
  RuleLVecQ        -> RuleLikeVectorQ,
  ORuleVecQ        -> OptionRuleVectorQ,
  DictLikeQ        -> AssociationLikeQ
];

DefineAliasRules[
  VecQ             -> VectorQ,
  DictVecQ         -> AssociationVectorQ,
  PairVecQ         -> PairVectorQ,
  ListVecQ         -> ListVectorQ,
  BoolVecQ         -> BooleanVectorQ,
  SymVecQ          -> SymbolVectorQ,
  ColVecQ          -> ColorVectorQ,
  StrVecQ          -> StringVectorQ,
  NatVecQ          -> NaturalVectorQ,
  IntVecQ          -> IntegerVectorQ,
  PosIntVecQ       -> PositiveIntegerVectorQ,
  RealVecQ         -> RealVectorQ,
  NumVecQ          -> NumberVectorQ,
  ExtNumVecQ       -> ExtendedNumberVectorQ
];

DefineAliasRules[
  AnyMatQ          -> AnyMatrixQ,
  DictMatQ         -> AssociationMatrixQ,
  PairMatQ         -> PairMatrixQ,
  ListMatQ         -> ListMatrixQ,
  BoolMatQ         -> BooleanMatrixQ,
  SymMatQ          -> SymbolMatrixQ,
  ColMatQ          -> ColorMatrixQ,
  StrMatQ          -> StringMatrixQ,
  NatMatQ          -> NaturalMatrixQ,
  IntMatQ          -> IntegerMatrixQ,
  PosIntMatQ       -> PositiveIntegerMatrixQ,
  RealMatQ         -> RealMatrixQ,
  NumMatQ          -> NumberMatrixQ,
  ExtNumMatQ       -> ExtendedNumberMatrixQ
];

DefineAliasRules[
  ArrQ             -> ArrayQ,
  DictArrQ         -> AssociationArrayQ,
  PairArrQ         -> PairArrayQ,
  ListArrQ         -> ListArrayQ,
  BoolArrQ         -> BooleanArrayQ,
  SymArrQ          -> SymbolArrayQ,
  ColArrQ          -> ColorArrayQ,
  StrArrQ          -> StringArrayQ,
  NatArrQ          -> NaturalArrayQ,
  IntArrQ          -> IntegerArrayQ,
  PosIntArrQ       -> PositiveIntegerArrayQ,
  RealArrQ         -> RealArrayQ,
  NumArrQ          -> NumberArrayQ,
  ExtNumArrQ       -> ExtendedNumberArrayQ
];

DefineAliasRules[
  StrOrVecQ        -> StringOrVectorQ,
  BoolOrVecQ       -> BooleanOrVectorQ,
  SymOrVecQ        -> SymbolOrVectorQ,
  IntOrVecQ        -> IntegerOrVectorQ,
  RuleOrVecQ       -> RuleOrVectorQ,
  RuleLOrVecQ      -> RuleLikeOrVectorQ,
  ORuleOrVecQ      -> OptionRuleOrVectorQ
];

(**************************************************************************************************)

DefineAliasRules[
  IntKeysQ         -> IntegerKeysQ,
  StrKeysQ         -> StringKeysQ,
  DictKeysQ        -> AssociationKeysQ,
  SymKeysQ         -> SymbolKeysQ,
  IntValsQ         -> IntegerValuesQ,
  StrValsQ         -> StringValuesQ,
  ListValsQ        -> ListValuesQ,
  DictValsQ        -> AssociationValuesQ,
  BoolValsQ        -> BooleanValuesQ,
  SymValsQ         -> SymbolValuesQ
];

DefineAliasRules[
  HasLenQ          -> HasLengthQ,
  HasDimsQ         -> HasDimensionsQ,
  SameLenQ         -> SameLengthQ,
  SameOKeysQ       -> SameOrderedKeysQ,
  AllSameLenQ      -> AllSameLengthQ,
  AllSameOKeysQ    -> AllSameOrderedKeysQ
];

DefineAliasRules[
  HasDupsQ         -> HasDuplicatesQ,
  DupFreeQ         -> DuplicateFreeQ
];

(**************************************************************************************************)

DefineAliasRules[
  StrMatchQ        -> StringMatchQ,
  StrDelimQ        -> StringStartsEndsQ,
  StrStartsQ       -> StringStartsQ,
  StrEndsQ         -> StringEndsQ,
  StrFreeQ         -> StringFreeQ,
  StrContainsQ     -> StringContainsQ,
  StrHasQ          -> StringContainsQ,
  ASCIIQ           -> PrintableASCIIQ
];

(*************************************************************************************************)

DefineAliasRules[
  Alt              -> Alternatives,
  HoldP            -> HoldPattern,
  BlankSeq         -> BlankSequence,
  BlankNullSeq     -> BlankNullSequence,
  Opt              -> Optional
];

DefinePatternRules[
  Blank1           -> Optional[_],
  Blank2           -> Repeated[_, {2}],
  Blank3           -> Repeated[_, {3}],
  Blank01          -> Repeated[_, {0,1}],
  Blank02          -> Repeated[_, {0,2}],
  Blank12          -> Repeated[_, {1,2}],
  Blank13          -> Repeated[_, {1,3}],
  Blank23          -> Repeated[_, {2,3}],
  BlankSeq2        -> Repeated[_, {2, Infinity}],
  BlankSeq3        -> Repeated[_, {3, Infinity}]
];

DefinePatternRules[
  VPattern         -> Verbatim[Pattern],
  VCondition       -> Verbatim[Condition],
  VPatternTest     -> Verbatim[PatternTest],
  VBlank           -> Verbatim[Blank],
  VBlankSeq        -> Verbatim[BlankSequence],
  VBlankNullSeq    -> Verbatim[BlankNullSequence],
  VAlt             -> Verbatim[Alternatives],
  VRepeated        -> Verbatim[Repeated],
  VExcept          -> Verbatim[Except],
  VVerbatim        -> Verbatim[Verbatim],
  VHoldP           -> Verbatim[HoldPattern]
];

DefinePatternRules[
  VFn              -> HoldPattern[Fn],
  VFunction        -> HoldPattern[Fn],
  VSet             -> HoldPattern[Set],
  VSetD            -> HoldPattern[SetDelayed],
  VTagSetD         -> HoldPattern[TagSetDelayed],
  VRuleD           -> HoldPattern[RuleDelayed]
];

(*************************************************************************************************)

DefinePatternRules[
  SingleP          -> List[_],
  PairP            -> List[_, _],
  TripleP          -> List[_, _, _]
];

DefinePatternRules[
  List1P           -> List[_],
  List2P           -> List[_, _],
  List3P           -> List[_, _, _],
  List4P           -> List[_, _, _, _],
  List23P          -> Alt[List[_, _], List[_, _, _]]
];

DefinePatternRules[
  DictP            -> _Dict ? HAtomQ,
  UDictP           -> _Dict ? HUDictQ,
  ODictP           -> _Dict ? HODictQ,
  ListDictP        -> Alt[_List, _Dict ? HAtomQ],
  EmptyP           -> _[],
  NonEmptyP        -> _[__],
  EmptyDataP       -> Alt[{}, EmptyDict, EmptyUDict],
  AtomP            -> Except[_Dict] ? HAtomQ,
  BoolP            -> False | True,
  SymP             -> _Sym ? HSymQ,
  SymStrP          -> Alt[_Sym, _Str] ? HAtomQ
];

(* TODO: make ColorP check for NumP *)
DefinePatternRules[
  DatumP           -> Alt[False, True, None, Null, _Str, _Int, _Real, _Rat, _Complex] ? HAtomQ,
  ColorP           -> Alt[_RGBColor, _GrayLevel, _CMYKColor, _Hue, _XYZColor, _LABColor, _LCHColor, _LUVColor, Opacity[_, _]],
  SideP            -> Left | Right | Bottom | Top,
  ExtSideP         -> Left | Right | Bottom | Top | BottomLeft | BottomRight | TopLeft | TopRight,
  PosDollarP       -> Alt[$0, $1, $2, $3, $4, $5, $6, $7, $8, $9],
  DollarP          -> Alt[$1, $2, $3, $4, $5, $6, $7, $8, $9],
  AnonFnP          -> HoldP[Fn[Null, ___] | Fn[_]]
];

DefinePatternRules[
  FormalSymP       -> _Sym ? HoldFormalSymbolQ,
  UserSymP         -> _Sym ? UserSymbolQ,
  InertSymP        -> _Sym ? InertSymbolQ,
  SystemSymP       -> _Sym ? SystemSymbolQ
];

DefinePatternRules[
  OnePartSpecP     -> _Int | _Key | _Str,
  MultiPartSpecP   -> (_Span | All | _List) ? MultiPartSpecQ,
  ExtPartSpecP     -> (_Int | _Key | _Str | _Span | All | _List) ? ExtPartSpecQ
];

(*************************************************************************************************)

DefinePatternRules[
  PiP              -> Alt[Pi, NPi],
  TauP             -> Alt[Tau, NTau],
  InfP             -> _DirInf
];

DefinePatternRules[
  ZeroIntP         -> 0,
  ZeroRealP        -> 0.,
  ZeroNumP         -> Alt[0, 0.],
  NumE             -> Alt[_Int, _Real, _Rat]
];

DefinePatternRules[
  IntP             -> _Int ? HAtomQ,
  NatP             -> _Int ? NatQ,
  RealP            -> _Real ? HAtomQ,
  NumP             -> NumE ? HAtomQ
];

DefinePatternRules[
  NonZeroIntP      -> Except[ZeroIntP,  IntP],
  NonZeroRealP     -> Except[ZeroRealP, RealP],
  NonZeroNumP      -> Except[ZeroNumP,  NumP]
];

DefinePatternRules[
  PosIntP          -> _Int ? PosIntQ,
  NegIntP          -> _Int ? NegIntQ,
  NonPosIntP       -> _Int ? NonPosIntQ,
  NonNegIntP       -> _Int ? NonNegIntQ
];

DefinePatternRules[
  PosRealP         -> _Real ? Positive,
  NegRealP         -> _Real ? Negative,
  NonPosRealP      -> _Real ? NonPositive,
  NonNegRealP      -> _Real ? NonNegative
];

DefinePatternRules[
  PosNumP          -> NumE ? Positive,
  NegNumP          -> NumE ? Negative,
  NonPosNumP       -> NumE ? NonPositive,
  NonNegNumP       -> NumE ? NonNegative
];

DefinePatternRules[
  ExtIntP          -> Alt[IntP,  InfP],
  ExtRealP         -> Alt[RealP, InfP],
  ExtNumP          -> Alt[NumP,  InfP],
  ExtNatP          -> Alt[_Int,  InfP] ? NonNegative,
  ExtPosIntP       -> Alt[_Int,  InfP] ? Positive,
  ExtPosRealP      -> Alt[_Real, InfP] ? Positive,
  ExtNonNegRealP   -> Alt[_Real, InfP] ? NonNegative,
  ExtPosNumP       -> Alt[NumE,  InfP] ? Positive,
  ExtNonNegNumP    -> Alt[NumE,  InfP] ? NonNegative
];

DefinePatternRules[
  UnitIntP         -> Alt[0, 1],
  UnitRealP        -> _Real ? WithinUQ,
  UnitNumP         -> NumE  ? WithinUQ
];

DefinePatternRules[
  ZeroP            -> ZeroNumP,
  NonZeroP         -> NonZeroNumP
];

(*************************************************************************************************)

DefinePatternRules[
  Zero2P           -> {ZeroP, ZeroP},
  PosInt2P         -> {PosIntP, PosIntP},
  Nat2P            -> {NatP, NatP},
  Num2P            -> Alt[List2P ? PackedQ, {_ ? NumberQ, _ ? NumberQ}],
  Num3P            -> Alt[List3P ? PackedQ, {_ ? NumberQ, _ ? NumberQ, _ ? NumberQ}],
  Num23P           -> Alt[List23P ? PackedQ, {Repeated[_ ? NumberQ, {2, 3}]}]
];

(*************************************************************************************************)

DefinePatternRules[
  Pos2P            -> _List ? Pos2Q,
  Pos3P            -> _List ? Pos3Q,
  PosAP            -> _List ? PosAQ,
  Pos2ListP        -> _List ? Pos2ListQ,
  Pos3ListP        -> _List ? Pos3ListQ,
  PosAListP        -> _List ? PosAListQ,
  Pos2ListsP       -> _List ? Pos2ListsQ,
  Pos3ListsP       -> _List ? Pos3ListsQ,
  PosAListsP       -> _List ? PosAListsQ,
  Pos2PairP        -> _List ? Pos2PairQ,
  Pos3PairP        -> _List ? Pos3PairQ,
  PosAPairP        -> _List ? PosAPairQ
];

(**************************************************************************************************)

(* TODO: replace these with _List ? ListVecQ, though it would need to be held! *)

DefinePatternRules[
  ListVecP         -> List[___List],
  DictVecP         -> List[___Dict],
  BoolVecP         -> List[BoolP...],
  SymVecP          -> List[___Sym],
  StrVecP          -> List[___Str],
  PairVecP         -> List[List[_, _]...],
  IntVecP          -> List[___Int],
  NatVecP          -> List[___Int ? NonNegative],
  PosIntVecP       -> List[___Int ? Positive],
  RealVecP         -> List[___Real],
  NumVecP          -> _List ? NumVecQ,
  ExtNumVecP       -> _List ? ExtNumVecQ
];

DefinePatternRules[
  NEListVecP       -> Except[{}, ListVecP],
  NEDictVecP       -> Except[{}, DictVecP],
  NEBoolVecP       -> Except[{}, BoolVecP],
  NESymVecP        -> Except[{}, SymVecP],
  NEStrVecP        -> Except[{}, StrVecP],
  NEPairVecP       -> Except[{}, PairVecP],
  NEIntVecP        -> Except[{}, IntVecP],
  NENatVecP        -> Except[{}, NatVecP],
  NEPosIntVecP     -> Except[{}, PosIntVecP],
  NERealVecP       -> Except[{}, RealVecP],
  NENumVecP        -> Except[{}, NumVecP]
];

DefinePatternRules[
  NEListP          -> Except[{}, _List],
  NEDictP          -> Except[EmptyDict, DictP],
  NEListDictP      -> Except[_[], ListDictP]
];

(**************************************************************************************************)

DefinePatternRules[
    RuleP          -> _Rule,
   RuleLP          -> Alt[_Rule, _RuleDelayed],
   ORuleP          -> Alt[Rule|RuleDelayed][SymStrP, _],
    SetLP          -> Alt[_Set, _SetDelayed],
   DelayP          -> Alt[_RuleDelayed, _SetDelayed, _TagSetDelayed, _UpSetDelayed]
];

DefinePatternRules[
   RuleSeqP        -> ___Rule,
  RuleLSeqP        -> RuleLP...,
  ORuleSeqP        -> ORuleP...,
   SetLSeqP        -> SetLP...,
  DelaySeqP        -> DelayP...
];

DefinePatternRules[
   RuleVecP        -> {RuleSeqP},
  RuleLVecP        -> {RuleLSeqP},
  ORuleVecP        -> {ORuleSeqP},
   SetLVecP        -> {SetLSeqP},
  DelayVecP        -> {DelaySeqP}
];

DefinePatternRules[
  RuleLSymP        -> Alt[Rule, RuleD],
   SetLSymP        -> Alt[Set, SetD],
  DelaySymP        -> Alt[RuleD, SetD, TagSetD, UpSetD]
];

(**************************************************************************************************)

DefinePatternRules[
  DictLikeP        -> Alt[DictP, RuleVecP],
  DictLP           -> Alt[DictP, RuleVecP]
];

(* JoinOrFail issues this message, but it isn't defined *)
Join::nonlist = "`` is not a list.";

(**************************************************************************************************)
(**************************************************************************************************)
(**************************************************************************************************)

DefineAliasRules[
  PackedArrayQ     -> Developer`PackedArrayQ,
  MachineRealQ     -> Developer`MachineRealQ,
  MachineIntegerQ  -> Developer`MachineIntegerQ,
  MachineComplexQ  -> Developer`MachineComplexQ
];

DefineAliasRules[
  VFreeQ           -> Internal`LiterallyAbsentQ,
  VContainsQ       -> Internal`LiterallyOccurringQ,
  ExceptionFreeQ   -> Internal`ExceptionFreeQ,
  ComplexPresentQ  -> Internal`HasComplex,
  ContainsDictQ    -> ContainsAssociationQ,
  PatternPresentQ  -> Internal`PatternPresentQ,
  PatternFreeQ     -> Internal`PatternFreeQ,
  CouldContainQ    -> System`Private`CouldContainQ
];

DefineAliasRules[
  AssocScanWhileQ    -> Association`ScanWhile,
  EmptyComplementQ   -> Language`EmptyComplementQ,
  EmptyIntersectionQ -> Language`EmptyIntersectionQ,
  SyntacticNegativeQ -> Internal`SyntacticNegativeQ,
  IntegerPartitionQ  -> Internal`IntegerPartitionQ,
  TensorTypeQ        -> Internal`TensorTypeQ,
  ValidSymbolNameQ   -> Internal`SymbolNameQ,
  HashSameQ          -> Internal`HashSameQ,
  Base64StringQ      -> Developer`Base64StringQ
];

(*************************************************************************************************)

(* System`Private` expression predicates *)
DefineAliasRules[
  SealedQ          -> System`Private`HoldNoEntryQ,
  UnsealedQ        -> System`Private`HoldEntryQ,
  ExprEntryQ       -> System`Private`EntryQ,
  ExprNoEntryQ     -> System`Private`NoEntryQ,
  ExprValidQ       -> System`Private`ValidQ,
  ExprInvalidQ     -> System`Private`NotValidQ,
  HoldExprEntryQ   -> System`Private`HoldEntryQ,
  HoldExprNoEntryQ -> System`Private`HoldNoEntryQ,
  HoldExprValidQ   -> System`Private`HoldValidQ,
  HoldExprInvalidQ -> System`Private`HoldNotValidQ,
  ExprMDataQ       -> System`Private`MDataQ,
  MightEvaluateQ   -> System`Private`MightEvaluateQ,
  HoldMaybeFnQ     -> System`Private`MightEvaluateWhenAppliedQ
];

(* System`Private` symbol predicates *)
DefineAliasRules[
  HasAnyCodesQ     -> System`Private`HasAnyCodesQ,
  HasAnyDefsQ      -> System`Private`HasAnyEvaluationsQ,
  HasDownCodeQ     -> System`Private`HasDownCodeQ,
  HasDownDefsQ     -> System`Private`HasDownEvaluationsQ,
  HasIValueQ       -> System`Private`HasImmediateValueQ,
  HasDValueQ       -> System`Private`HasDelayedValueQ,
  HasNoCodesQ      -> System`Private`HasNoCodesQ,
  HasNoDefsQ       -> System`Private`HasNoEvaluationsQ,
  HasOwnDefsQ      -> System`Private`HasOwnEvaluationsQ,
  HasPrintCodeQ    -> System`Private`HasPrintCodeQ,
  HasSubCodeQ      -> System`Private`HasSubCodeQ,
  HasSubDefsQ      -> System`Private`HasSubEvaluationsQ,
  HasUpCodeQ       -> System`Private`HasUpCodeQ,
  HasUpDefsQ       -> System`Private`HasUpEvaluationsQ
];

MaybeFnQ[f_] := System`Private`MightEvaluateWhenAppliedQ[f];

SetAttributes[HasUsageQ, HoldAllComplete];
HasUsageQ[s_Symbol ? HAtomQ]  := StringQ[MessageName[s, "usage"]];
HasUsageQ[_] := False;

(*************************************************************************************************)

DefineAliasRules[
  ContainedSymbols       -> System`Utilities`SymbolList,
  GlobalWeakTablePut     -> System`Utilities`ExprLookupAdd,
  GlobalWeakTableGet     -> System`Utilities`ExprLookup,
  HashTable              -> System`Utilities`HashTable,
  HashTableAdd           -> System`Utilities`HashTableAdd,
  HashTableClone         -> System`Utilities`HashTableClone,
  HashTableContainsQ     -> System`Utilities`HashTableContainsQ,
  HashTableGet           -> System`Utilities`HashTableGet,
  HashTableKeys          -> System`Utilities`HashTableKeys,
  HashTableMapAt         -> System`Utilities`HashTableMapAt,
  HashTableQ             -> System`Utilities`HashTableQ,
  HashTableRemove        -> System`Utilities`HashTableRemove,
  HashTableSet           -> System`Utilities`HashTableSet,
  HashTableToAssociation -> System`Utilities`HashTableToAssociation,
  HashTableValues        -> System`Utilities`HashTableValues
];

(*************************************************************************************************)

DefineAliasRules[
  Id               -> Identity,
  Rev              -> Reverse,
  At               -> Construct,
  Make             -> Construct,
  HoldLen          -> HoldLength,
  HoldSeqLen       -> HoldSequenceLength,
  Len              -> Length,
  Len2             -> Length2,
  LenN             -> LengthN,
  Dims             -> Dimensions,
  DimN             -> DimensionN,
  Vals             -> Values,
  ToVals           -> ToValues,
  KeysVals         -> KeysValues,
  MapVals          -> MapValues,
  MapValsP         -> MapValuesP,
  MapF             -> MapFirst,
  MapL             -> MapLast,
  MapM             -> MapMost,
  MapR             -> MapRest
];

(*************************************************************************************************)

DefineAliasRules[
  Rep              -> Replace,
  RepRep           -> ReplaceRepeated,
  RepAll           -> ReplaceAll,
  VecRep           -> VectorReplace
];

(*************************************************************************************************)

DefineAliasRules[
  DictSum          -> AssociationSum,
  UDictSum         -> UnorderedAssociationSum,
  DictPlus         -> AssociationPlus,
  UDictPlus        -> UnorderedAssociationPlus,
  UCounts          -> UnorderedCounts,

  InvertDict       -> InvertAssociation,
  InvertUDict      -> InvertUnorderedAssociation,

  ListDictParts    -> ListAssociationParts,

  DictMap          -> AssociationMap,
  DictMapApply     -> AssociationMapApply,
  DictMapThread    -> AssociationMapThread,
  DictThread       -> AssociationThreadOp,

  UDictMap         -> UnorderedAssociationMap,
  UDictMapApply    -> UnorderedAssociationMapApply,
  UDictMapThread   -> UnorderedAssociationMapThread,
  UDictThread      -> UnorderedAssociationThread,

  RangeDict        -> RangeAssociation,
  RangeUDict       -> RangeUnorderedAssociation,
  UDictRange       -> UnorderedAssociationRange,
  DictRange        -> AssociationRange,

  ConstDict        -> ConstantAssociation,
  ConstUDict       -> ConstantUnorderedAssociation,
  TrueDict         -> ConstantTrueAssociation,
  FalseDict        -> ConstantFalseAssociation,

  PairsToDict      -> PairsToAssociation,
  RulesToDict      -> RulesToAssociation,
  PairsToUDict     -> PairsToUnorderedAssociation,
  RulesToUDict     -> RulesToUnorderedAssociation,
  DictToPairs      -> AssociationToPairs,
  DictToRules      -> AssociationToRules,

  LevelDict        -> LevelAssociation,
  OccDict          -> OccurenceAssociation,
  ArgDict          -> ArgumentAssociation,
  LeafDict         -> LeafAssociation,
  PartDict         -> PartAssociation
];

(*************************************************************************************************)

DefineAliasRules[
  PosIndex         -> PositionIndex,
  ConstRules       -> ConstantRules,
  ConstList        -> ConstantArray,
  Parts            -> ExtractIndices,
  Occs             -> Occurences,
  OccsPos          -> OccurencePositions,
  Select1          -> SelectFirst,
  SelectIndex1     -> SelectFirstIndex,
  VectorIndex1     -> FirstVectorIndex,
  VectorIndex1Of   -> FirstVectorIndexOf,
  DelCases         -> DeleteCases,
  DelNone          -> DeleteNone,
  DelNull          -> DeleteNull,
  DelDups          -> DeleteDuplicates,
  DelDupsBy        -> DeleteDuplicatesBy,
  LenRange         -> LengthRange,
  RangeLen         -> LengthRange
];

(*************************************************************************************************)

DefineAliasRules[
  SymName          -> HoldSymbolName,
  SymContext       -> HoldSymbolContext,
  SymPath          -> HoldSymbolPath
];

(*************************************************************************************************)

DefineAliasRules[
  Inter            -> Intersection,
  Compl            -> Complement
];

(*************************************************************************************************)

DefineAliasRules[
  ToAltPat         -> ToAltPattern,
  ToLHSPat         -> ToLHSPattern,
  PatHeadSym       -> PatternHeadSymbol,
  DefHeadSym       -> DefinitionHeadSymbol,
  ToBlankSeq       -> ToBlankSequence,
  ToBlankNullSeq   -> ToBlankNullSequence
];

(*************************************************************************************************)

DefineAliasRules[
  ToRowVec         -> ToRowVector,
  ToColVec         -> ToColumnVector
];

(*************************************************************************************************)

DefineAliasRules[
  SeqLen           -> SequenceLength,
  Seq0             -> SequenceNothing,
  Seq1             -> SequenceFirst,
  Seq2             -> SequenceSecond,
  Seq3             -> SequenceThird,
  SeqN             -> SequenceLast,
  SeqMost          -> SequenceMost,
  SeqRest          -> SequenceRest,
  SeqReverse       -> SequenceReverse,
  Seq12            -> SequenceFirstSecond,
  Seq21            -> SequenceSecondFirst
];

DefineAliasRules[
  IntDigits        -> IntegerDigits,
  IntStr           -> SafeIntegerString,
  HexStr           -> HexString,
  Chars            -> Characters,
  FromCharCode     -> FromCharacterCode,
  ToCharCode       -> ToCharacterCode,
  CharRange        -> CharacterRange,
  Dist             -> EuclideanDistance
];

(*************************************************************************************************)

DefineAliasRules[
  Char1            -> StringFirst,
  CharN            -> StringLast,
  StrFirst         -> StringFirst,
  StrRest          -> StringRest,
  StrMost          -> StringMost,
  StrLast          -> StringLast,
  StrFirstRest     -> StringFirstRest,
  StrMostLast      -> StringMostLast,
  StrCases         -> StringCases,
  StrDelete        -> StringDelete,
  StrDrop          -> StringDrop,
  StrInsert        -> StringInsert,
  StrJoin          -> StringJoin,
  StrLen           -> StringLength,
  StrPadLeft       -> StringPadLeft,
  StrPadRight      -> StringPadRight,
  StrPos           -> StringPosition,
  StrPosL          -> StringPositionLeft,
  StrPosR          -> StringPositionRight,
  StrCaseF         -> StringCaseFirst,
  StrCaseL         -> StringCaseLast,
  StrSelect        -> StringSelect,
  StrDiscard       -> StringDiscard,
  StrSelectDiscard -> StringSelectDiscard,
  StrSplitPos      -> StringSplitPositions,
  StrSplitL        -> StringSplitBefore,
  StrSplitR        -> StringSplitAfter,
  StrTake          -> StringTake,
  StrTrim          -> StringTrim,
  StrTrimL         -> StringTrimLeft,
  StrTrimR         -> StringTrimRight,
  StrTrimLR        -> StringTrimLeftRight,
  StrSegment       -> StringSegment,
  StrSegmentL      -> StringSegmentBefore,
  StrSegmentR      -> StringSegmentAfter,
  StrRep           -> StringReplace,
  StrRepPart       -> StringReplacePart,
  StrRev           -> StringReverse,
  StrSplit         -> StringSplit,
  StrPre           -> StringPrepend,
  StrApp           -> StringAppend
];

(*************************************************************************************************)

DefineAliasRules[
  RandAtom         -> RandomAtom,
  RandDatum        -> RandomDatum,
  RandSym          -> RandomSymbol,
  RandLet          -> RandomLetter,
  RandDec          -> RandomDecimal,
  RandBit          -> RandomBit,
  RandNat          -> RandomNatural,
  RandBool         -> RandomBoolean,
  RandSign         -> RandomUnitInteger,
  RandRange        -> RandomRange,
  RandReal         -> RandomUnitReal,
  RandNorm         -> RandomUnitNormal
];

(*************************************************************************************************)

DefineAliasRules[
  UnpackDict       -> UnpackAssociation,
  PackDict         -> PackAssociation,
  SetInherit       -> SetInherited,
  AssocTo          -> AssociateTo,
  SubFrom          -> SubtractFrom
];

DefineAliasRules[
  MakeSetD         -> MakeSetDelayed,
  MakeTagSetD      -> MakeTagSetDelayed,
  MakeUpSetD       -> MakeUpSetDelayed
];

(*************************************************************************************************)

DefineAliasRules[
  InternalData     -> System`Private`InternalData
];

(*************************************************************************************************)

DefineAliasRules[
  NBObject         -> NotebookObject,
  VertexColorFn    -> VertexColorFunction
];

(*************************************************************************************************)

(* System`Private` flag manipulation *)
DefineAliasRules[
  MakeValid            -> System`Private`ConstructValid,
  MakeSealed           -> System`Private`ConstructNoEntry,
  ConstructValidExpr   -> System`Private`ConstructValid,
  ConstructNoEntryExpr -> System`Private`ConstructNoEntry,
  HoldSetNoEntryExpr   -> System`Private`HoldSetNoEntry,
  HoldSetValidExpr     -> System`Private`HoldSetValid
];

ConstructValidExpr[head_, args___] := System`Private`HoldSetValid[head[args]];

(*************************************************************************************************)

(* Developer` base64 utilities *)
DefineAliasRules[
  ReadRawJSONFile          -> Developer`ReadRawJSONFile,
  ReadRawJSONStream        -> Developer`ReadRawJSONStream,
  WriteRawJSONFile         -> Developer`WriteRawJSONFile,
  WriteRawJSONStream       -> Developer`WriteRawJSONStream,
  CellInformation          -> Developer`CellInformation,
  ReadRawJSONString        -> Developer`ReadRawJSONString,
  WriteRawJSONString       -> Developer`WriteRawJSONString,
  DecodeBase64             -> Developer`DecodeBase64,
  EncodeBase64             -> Developer`EncodeBase64,
  DecodeBase64ToByteArray  -> Developer`DecodeBase64ToByteArray
];

(*************************************************************************************************)

(* Developer` PackedArray utilities *)
DefineAliasRules[
  ToList           -> Developer`ToList,
  ToPackedArray    -> Developer`ToPackedArray,
  ToPacked         -> Developer`ToPackedArray,
  FromPackedArray  -> Developer`FromPackedArray,
  PackedArrayType  -> Internal`PackedArrayType
];

(*************************************************************************************************)

(* data structures *)
DefineAliasRules[
  StringHash       -> Data`StringHash,
  StringHash       -> Data`StringHash,
  BagPart          -> Internal`BagPart,
  Bag              -> Internal`Bag,
  BagLength        -> Internal`BagLength,
  StuffBag         -> Internal`StuffBag
];

(*************************************************************************************************)

(* Internal` functions *)
DefineAliasRules[
  FastQuietCheck               -> Internal`UnsafeQuietCheck,
  WithLocalSettings            -> Internal`WithLocalSettings,
  WithTimestampsPreserved      -> Internal`WithTimestampsPreserved,
  InheritedBlock               -> Internal`InheritedBlock,
  IBlock                       -> Internal`InheritedBlock,
  ContainsListRepresentationQ  -> Internal`ContainsListRepresentationQ,
  RepetitionFromMultiplicity   -> Internal`RepetitionFromMultiplicity,
  Reciprocal                   -> Internal`Reciprocal,
  OutermostToInnermost         -> Internal`OutermostToInnermost,
  InnermostToOutermost         -> Internal`InnermostToOutermost,
  TransposeInPlace             -> Internal`TransposeInPlace,
  ListJoin                     -> Internal`JoinOrFail
];

(*************************************************************************************************)

(* Internal` functions *)
DefineAliasRules[
  MapThreadMin         -> Random`Private`MapThreadMin,
  MapThreadMax         -> Random`Private`MapThreadMax,
  FastNumericIndices   -> Random`Private`PositionsOf
];

(*************************************************************************************************)

(* ToPackedTree: packing of tree into a special expression, not sure how we map elements yet *)
(* AbsDelta: auto-broadcasts Abs[#1 - #2]& *)
DefineAliasRules[
  AbsDelta         -> NumericalMath`AbsoluteError,
  PackedTree       -> NumericalMath`Derivatives`PackedExpression,
  ToPackedTree     -> NumericalMath`Derivatives`ToPackedExpression,
  DimsTree         -> NumericalMath`Derivatives`RaggedDimensions,
  DimsProd         -> NumericalMath`Derivatives`NumberOfElements,
  FlatProd         -> FlatProduct
];

(*************************************************************************************************)

(* Multisets[list, k] or [n, k] *)
DefineAliasRules[
  SplitLengths         -> GroupTheory`Tools`PartitionRagged,
  Multisets            -> GroupTheory`Tools`Multisets,
  MultiSubsets         -> GroupTheory`Tools`MultiSubsets,
  ReplaceSuccessive    -> GroupTheory`Tools`ConsecutiveReplace,
  SublistPosition      -> GroupTheory`Tools`SublistPosition
];

(**************************************************************************************************)

(* IntegerString is badly named! *)
SafeIntegerString[n_Integer]       := safeIntStr @ n;
SafeIntegerString[items:ListDictP] := If[Positive @ Min @ items, IntegerString @ items, safeIntStr @ items];
SafeIntegerString[expr_]           := (Message[SafeIntegerString::intStrNotInteger, expr]; $Failed);

SetAttributes[safeIntStr, Listable];
safeIntStr[n_Integer ? Negative] := StringJoin["-", IntegerString @ n];
safeIntStr[n_Integer]            := IntegerString @ n;
safeIntStr[expr_]                := (Message[SafeIntegerString::intStrNotInteger, expr]; $Failed);

NatStr[n_Integer ? Positive]                      := IntegerString @ n;
NatStr[items:ListDictP /; Positive @ Min @ items] := IntegerString @ items;
NatStr[expr_]                                     := (Message[NatStr::intStrNotInteger, expr]; $Failed);

NatStr::negative = "Provided integer `` was negative."
General::intStrNotInteger = "Provided value `` was not an integer or list or association of these."

(*************************************************************************************************)

DefineAliasRules[
  FmA -> \[FormalCapitalA],
  FmB -> \[FormalCapitalB],
  FmC -> \[FormalCapitalC],
  FmD -> \[FormalCapitalD],
  FmE -> \[FormalCapitalE],
  FmF -> \[FormalCapitalF],
  FmG -> \[FormalCapitalG],
  FmH -> \[FormalCapitalH],
  FmI -> \[FormalCapitalI],
  FmJ -> \[FormalCapitalJ],
  FmK -> \[FormalCapitalK],
  FmL -> \[FormalCapitalL],
  FmM -> \[FormalCapitalM],
  FmN -> \[FormalCapitalN],
  FmO -> \[FormalCapitalO],
  FmP -> \[FormalCapitalP],
  FmQ -> \[FormalCapitalQ],
  FmR -> \[FormalCapitalR],
  FmS -> \[FormalCapitalS],
  FmT -> \[FormalCapitalT],
  FmU -> \[FormalCapitalU],
  FmV -> \[FormalCapitalV],
  FmW -> \[FormalCapitalW],
  FmX -> \[FormalCapitalX],
  FmY -> \[FormalCapitalY],
  FmZ -> \[FormalCapitalZ]
];
