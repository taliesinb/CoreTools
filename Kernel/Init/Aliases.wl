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

  "OptionSymbol",
    VertexColorFn,

  "SpecialVariable",
    $Fail,

  "SpecialFunction",
    ConstructValidExpr, ConstructNoEntryExpr, WithTimestampsPreserved, HoldC, HoldComp, HoldSetNoEntryExpr, HoldSetValidExpr,

  "MutatingFunction",
    StuffBag, SetD, TagSetD, UpSetD, AssocTo,

  "TypeHead",
    Str, Int, Sym,

  "DataHead",
    Dict, Assoc, UDict, UAssoc, UAssociation, UnorderedAssociation,
    RuleD, InternalData,
    DEdge, UEdge,
    Bag,
    PackedTree,

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
    HoldP, Regex, Alt,
    VPattern, VCondition, VPatternTest, VBlank, VBlankSeq, VBlankNullSeq, VAlt, VRepeated, VExcept, VVerbatim, VHoldP,
    VFn, VFunction, VSet, VSetD, VTagSetD, VRule, VRuleD,
    Blank1, Blank2, Blank3, Blank01, Blank02, Blank12, Blank13, Blank23, BlankSeq2, BlankSeq3, BlankNullSeq, BlankSeq,

  "Symbol",
    Auto, Inherit, Inf, Tau,

  "PatternSymbol",
    AtomP, DatumP, BoolP, ZeroP, NonZeroP, NonZeroIntP, Zero2P, NatP, Nat2P, PosIntP, PosInt2P,
    NumP, Num2P, Num23P, Num3P,
    Pos2P, Pos2ListP, Pos2ListsP, Pos2PairP,
    Pos3P, Pos3ListP, Pos3ListsP, Pos3PairP,
    PosAP, PosAListP, PosAListsP, PosAPairP,
    UnitNumP,
    ExtNumP, ExtNatP, ExtIntP, ExtPosIntP,
    PosNumP, ExtPosNumP, ColorP, SideP, ExtSideP,
    OnePartSpecP, MultiPartSpecP, ExtPartSpecP,
    SymP, SymStrP, FormalP, PosDollarP, DollarP,
    SingleP, PairP, ListVecP, AssocVecP, BoolVecP, SymVecP, StrVecP, PairVecP, IntVecP, NatVecP, PosIntVecP, RealVecP, NumVecP, ExtNumVecP,
    ListDictP, DictLikeP,
    RuleP,    RuleLP,    ORuleP     SetLP,    DelayP,
    RuleSeqP, RuleLSeqP, ORuleSeqP, SetLSeqP, DelaySeqP
    RuleVecP, RuleLVecP, ORuleVecP, SetLVecP, DelayVecP
    RuleLSymP, SetLSymP, DelaySymP
    EmptyP, NonEmptyP, EmptyDataP, EmptyAssoc, EmptyDict,
    UserSymbolP, InertSymbolP, SystemSymbolP,
    AnonFnP,

  "SlotSymbol",
    FmA, FmB, FmC, FmD, FmE, FmF, FmG, FmH, FmI, FmJ, FmK, FmL, FmM, FmN, FmO, FmP, FmQ, FmR, FmS, FmT, FmU, FmV, FmW, FmX, FmY, FmZ,

  "SlotVariable",
    $, $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $$, $LHS, $RHS,

  "MessageFunction",
    ThrowMsg, ErrorMsg, ReturnMsg,

  "GraphicsDirective",
    APointSize, AThickness, ADashing,

  "Function",
    PosIndex, RangeAssoc, AssocRange, UAssocRange, RangeUAssoc,
    SeqLen,
    Seq0, Seq1, Seq2, Seq3, SeqN, SeqMost, SeqRest, SeqReverse,
    Seq12, Seq21,

    LenRange, RangeLen, ConstAssoc, ConstRules, ConstList, ConstUAssoc, ConstTrueAssoc, PairsToAssoc, AssocToPairs,
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

     InvertDict,  DictRange,  RangeDict,  ConstDict,  PairsToDict,  RulesToDict, DictToPairs, DictToRules,
    InvertUDict, UDictRange, RangeUDict, ConstUDict, PairsToUDict, RulesToUDict, ConstTrueDict,
     DictMap,  DictMapApply,  DictMapThread,  DictThread,
    UDictMap, UDictMapApply, UDictMapThread, UDictThread,
    AssocMap, UAssocThread, AssocMapThread,

    HoldLen, Len, Len2, LenN, Dims, Id, Rev, IntDigits,
    SafeIntegerString,
    IntStr, NatStr, HexStr, Chars, FromCharCode, ToCharCode, CharRange,
    Char1, CharN, StrFirst, StrRest, StrMost, StrLast, StrFirstRest, StrMostLast,
    StrCases, StrDelete, StrDrop, StrExpr,
    StrExtract, StrInsert, StrJoin, StrLen, StrPadLeft, StrPadRight, StrPartition, StrPos, StrPosL, StrPosR,
    StrCaseF, StrCaseL, StrSelect, StrDiscard, StrSelectDiscard,
    StrSplitPos, StrSplitL, StrSplitR, StrTake, StrTrim, StrTrimL, StrTrimR, StrTrimLR,
    StrSegment, StrSegmentL, StrSegmentR, StrRepeat, StrRep, StrRepPart, StrRev, StrSplit, StrPre, StrApp,

    DecodeBase64, DecodeBase64ToByteArray, EncodeBase64,
    ToList, ToPackedArray, ToPacked, FromPackedArray,
    PackedArrayForm, PackedArrayType,

    BagPart, BagLength,
    RepetitionFromMultiplicity, Reciprocal,
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

  "MutatingFunction",
    UnpackDict, PackDict, SetInherit,

  "Predicate",
    AutoQ, InheritQ, InfQ, NotAutoQ, NotInfQ,
    ZeroIntQ, NonZeroIntQ,
    PosQ, NegQ, NonPosQ, NonNegQ, NonPosIntQ, NonNegIntQ,

    StrQ, StrMatchQ, StrDelimQ, StrStartsQ, StrEndsQ, StrFreeQ, StrContainsQ, StrHasQ, ASCIIQ,
    BoolQ, IntQ, PosIntQ, NegIntQ, NatQ, NumQ, ExtNumQ, ORuleQ,

    AssocQ, VecQ, AssocVecQ, RuleVecQ, RuleLVecQ, ORuleVecQ, DictLQ,
    StrOrVecQ, BoolOrVecQ, SymOrVecQ, IntOrVecQ, RuleOrVecQ, RuleLOrVecQ, ORuleOrVecQ,
    DictVecQ, PairVecQ, ListVecQ, BoolVecQ, SymVecQ, StrVecQ, StrVecQ, NatVecQ, IntVecQ, PosIntVecQ, RealVecQ, NumVecQ, ExtNumVecQ,
    DictMatQ, PairMatQ, ListMatQ, BoolMatQ, SymMatQ, StrMatQ, NatMatQ, IntMatQ, PosIntMatQ, RealMatQ, NumMatQ, ExtNumMatQ,
    PairArrQ, ListArrQ, BoolArrQ, SymArrQ, StrArrQ, NatArrQ, IntArrQ, PosIntArrQ, RealArrQ, NumArrQ, ExtNumArrQ, DictArrQ,

    ExprEntryQ, ExprNoEntryQ, ExprValidQ, ExprInvalidQ, HoldExprEntryQ, HoldExprNoEntryQ, HoldExprValidQ, HoldExprInvalidQ, ExprMDataQ, MightEvaluateQ, HoldMaybeFnQ, MaybeFQ, MaybeFnQ, ExprWillNotEvaluateQ, ExprWillNotEvaluateWhenAppliedQ,
    HasAnyCodesQ, HasAnyDefsQ, HasDValueQ, HasDownCodeQ, HasDownDefsQ, HasIValueQ, HasNoCodesQ, HasNoDefsQ, HasOwnDefsQ, HasPrintCodeQ, HasSubCodeQ, HasSubDefsQ, HasUpCodeQ, HasUpDefsQ,
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
    AssocScanWhileQ, EmptyComplementQ, EmptyIntersectionQ, HashSameQ, NValueQ,
    HAssocQ, HPackedQ,
    DictQ, ListDictQ, UDictQ, ODictQ, NonEmptyDictQ, NonEmptyListDictQ,
    DictOfQ, ListDictOfQ, NonEmptyDictOfQ, NonEmptyListDictOfQ,
    ContainsDictQ,
    HasLenQ, HasDimsQ, IntKeysQ, StrKeysQ, DictKeysQ, SymKeysQ, IntValsQ, StrValsQ, ListValsQ, DictValsQ, BoolValsQ, SymValsQ, HasDupsQ, RuleDQ,
    SameLenQ, AllSameLenQ,
    FailedQ, FailQ, NotFailedQ, NotFailQ,

    SameOKeysQ, AllSameOKeysQ,

    NEDictQ, NEListDictQ, NEDictOfQ, NEListDictOfQ,
    NEAllSameQ, NEAllSameByQ,
    NEAllSameLengthQ, NEAllSameHeadsQ, NEAllSamePartsQ, NEAllSameSetsQ, NEAllSameKeysQ, NEAllSameOKeysQ,

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

  "Predicate",
    HashTableQ, HashTableContainsQ,

  "PredicateOperator",
    DictOf, ListDictOf,
    NonEmptyDictOf, NonEmptyListDictOf,

  "Function",
    DRowStr, RiffRowStr, SpaceRowStr, CommaRowStr,

  "BoxFunction",
    DRowBox, DRiffRowBox
];

(*************************************************************************************************)

Protect[$, $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $$, $LHS, $RHS]

SetAttributes[{DefineAliasRules, DefinePatternRules, defineAlias, definePattern}, HoldAllComplete];

DefineAliasRules[rules___Rule]   := iDefineRules[defineAlias,   Hold @ rules];
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

DefineAliasRules[
  APointSize -> AbsolutePointSize,
  AThickness -> AbsoluteThickness,
  ADashing   -> AbsoluteDashing
];

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
  VHoldP         -> Verbatim[HoldPattern]
];

DefinePatternRules[
  VFn            -> HoldPattern[Function],
  VFunction      -> HoldPattern[Function],
  VSet           -> HoldPattern[Set],
  VSetD          -> HoldPattern[SetDelayed],
  VTagSetD       -> HoldPattern[TagSetDelayed],
  VRuleD         -> HoldPattern[RuleDelayed]
];

DefinePatternRules[
  EmptyP         -> _[],
  NonEmptyP      -> _[__],
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
  FormalP        -> _Symbol ? HoldFormalSymbolQ,
  SymStrP        -> Alternatives[_Symbol, _String] ? Developer`HoldAtomQ,
  PosDollarP     -> Alternatives[$0, $1, $2, $3, $4, $5, $6, $7, $8, $9],
  DollarP        -> Alternatives[$1, $2, $3, $4, $5, $6, $7, $8, $9],
  AssocP         -> _Association ? Developer`HoldAtomQ,
  UserSymbolP    -> _Symbol ? UserSymbolQ,
  InertSymbolP   -> _Symbol ? InertSymbolQ,
  SystemSymbolP  -> _Symbol ? SystemSymbolQ,
  AnonFnP        -> HoldP[Function[Null, ___] | Function[_]]
];

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

With[
  {emptyAssoc = Association[], dirInf = DirectedInfinity[1], tau = 2. * Pi},
  DefinePatternRules[
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
  NumP           -> Alternatives[_Integer, _Real, _Rational],
  PosNumP        -> Alternatives[_Integer, _Real, _Rational] ? Positive,
  UnitNumP       -> Alternatives[_Integer, _Real, _Rational] ? UnitNumberQ,
  ExtNumP        -> Alternatives[_Integer, _Real, _Rational, _DirectedInfinity],
  ExtNatP        -> Alternatives[_Integer ? NonNegative, _DirectedInfinity],
  ExtIntP        -> Alternatives[_Integer, _DirectedInfinity, _DirectedInfinity],
  ExtPosIntP     -> Alternatives[_Integer ? Positive, _DirectedInfinity],
  ExtPosNumP     -> Alternatives[_Integer, _Real, _Rational, _DirectedInfinity] ? Positive,
  Num2P          -> {_ ? NumberQ, _ ? NumberQ},
  Num3P          -> {_ ? NumberQ, _ ? NumberQ, _ ? NumberQ}
  Num23P         -> {Repeated[_ ? NumberQ, {2, 3}]}
];

DefinePatternRules[
  Pos2P          -> _List ? Pos2Q,
  Pos3P          -> _List ? Pos3Q,
  PosAP          -> _List ? PosAQ,
  Pos2ListP      -> _List ? Pos2ListQ,
  Pos3ListP      -> _List ? Pos3ListQ,
  PosAListP      -> _List ? PosAListQ,
  Pos2ListsP     -> _List ? Pos2ListsQ,
  Pos3ListsP     -> _List ? Pos3ListsQ,
  PosAListsP     -> _List ? PosAListsQ,
  Pos2PairP      -> _List ? Pos2PairQ,
  Pos3PairP      -> _List ? Pos3PairQ,
  PosAPairP      -> _List ? PosAPairQ
];

(**************************************************************************************************)

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

(**************************************************************************************************)

DefinePatternRules[
  DictLikeP      -> Alternatives[_Association ? AssociationQ,  {Alternatives[_Rule, _RuleDelayed]...}],
  ListDictP      -> Alternatives[_List, _Association ? Developer`HoldAtomQ]
];

(**************************************************************************************************)

DefinePatternRules[
    RuleP        -> _Rule,
   RuleLP        -> Alternatives[_Rule, _RuleDelayed],
   ORuleP        -> Alternatives[Rule|RuleDelayed][SymStrP, _],
    SetLP        -> Alternatives[_Set, _SetDelayed],
   DelayP        -> Alternatives[_RuleDelayed, _SetDelayed, _TagSetDelayed, _UpSetDelayed]
];

DefinePatternRules[
   RuleSeqP      -> RuleP...,
  RuleLSeqP      -> RuleLP...,
  ORuleSeqP      -> ORuleP...,
   SetLSeqP      -> SetLP...,
  DelaySeqP      -> DelayP...
];

DefinePatternRules[
   RuleVecP      -> {RuleSeqP},
  RuleLVecP      -> {RuleLSeqP},
  ORuleVecP      -> {ORuleSeqP},
   SetLVecP      -> {SetLSeqP},
  DelayVecP      -> {DelaySeqP}
];

DefinePatternRules[
  RuleLSymP      -> Alternatives[Rule, RuleDelayed],
   SetLSymP      -> Alternatives[Set, SetDelayed],
  DelaySymP      -> Alternatives[RuleDelayed, SetDelayed, TagSetDelayed, UpSetDelayed]
];

(**************************************************************************************************)

DefineAliasRules[
  DictQ                         -> AssociationQ,
  ListDictQ                     -> Developer`ListOrAssociationQ,
  UDictQ                        -> UnorderedAssociationQ,
  ODictQ                        -> OrderedAssociationQ,
  NonEmptyDictQ                 -> NonEmptyAssociationQ,
  NonEmptyListDictQ             -> NonEmptyListAssociationQ,
  DictOfQ                       -> AssociationOfQ,
  ListDictOfQ                   -> ListAssociationOfQ,
  NonEmptyDictOfQ               -> NonEmptyAssociationOfQ,
  NonEmptyListDictOfQ           -> NonEmptyListAssociationOfQ,
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
  SameOKeysQ                    -> SameOrderedKeysQ,
  AllSameOKeysQ                 -> AllSameOrderedKeysQ,
  NEDictQ                       -> NonEmptyDictQ,
  NEListDictQ                   -> NonEmptyListDictQ,
  NEDictOfQ                     -> NonEmptyDictOfQ,
  NEListDictOfQ                 -> NonEmptyListDictOfQ,
  NEAllSameQ                    -> NonEmptyAllSameQ,
  NEAllSameByQ                  -> NonEmptyAllSameByQ,
  NEAllSameLengthQ              -> NonEmptyAllSameLengthQ,
  NEAllSameHeadsQ               -> NonEmptyAllSameHeadsQ,
  NEAllSamePartsQ               -> NonEmptyAllSamePartsQ,
  NEAllSameSetsQ                -> NonEmptyAllSameSetsQ,
  NEAllSameKeysQ                -> NonEmptyAllSameKeysQ,
  NEAllSameOKeysQ               -> NonEmptyAllSameOrderedKeysQ
];

DefineAliasRules[
  AutoQ                         -> AutomaticQ,
  InheritQ                      -> InheritedQ,
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
  StrHasQ                       -> StringContainsQ,
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
  NumQ                          -> RealValuedNumberQ,
  ExtNumQ                       -> ExtendedNumberQ,
  ORuleQ                        -> OptionRuleQ
];

DefineAliasRules[
  AssocVecQ                     -> Developer`AssociationVectorQ,
  DictVecQ                      -> Developer`AssociationVectorQ,
  RuleVecQ                      -> RuleVectorQ,
  RuleLVecQ                     -> RuleLikeVectorQ,
  ORuleVecQ                     -> OptionRuleVectorQ,
  DictLikeQ                     -> AssociationLikeQ,
  AssocQ                        -> AssociationQ,
  VecQ                          -> VectorQ,
  PairVecQ                      -> PairVectorQ,
  ListVecQ                      -> ListVectorQ,
  BoolVecQ                      -> BooleanVectorQ,
  SymVecQ                       -> SymbolVectorQ,
  StrVecQ                       -> Developer`StringVectorQ,
  NatVecQ                       -> NaturalVectorQ,
  IntVecQ                       -> IntegerVectorQ,
  PosIntVecQ                    -> PositiveIntegerVectorQ,
  RealVecQ                      -> RealVectorQ,
  NumVecQ                       -> NumberVectorQ,
  ExtNumVecQ                    -> ExtendedNumberVectorQ
];

DefineAliasRules[
  StrOrVecQ                     -> Developer`StringOrStringVectorQ,
  BoolOrVecQ                    -> BooleanOrVectorQ,
  SymOrVecQ                     -> SymbolOrVectorQ,
  IntOrVecQ                     -> IntegerOrVectorQ,
  RuleOrVecQ                    -> RuleOrVectorQ,
  RuleLOrVecQ                   -> RuleLikeOrVectorQ,
  ORuleOrVecQ                   -> OptionRuleOrVectorQ
];

DefineAliasRules[
  DictMatQ                      -> AssociationMatrixQ,
  PairMatQ                      -> PairMatrixQ,
  ListMatQ                      -> ListMatrixQ,
  BoolMatQ                      -> BooleanMatrixQ,
  SymMatQ                       -> SymbolMatrixQ,
  StrMatQ                       -> StringMatrixQ,
  NatMatQ                       -> NaturalMatrixQ,
  IntMatQ                       -> IntegerMatrixQ,
  PosIntMatQ                    -> PositiveIntegerMatrixQ,
  RealMatQ                      -> RealMatrixQ,
  NumMatQ                       -> NumberMatrixQ,
  ExtNumMatQ                    -> ExtendedNumberMatrixQ
];

DefineAliasRules[
  PairArrQ                      -> PairArrayQ,
  ListArrQ                      -> ListArrayQ,
  BoolArrQ                      -> BooleanArrayQ,
  SymArrQ                       -> SymbolArrayQ,
  StrArrQ                       -> StringArrayQ,
  NatArrQ                       -> NaturalArrayQ,
  IntArrQ                       -> IntegerArrayQ,
  PosIntArrQ                    -> PositiveIntegerArrayQ,
  RealArrQ                      -> RealArrayQ,
  NumArrQ                       -> NumberArrayQ,
  ExtNumArrQ                    -> ExtendedNumberArrayQ,
  DictArrQ                      -> AssociationArrayQ
];

DefineAliasRules[
  DupFreeQ                      -> DuplicateFreeQ,
  AssocScanWhileQ               -> Association`ScanWhile,
  EmptyComplementQ              -> Language`EmptyComplementQ,
  EmptyIntersectionQ            -> Language`EmptyIntersectionQ,
  HAssocQ                       -> HoldAssociationQ,
  HPackedQ                      -> HoldPackedArrayQ
];

DefineAliasRules[
  UnpackDict                    -> UnpackAssociation,
  PackDict                      -> PackAssociation,
  SetInherit                    -> SetInherited
];

(* Developer` predicates *)
(* atoms count as not EmptyQ somehow, we define our own EmptyQ*)
DefineAliasRules[
  AssociationVectorQ            -> Developer`AssociationVectorQ,
  ListOrAssociationQ            -> Developer`ListOrAssociationQ,
  StringOrStringVectorQ         -> Developer`StringOrStringVectorQ,
  StringVectorQ                 -> Developer`StringVectorQ,
  UnsafeEmptyQ                  -> Developer`EmptyQ,
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

DefineAliasRules[
  HasLenQ                       -> HasLengthQ,
  HasDimsQ                      -> HasDimensionsQ,
  IntKeysQ                      -> IntegerKeysQ,
  StrKeysQ                      -> StringKeysQ,
  DictKeysQ                     -> AssociationKeysQ,
  SymKeysQ                      -> SymbolKeysQ,
  IntValsQ                      -> IntegerValuesQ,
  StrValsQ                      -> StringValuesQ,
  ListValsQ                     -> ListValuesQ,
  DictValsQ                     -> AssociationValuesQ,
  BoolValsQ                     -> BooleanValuesQ,
  SymValsQ                      -> SymbolValuesQ,
  SameLenQ                      -> SameLengthQ,
  HasDupsQ                      -> HasDuplicatesQ,
  ContainsDictQ                 -> ContainsAssociationQ,
  RuleDQ                        -> RuleDelayedQ
];


(* JoinOrFail issues this message, but it isn't defined *)
Join::nonlist = "`` is not a list.";

(* Internal` predicates *)
DefineAliasRules[
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
  HashSameQ                     -> Internal`HashSameQ
];

(*************************************************************************************************)

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
  HoldMaybeFnQ                  -> System`Private`MightEvaluateWhenAppliedQ
];

(* System`Private` symbol predicates *)
DefineAliasRules[
  HasAnyCodesQ                  -> System`Private`HasAnyCodesQ,
  HasAnyDefsQ                   -> System`Private`HasAnyEvaluationsQ,
  HasDownCodeQ                  -> System`Private`HasDownCodeQ,
  HasDownDefsQ                  -> System`Private`HasDownEvaluationsQ,
  HasIValueQ                    -> System`Private`HasImmediateValueQ,
  HasDValueQ                    -> System`Private`HasDelayedValueQ,
  HasNoCodesQ                   -> System`Private`HasNoCodesQ,
  HasNoDefsQ                    -> System`Private`HasNoEvaluationsQ,
  HasOwnDefsQ                   -> System`Private`HasOwnEvaluationsQ,
  HasPrintCodeQ                 -> System`Private`HasPrintCodeQ,
  HasSubCodeQ                   -> System`Private`HasSubCodeQ,
  HasSubDefsQ                   -> System`Private`HasSubEvaluationsQ,
  HasUpCodeQ                    -> System`Private`HasUpCodeQ,
  HasUpDefsQ                    -> System`Private`HasUpEvaluationsQ
];

MaybeFnQ[f_] := System`Private`MightEvaluateWhenAppliedQ[f];

SetAttributes[HasUsageQ, HoldAllComplete];
HasUsageQ[s_Symbol ? Developer`HoldAtomQ]  := StringQ[MessageName[s, "usage"]];
HasUsageQ[_] := False;

(*************************************************************************************************)

DefineAliasRules[
  ContainedSymbols              -> System`Utilities`SymbolList,

  GlobalWeakTablePut            -> System`Utilities`ExprLookupAdd,
  GlobalWeakTableGet            -> System`Utilities`ExprLookup,

  HashTable                     -> System`Utilities`HashTable,
  HashTableAdd                  -> System`Utilities`HashTableAdd,
  HashTableClone                -> System`Utilities`HashTableClone,
  HashTableContainsQ            -> System`Utilities`HashTableContainsQ,
  HashTableGet                  -> System`Utilities`HashTableGet,
  HashTableKeys                 -> System`Utilities`HashTableKeys,
  HashTableMapAt                -> System`Utilities`HashTableMapAt,
  HashTableQ                    -> System`Utilities`HashTableQ,
  HashTableRemove               -> System`Utilities`HashTableRemove,
  HashTableSet                  -> System`Utilities`HashTableSet,
  HashTableToAssociation        -> System`Utilities`HashTableToAssociation,
  HashTableValues               -> System`Utilities`HashTableValues
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
  NBObject       -> NotebookObject,
  VertexColorFn  -> VertexColorFunction
];

DefineAliasRules[
  Sym            -> Symbol,
  Dict           -> Association,
  Assoc          -> Association,
  Str            -> String,
  Int            -> Integer,
  RuleD          -> RuleDelayed,
  DEdge          -> DirectedEdge,
  UEdge          -> UndirectedEdge
];

(*************************************************************************************************)

DefineAliasRules[
  Regex          -> RegularExpression,
  StrExpr        -> StringExpression
];

(*************************************************************************************************)

DefineAliasRules[
  Alt            -> Alternatives,
  HoldP          -> HoldPattern,
  BlankSeq       -> BlankSequence,
  BlankNullSeq   -> BlankNullSequence
];

DefinePatternRules[
  Blank1         -> Optional[_],
  Blank2         -> Repeated[_, {2}],
  Blank3         -> Repeated[_, {3}],
  Blank01        -> Repeated[_, {0,1}],
  Blank02        -> Repeated[_, {0,2}],
  Blank12        -> Repeated[_, {1,2}],
  Blank13        -> Repeated[_, {1,3}],
  Blank23        -> Repeated[_, {2,3}],
  BlankSeq2      -> Repeated[_, {2, Infinity}],
  BlankSeq3      -> Repeated[_, {3, Infinity}]
];

(*************************************************************************************************)

DefineAliasRules[
  Auto           -> Automatic,
  Inherit        -> Inherited,
  SetD           -> SetDelayed,
  TagSetD        -> TagSetDelayed,
  UpSetD         -> UpSetDelayed,
  AssocTo        -> AssociateTo
];

(*************************************************************************************************)

DefineAliasRules[
  Eval           -> Evaluate,
  NoEval         -> Unevaluated,
  MaybeEval      -> RuleCondition,
  RuleEval       -> RuleCondition,
  FailEval       -> Fail,

  Fn             -> Function,
  EvalMap        -> EvaluateMap,
  Seq            -> Sequence,
  Then           -> CompoundExpression,
  HoldC          -> HoldComplete,
  HoldComp       -> HoldComplete,
  $Fail          -> $Failed
];

(* TODO: retire HoldC *)

(*************************************************************************************************)

DefineAliasRules[
  Inter          -> Intersection,
  Compl          -> Complement
];

(*************************************************************************************************)

DefineAliasRules[
  InvertDict     -> InvertAssociation,
  InvertUDict    -> InvertUnorderedAssociation,

  ListDictParts  -> ListAssociationParts,

  DictMap        -> AssociationMap,
  DictMapApply   -> AssociationMapApply,
  DictMapThread  -> AssociationMapThread,
  DictThread     -> AssocThread,

  UDictMap       -> UnorderedAssociationMap,
  UDictMapApply  -> UnorderedAssociationMapApply,
  UDictMapThread -> UnorderedAssociationMapThread,
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
  PairsToUDict   -> PairsToUnorderedAssociation,
  RulesToUDict   -> RulesToUnorderedAssociation,
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
  MapF           -> MapFirst,
  MapL           -> MapLast,
  MapM           -> MapMost,
  MapR           -> MapRest,

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
  ToRowVec       -> ToRowVector,
  ToColVec       -> ToColumnVector
];

(*************************************************************************************************)

DefineAliasRules[
  Rep            -> Replace,
  RepRep         -> ReplaceRepeated,
  RepAll         -> ReplaceAll,
  VecRep         -> VectorReplace
];

DefineAliasRules[
  Parts          -> ExtractIndices,
  Select1        -> SelectFirst,
  SelectIndex1   -> SelectFirstIndex,
  VectorIndex1   -> FirstVectorIndex,
  VectorIndex1Of -> FirstVectorIndexOf,
  DelCases       -> DeleteCases,
  DelNone        -> DeleteNone,
  DelNull        -> DeleteNull,
  DelDups        -> DeleteDuplicates,
  DelDupsBy      -> DeleteDuplicatesBy,
  LenRange       -> LengthRange,
  RangeLen       -> LengthRange
];

DefineAliasRules[
  At             -> Construct,
  Make           -> Construct,
  MakeSetD       -> MakeSetDelayed,
  MakeTagSetD    -> MakeTagSetDelayed,
  MakeUpSetD     -> MakeUpSetDelayed,
  HoldLen        -> HoldLength,
  Len            -> Length,
  Len2           -> Length2,
  LenN           -> LengthN,
  Dims           -> Dimensions
];

DefineAliasRules[
  SeqLen         -> SequenceLength,
  Seq0           -> SequenceNothing,
  Seq1           -> SequenceFirst,
  Seq2           -> SequenceSecond,
  Seq3           -> SequenceThird,
  SeqN           -> SequenceLast,
  SeqMost        -> SequenceMost,
  SeqRest        -> SequenceRest,
  SeqReverse     -> SequenceReverse,
  Seq12          -> SequenceFirstSecond,
  Seq21          -> SequenceSecondFirst
];

DefineAliasRules[
  Id             -> Identity,
  Rev            -> Reverse,
  IntDigits      -> IntegerDigits,
  IntStr         -> SafeIntegerString,
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
  StrSelect      -> StringSelect,
  StrDiscard     -> StringDiscard,
  StrSelectDiscard -> StringSelectDiscard,
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
  StrRep         -> StringReplace,
  StrRepPart     -> StringReplacePart,
  StrRev         -> StringReverse,
  StrSplit       -> StringSplit,
  StrPre         -> StringPrepend,
  StrApp         -> StringAppend
];

DefineAliasRules[
  RandAtom       -> RandomAtom,
  RandDatum      -> RandomDatum,
  RandSym        -> RandomSymbol,
  RandLet        -> RandomLetter,
  RandDec        -> RandomDecimal,
  RandBit        -> RandomBit,
  RandNat        -> RandomNatural,
  RandBool       -> RandomBoolean,
  RandSign       -> RandomUnitInteger,
  RandRange      -> RandomRange,
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
  WriteRawJSONFile              -> Developer`WriteRawJSONFile,
  WriteRawJSONStream            -> Developer`WriteRawJSONStream,
  CellInformation               -> Developer`CellInformation,
  ReadRawJSONString             -> Developer`ReadRawJSONString,
  WriteRawJSONString            -> Developer`WriteRawJSONString,
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
  PackedArrayType               -> Internal`PackedArrayType,
  PackedQ                       -> Developer`PackedArrayQ,
  PackedArrayQ                  -> Developer`PackedArrayQ
];

(* data structures *)
DefineAliasRules[
  StringHash                    -> Data`StringHash,
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
  OutermostToInnermost          -> Internal`OutermostToInnermost,
  InnermostToOutermost          -> Internal`InnermostToOutermost,
  TransposeInPlace              -> Internal`TransposeInPlace,
  ListJoin                      -> Internal`JoinOrFail
];

(* Internal` functions *)
DefineAliasRules[
  MapThreadMin                  -> Random`Private`MapThreadMin,
  MapThreadMax                  -> Random`Private`MapThreadMax,
  FastNumericIndices            -> Random`Private`PositionsOf
];

(* ToPackedTree: packing of tree into a special expression, not sure how we map elements yet *)
(* AbsDelta: auto-broadcasts Abs[#1 - #2]& *)
DefineAliasRules[
  AbsDelta                      -> NumericalMath`AbsoluteError,
  PackedTree                    -> NumericalMath`Derivatives`PackedExpression,
  ToPackedTree                  -> NumericalMath`Derivatives`ToPackedExpression,
  DimsTree                      -> NumericalMath`Derivatives`RaggedDimensions,
  DimsProd                      -> NumericalMath`Derivatives`NumberOfElements,
  FlatProd                      -> FlatProduct
];

(* Multisets[list, k] or [n, k] *)
DefineAliasRules[
  SplitLengths                  -> GroupTheory`Tools`PartitionRagged,
  Multisets                     -> GroupTheory`Tools`Multisets,
  MultiSubsets                  -> GroupTheory`Tools`MultiSubsets,
  ReplaceSuccessive             -> GroupTheory`Tools`ConsecutiveReplace,
  SublistPosition               -> GroupTheory`Tools`SublistPosition
];

(* expr stePartitionRagged;
expr steMultisets;
expr steMultiSubsets;
expr steGeneralizedTuples;
expr steConsecutiveReplace;
expr steIntegerPartitionCounts;
expr steSublistPosition;

 *)
DefineAliasRules[
  ThrowMsg                      -> ThrowErrorMessage,
  ErrorMsg                      -> ErrorMessage,
  ReturnMsg                     -> ReturnMessage
];

NatStr::negative = "Provided integer `` was negative."
General::intStrNotInteger = "Provided value `` was not an integer or list or association of these."

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
