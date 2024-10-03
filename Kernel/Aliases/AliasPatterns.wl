PackageExports[
  "PatternHead",
    Alt, HoldP, BlankSeq, BlankNullSeq, Opt, PatRep,
    Blank1, Blank2, Blank3, Blank01, Blank02, Blank12, Blank13, Blank23, BlankSeq2, BlankSeq3,
    VPattern, VCondition, VPatternTest, VBlank, VBlankSeq, VBlankNullSeq, VAlt, VRepeated, VExcept, VVerbatim, VHoldP,
    VFn, VFunction, VSet, VSetD, VTagSetD, VRule, VRuleD,

  "PatternSymbol",
    PatternP,

    SingleP, PairP, TripleP,
    List1P, List2P, List3P, List4P, List23P,
    DictP, UDictP, ODictP, ListDictP, EmptyP, NonEmptyP, EmptyDataP, AtomP, BoolP, SymP, StrP, SymStrP,
    DatumP, ColorP, SideP, ExtSideP, PosDollarP, DollarP, AnonFnP,
    AtomFormP, CompoundFormP,
    FormalSymP, UserSymP, InertSymP, SystemSymP,

    PiP, TauP, InfP, MathSymP, ExtMathSymP,
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

    Zero2P, PosInt2P, Nat2P, Int2P, Num2P, Num3P, Num23P,
    Pos2P, Pos2ListP, Pos2ListsP, Pos2PairP,
    Pos3P, Pos3ListP, Pos3ListsP, Pos3PairP,
    PosAP, PosAListP, PosAListsP, PosAPairP,

    BoolOrVecP, SymOrVecP, IntOrVecP, NatOrVecP, RealOrVecP, RuleOrVecP, RuleLOrVecP, ORuleOrVecP,
    ListVecP, DictVecP, BoolVecP, SymVecP, StrVecP, PairVecP, IntVecP, NatVecP, PosIntVecP, RealVecP, NumVecP, ExtNumVecP,
    NEListVecP, NEDictVecP, NEBoolVecP, NESymVecP, NEStrVecP, NEPairVecP, NEIntVecP, NENatVecP, NEPosIntVecP, NERealVecP, NENumVecP,
    NEListP, NEDictP, NEListDictP,

    RuleP,    RuleLP,    ORuleP,    SetLP,    DelayP,
    RuleSeqP, RuleLSeqP, ORuleSeqP, SetLSeqP, DelaySeqP,
    RuleVecP, RuleLVecP, ORuleVecP, SetLVecP, DelayVecP,
    RuleLSymP, SetLSymP, DelaySymP,
    DictLikeP, DictLP
];

PrivateExports[
  "PatternSymbol", OnePartSpecP, MultiPartSpecP, ExtPartSpecP
];

(*************************************************************************************************)

DefineAliasRules[
  Alt              -> Alternatives,
  HoldP            -> HoldPattern,
  BlankSeq         -> BlankSequence,
  BlankNullSeq     -> BlankNullSequence,
  Opt              -> Optional,
  PatRep           -> Repeated
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
  VSetD            -> HoldPattern[SetD],
  VTagSetD         -> HoldPattern[TagSetD],
  VRuleD           -> HoldPattern[RuleD],
  VRule            -> HoldPattern[Rule]
];

DefinePatternRules[
  PatternP -> Alt[
    _Pattern, _Alt,
    _PatternTest, _Condition,
    _Blank, _BlankSeq, _BlankNullSeq,
    _Repeated, _RepeatedNull, _Longest, _Shortest,
    _Except, _HoldPattern,
    _Verbatim, _Optional
  ]
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
  SymP             -> _Sym ? HAtomQ,
  StrP             -> _Str ? HAtomQ,
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
  AtomFormP        -> Alt[_Image, _Graph, (_Sym ? AtomFormHeadQ)[___]],
  CompoundFormP    -> Alt[
    _Row, _Column, _Grid, _Pane, _Framed, _Labeled,
    _EventHandler, _Annotation, _Tooltip, _Style,
    (_Sym ? CompoundFormHeadQ)[___]
  ]
];

DefinePatternRules[
  FormalSymP       -> _Sym ? FormalSymbolQ,
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
  MathSymP         -> Alt[Plus, Times, Minus, Subtract, Divide, Power],
  ExtMathSymP      -> Alt[Re, Im, Arg, Abs, Root, CubeRoot, Surd, Sqrt, Exp, Log, Log10, Log2, NCTimes]
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
  Int2P            -> {IntP, IntP},
  Nat2P            -> {NatP, NatP},
  Num2P            -> Alt[List2P  ? PackedQ, {_ ? NumQ, _ ? NumQ}],
  Num3P            -> Alt[List3P  ? PackedQ, {_ ? NumQ, _ ? NumQ, _ ? NumQ}],
  Num23P           -> Alt[List23P ? PackedQ, {Repeated[_ ? NumQ, {2, 3}]}]
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
  BoolOrVecP       -> Alt[BoolP,  _List ? BoolVecQ],
  SymOrVecP        -> Alt[SymP,   _List ? SymVecQ],
  IntOrVecP        -> Alt[IntP,   _List ? IntVecQ],
  NatOrVecP        -> Alt[NatP,   _List ? NatVecQ],
  RealOrVecP       -> Alt[RealP,  _List ? RealVecQ],
  RuleOrVecP       -> Alt[RuleP,  _List ? RuleVecQ],
  RuleLOrVecP      -> Alt[RuleLP, _List ? RuleLVecQ],
  ORuleOrVecP      -> Alt[ORuleP, _List ? ORuleVecQ]
];

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
   RuleLP          -> Alt[_Rule, _RuleD],
   ORuleP          -> Alt[Rule|RuleD][SymStrP, _],
    SetLP          -> Alt[_Set, _SetD],
   DelayP          -> Alt[_RuleD, _SetD, _TagSetD, _UpSetD]
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
