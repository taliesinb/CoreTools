PackageExports[
  "StrPatSym",

    LSpanS, TSpanS, DSpanS, DEdgeS, UEdgeS, FnS, RuleS, RuleDS,
    LDotsS, CDotsS, VDotsS, HLineS, VLineS, VSepS,
    Com, TabS, ColonS, SColonS,
    NL, NLNL, SQ, DQ, BS, BSSQ, BSDQ, BSBS,
    ComSpc, ComSpc1, ComSpc2, ComSpc3, ComSpc4,

    Spc, SpcNB,
    Spc0,  Spc1,  Spc2,  Spc3,  Spc4,  Spc5,
    SpcM0, SpcM1, SpcM2, SpcM3, SpcM4, SpcM5,

    DotS, CircS, DiskS, BullS, CompS, SCircS, SDiskS,
    ImpliesS, EqualS, NEqualS, CongS, NCongS, ApproxS, NotApproxS, IsoS, NotIsoS,

    LArr,  RArr,  UArr,  DArr,
    LMaps, RMaps, UMaps, DMaps,
    LArr2, LArrow, LArrow2,
    RArr2, RArrow, RArrow2,

    OrS, AndS, NandS, NorS, XorS, NotS, ForAllS, ExistsS,

    MinusS, PlusS, TimesS, CircDotS, CircPlusS, CircTimesS,
    SumS, ProdS, CoProdS, UnionS, InterS, SqrUnionS, SqrInterS,

    LBrace, RBrace, LParen, RParen, LBracket, RBracket, LFloor, RFloor, LCeil, RCeil, LAngle, RAngle, LSkel, RSkel, LDBracket, RDBracket, LAssoc, RAssoc,

  "Deprecated",
    LSpan, TSpan, Newline, DNewline, SQuote, DQuote, DQuoteEsc, Dots, CDots, VDots
];

(*************************************************************************************************)

(* symbol equivalents *)
DefineLiteralRules[
  LSpanS     -> "\[SpanFromLeft]",
  TSpanS     -> "\[SpanFromAbove]",
  DSpanS     -> "\[SpanFromBoth]",
  DEdgeS     -> "\[DirectedEdge]",
  UEdgeS     -> "\[UndirectedEdge]",
  FnS        -> "\[Function]",
  RuleS      -> "\[Rule]",
  RuleDS     -> "\[RuleDelayed]"
];

(* oriented things *)
DefineLiteralRules[
  LDotsS     -> "\[Ellipsis]",
  CDotsS     -> "\[CenterEllipsis]",
  VDotsS     -> "\[VerticalEllipsis]",
  HLineS     -> "\[HorizontalLine]",
  VLineS     -> "\[VerticalLine]",
  VSepS      -> "\[VerticalSeparator]"
];

(* punctuation *)
DefineLiteralRules[
  Com        -> ",",
  TabS       -> "\t",
  ColonS     -> ":",
  SColonS    -> ";",
  ComSpc     -> ", ",
  ComSpc1    -> ",\[VeryThinSpace]",
  ComSpc2    -> ",\[VeryThinSpace]\[VeryThinSpace]",
  ComSpc3    -> ",\[ThinSpace]",
  ComSpc4    -> ",\[MediumSpace]"
];

DefineLiteralRules[
  SQ         -> "'",
  DQ         -> "\"",
  BSSQ       -> "\\'",
  BS         -> "\\",
  BSDQ       -> "\\\"",
  BSBS       -> "\\\\",
  NL         -> "\n",
  NLNL       -> "\n\n"
];

(* spaces *)
DefineLiteralRules[
  Spc        -> " ",
  SpcNB      -> "\[NonBreakingSpace]",
  Spc0       -> "\[InvisibleSpace]",
  Spc1       -> "\[VeryThinSpace]",
  Spc2       -> "\[VeryThinSpace]\[VeryThinSpace]",
  Spc3       -> "\[ThinSpace]",
  Spc4       -> "\[MediumSpace]",
  Spc5       -> "\[ThickSpace]",
  SpcM0      -> "\[InvisibleSpace]",
  SpcM1      -> "\[NegativeVeryThinSpace]",
  SpcM2      -> "\[NegativeVeryThinSpace]\[NegativeVeryThinSpace]",
  SpcM3      -> "\[NegativeThinSpace]",
  SpcM4      -> "\[NegativeMediumSpace]",
  SpcM5      -> "\[NegativeThickSpace]"
];

(* icons *)
DefineLiteralRules[
  BulS       -> "\[Bullet]",
  DotS       -> "\[CenterDot]",
  CircS      -> "\[EmptyCircle]",
  DiskS      -> "\[FilledCircle]",
  CompS      -> "\[SmallCircle]",
  BullS      -> "\[Bullet]",
  SCircS     -> "\[EmptySmallCircle]",
  SDiskS     -> "\[FilledSmallCircle]"
];

(* brackets *)
DefineLiteralRules[
  LBrace     -> "{",
  RBrace     -> "}",
  LParen     -> "(",
  RParen     -> ")",
  LBracket   -> "[",
  RBracket   -> "]",
  LFloor     -> "\[LeftFloor]",
  RFloor     -> "\[RightFloor]",
  LCeil      -> "\[LeftCeiling]",
  RCeil      -> "\[RightCeiling]",
  LAngle     -> "\[LeftAngleBracket]",
  RAngle     -> "\[RightAngleBracket]",
  LSkel      -> "\[LeftGuillemet]",
  RSkel      -> "\[RightGuillemet]",
  LDBracket  -> "\[LeftDoubleBracket]",
  RDBracket  -> "\[RightDoubleBracket]",
  LAssoc     -> "\[LeftAssociation]",
  RAssoc     -> "\[RightAssociation]"
];

(* relations *)
DefineLiteralRules[
  ImpliesS   -> "\[Implies]",
  EqualS     -> "\[Equal]",
  NEqualS    -> "\[NotEqual]",
  CongS      -> "\[Congruent]",
  NCongS     -> "\[NotCongruent]",
  ApproxS    -> "\[TildeTilde]",
  NotApproxS -> "\[NotTildeTilde]",
  IsoS       -> "\[TildeFullEqual]",
  NotIsoS    -> "\[NotTildeFullEqual]"
];

(* arrows *)
DefineLiteralRules[
  LArr       -> "\[LeftArrow]",
  RArr       -> "\[RightArrow]",
  UArr       -> "\[UpArrow]",
  DArr       -> "\[DownArrow]",
  LMaps      -> "\[LeftArrowBar]",
  RMaps      -> "\[RightArrowBar]",
  UMaps      -> "\[UpArrowBar]",
  DMaps      -> "\[DownArrowBar]",
  LArr2      -> "\[DoubleLeftArrow]",
  RArr2      -> "\[DoubleRightArrow]",
  LArrow     -> "\[LongLeftArrow]",
  RArrow     -> "\[LongRightArrow]",
  LArrow2    -> "\[DoubleLongLeftArrow]",
  RArrow2    -> "\[DoubleLongRightArrow]"
];

(* logic *)
DefineLiteralRules[
  OrS        -> "\[Or]",
  AndS       -> "\[And]",
  NandS      -> "\[Nand]",
  NorS       -> "\[Nor]",
  XorS       -> "\[Xor]",
  NotS       -> "\[Not]",
  ForAllS    -> "\[ForAll]",
  ExistsS    -> "\[Exists]"
];

(* algebra *)
DefineLiteralRules[
  MinusS     -> "\[Minus]",
  PlusS      -> "\[RawPlus]",
  TimesS     -> "\[Times]",
  CircDotS   -> "\[CircleDot]",
  CircPlusS  -> "\[CirclePlus]",
  CircTimesS -> "\[CircleTimes]"
];

(* sets *)
DefineLiteralRules[
  SumS       -> "\[Sum]",
  ProdS      -> "\[Product]",
  CoProdS    -> "\[Coproduct]",
  UnionS     -> "\[Union]",
  InterS     -> "\[Intersection]",
  SqrUnionS  -> "\[SquareUnion]",
  SqrInterS  -> "\[SquareIntersection]"
];

(* legacy *)
DefineLiteralRules[
  LSpan      -> "\[SpanFromLeft]",
  TSpan      -> "\[SpanFromAbove]",
  Newline    -> "\n",
  DNewline   -> "\n\n",
  SQuote     -> "'",
  DQuote     -> "\"",
  DQuoteEsc  -> "\\\"",
  Dots       -> "\[Ellipsis]",
  CDots      -> "\[CenterEllipsis]",
  VDots      -> "\[VerticalEllipsis]"
];

