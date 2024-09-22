PackageExports[
  "StrPatternSymbol",
    Dots, CDots, VDots,
    LAssoc, LBrace, LParen, LAngle, LGAngle, LBracket,
    RAssoc, RBrace, RParen, RAngle, RGAngle, RBracket,
    Newline, DNewline,
    LSpan, TSpan,
    SQuote, DQuote, DQuoteEsc,

  "Symbol",
    EmptyDict, EmptyUDict, EmptyODict,
    Inf, PosInf, NegInf,
    Tau,  Root2,  Root3,
    NTau, NRoot2, NRoot3, NPi
];

(*************************************************************************************************)

(* TODO: add suffix of S for String *)
DefineLiteralRules[
  Dots       -> "\[Ellipsis]",
  CDots      -> "\[CenterEllipsis]",
  VDots      -> "\[VerticalEllipsis]",
  LSpan      -> "\[SpanFromLeft]",
  TSpan      -> "\[SpanFromAbove]",
  LBrace     -> "{",
  RBrace     -> "}",
  LParen     -> "(",
  RParen     -> ")",
  LAngle     -> "\[LeftAngleBracket]",
  RAngle     -> "\[RightAngleBracket]",
  LGAngle    -> "\[LeftGuillemet]",
  RGAngle    -> "\[RightGuillemet]",
  LBracket   -> "[",
  RBracket   -> "]",
  LAssoc     -> "\[LeftAssociation]",
  RAssoc     -> "\[RightAssociation]",
  Newline    -> "\n",
  DNewline   -> "\n\n",
  SQuote     -> "'",
  DQuote     -> "\"",
  DQuoteEsc  -> "\\\""
];

DefineLiteralRules[
  EmptyDict  -> Evaluate @ Dict[],
  EmptyUDict -> Evaluate @ UDict[],
  EmptyODict -> Evaluate @ ODict[]
];

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
