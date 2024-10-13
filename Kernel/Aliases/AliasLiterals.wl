PackageExports[
  "Symbol",
    EmptyList, EmptyDict, EmptyUDict, EmptyODict,
    Inf, PosInf, NegInf,
    Tau,  Root2,  Root3,
    NTau, NRoot2, NRoot3, NPi
];

(*************************************************************************************************)

DefineLiteralRules[
  EmptyList  -> List[],
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
