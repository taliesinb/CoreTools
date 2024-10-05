PackageExports[
  "ScopingFunction",  InheritedBlock, IBlock,
  "TypeHead",         Int, Rat, Sym, Str,
  "DataHead",         Dict, UDict, ODict, RuleD, DEdge, UEdge, DirInf, USet, OSet, MSet,
  "SeqFn",            Seq,
  "HoldHead",         HoldC, HoldComp, HoldM, SetM, SetDM, RuleDM, PrivHold, PrivSeq, PrivHoldSeq,
  "ControlFlow",      Fn, Then, Eval, NoEval, RuleEval, MaybeEval, FailEval,
  "SymbolicHead",     NCTimes,
  "Predicate",        PackedQ,
  "SpecialVariable",  $Fail,
  "Symbol",           Auto, Inherit, HInf, Tru, Fal
  "StrPatHead",       Regex, StrExpr,
  "MessageFunction",  MsgName,
  "MutatingFunction", SetD, TagSetD, UpSetD, AssocTo, SubFrom,
  "SlotSymbol",       FmA, FmB, FmC, FmD, FmE, FmF, FmG, FmH, FmI, FmJ, FmK, FmL, FmM, FmN, FmO, FmP, FmQ, FmR, FmS, FmT, FmU, FmV, FmW, FmX, FmY, FmZ
];

(*************************************************************************************************)

DefineAliasRules[
  ODict      -> Association,
  Dict       -> Association,
  UDict      -> Data`UnorderedAssociation,
  DirInf     -> DirectedInfinity
];

DefineAliasRules[
  NCTimes    -> NonCommutativeMultiply
];

DefineAliasRules[
  Sym        -> Symbol,
  Str        -> String,
  Int        -> Integer,
  Rat        -> Rational,
  RuleD      -> RuleDelayed,
  DEdge      -> DirectedEdge,
  UEdge      -> UndirectedEdge
];

DefineAliasRules[
  MSet       -> Multiset,
  USet       -> UnorderedSet,
  OSet       -> OrderedSet
];

DefineAliasRules[
  SetD       -> SetDelayed,
  TagSetD    -> TagSetDelayed,
  UpSetD     -> UpSetDelayed,
  AssocTo    -> AssociateTo,
  SubFrom    -> SubtractFrom
];

DefineAliasRules[
  PackedQ    -> Developer`PackedArrayQ
];

DefineAliasRules[
  Auto       -> Automatic,
  Inherit    -> Inherited,
  HInf       -> Infinity,
  Tru        -> True,
  Fal        -> False
];

DefineAliasRules[
  $Fail      -> $Failed
];

DefineAliasRules[
  MsgName    -> MessageName
];

DefineAliasRules[
  Regex      -> RegularExpression,
  StrExpr    -> StringExpression
];

DefineAliasRules[
  Then       -> CompoundExpression,
  Seq        -> Sequence
];

DefineAliasRules[
  PrivHold    -> PrivateHoldComplete,
  PrivSeq     -> PrivateSequence,
  PrivHoldSeq -> PrivateHoldCompleteSequence
];

DefineAliasRules[
  Eval       -> Evaluate,
  NoEval     -> Unevaluated
];

DefineAliasRules[
  MaybeEval  -> RuleCondition,
  RuleEval   -> RuleCondition,
  FailEval   -> Fail
];

NonCanonAliases @ DefineAliasRules[
  Fn         -> Function,
  HoldM      -> MacroHold,
  SetM       -> MacroSet,
  SetDM      -> MacroSetDelayed,
  RuleDM     -> MacroRuleDelayed,
  HoldComp   -> HoldComplete,
  HoldC      -> HoldComplete
];

NonCanonAliases @ DefineAliasRules[
  FmA        -> \[FormalCapitalA],
  FmB        -> \[FormalCapitalB],
  FmC        -> \[FormalCapitalC],
  FmD        -> \[FormalCapitalD],
  FmE        -> \[FormalCapitalE],
  FmF        -> \[FormalCapitalF],
  FmG        -> \[FormalCapitalG],
  FmH        -> \[FormalCapitalH],
  FmI        -> \[FormalCapitalI],
  FmJ        -> \[FormalCapitalJ],
  FmK        -> \[FormalCapitalK],
  FmL        -> \[FormalCapitalL],
  FmM        -> \[FormalCapitalM],
  FmN        -> \[FormalCapitalN],
  FmO        -> \[FormalCapitalO],
  FmP        -> \[FormalCapitalP],
  FmQ        -> \[FormalCapitalQ],
  FmR        -> \[FormalCapitalR],
  FmS        -> \[FormalCapitalS],
  FmT        -> \[FormalCapitalT],
  FmU        -> \[FormalCapitalU],
  FmV        -> \[FormalCapitalV],
  FmW        -> \[FormalCapitalW],
  FmX        -> \[FormalCapitalX],
  FmY        -> \[FormalCapitalY],
  FmZ        -> \[FormalCapitalZ]
];
