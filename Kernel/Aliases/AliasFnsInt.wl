PackageExports[
  "ControlFlow", WithTimestampsPreserved,
  "Predicate",   HasLValFnQ,
  "MetaFn",      SetLValFn,
  "ControlFlow", LValEval,
  "TagSymbol",   LValOk, LValFail,
  "HoldFn",      LValParts, LValHead,
  "Function",    PatComp
];

(*************************************************************************************************)

LValEval::usage = "LValEval[a = b] gives None if no mut handler on a, $Failed if it failed, or HoldComplete[expr] for its result."

DefineAliasRules[
  HasLValFnQ     -> Language`HasMutationHandlerQ,
  SetLValFn      -> Language`SetMutationHandler,
  LValEval       -> Language`HandleMutation,
  LValOk         -> Language`MutationFallthrough,
  LValFail       -> Fail,
  LValParts      -> Language`DestructureLValue,
  LValHead       -> Language`GetLValueSymbol
];

(*************************************************************************************************)

PatComp::usage = "PatComp[p1, p2] returns 'Identical', 'Equivalent', 'Specific', 'Disjoint', 'Incomporable', 'Error'."

DefineAliasRules[
  WithTimestampsPreserved -> Internal`WithTimestampsPreserved,
  PatComp                 -> Internal`ComparePatterns
]
