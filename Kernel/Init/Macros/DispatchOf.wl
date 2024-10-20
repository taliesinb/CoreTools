SystemExports[
  "ScopingFunction",
    DispatchOf, ExtendDispatchOf
];

PackageExports[
  "MessageFunction", ThrowUnmatchedError
];

(*************************************************************************************************)

DispatchOf::badCaseDefinition = "Bad Case definition for ``.";

SetHoldA[DispatchOf, ExtendDispatchOf];

ComplexMacroDefs[DispatchOf,
  SetD[LHS_, DispatchOf[args___]] := attachedDispatchOf[LHS, True, Hold[pre], args]
];

ComplexMacroDefs[ExtendDispatchOf,
  SetD[LHS_, ExtendDispatchOf[args___]] := attachedDispatchOf[LHS, False, Hold[pre], args]
];

SetHoldC[attachedDispatchOf]

attachedDispatchOf[lhs_, excl_, pre_, arg_SetD] := attachedCaseOf[lhs, excl, pre, Then[arg]];
attachedDispatchOf[lhs_, excl_, pre_, Then[args__SetD, Null...]] := Module[
  {holds, sym},
  sym = PatHead @ lhs;
  holds = MapApply[HoldC, NoEval @ List @ args];
  res = holds /. $ :> lhs;
  If[excl,
    blank = ToBlankP @ sym;
    res = Join[res, Map[toUnmatched[sym], holds /. $ -> blank]];
  ];
  SetD @@@ res;
];

toUnmatched[Hold[sym_]][HoldC[lhs_, _]] := HoldC[LHS:lhs, ThrowUnmatchedError[sym, LHS]];
