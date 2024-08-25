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

DefineComplexMacro[DispatchOf, {
  SetDelayed[lhs_, DispatchOf[args___]] :> attachedDispatchOf[lhs, True, Hold[pre], args]
}];

DefineComplexMacro[ExtendDispatchOf, {
  SetDelayed[lhs_, ExtendDispatchOf[args___]] :> attachedDispatchOf[lhs, False, Hold[pre], args]
}];

SetHoldC[attachedDispatchOf]

attachedDispatchOf[lhs_, excl_, pre_, arg_SetDelayed] := attachedCaseOf[lhs, excl, pre, CompoundExpression[arg]];
attachedDispatchOf[lhs_, excl_, pre_, CompoundExpression[args__SetDelayed, Null...]] := Module[
  {holds, sym},
  sym = PatHeadSym @ lhs;
  holds = MapApply[HoldComplete, NoEval @ List @ args];
  res = holds /. $ :> lhs;
  If[excl,
    blank = ToBlank @ sym;
    res = Join[res, Map[toUnmatched[sym], holds /. $ -> blank]];
  ];
  SetDelayed @@@ res;
];

toUnmatched[Hold[sym_]][HoldComp[lhs_, _]] := HoldComp[$LHS:lhs, ThrowUnmatchedError[sym, $LHS]];
