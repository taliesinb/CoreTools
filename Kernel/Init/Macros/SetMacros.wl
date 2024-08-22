SystemExports[
  "MutatingFunction", SetInitial, SetDelayedInitial, SetCached
];

(**************************************************************************************************)

SetHoldA @ SetInitial

DefineSimpleMacro[SetInitial, {
  SetInitial[lhs_Sym, rhs_]     :> If[System`Private`HasNoEvaluationsQ[lhs], Set[lhs, rhs]],
  SetInitial[lhs__Sym, rhs_]    :> setMultiInitial[Hold[lhs], rhs]
}]

SetHoldA @ setMultiInitial;

setMultiInitial[lhs_Hold, rhs2_] := Module[{rhs := (rhs = rhs2)},
  Scan[
    Function[sym, If[System`Private`HasNoEvaluationsQ[sym], Set[sym, rhs2]], HoldFirst],
    lhs
  ]
]

(**************************************************************************************************)

SetHoldA @ SetDelayedInitial

DefineSimpleMacro[SetDelayedInitial,
  SetDelayedInitial[lhs_, rhs_] :> If[System`Private`HasNoEvaluationsQ[lhs], SetDelayed[lhs, rhs]]
]

(**************************************************************************************************)

SetHoldA @ SetCached

DefineSimpleMacro[SetCached,
  SetCached[lhs_, rhs_] :> SetDelayed[lhs, Set[lhs, rhs]]
]

