SystemExports[
  "MutatingFunction", SetInitial, SetDelayedInitial, SetCached
];

(**************************************************************************************************)

SetHoldA @ SetInitial

SimpleMacroDefs[
  SetInitial[lhs_Sym, rhs_]  := If[HasNoDefsQ[lhs], Set[lhs, rhs]],
  SetInitial[lhs__Sym, rhs_] := setMultiInitial[Hold[lhs], rhs]
];

SetHoldA @ setMultiInitial;

setMultiInitial[lhs_Hold, rhs2_] := Module[{rhs := (rhs = rhs2)},
  Scan[
    Function[sym, If[HasNoDefsQ[sym], Set[sym, rhs2]], HoldFirst],
    lhs
  ]
]

(**************************************************************************************************)

SetHoldA @ SetDelayedInitial

SimpleMacroDefs[
  SetDelayedInitial[lhs_, rhs_] := If[HasNoDefsQ[lhs], SetDelayed[lhs, rhs]]
]

(**************************************************************************************************)

SetHoldA @ SetCached

SimpleMacroDefs[
  SetCached[lhs_, rhs_] := SetDelayed[lhs, Set[lhs, rhs]]
]

