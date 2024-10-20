SystemExports[
  "ControlFlow", IfAll, IfNone, IfNull, IfAuto, IfDefault, IfFailed, IfMissing, IfInherited, IfScaled, IfFailed, IfCorrupt, IfInvalid,
  "MutFn",       SetAll, SetNone, SetNull, SetAuto, SetDefault, SetFailed, SetMissing, SetInherited, SetScaled
];

PackageExports[
  "ControlFlow", IfInherit,
  "MutatingFn",  SetInherit
];

(*************************************************************************************************)

DeclaredHere @ SetInherited;

SetHoldA[SetAll, SetNone, SetNull, SetAuto, SetDefault, SetFailed, SetMissing, SetInherit, SetScaled]

SimpleMacroDefs[
  SetAll     [lhs_, rhs_] := If[lhs === All,      lhs = rhs, lhs],
  SetNone    [lhs_, rhs_] := If[lhs === None,     lhs = rhs, lhs],
  SetNull    [lhs_, rhs_] := If[lhs === Null,     lhs = rhs, lhs],
  SetAuto    [lhs_, rhs_] := If[lhs === Auto,     lhs = rhs, lhs],
  SetInherit [lhs_, rhs_] := If[lhs === Inherit,  lhs = rhs, lhs],
  SetDefault [lhs_, rhs_] := If[lhs === DefV, lhs = rhs, lhs],
  SetFailed  [lhs_, rhs_] := If[FailureQ[lhs],    lhs = rhs, lhs],
  SetMissing [lhs_, rhs_] := If[MissingQ[lhs],    lhs = rhs, lhs],
  SetScaled  [lhs_, rhs_] := If[MatchQ[lhs, Scaled[_ ? NumericQ]], lhs //= First /* N; lhs *= rhs]
];

(*************************************************************************************************)

DeclaredHere @ IfInherited;

SetHoldA[IfAll, IfNone, IfNull, IfAuto, IfDefault, IfFailed, IfMissing, IfInherit, IfScaled, IfFailed, IfCorrupt, IfInvalid]

SimpleMacroDefs[
  IfAll     [rhs_] := Replace[All      :> rhs], IfAll     [lhs_, rhs_] := Replace[lhs, All        :> rhs],
  IfNone    [rhs_] := Replace[None     :> rhs], IfNone    [lhs_, rhs_] := Replace[lhs, None       :> rhs],
  IfNull    [rhs_] := Replace[Null     :> rhs], IfNull    [lhs_, rhs_] := Replace[lhs, Null       :> rhs],
  IfAuto    [rhs_] := Replace[Auto     :> rhs], IfAuto    [lhs_, rhs_] := Replace[lhs, Auto       :> rhs],
  IfInherit [rhs_] := Replace[Inherit  :> rhs], IfInherit [lhs_, rhs_] := Replace[lhs, Inherited  :> rhs],
  IfDefault [rhs_] := Replace[DefV     :> rhs], IfDefault [lhs_, rhs_] := Replace[lhs, DefV       :> rhs],
  IfMissing [rhs_] := Replace[_Missing :> rhs], IfMissing [lhs_, rhs_] := Replace[lhs, _Missing   :> rhs],
  IfFailed  [rhs_] := Replace[$Failed  :> rhs], IfFailed  [lhs_, rhs_] := Replace[lhs, $Failed    :> rhs],
  IfCorrupt [rhs_] := Replace[$Corrupt :> rhs], IfCorrupt [lhs_, rhs_] := Replace[lhs, $Corrupt   :> rhs],
  IfInvalid [rhs_] := Replace[$Invalid :> rhs], IfInvalid [lhs_, rhs_] := Replace[lhs, $Invalid   :> rhs]
];
