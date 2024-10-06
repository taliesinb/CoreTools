SystemExports[
  "ControlFlow",
    IfAll, IfNone, IfAuto, IfInherited, IfMissing, IfFailed, IfCorrupt, IfInvalid,
  "MutatingFunction",
    SetAll, SetNone, SetAuto, SetFailed, SetMissing, SetInherited, SetScaledFactor
];

(*************************************************************************************************)

SetHoldA[SetAll, SetNone, SetAuto, SetFailed, SetMissing, SetInherited, SetScaledFactor]

SimpleMacroDefs[
  SetAll[lhs_, rhs_]          := If[lhs === All,       lhs = rhs, lhs],
  SetNone[lhs_, rhs_]         := If[lhs === None,      lhs = rhs, lhs],
  SetAuto[lhs_, rhs_]         := If[lhs === Automatic, lhs = rhs, lhs],
  SetFailed[lhs_, rhs_]       := If[FailureQ[lhs],     lhs = rhs, lhs],
  SetMissing[lhs_, rhs_]      := If[MissingQ[lhs],     lhs = rhs, lhs],
  SetInherited[lhs_, rhs_]    := If[lhs === Inherited, lhs = rhs, lhs],
  SetScaledFactor[lhs_, rhs_] := If[MatchQ[lhs, Scaled[_ ? NumericQ]], lhs //= First /* N; lhs *= rhs]
];

(*************************************************************************************************)

SetHoldA[IfAll, IfNone, IfAuto, IfInherited, IfMissing, IfFailed, IfCorrupt, IfInvalid]

SimpleMacroDefs[
  IfAll      [rhs_] := Replace[All       :> rhs], IfAll      [lhs_, rhs_] := Replace[lhs, All        :> rhs],
  IfNone     [rhs_] := Replace[None      :> rhs], IfNone     [lhs_, rhs_] := Replace[lhs, None       :> rhs],
  IfAuto     [rhs_] := Replace[Auto      :> rhs], IfAuto     [lhs_, rhs_] := Replace[lhs, Auto       :> rhs],
  IfInherited[rhs_] := Replace[Inherited :> rhs], IfInherited[lhs_, rhs_] := Replace[lhs, Inherited  :> rhs],
  IfMissing  [rhs_] := Replace[_Missing  :> rhs], IfMissing  [lhs_, rhs_] := Replace[lhs, _Missing   :> rhs],
  IfFailed   [rhs_] := Replace[$Failed   :> rhs], IfFailed   [lhs_, rhs_] := Replace[lhs, $Failed    :> rhs],
  IfCorrupt  [rhs_] := Replace[$Corrupt  :> rhs], IfCorrupt  [lhs_, rhs_] := Replace[lhs, $Corrupt   :> rhs],
  IfInvalid  [rhs_] := Replace[$Invalid  :> rhs], IfInvalid  [lhs_, rhs_] := Replace[lhs, $Invalid   :> rhs]
];
