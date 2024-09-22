SystemExports[
  "ControlFlow",
    SubAll, SubNone, SubAuto, SubInherited, SubMissing, SubFailed,
  "MutatingFunction",
    SetAll, SetNone, SetAuto, SetFailed, SetMissing, SetInherited, SetScaledFactor
];

(*************************************************************************************************)

SetHoldA[SetAll, SetNone, SetAuto, SetFailed, SetMissing, SetInherited, SetScaledFactor]

DefineSimpleMacro[SetAll,                   SetAll[lhs_, rhs_] :> If[lhs === All,       lhs = rhs, lhs]];
DefineSimpleMacro[SetNone,                 SetNone[lhs_, rhs_] :> If[lhs === None,      lhs = rhs, lhs]];
DefineSimpleMacro[SetAuto,                 SetAuto[lhs_, rhs_] :> If[lhs === Automatic, lhs = rhs, lhs]];
DefineSimpleMacro[SetFailed,             SetFailed[lhs_, rhs_] :> If[FailureQ[lhs],     lhs = rhs, lhs]];
DefineSimpleMacro[SetMissing,           SetMissing[lhs_, rhs_] :> If[MissingQ[lhs],     lhs = rhs, lhs]];
DefineSimpleMacro[SetInherited,       SetInherited[lhs_, rhs_] :> If[lhs === Inherited, lhs = rhs, lhs]];
DefineSimpleMacro[SetScaledFactor, SetScaledFactor[lhs_, rhs_] :> If[MatchQ[lhs, Scaled[_ ? NumericQ]], lhs //= First /* N; lhs *= rhs]];

(*************************************************************************************************)

(* TODO: rename these IfAll, IfNone, etc *)
SetHoldA[SubAll, SubNone, SubAuto, SubInherited, SubMissing, SubFailed]

DefineSimpleMacro[SubAll,          {SubAll      [rhs_] :> Replace[All       :> rhs], SubAll      [lhs_, rhs_] :> Replace[lhs, All        :> rhs]}];
DefineSimpleMacro[SubNone,         {SubNone     [rhs_] :> Replace[None      :> rhs], SubNone     [lhs_, rhs_] :> Replace[lhs, None       :> rhs]}];
DefineSimpleMacro[SubAuto,         {SubAuto     [rhs_] :> Replace[Auto      :> rhs], SubAuto     [lhs_, rhs_] :> Replace[lhs, Auto       :> rhs]}];
DefineSimpleMacro[SubInherited,    {SubInherited[rhs_] :> Replace[Inherited :> rhs], SubInherited[lhs_, rhs_] :> Replace[lhs, Inherited  :> rhs]}];
DefineSimpleMacro[SubMissing,      {SubMissing  [rhs_] :> Replace[_Missing  :> rhs], SubMissing  [lhs_, rhs_] :> Replace[lhs, _Missing   :> rhs]}];
DefineSimpleMacro[SubFailed,       {SubFailed   [rhs_] :> Replace[$Failed   :> rhs], SubFailed   [lhs_, rhs_] :> Replace[lhs, $Failed    :> rhs]}];
