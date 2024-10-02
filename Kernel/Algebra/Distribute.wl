SystemExports[
  "Function",     FormalDistribute, HoldFormalDistribute
];

PrivateExports[
  "SymbolicHead", DHold, DIn, DOut
];

(**************************************************************************************************)

SetStrict @ FormalDistribute;

FormalDistribute::usage =
"FormalDistribute[expr$] takes any occurences of FormalPlus[$$] expressions, makes all possible choices
from their arguments, and produces a single FormalPlus[$$].
FormalDistribute[expr$, symbol$] treats symbol$ as the carrier of formal choices."

FormalDistribute[e_, h_Sym:FormalPlus] := ReleaseHold @ iFormalDistribute[e, h];

(**************************************************************************************************)

SetStrict @ SetHoldA @ HoldFormalDistribute;

HoldFormalDistribute::usage =
"HoldFormalDistribute[$$] is like FormalDistribute but returns an outer wrapper of
HoldComplete[$$] without evaluating the expression or its distributed forms.
."

HoldFormalDistribute[e_, h_Sym:FormalPlus] := iFormalDistribute[e, h];

(**************************************************************************************************)

SetHoldA[iFormalDistribute];

iFormalDistribute = CaseOf[
  $[e_, h_] /; HoldVFreeQ[e, h]     := HoldC[e];
  $[e_, h_] /; HoldArgsFreeQ[e, _h] := HoldC[e];
  $[e_, h_] := processResult[h, distribute[DIn[e] /. h -> DIn]]
];

processResult[h_, _]             := InternalError;
processResult[h_, DIn[a___DOut]] := Part[HoldC[h[a]], All, All, 1];

(**************************************************************************************************)

mapDIn[fn_, DIn[a___]] := Map[fn, NoEval @ a]

SetStrict[distribute, distribute1, expandChoices];
SetHoldC[distribute1];

distribute[din_DIn] := distribute0 @ Flatten[din, Inf, DIn];

distribute0[DIn[in_]] := Make[DIn, distribute1[in]];
distribute0[DIn[in___]] := Make[DIn, Map[distribute1, NoEval @ in]];

distribute1[in_DOut] := o;
distribute1[expr_] /; HoldVFreeQ[expr, DIn] := DOut[expr];
distribute1[expr_] := Module[
  {slots, choices, fnBody, fn, sym},
  Collecting[{slots, choices},
    fnBody = ReplaceAll[
      DOut[expr],
      in_DIn :> RuleEval[
        sym = Unique[];
        slots @ sym;
        choices @ distribute @ in;
        sym
      ]
    ]
  ];
  fn = Construct[Fn, slots, fnBody, HoldAllComplete];
  res = DIn @@ expandChoices[fn, choices];
  res
];

expandChoices = CaseOf[
  $[fn_, {}]             := {fn[]};
  (* $[fn_, {in_DIn}]       := List @ mapDIn[fn, in]; *)
  $[fn_, choices_List]   := MapApply[fn, Tuples @ Apply[DOut, choices]]
];

SetFlat @ SetHoldA[DIn, DOut];
