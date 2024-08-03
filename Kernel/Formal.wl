SystemExports[
  "SymbolicHead",
    FormalTimes, FormalPlus, FormalPower,
    FormalOne, FormalZero,
    FormalFn,
  "Function",
    FormalDistribute,
  "Predicate",
    FormalPlusQ
];

PackageExports[
  "SymbolicHead",
    FTimes, FPlus, FPower, DistHold,
  "Symbol",
    FOne, FZero
];

(**************************************************************************************************)

(* rename FormalPlus, FormalTimes? *)

(* DefineAliasRules[
  FormalTimes  -> NonCommutativeMultiply,
  ATimes         -> NonCommutativeMultiply,
];
 *)

DefineAliasRules[
  FTimes -> FormalTimes,
  FPlus  -> FormalPlus,
  FPower -> FormalPower,
  FOne   -> FormalOne,
  FZero  -> FormalZero
];

SetAttributes[FormalPlus,  {Flat, Orderless}];
SetAttributes[FormalTimes, {Flat, Orderless}];

(**************************************************************************************************)

DefinePredicate1[FormalPlusQ]

FormalPlusQ[_FormalPlus] := True;

(**************************************************************************************************)

FormalFn[fn_][args___] := FormalDistribute @ NoEval @ fn[args];

(**************************************************************************************************)


CoreBoxes[FormalPlus[args__]]  := RiffledRowBox[$formalPlus][MapMakeBoxes[{args}]];
CoreBoxes[FormalTimes[args__]]   := RiffledRowBox[$formalTimes][MapMakeBoxes[{args}]];
CoreBoxes[FormalPower[a_, b_]]  := SuperscriptBox[MakeBoxes @ a, MakeBoxes @ b];

$formalPlus  := $formalPlus  = MarginBox[.5] @ StyleBox["\[RawPlus]", FontFamily -> "Roboto", FontSize -> (2 + Inherited), FontColor -> $DarkGray];
$formalTimes := $formalTimes = MarginBox[.5] @ StyleBox["\[Times]",   FontFamily -> "Roboto", FontSize -> (2 + Inherited), FontColor -> $DarkGray];

(**************************************************************************************************)

FormalDistribute::usage =
"FormalDistribute[expr$] takes any occurences of FormalPlus[$$] expressions, makes all possible choices
from their arguments, and produces a single FormalPlus[$$].
FormalDistribute[expr$, symbol$] treats symbol$ as the carrier of formal choices."

FormalDistribute[e_, h_Sym:FPlus] := iFormalDistribute[h, e];

(**************************************************************************************************)

iFormalDistribute[h_, expr_] := Which[
  VFreeQ[NoEval @ expr, h],    expr,
  HoldHasHeadQ[expr, h],       If[HoldFreeWithinQ[expr, h], expr, distHead0[h, expr]],
  True,                        Apply[h, distHead0[h, expr]]
];

(**************************************************************************************************)

DeclareHoldAll[DistHold, distHead0, collectHead, iFormalDistribute];

distHead0[h_, expr_] /; HoldHasHeadQ[expr, h] := distHead1[h, HoldApply[DistHold, expr]];
distHead0[h_, expr_] := distHead1[h, DistHold @ expr];

distHead1[h_, expr_] /; HoldFreeQ[expr, h] := expr;
distHead1[h_, expr_] := Module[{slots, sums, body},
  Collecting[{slots, sums}, body = expr /. e_h :> RuleEval @ collectHead[h, e, slots, sums]];
  distHead2[Construct[Fn, slots, body, HoldAllComplete], sums]
];

collectHead[h_, headExpr_, slots_, sums_] := With[
  {sym = Unique[]},
  slots @ sym;
  sums @ distHead1[h, HoldApply[DistHold, headExpr]];
  sym
]

distHead2[fn_, {}]             := fn[];
distHead2[fn_, {sum_DistHold}] := flattenDistHold @ EvaluateMap[fn, sum];
distHead2[fn_, sums_List]      := flattenDistHold @ MapApply[fn, Tuples[DistHold @@ sums]];

flattenDistHold[expr_List] := flattenDistHold[DistHold @@ expr];
flattenDistHold[expr_]     := Flatten[expr, Inf, DistHold];
