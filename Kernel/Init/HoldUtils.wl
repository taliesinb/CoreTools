PackageExports[
  "HoldFn",
    HoldTake, HoldTakeArgs, HoldMap2,
  "HoldFn",
    HTake, HTakeArgs,
    HUnmake, HCUnmake,
    HoldCMap, HoldCRaise, HoldCLower,
  "Function",
    Unmake, Remake
];

(*************************************************************************************************)

(* these are aliases for functions defined here *)
DefineAliasRules[
  HTake      -> HoldTake,
  HTakeArgs  -> HoldTakeArgs
];

(**************************************************************************************************)

SetHoldA[HoldMap2];

"HoldMap2[fn$, args$] maps fn$ over args$ without evaluating args$, at level 2."

HoldMap2[f_, args_]                      := Map[f, Unevaluated[args], {2}];
HoldMap2[Function[body_], args_]         := Map[Function[Null, body, HoldAllComplete], Unevaluated[args], {2}];
HoldMap2[Function[args_, body_], args_]  := Map[Function[args, body, HoldAllComplete], Unevaluated[args], {2}];

(*************************************************************************************************)

HoldCMap[fn_, list_List] := HoldCRaise @ Map[fn, NoEval @ list];
HoldCRaise[list_List]    := Thread[list, HoldC];
HoldCLower[hold_HoldC]   := Thread[hold];

(*************************************************************************************************)

Unmake[h_[a___], w_:List] := w[h, a];
Remake[w_[h_, a___]]      := w[h[a]];

(*************************************************************************************************)

SetHoldA[HUnmake];
SetHoldC[HCUnmake];

HUnmake[h_[a___],  w_:Hold]  := w[h, a];
HCUnmake[h_[a___], w_:HoldC] := w[h, a];

(**************************************************************************************************)

SetStrict @ SetHoldF[HoldTake, HoldTakeArgs];

HoldTake[e_, n_Int, fn_]     := Apply[fn, HoldTake[e, n]];
HoldTake[e_, n_Int]          := Take[HoldC @ e, All, UpTo @ n];
HoldTake[d:DictP, n_Int]     := At[HoldC, Take[EnsureODict @ d, UpTo @ n]];
HoldTake[e:AtomP, _Int]      := HoldC @ e;

HoldTakeArgs[e_, n_Int]      := HoldTakeArgs[e, n, HoldC];
HoldTakeArgs[e_, n_Int, fn_] := Extract[NoEval @ e, Thread @ List @ Range @ n, fn];
HoldTakeArgs[AtomP, _Int, _] := {};
HoldTakeArgs[d:DictP, n_Int, fn_] := Module[{bag = Bag[], i = 0},
  AssocScanWhileQ[d, Fn[If[++i <= n, StuffBag[bag, fn @ #]; True]]];
  BagPart[bag, All]
];

