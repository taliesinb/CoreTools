SystemExports[
  "ControlFlow", HoldMap, HoldScan, HoldHead, HoldHash, HoldApply, HoldTake, HoldTakeArgs, EvaluateMap
];

PackageExports[
  "ControlFlow", HMap, HoldMap2, HMap2, HScan, HHead, HHash, HApply, HTake, HTakeArgs, EvalMap,
  "ControlFlow", HMake, HUnmake, HCUnmake, HoldCMap, HoldCRaise, HoldCLower,
  "Function",    Unmake, Remake, HByteCount
];

(*************************************************************************************************)

DefineAliasRules[
  HMap       -> HoldMap,
  HMap2      -> HoldMap2,
  HScan      -> HoldScan,
  HHead      -> HoldHead,
  HHash      -> HoldHash,
  HApply     -> HoldApply,
  HTake      -> HoldTake,
  HTakeArgs  -> HoldTakeArgs,
  EvalMap    -> EvaluateMap,
  HByteCount -> HoldByteCount
];

(*************************************************************************************************)

HoldCMap[fn_, list_List] := HoldCRaise @ Map[fn, NoEval @ list];
HoldCRaise[list_List]    := Thread[list, HoldC];
HoldCLower[hold_HoldC]   := Thread[hold];

(*************************************************************************************************)

SetHoldA[HMake, HUnmake];
SetHoldC[HCUnmake];

HMake[f_, a___]                     := f[a];
HUnmake[h_[a___], w_:Hold]          := w[h, a];
HCUnmake[h_[a___], w_:HoldComplete] := w[h, a];

Unmake[h_[a___], w_:List] := w[h, a];
Remake[w_[h_, a___]]      := w[h[a]];

(**************************************************************************************************)

SetStrict @ SetHoldA[HoldApply]

HoldApply[f_, _[a___]] := f[a];

(**************************************************************************************************)

SetStrict @ SetHoldF[HoldTake, HoldTakeArgs];

HoldTake[e_, n_Int, fn_]      := Apply[fn, HoldTake[e, n]];
HoldTake[e_, n_Int]           := Take[HoldC @ e, All, UpTo @ n];
HoldTake[d:DictP, n_Int]      := At[HoldC, Take[EnsureODict @ d, UpTo @ n]];
HoldTake[e:AtomP, _Int]       := HoldC @ e;

HoldTakeArgs[e_, n_Int]       := HoldTakeArgs[e, n, HoldC];
HoldTakeArgs[e_, n_Int, fn_]  := Extract[NoEval @ e, Thread @ List @ Range @ n, fn];
HoldTakeArgs[d:DictP, n_Int, fn_] := Module[{bag = Bag[], i = 0},
  AssocScanWhileQ[d, Fn[If[++i <= n, StuffBag[bag, fn @ #]; True]]];
  BagPart[bag, All]
];
HoldTakeArgs[AtomP, _Int, _] := {};

(**************************************************************************************************)

HoldMap::usage = "HoldMap[fn$, args$] maps fn$ over args$ without evaluating args$.";
HoldScan::usage = "HoldScan[fn$, args$] maps fn$ over args$ without evaluating args$.";

SetStrict @ SetHoldC[HoldHead, HoldMap, HoldMap2, HoldScan, HoldHash]

HoldMap[f_, args_]                      := Map[f, Unevaluated[args]];
HoldMap[Function[body_], args_]         := Map[Function[Null, body, HoldAllComplete], Unevaluated[args]];
HoldMap[Function[args_, body_], args_]  := Map[Function[args, body, HoldAllComplete], Unevaluated[args]];

HoldMap2[f_, args_]                      := Map[f, Unevaluated[args], {2}];
HoldMap2[Function[body_], args_]         := Map[Function[Null, body, HoldAllComplete], Unevaluated[args], {2}];
HoldMap2[Function[args_, body_], args_]  := Map[Function[args, body, HoldAllComplete], Unevaluated[args], {2}];

HoldScan[f_, args_]                     := Scan[f, Unevaluated[args]];
HoldScan[Function[body_], args_]        := Scan[Function[Null, body, HoldAllComplete], Unevaluated[args]];
HoldScan[Function[args_, body_], args_] := Scan[Function[args, body, HoldAllComplete], Unevaluated[args]];

HoldHead[e_]      := Head[NoEval @ e];
HoldHead[e_, fn_] := Head[NoEval @ e, fn];

HoldHash[e_] := Hash @ NoEval @ e;

(**************************************************************************************************)

(* unlike Map, EvaluateMap[f, Hold[1,2,3]] actually does something *)
EvaluateMap[f_, args:ListDictP] := Map[f, args];
EvaluateMap[f_, h_[args___]]    := Apply[h, HoldMap[f, {args}]];
