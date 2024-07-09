SystemExports[
  "Function",
    AtIndices, AtPart,
    MapEnds, MapFirst, MapLast, MapMost, MapRest, MapFirstRest, MapMostLast, MapFirstLast,
    MapCol, MapCol1, MapCol2, MapCol3,
    MapRow, MapRow1, MapRow2, MapRow3,
    ZipMap, ZipScan, Bimap, Comap,
    MaybeMap,
    MapP, ScanP,
    RangeArray, Dimension, IndexArray, IndexList,
    MapLeaves, ApplyLastAxis, MapLastAxis, MapAxis, MapAxisP, ScanAxisP, ApplyAxis,
    MapSeq,
    VectorReplace,
    UniqueValue, UniqueValueBy,
    MapValues
];

PackageExports[
  "Function",
    Dim, ScanIndexed,
  "Operator",
    SeqLens, RevLens, IfLens, FlipLens, AxisLens
];

(*************************************************************************************************)

DeclareHoldRest[UniqueValue, UniqueValueBy];
DeclareHoldFirst[iUniqueValue];

UniqueValue[list_, else_:None]        := iUniqueValue[else, list];
UniqueValueBy[list_, fn_, else_:None] := iUniqueValue[else, fn /@ list];

iUniqueValue[else_, {}]        := else;
iUniqueValue[else_, {elem_}]   := elem;
iUniqueValue[else_, expr_]     := iUniqueValue[else, Args @ expr];
iUniqueValue[else_, list_List] := Replace[Union @ list, {{u_} :> u, _ :> else}];

(*************************************************************************************************)

DeclareCurry1[MapValues]

MapValues[f_, expr_List]  := Map[f, expr];
MapValues[f_, expr_Assoc] := Map[f, Values @ expr];
MapValues[f_, expr_]      := Map[f, Level[expr, 1]];

(*************************************************************************************************)

DeclareCurry2[VectorReplace]

VectorReplace[vector_, rule_] := Replace[vector, rule, {1}];

(**************************************************************************************************
`AtIndices[f, {i_1, i_2, ..},  {e_1, e_2, ..}]` does `f @` selectively on elements `e_{i_1}, e_{i_2}, ..`.
* indices that don't exist are skipped.
*)

(* this used to be called MapIndices *)

DeclareCurry12[AtIndices]

AtIndices[_, _, expr_ ? ZeroLenQ] := expr;

AtIndices[f_, {}, expr_] := expr;

AtIndices[f_, indices_, expr_] := AtPart[f, List /@ indices, expr];

(**************************************************************************************************
`AtPart[f, part, e]` does `f @` selectively on `part` of `e`.
* indices that don't exist are skipped.
*)

AtPart[_, _, expr_ ? ZeroLenQ] := expr;

AtPart[f_, part_, expr_] := FastQuietCheck[MapAt[f, expr, part], expr];

AtPart[f_, parts:{__List}, expr_] := FastQuietCheck[
  MapAt[f, expr, parts],
  Fold[e |-> FastQuietCheck[{e1, p} |-> MapAt[f, e1, p], e]&, expr, parts]
];

DeclareCurry1[MapEnds, MapFirst, MapLast, MapMost, MapRest]

MapEnds[f_, expr_]  := If[Length[expr] == 1, Map[f, expr], AtPart[f, {{1}, {-1}}, expr]];
MapFirst[f_, expr_] := AtPart[f, 1, expr];
MapLast[f_, expr_]  := AtPart[f, -1, expr];
MapMost[f_, expr_]  := AtPart[f, 1;;-2, expr];
MapRest[f_, expr_]  := AtPart[f, 2;; 1, expr];

DeclareCurry12[MapFirstRest, MapMostLast, MapFirstLast]

MapFirstRest[f_, g_, expr_] := AtPart[g, 2;;-1, AtPart[f, 1, expr]];
MapMostLast[f_, g_, expr_] := AtPart[f, 1;;-2, AtPart[g, -1, expr]];
MapFirstLast[f_, g_, expr_] := MapFirst[f, If[Len[expr] === 1, g, MapLast[g, expr]]];

(**************************************************************************************************)

DeclareCurry12[MapCol]
DeclareCurry1[MapCol1, MapCol2, MapCol3];

MapCol[f_, n_Integer, arr_] := MapAt[f, arr, {All, n}];

MapCol1[f_, arr_] := MapAt[f, arr, {All, 1}];
MapCol2[f_, arr_] := MapAt[f, arr, {All, 2}];
MapCol3[f_, arr_] := MapAt[f, arr, {All, 3}];

(**************************************************************************************************)

DeclareCurry12[MapRow]
DeclareCurry1[MapRow1, MapRow2, MapRow3];

MapRow[f_, n_Integer, arr_] := MapAt[f, arr, {n, All}];

MapRow1[f_, arr_] := MapAt[f, arr, {1, All}];
MapRow2[f_, arr_] := MapAt[f, arr, {2, All}];
MapRow3[f_, arr_] := MapAt[f, arr, {3, All}];

(**************************************************************************************************)

ZipMap[f_, a_]     := Map[f, a];
ZipMap[f_, a_, b_] := MapThread[f, {a, b}];
ZipMap[f_, as__]   := MapThread[f, {as}];

ZipScan[f_, args___] := ZipMap[NullifyFunction @ f, args];

(**************************************************************************************************)

DeclareCurry1[Comap, Bimap, MaybeMap]

Comap[fns_, arg_] := Map[fn |-> fn[arg], fns];
Comap[fns_, arg_, level_] := Map[fn |-> fn[arg], fns, level];

Bimap[f_, a_, b__] := MapThread[Construct, {f, a, b}];
Bimap[f_, a_]      := MapThread[Construct, {f, a}];

MaybeMap[f_, a:ListDictP] := Map[f, a];
MaybeMap[f_, a_]          := f @ a;

(**************************************************************************************************)

DeclareCurry1[ScanIndexed]

(* TODO: there is a weakness here, which is that if the outer heads are held, the map
won't evaluate, and if they arne't, they *will* evaluate, with Nulls in them!
*)

ScanIndexed[f_, expr_] := Module[{i = 1}, Scan[v |-> f[v, {i++}], expr]]
ScanIndexed[f_, assoc_Association] := Association`ScanWhile[assoc, scanIFnA[f]];
scanIFnA[f_][k_ -> v_] := (f[v, {k}]; True)

ScanIndexed[f_, expr_, level_] := Module[
  {posList = Position[expr, _, level, Heads -> False], i = 1},
  Scan[f[#1, Part[posList, i++]]&, Level[expr, level, HoldComplete]]
];

(**************************************************************************************************)

DeclareCurry1[ScanP]

ScanP[f_, elems_] := Module[{i = 1}, Scan[v |-> f[v, i++], elems]];
ScanP[f_, assoc_Association] := (Association`ScanWhile[assoc, scanPFnA[f]];)
scanPFnA[f_][k_ -> v_] := (f[v, k]; True)

(**************************************************************************************************)

DeclareCurry1[MapP]

MapP[f_, elems_]            := MapIndexed[mapPFnE[f], elems];
MapP[f_, assoc_Association] := MapIndexed[mapPFnA[f], assoc];
mapPFnE[f_][v_, i_] := f[v, First @ i];
mapPFnA[f_][v_, k_] := f[v, First @ First @ k];

(**************************************************************************************************)

RangeArray[n_Integer]               := Range[n];
RangeArray[ns:{__Integer}]          := Array[List, ns];

DeclareCurry2[Dimension, IndexArray, IndexList]

Dimension[All, array_]                   := Dimensions @ array;
Dimension[n_Integer, array_]             := Part[Dimensions[array], n];
Dimension[n_Integer ? Positive, array_]  := Last @ Dimensions[array, n];
Dimension[ns:{__Integer}, array_]        := Part[Dimensions[array, Max @ ns], ns];

IndexArray[spec_, array_]           := RangeArray @ Dimension[spec, array];

IndexList[n_Integer, array_]        := Range @ Dimension[n, array];
IndexList[spec:{__Integer}, array_] := Tuples @ Dimension[spec, array];

DefineAliasRules[Dim -> Dimension]

(**************************************************************************************************)

DeclareCurry1[MapLeaves, ApplyLastAxis, MapLastAxis]

MapLeaves[f_, arr_]     := Map[f, arr, {-1}];
ApplyLastAxis[f_, arr_] := Apply[f, arr, {-2}];
MapLastAxis[f_, arr_]   := Map[f, arr, {-2}];

(**************************************************************************************************)

DeclareCurry12[MapAxis, MapAxisP, ScanAxisP, ApplyAxis]

prep[e_, p_] := Prepend[p, e];

MapAxis[f_, n_ ? Negative, arr_] := Map[f, arr, {n}];
MapAxis[f_, n_, arr_]   := toAxis[Map[f, #, {-2}]&, n, arr];
MapAxisP[f_, n_, arr_]  := toAxis[MapIndexed[prep /* f, #, {-2}]&, n, arr];

ApplyAxis[f_, n_, arr_] := toAxis[Apply[f, #, {-2}]&, n, arr];

toAxis[fn_, n_, arr_] := Locals[
  a = ArrayDepth[a];
  t = MoveToPermutation[n -> a];
  Transpose[fn @ Transpose[a, t], Ordering @ t]
];

ScanAxisP[f_, n_, arr_]  := toAxis[ScanIndexed[prep /* f, #, {-2}]&, n, arr];

(**************************************************************************************************)

MapSeq[f_, arg_] := f[arg];
MapSeq[f_, args___] := Map[f, Unevaluated @ args];

(**************************************************************************************************)

(* similar to GU's Parts lenses.
TODO: figure out how we can keep a windowed array
 *)

SeqLens[f_][a___]  := Apply[Sequence, f @ {a}];
RevLens[f_][a_]    := Reverse @ f @ Reverse @ a;
IfLens[p_, f_][a_] := If[p[a], f[a], a];
FlipLens[f_][a_]  := Transpose @ f @ Transpose @ a;
AxisLens[f_, i_Integer][a_] := MapAxis[f, i, a];
