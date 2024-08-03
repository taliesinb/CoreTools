SystemExports[
  "Function",
    MapIndex, MapIndices, MapPart, MapExprPaths,
    MapEnds, MapFirst, MapLast, MapMost, MapRest, MapFirstRest, MapMostLast, MapFirstLast,
    MapCol, MapCol1, MapCol2, MapCol3,
    MapRow, MapRow1, MapRow2, MapRow3,
    ZipMap, ZipMapP, ZipScan, ZipScanP, Bimap, Comap,
    MaybeMap, MapFlip,
    MapP, ScanP, ScanApply,
    RangeArray, Dimension, IndexArray, IndexList,
    MapLeaves, ApplyLastAxis, MapLastAxis, MapAxis, MapAxisP, ScanAxisP, ApplyAxis,
    MapSeq,
    VectorReplace,
    UniqueValue, UniqueValueBy,
    MapValues, MapValuesP,
    ListDictMap,
  "MutatingFunction",
    PathScanP, PathMapP, PathScan, PathMap,
  "MessageFunction",
    EnsureNiceMessage
];

PackageExports[
  "Function",
    Map2, Dim, ScanIndexed,
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

DeclareCurry1[MapValues, MapValuesP]

MapValues[f_, list_List] := Map[f, list];
MapValues[f_, dict_Dict] := Map[f, Values @ dict];
MapValues[f_, expr_]     := Map[f, Level[expr, 1]];

MapValuesP[f_, list_List] := MapThread[f, {list, RangeLen @ list}];
MapValuesP[f_, dict_Dict] := MapThread[f, {Values @ dict, Keys @ dict}];
MapValuesP[f_, expr_]     := MapValuesP[f, Level[expr, 1]];

(*************************************************************************************************)

DeclareCurry12[ListDictMap]

ListDictMap[fl_, fd_, expr_List] := Map[fl, expr];
ListDictMap[fl_, fd_, expr_Dict] := KeyValueMap[fd, expr];
ListDictMap[_, _, expr_] := ErrorMsg[ListDictMap::notListOrAssociation, expr];
ListDictMap::notListOrAssociation = "`` must be a list or an association.";

(*************************************************************************************************)

DeclareCurry2[VectorReplace]

VectorReplace[vector_, rule_] := Replace[vector, rule, {1}];

(*************************************************************************************************)

MapIndex::usage =
"MapIndex[f, index$, expr$]` does `f @` selectively on a single index of expr$.
* index can be i$, 'str$', or Key[$$], or None.
* an index that doesn't exist is skipped.
* None does nothing.";

DeclareCurry12[MapIndex]

MapIndex = CaseOf[
  $[f_, None, expr_]           := expr;
  $[f_, p:OnePartSpecP, expr_] := FastQuietCheck[MapAt[f, expr, p], expr];
];

(*************************************************************************************************)

MapIndices::usage =
"MapIndices[f, indices$, expr$]` does `f @` selectively on indices of expr$.
* indices can be All, Span[$$], or List[p$1, p$2, $$].
* indices that don't exist are skipped.";

DeclareCurry12[MapIndices]

MapIndices = CaseOf[
  $[f_, {}, expr_]         := expr;
  $[f_, _, EmptyP]         := expr;
  $[f_, All, expr_]        := Map[f, expr];
  $[f_, inds_, expr_]      := fastMapPart[f, inds, expr];
];

fastMapPart[_, _, expr:EmptyP]    := expr; (* because e.g. MapAt[g, {}, 2 ;; -1] doesn't issue messages *)
fastMapPart[f_, inds_List, expr_] := FastQuietCheck[MapAt[f, expr, List /@ inds], slowMapPart[f, inds, expr]]
fastMapPart[f_, part_, expr_]     := FastQuietCheck[MapAt[f, expr, part], slowMapPart[f, part, expr]]

slowMapPart[f_, All, expr_]                       := Map[f, expr];
slowMapPart[f_, part:({__Int}|_Int|_Span), expr_] := MapAt[f, expr, List /@ ParsePart[part, Len @ expr]];
slowMapPart[_, _Key | _Str, expr_]                := expr;
slowMapPart[f_, parts_List, expr_]                := MapAt[f, expr, Select[parts, validPartQ[expr]]];
slowMapPart[f_, spec_, expr_]                     := (Message[General::invalidSubPart, spec]; expr);

General::invalidSubPart = "`` is not a valid part list specification."
validPartQ[e_][Key[k_]]          := DictQ[e] && KeyExistsQ[e, k];
validPartQ[e_][k_Str]            := DictQ[e] && KeyExistsQ[e, k];
validPartQ[e_][n_Int ? Positive] := 0 < n <= Len[e];
validPartQ[e_][n_Int ? Negative] := -Len[e] <= n < 0;
validPartQ[e_][spec_]            := (Message[General::invalidSubPart, spec]; False);

(*************************************************************************************************)

MapPart::usage =
"MapPart[f, part$, expr$]` does `f @` selectively on parts$.
* parts$ should be a list of part elements, each of which can be:
  * a scalar part specification: i$, 'str$', or Key[$$]
  * a vector part specification: All, Span[$$], List[p$1, p$2, $$]
* successive elements select deeper into expr$.
* an empty list yields `f$[expr$]`.
* parts that don't exist are skipped.
";

MapPart = CaseOf[
  $[f_, {}, expr_]                := f[expr];
  $[_, _List, expr_ ? EmptyQ]     := expr;
  $[f_,  p:{__Int}, expr_]        := FastQuietCheck[MapAt[f, expr, p], expr];
  $[f_, {part_}, expr_]           := fastMapPart[f, part, expr];
  $[f_, parts_List, expr_]        := fastMapPartDeep[f, parts, expr];
  $[f_, spec_, expr_]             := ErrorMsg["partNotList", spec];
];

MapPart::partNotList = "Part specification `` was not a list.";

fastMapPartDeep[_, _, expr:EmptyP] := expr;
fastMapPartDeep[f_, parts_, expr_] /; MemberQ[parts, _List] := slowMapPartDeep[f, Seq @@ parts] @ expr;
fastMapPartDeep[f_, parts_, expr_]            := FastQuietCheck[MapAt[f, expr, parts], slowMapPartDeep[f, Seq @@ parts] @ expr];
slowMapPartDeep[f_, part_][expr_]             := fastMapPart[f, part, expr];
slowMapPartDeep[f_, part_, partRest__][expr_] := fastMapPart[slowMapPartDeep[f, partRest], part, expr];

(*************************************************************************************************)

MapExprPaths::usage =
"MapExprPaths[f, ExprPath[$$], expr$]` does `f @` selectively on a path within expr.
MapExprPaths[f, {path$1, path$2, $$}, expr$]` does `f @` on several paths.
* parts that don't exist are skipped.";

MapExprPaths = CaseOf[
  $[_, _, expr_ ? EmptyQ]           := expr;
  $[f_, {}, expr_]                  := expr;
  $[f_, ExprPath[], expr_]          := f[expr];
  $[f_, ExprPath[part__], expr_]    := safeMapAt[f, {part}, expr];
  $[f_, {p_ExprPath}, expr_]        := $[f, p, expr];
  $[f_, parts:{__ExprPath}, expr_]  := safeMapAtMulti[f, List @@@ parts, expr];
];

safeMapAtMulti[f_, parts_, expr_] := FastQuietCheck[
  MapAt[f, expr, parts],
  Fold[e |-> safeMapAt[f, part, e], expr, parts];
];

(**************************************************************************************************)

DeclareCurry1[MapEnds, MapFirst, MapLast, MapMost, MapRest]

MapEnds[f_, expr_ ? EmptyQ]  := expr;
MapEnds[f_, expr_ ? SingleQ] := Map[f, expr];
MapEnds[f_, expr_] := MapAt[f, expr, {{1}, {-1}}];

MapFirst[f_, expr_] := safeMapAt[f, 1, expr];
MapLast[f_, expr_]  := safeMapAt[f, -1, expr];
MapMost[f_, expr_]  := safeMapAt[f, 1;;-2, expr];
MapRest[f_, expr_]  := safeMapAt[f, 2;; 1, expr];

DeclareCurry12[MapFirstRest, MapMostLast, MapFirstLast]

MapFirstRest[f_, g_, expr_] := safeMapAt[g, 2;;-1, safeMapAt[f, 1, expr]];
MapMostLast[f_, g_, expr_]  := safeMapAt[f, 1;;-2, safeMapAt[g, -1, expr]];
MapFirstLast[f_, g_, expr_] := MapFirst[f, If[Len[expr] === 1, expr, MapLast[g, expr]]];

safeMapAt[_, _, expr:EmptyP] := expr; (* because e.g. MapAt[g, {}, 2 ;; -1] doesn't issue messages *)
safeMapAt[f_, part_, expr_] := FastQuietCheck[MapAt[f, expr, part], expr];

(**************************************************************************************************)

DeclareCurry1[Map2]

Map2[f_, matrix_] := Map[f, matrix, {2}];

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

ZipMap[f_, a_]          := Map[f, a];
ZipMap[f_, a_, b_]      := EnsureNiceMessage @ MapThread[f, {a, b}];
ZipMap[f_, as__]        := EnsureNiceMessage @ MapThread[f, {as}];

ZipMapP[f_, a_]         := MapP[f, a];
ZipMapP[f_, a_, b_]     := EnsureNiceMessage @ MapThread[f, {a, b, Range @ Len @ a}];
ZipMapP[f_, a_, bs__]   := EnsureNiceMessage @ MapThread[f, {a, bs, Range @ Len @ a}];

ZipScan[f_, args___]    := ZipMap[NullifyFunction @ f, args];
ZipScanP[f_, args___]   := ZipMapP[NullifyFunction @ f, args];

(**************************************************************************************************)


General::invalidMapThread = "A malformed call to MapThread occurred: ``.";
General::mapThreadDimMismatch = "Length mismatch for MapThread: ``.";
EnsureNiceMessage[HoldP @ MapThread[_, args:{__List}]] :=
  ThrowMsg["mapThreadDimMismatch", Len /@ args];

General::mapThreadKeyMismatch = "Key mismatch for MapThread: ``.";
EnsureNiceMessage[HoldP @ MapThread[_, args:{__Dict}]] := Module[{keys = Keys /@ args},
  ErrorMsg["mapThreadKeyMismatch", Complement[Union @@ keys, Intersection @@ keys]]
];

General::mapThreadHeadMismatch = "Head mismatch for MapThread: ``.";
EnsureNiceMessage[HoldP @ MapThread[_, args_List]] :=
  ThrowMsg["mapThreadHeadMismatch", Head /@ args];

EnsureNiceMessage[m_MapThread] := ErrorMsg["invalidMapThread", InputForm @ Hold @ m];
EnsureNiceMessage[res_] := res;

(**************************************************************************************************)

DeclareCurry1[Comap, Bimap, MaybeMap, MapFlip]

Comap[fns_, arg_] := Map[fn |-> fn[arg], fns];
Comap[fns_, arg_, level_] := Map[fn |-> fn[arg], fns, level];

Bimap[f_, a_, b__] := EnsureNiceMessage @ MapThread[Construct, {f, a, b}];
Bimap[f_, a_]      := EnsureNiceMessage @ MapThread[Construct, {f, a}];

MaybeMap[f_, a:ListDictP] := Map[f, a];
MaybeMap[f_, a_]          := f @ a;

(* this is morally the same as MapThread, but communicates a different intent,
since it is not operating on a tuple of lists but a list of lists *)
MapFlip[fn_, list_ ? AnyMatrixQ] := Map[f, Flip @ list];
MapFlip[_, arg2_] := ErrorMsg[MapFlip::notRectangular, arg2];

MapFlip::notRectangular = "The second argument was not rectangular: ``."

(**************************************************************************************************)

DeclareCurry1[ScanIndexed]

(* TODO: there is a weakness here, which is that if the outer heads are held, the map
won't evaluate, and if they arne't, they *will* evaluate, with Nulls in them!
*)

ScanIndexed[f_, expr_]      := Module[{i = 1}, Scan[v |-> f[v, {i++}], expr]]
ScanIndexed[f_, assoc_Dict] := (AssocScanWhileQ[assoc, rule |-> Then[f[P2 @ rule, List @ P1 @ rule], True]];)

ScanIndexed[f_, expr_, level_] := Module[
  {posList = Position[expr, _, level, Heads -> False], i = 1},
  Scan[elem |-> f[elem, Part[posList, i++]]&, Level[expr, level, HoldComplete]]
];

(**************************************************************************************************)

DeclareCurry1[ScanP, MapP]

ScanP[f_, expr_]        := Module[{i = 1}, Scan[v |-> f[v, i++], expr]];
ScanP[f_, dict_Dict]    := (AssocScanWhileQ[dict, kvFn[f]];)
kvFn[f_][k_ -> v_] := Then[f[v, k], True];

MapP[f_, expr_]         := MapIndexed[{v, i} |-> f[v, P1 @ i], expr];
MapP[f_, dict_Dict]     := MapIndexed[{v, i} |-> f[v, P11 @ i], dict];

(**************************************************************************************************)

DeclareCurry1[ScanApply]

ScanApply[f_, expr_] := ThenNull @ MapApply[NullifyFunction @ f, expr];

(**************************************************************************************************)

DeclareHoldFirst @ DeclareCurry12[PathScanP, PathMapP, PathScan, PathMap]

PathScanP::usage =
"PathScanP[stackSymbol, fn, expr] is like ScanP, but appends the current part it is visiting onto stackSymbol."

(* TODO: rename this to StackMap *)
TopLevelEvaluateMacro[
PathScanP[s_, f_, expr_]     := Block[{s = Append[s, Null], i = 0}, Scan[v |-> f[v, PN[s] = ++i], expr]];
PathScanP[s_, f_, dict_Dict] := BlockAppend[s, Null, AssocScanWhileQ[dict, Apply[{k, v} |-> Then[PN[s] = k, f[v, k], True]]];];
PathMapP[s_, f_, expr_]      := BlockAppend[s, Null, MapIndexed[{v, i} |-> f[v, PN[s] = P1 @ i], expr]];
PathMapP[s_, f_, dict_Dict]  := BlockAppend[s, Null, MapIndexed[{v, i} |-> f[v, PN[s] = P11 @ i], dict]];
PathScan[s_, f_, expr_]     := Block[{s = Append[s, Null], i = 0}, Scan[v |-> f[PN[s] = ++i; v], expr]];
PathScan[s_, f_, dict_Dict] := BlockAppend[s, Null, AssocScanWhileQ[dict, Apply[{k, v} |-> Then[PN[s] = k, f[v], True]]];];
PathMap[s_, f_, expr_]      := BlockAppend[s, Null, MapIndexed[{v, i} |-> f[PN[s] = P1 @ i; v], expr]];
PathMap[s_, f_, dict_Dict]  := BlockAppend[s, Null, MapIndexed[{v, i} |-> f[PN[s] = P11 @ i; v], dict]];
];

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

(* TODO: check if depth reduced, message if so.
also, the old code was broken, so double check this now *)
toAxis[fn_, n_, arr_] := Locals[
  d = ArrayDepth[arr];
  t = MoveToPermutation[n -> d, d];
  Transpose[fn @ Transpose[arr, t], Ordering @ t]
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
