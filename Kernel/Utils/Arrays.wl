SystemExports[
  "Function",
    Length2, LengthN, SumNormalize, ToStochasticArray,
    ToPackedReals, ToPackedInts, EnsurePacked, EnsurePackedReals, EnsurePackedInts,
    ThreadPlus, ThreadTimes, ThreadSubtract, ThreadDivide, ThreadAnd, ThreadOr, ThreadNot, ThreadMin, ThreadMax,
    ThreadLess, ThreadLessEqual, ThreadGreater, ThreadGreaterEqual, ThreadEqual, ThreadUnequal, ThreadSame, ThreadUnsame,
    Zip, Flip,
    MoveAxis, DotAxis,
    MoveToPermutation,
    FormalSymbolArray,
    SparseRules, SparseRows, SparseColumns,
    NDistMatrix, DistMatrix, NSqDistMatrix, SqDistMatrix, MinimumDistance,
    Ones, Zeros, Eye,
    ToRowVector, ToColumnVector,
    FlatSum, FlatProduct,
  "ControlFlowFunction",
    ArrayTable,
  "SymbolicHead",
    Broadcast,
  "Predicate",
    BroadcastQ, BMatchQ, BTrueQ, strongDimsSeq,
  "Function",
    ToBroadcast, FromBroadcast,
    ToBroadcastRows, FromBroadcastRows,
    BroadcastAt, BroadcastMap
];

PackageExports[
  "MessageFunction",     ThrowRealArrayMsg,
  "Head",                SymbolicDot,
  "Function",            Dots,
  "SymbolicHead",        Broad,
  "Predicate",           BroadQ, BSameShapeQ,
  "Function",            BDims, LikeB, BAt, BMap, FromB, ToB, ToBN, BLen, BLike,
  "ControlFlowFunction", BSeq, BVal, BSeqN
];

(**************************************************************************************************)

FlatSum[e_]     := Total[e, Infinity];
FlatProduct[e_] := Apply[Times, e, All]

(**************************************************************************************************)

DefineAliasRules[
  Broad    -> Broadcast,
  BroadQ   -> BroadcastQ,
  FromB    -> FromBroadcast,
  ToB      -> ToBroadcast,
  BAt      -> BroadcastAt,
  BMap     -> BroadcastMap
];

(**************************************************************************************************)

SetPred1 @ BroadcastQ;

BroadcastQ[_Broadcast] := True

(**************************************************************************************************)

ToBroadcast[list_List, fn_:Id]     := If[AllSameQ @ list, Broadcast[First @ list, Len @ list], fn @ list];
ToBroadcast[b_Broadcast, Blank01]  := b;

FromBroadcast[list_, Blank01]      := list;
FromBroadcast[Broadcast[b_], n_]   := ConstList[b, n];
FromBroadcast[Broadcast[b_, n_]]   := ConstList[b, n];

(**************************************************************************************************)

BSeq[]              := Seq[];
BSeq[Broad[b_, n_]] := ConstList[b, n];
BSeq[Broad[_]]      := InternalError;
BSeq[Broad[b1_, n_], Broad[b2_, n_]] := Seq[ConstList[b1, n], ConstList[b2, n]];
BSeq[Broad[b1_],     Broad[b2_, n_]] := Seq[ConstList[b1, n], ConstList[b2, n]];
BSeq[Broad[b1_, n_], Broad[b2_]]     := Seq[ConstList[b1, n], ConstList[b2, n]];
BSeq[a_List]        := a;
BSeq[ms___]         := Map[ToBN[BLen @ ms], NoEval @ ms];

(**************************************************************************************************)

ToBN[n_][Broad[b_]]     := ConstList[b, n];
ToBN[n_][Broad[b_, n_]] := ConstList[b, n];
ToBN[n_][a_List] /; Len[a] == n := a;
ToBN[n_][e_]            := e;

(**************************************************************************************************)

BVal[b_Broad]       := P1 @ b;
BVal[b__Broad]      := SeqCol1 @ b;
BVal[_]             := InternalError;

BLen[Broad[_]]      := None;
BLen[Broad[_, n_]]  := n;
BLen[ms___]         := ToUnique[weakDimsSeq @ ms, InternalError, None];
_BLen               := None;

(**************************************************************************************************)

SetPred1 @ BSameShapeQ;

BSameShapeQ[]      := True;
BSameShapeQ[_]     := True;
BSameShapeQ[ms___] := ToUnique[weakDims @ {ms}, False, False];

(**************************************************************************************************)

weakDimsSeq[a_]   := List @ weakDims @ a;
weakDimsSeq[a___] := Map[weakDims, {a}];
weakDims = CaseOf[
  Broad[_]     := Nothing;
  Broad[_, n_] := n;
  a_List       := Len @ a;
  _            := InternalError
];

strongDimsSeq[a_]   := List @ strongDims @ a;
strongDimsSeq[a___] := Map[strongDims, {a}];
strongDims = CaseOf[
  Broad[_]     := None;
  Broad[_, n_] := n;
  a_List       := Len @ a;
  _            := InternalError
];

(**************************************************************************************************)

BLike[d_, Broad[_]]     := Broad[d];
BLike[d_, Broad[_, n_]] := Broad[d, n];
BLike[d_, a_List]       := Broad[d, Len @ a];
BLike[d_, ms__]         := Broad[d, BLen @ ms];
BLike[d_]               := Broad[d];

(**************************************************************************************************)

SetPred1 @ BroadcastQ;

BroadcastQ[_Broadcast] := True

BAt = CaseOf[
  $[f_Broad]            := MapF[At, f];
  $[f_Broad, bs__Broad] := BLike[At @ BVal[f, bs], f, bs];
  $[f_Broad, ms__]      := ZipMap[BVal @ f, BSeq @ ms];
  $[f_, b1_Broad]       := MapF[f, b1];
  $[f_, bs__Broad]      := BLike[f @ BVal @ bs, bs];
  $[f_, bs__]           := Map[f, BSeq @ bs];
];

f_Broad[args___] := BAt[f, args];

BMap = CaseOf[
  $[f_Broad, as___]    := BAt[f, as];
  $[f_List, as__List]  := ZipMap[At, f, as];
  (* $[f_List, bs__Broad] := ZipMap[At, f, BSeq @ ms]; *) (* broadcasts second axis *)
  $[f_List, ms__]      := ZipMap[At, f, BSeq @ ms];
  $[f_, a1_List]       := Map[f, a1];
  $[f_, as__List]      := ZipMap[f, as];
  $[f_, b1_Broad]      := MapF[f, b1];
  $[f_, bs__Broad]     := BLike[f @ BVal @ bs, bs];
  $[f_, ms__]          := ZipMap[f, BSeq @ ms];
];

(**************************************************************************************************)

SetCurry2[BMatchQ, BTrueQ]

BMatchQ[list_, patt_]   := BTrueQ[MatchQ @ patt, list];
BMatchQ[list_, b_Broad] := BTrueQ[MapF[MatchQ, b], list];

BTrueQ = CaseOf[
  $[f_, a_]            := TrueQ @ f @ a;
  $[f_, b_Broad]       := TrueQ @ f @ BVal @ b;
  $[f_Broad, b_Broad]  := TrueQ @ At[BVal @ f, BVal @ b];  (* TODO: test len *)
  $[f_Broad, a_List]   := AllAreTrueQ @ Map[BVal @ f, a]; (* TODO: test len *)
  $[f_List, a_List]    := AllAreTrueQ @ ZipMap[At, f, a];
  $[f_List, m_]        := AllTrue[m, f];
];

(**************************************************************************************************)

ToBroadcastRows[matrix_List, n:Blank01] := Catch[
  Map[vec |-> ToBroadcast[vec, Throw[Null, Null]&], matrix],
  Null, FromBroadcastRows[matrix, n]&
];

FromBroadcastRows[matrix_List, n:Blank01] :=
  Map[vec |-> FromBroadcast[vec, n], matrix];

(**************************************************************************************************)

DeclareStrict[ToRowVector, ToColumnVector]

ToRowVector[e_List]    := List @ e;
ToColumnVector[e_List] := Map[List, e];

DeclareHoldFirst[ArrayTable]

ArrayTable[body_]                 := body;
ArrayTable[body_, shape__Int]     := Array[ThenOp[body], shape];
ArrayTable[body_, shape:{___Int}] := Array[ThenOp[body], shape];

(**************************************************************************************************)

Ones[d___]  := makeConstArr[1, d];
Zeros[d___] := makeConstArr[0, d];
Dots[d___]  := makeConstArr[SymbolicDot, d];
Eye = IdentityMatrix;

makeConstArr[c_, args___] := ConstantArray[c, FlatList[args]];

CoreBoxes[SymbolicDot] := "\[Bullet]";

(**************************************************************************************************)

DeclareStrict[ThrowRealArrayMsg]

ThrowRealArrayMsg[array_, d_Int] := ThrowRealArrayMsg[array, {d, d}];
ThrowRealArrayMsg[array_, {minD_Int, maxD_Int}] := Module[
  {depth, depthStr, badPos, badValue},
  depth = ArrayDepth[array];
  depthStr = If[minD == maxD, minD, StringForm["`` to ``", minD, maxD]];
  If[!TrueQ[minD <= depth <= maxD], ThrowErrorMessage["realArrayDepthBad", depth, depthStr]];
  badPos = FirstPosition[array, Except[_Real | _Integer | _Rational], {d}, Heads -> False];
  If[badPos === None, ThrowErrorMessage["realArray", depthStr]];
  badValue = Extract[array, badPos];
  ThrowErrorMessage["realArrayValue", depthStr, badValue, badPos]
];

General::realArray = "Expected a real-valued array of depth ``."
General::realArrayValue = "Expected a numeric array of depth ``, but non-numeric value `` occurred at position ``."
General::realArrayDepthBad = "Expected a real-valued array of depth ``, but got an array of depth ``.";

(**************************************************************************************************)

(* this doesn't handle lists of assocs *)
Length2[{}]       := None;
Length2[{a_List}] := Len @ a;
Length2[arr_]     := FastQuietCheck[Part[Dims[arr, 2], 2], None];

LengthN[arr_]     := Last[Dimensions @ arr, None];

(*************************************************************************************************)

ToStochasticArray[e_ ? NumberArrayQ] := ToPackedReals[e / Total[e, {-1}]];
SumNormalize[e_] := e / Total[e];

(**************************************************************************************************)

ToPackedInts[arr_]  := ToPackedArray[arr, Integer];
ToPackedReals[arr_] := ToPackedArray[N @ arr, Real];

DeclareHoldRest[EnsurePackedReals, EnsurePackedInts, EnsurePacked]

     EnsurePacked[arr_, else_] := Ensure[ToPackedArray[arr],           PackedQ, else];
 EnsurePackedInts[arr_, else_] := Ensure[ToPackedArray[arr, Integer],  PackedQ, else];
EnsurePackedReals[arr_, else_] := Ensure[ToPackedArray[N @ arr, Real], PackedQ, else];

(*************************************************************************************************)

DeclareCurry1[ThreadPlus, ThreadTimes, ThreadSubtract, ThreadDivide]

ThreadPlus[a_, b_]     := Threaded[a] + b;
ThreadTimes[a_, b_]    := Threaded[a] * b;
ThreadSubtract[a_, b_] := Threaded[a] - b;
ThreadDivide[a_, b_]   := Threaded[a] / b;

(**************************************************************************************************)

ThreadAnd[args__]   := And @@@ Zip[args];
ThreadOr[args__]    := Or @@@ Zip[args];
ThreadNot[arg_List] := Map[Not, arg];

(**************************************************************************************************)

DeclareListable[ThreadMin, ThreadMax]

ThreadMin[a__] := Min[a];
ThreadMax[a__] := Max[a];

(**************************************************************************************************)

ThreadLess[lists___]         := MapThread[Less, {lists}];
ThreadLessEqual[lists___]    := MapThread[LessEqual, {lists}];
ThreadGreater[lists___]      := MapThread[Greater, {lists}];
ThreadGreaterEqual[lists___] := MapThread[GreaterEqual, {lists}];
ThreadEqual[lists___]        := MapThread[Equal, {lists}];
ThreadUnequal[lists___]      := MapThread[Unequal, {lists}];
ThreadSame[lists___]         := MapThread[SameQ, {lists}];
ThreadUnsame[lists___]       := MapThread[UnsameQ, {lists}];

(*************************************************************************************************)

DeclareStrict[Zip];

Zip[seq___List] := Transpose[{seq}];

(**************************************************************************************************
`Flip[x]` gives the `Transpose[x]`.
*)

Flip = Transpose;

(**************************************************************************************************
`MoveAxis[c, i -> j]` sends axis `i` to axis `j`.
*)

DeclareCurry2[MoveAxis]

MoveAxis[arr_, s_ -> s_] := arr;
MoveAxis[arr_, 1 -> -1]  := OutermostToInnermost @ arr;
MoveAxis[arr_, -1 -> 1]  := InnermostToOutermost @ arr;
MoveAxis[arr_, spec_]    := Transpose[arr, MoveToPermutation[spec, ArrayDepth @ spec]];

(*************************************************************************************************)

(* TODO: rename to ArrayContract? *)
DotAxis[a_, axisA_Int, axisB_Int, b_] := Dot[MoveAxis[a, axisA -> -1], MoveAxis[b, axisB -> 1]];

(*************************************************************************************************)

MoveToPermutation[spec_, n_Integer] :=
  moveToPerm[n, spec];

(*************************************************************************************************)

General::invalidAxisMove = "`` is not a valid axis move specification.";

moveToPerm[n_, spec_] := moveToPerm0[n, spec /. i_Negative :> (d - i + 1)];

moveToPerm0 = CaseOf[
  $[n_, s_Integer -> t_Integer] := moveToPerm1[n, {{s}, {t}}];
  $[n_, s_List -> t_List]       := moveToPerm1[n, {s, t}];
  $[n_, st:{__Rule}]            := moveToPerm1[n, {Keys @ st, Values @ st}];
  $[_, e_]                      := Then[Message[MoveAxis::invalidAxisMove, e], {}];
];

moveToPerm1[n_, spec_] := moveToPerm1[n, spec] =
  moveToPerm2[n, spec /. i_ ? Negative :> (n + 1 - i)];

moveToPerm2 = CaseOf[
  $[n_, {{s_}, {s_}}]           := {};
  $[n_, {{s_}, {1 }}] /; s <= n := RotateLeft @ Range @ s;
  $[n_, {{1 }, {s_}}] /; s <= n := RotateRight @ Range @ s;
  $[n_, {{s_}, {t_}}] /; Max[s, t] <= n := Locals[
    inds = Range @ Max[s, t, n];
    If[s < t,
      Part[inds, s ;; t] //= RotateLeft,
      Part[inds, t ;; s] //= RotateRight
    ];
    inds
  ];
  $[n_, {s_, t_}] /; n > 0 := Locals[
    max = Max[s, t, n];
    inds = Delete[Range @ max, List /@ s];
    inds = PadRight[inds, max];
    inds = Insert[inds, 0, List /@ t];
    Part[inds, t] = s;
    Ordering @ Take[inds, max]
  ]
];

(*************************************************************************************************)

$formalsRoman = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

FormalSymbolArray[dims_, str_Str] := FormalSymbolArray[dims, P11 @ StringPosition[$formalsRoman, str] - 1];

FormalSymbolArray[dims_, offset_:0] := Block[{n = 0 + offset}, Array[Symbol @ FromCharCode[63488 + n++]&, dims]];

(**************************************************************************************************)

DeclareStrict[SparseRules]

SparseRules[{} | <||>, sz_] := SparseArray[{}, sz];

SparseRules[assoc_Assoc, sz_] := SparseArray[Normal @ assoc, sz];

SparseRules[list:{___Int} ? DuplicateFreeQ, sz_] := SparseArray[Thread[list -> 1], sz];

SparseRules[list:{___List} ? DuplicateFreeQ, sz_] := SparseArray[Thread[list -> 1], sz];

SparseRules[list:{___List}, sz_] := SparseArray[Normal @ Counts @ list, sz];

SparseRules[list:{___Rule}, sz_] := SparseArray[sumRules @ list, sz];

(**************************************************************************************************)

sumRules[rules_] := Normal @ Merge[rules, Total];

(**************************************************************************************************)

DeclareStrict[SparseRows]

SparseRows[rowSpecs_List, n_Int] := SparseArray[
  Flatten @ MapP[rowSpecToFullSpec, rowSpecs],
  {Len @ rowSpecs, n}
];

SparseRows[rowSpecs_List] := SparseArray[
  Flatten @ MapP[rowSpecToFullSpec, rowSpecs]
];

rowSpecToFullSpec[{}, row_] := {};

rowSpecToFullSpec[cols:{__Rule}, row_] := MapApply[{row, #1} -> #2&, sumRules @ cols];

rowSpecToFullSpec[cols_List -> k_, row_] := sumRules @ Map[{row, #} -> k&, cols];
rowSpecToFullSpec[cols_List, row_] := sumRules @ Map[{row, #} -> 1&, cols];

rowSpecToFullSpec[col_Int -> k_, row_] := {{row, col} -> k};
rowSpecToFullSpec[col_Int, row_] := {{row, col} -> 1};

(**************************************************************************************************)

DeclareStrict[SparseColumns]

SparseColumns[args___] := Transpose @ SparseRows[args];

(**************************************************************************************************)

NDistMatrix[a_]       := DistMatrix[EnsurePackedReals[a, $Failed]];
NDistMatrix[a_, b_]   := DistMatrix[EnsurePackedReals[a, $Failed], EnsurePackedReals[b, $Failed]];

DistMatrix[{}] := {};
DistMatrix[{}, _] := {};
DistMatrix[_, {}] := {};
DistMatrix[a_ ? RealMatrixQ]                   := $distMatrixFn1[a, $distCodeSqrEuc, False];
DistMatrix[a_ ? RealMatrixQ, b_ ? RealMatrixQ] := $distMatrixFn2[a, b, $distCodeEuc, False];
DistMatrix[a_ ? VectorQ]                       := Block[{Get}, DistanceMatrix[a]];
DistMatrix[a_ ? VectorQ, b_ ? VectorQ]         := DistanceMatrix[a, b];
(* ^ unfortunately system one is faster in this case but loads a bunch of junk *)

General::distMatrixBadArray = "Input was not a vector or matrix or a pair of these of matching rank.";
s_DistMatrix := (Message[DistMatrix::distMatrixBadArray]; $Failed);

(**************************************************************************************************)

NSqDistMatrix[a_]     := SqDistMatrix[EnsurePackedReals[a, $Failed]];
NSqDistMatrix[a_, b_] := SqDistMatrix[EnsurePackedReals[a, $Failed], EnsurePackedReals[b, $Failed]];

SqDistMatrix[{}] := {};
SqDistMatrix[{}, _] := {};
SqDistMatrix[_, {}] := {};
SqDistMatrix[a_ ? RealMatrixQ]                   := $distMatrixFn1[a, $distCodeSqrEuc, False];
SqDistMatrix[a_ ? RealMatrixQ, b_ ? RealMatrixQ] := $distMatrixFn2[a, b, $distCodeSqrEuc, False];
SqDistMatrix[a_ ? VectorQ]                       := DistanceMatrix[a, DistanceFunction -> SquaredEuclideanDistance];
SqDistMatrix[a_ ? VectorQ, b_ ? VectorQ]         := DistanceMatrix[a, b, DistanceFunction -> SquaredEuclideanDistance];

s_SqDistMatrix := (Message[SqDistMatrix::distMatrixBadArray]; $Failed);

(**************************************************************************************************)

MinimumDistance[{}] := 0;
MinimumDistance[coords_] := Locals[
  dists = DistMatrix[N @ coords];
  Min @ DeleteCases[Flatten @ dists, ZeroP]
];

(**************************************************************************************************)

$distMatrixFn1 := ($loadNumericArrayFns; $distMatrixFn1)
$distMatrixFn2 := ($loadNumericArrayFns; $distMatrixFn2)

$loadNumericArrayFns := (
  Clear[$loadNumericArrayFns];
  {getDistCode, $distMatrixFn1, $distMatrixFn2} = GetPackageSymbol[
    "NumericArrayUtilities`DistanceMatrix`PackagePrivate`",
    {"$extractLLDMMethod", "mTensorDistanceMatrix1Arg", "mTensorDistanceMatrix2Arg"}
  ];
  (* the final bool is whether to use OpenMP. maybe worth trying? *)
  $distCodeSqrEuc = getDistCode["SquaredEuclideanDistance"];
  $distCodeEuc    = getDistCode["EuclideanDistance"];
);
