SystemExports[
  "Function",
    ThreadPlus, ThreadTimes, ThreadSubtract, ThreadDivide, ThreadAnd, ThreadOr, ThreadNot,
    ThreadLess, ThreadLessEqual, ThreadGreater, ThreadGreaterEqual, ThreadEqual, ThreadUnequal, ThreadSame, ThreadUnsame,
    ThreadMin, ThreadMax, VectorMin, VectorMax
];

PackageExports[
  "SymbolicHead", Broadcast,
  "Predicate",    BroadcastQ, BMatchQ, BTrueQ, strongDimsSeq,
  "Function",     ToBroadcast, FromBroadcast,
                  ToBroadcastRows, FromBroadcastRows,
                  BroadcastAt, BroadcastMap
  "SymbolicHead", Broad,
  "Predicate",    BroadQ, BSameShapeQ,
  "Function",     BAt, BMap, FromB, ToB, ToBN, BLen, BLike,
  "ControlFlow",  BSeq, BVal,
  "MetaFunction", DefineBroadcast
];

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

DefineBroadcast[fn_, ifn_] := Then[
  fn[a_List, b_List]           := If[SameLenQOrMsg[a, b, fn], MapThread[ifn, {a, b}], $Failed];
  fn[a_List, b:AtomP]          := Thread @ ifn[a, b];
  fn[a:AtomP, b_List]          := Thread @ ifn[a, b];
  fn[a:AtomP, a_List, b:AtomP] := Thread @ ifn[a, b];
  e_fn                         := ErrorMsg["broadcastNoLists", HoldForm @ e];
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

(*************************************************************************************************)

"ThreadPlus[vec$, arr$] threads vec + array."
"ThreadTimes[vec$, arr$] threads vec * array."
"ThreadSubtract[vec$, arr$] threads vec - array."
"ThreadDivide[vec$, arr$] threads vec / array."

SetCurry1[ThreadPlus, ThreadTimes, ThreadSubtract, ThreadDivide]

ThreadPlus[a_, b_]     := Threaded[a] + b;
ThreadTimes[a_, b_]    := Threaded[a] * b;
ThreadSubtract[a_, b_] := Threaded[a] - b;
ThreadDivide[a_, b_]   := Threaded[a] / b;

(**************************************************************************************************)

"ThreadAnd[vec$1, vec$2, $$] threads And elementwise over vec$i."
"ThreadOr[vec$1, vec$2, $$] threads Or elementwise over vec$i."
"ThreadNot[vec$] evaluates Not elementwise over vec$."

ThreadAnd[args__]   := And @@@ Zip[args];
ThreadOr[args__]    := Or @@@ Zip[args];
ThreadNot[arg_List] := Map[Not, arg];

(**************************************************************************************************)

SetListable[ThreadMin, ThreadMax]

"ThreadMin[arr$1, arr$2, $$] threads Min elementwise over the array$i, which can also be scalars."
"ThreadMax[arr$1, arr$2, $$] threads Max elementwise over the array$i, which can also be scalars."

ThreadMin[a__] := Min[a];
ThreadMax[a__] := Max[a];

(**************************************************************************************************)

"VectorMin[vec$1, vec$2, $$] threads Min elementwise over the vector$i."
"VectorMax[vec$1, vec$2, $$] threads Max elementwise over the vector$i."

SetStrict[VectorMin, VectorMax];

VectorMin[vecs__List] := MapThreadMin @ List @ vecs;
VectorMax[vecs__List] := MapThreadMax @ List @ vecs;

(**************************************************************************************************)

(* TODO: Support Broad here! *)

DeclaredHere[ThreadLess, ThreadLessEqual, ThreadGreater, ThreadGreaterEqual, ThreadEqual, ThreadUnequal];

"ThreadLess[args$$] threads Less elementwise over args$$, broadcasting non-lists."
"ThreadLessEqual[args$$] threads LessEqual elementwise over args$$, broadcasting non-lists."
"ThreadGreater[args$$] threads Greater elementwise over args$$, broadcasting non-lists."
"ThreadGreaterEqual[args$$] threads GreaterEqual elementwise over args$$, broadcasting non-lists."
"ThreadEqual[args$$] threads Equal elementwise over args$$, broadcasting non-lists."
"ThreadUnequal[args$$] threads Unequal elementwise over args$$, broadcasting non-lists."
"ThreadSame[lists$$] threads Same elementwise over the lists$$, broadcasting non-lists."
"ThreadUnsame[lists$$] threads Unsame elementwise over the lists$$, broadcasting non-lists."

General::broadcastNoLists = "Broadcasted operation requires between one and three lists: ``.";

DefineBroadcast[ThreadLess, Less];
DefineBroadcast[ThreadLessEqual, LessEqual];
DefineBroadcast[ThreadGreater, Greater];
DefineBroadcast[ThreadGreaterEqual, GreaterEqual];
DefineBroadcast[ThreadEqual, Equal];
DefineBroadcast[ThreadUnequal, Unequal];

(*************************************************************************************************)

SetStrict[ThreadSame, ThreadUnsame];

ThreadSame[lists___List]   := MapThread[SameQ, {lists}];
ThreadUnsame[lists___List] := MapThread[UnsameQ, {lists}];
