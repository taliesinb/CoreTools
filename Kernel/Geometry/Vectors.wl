PackageExports[
  "Function",
    VecReject,
    VecProject,
    VecReflect, VecReflectV, VecReflectH,
    VecFlip,
    VecTangent,
  "Function",
    VecRotTo, VecRot,
    VecScaleTo,
    VecScaleRotTrans,
    VecScaleRot,
    VecTrans,
    RotToMatrix,
    RotMatrix,
    PairAngle,
  "Function",
    VecRot45, VecRot45CW,
    VecRot60, VecRot60CW,
    VecRot90, VecRot90CW,
    VecRot120, VecRot120CW,
    VecRot180, VecRot180CW
];


(**************************************************************************************************)

VecTangent[a_, b_] := N @ Normalize[b - a];

(**************************************************************************************************)

"VecReflect[v$, rv$] reflects the vector v$ in the hyperplane perpendicular to rv$."

SetCurry2[VecReflect]

VecReflect[v_, rv_] := Expand[v - (2 * Dot[rv, v] / Dot[rv, rv]) * rv];

VecReflectH[v_]     := Threaded[{-1, 1}] * v;
VecReflectV[v_]     := Threaded[{1, -1}] * v;

VecFlip[v_? MatrixQ] := Rev[v, 2];
VecFlip[v_List]      := Rev[v];

(**************************************************************************************************)

"VecReject[u$, v$] gives the component of u$ that is orthogonal to v$.
VecReject[{u$1, u$2, $$}, v$] gives the list of rejections u$i onto v$.
VecReject[{u$1, u$2, $$}, {v$1, v$2, $$}] gives the list of rejections u$i onto v$i."

VecReject[u_ ? MatrixQ, v_ ? MatrixQ] := MapThread[VecReject, {u, v}];
VecReject[u_ ? MatrixQ, v_ ? VecQ]    := Map[VecReject[u, #]&, v];
VecReject[u_, v_]                     := u - Projection[u, v];

(**************************************************************************************************)

"VecProject[u$, v$] gives the vector projection of the vector u$ onto the vector v$.
VecProject[{u$1, u$2, $$}, v$] gives the list of projections u$i onto v$.
VecProject[{u$1, u$2, $$}, {v$1, v$2, $$}] gives the list of projection u$i onto v$i."

VecProject[u_ ? MatrixQ, v_ ? MatrixQ] := MapThread[VecProject, {u, v}];
VecProject[u_ ? MatrixQ, v_ ? VecQ]    := Map[VecProject[u, #]&, v];
VecProject[u_, v_]                     := Projection[u, v];

(**************************************************************************************************)

PairAngle[m_ ? NumMatQ] := Map[PairAngle, m];
PairAngle[{x_, y_}] := ArcTan[x, y];
PairAngle[{0|0., 0|0.}] := 0;

(**************************************************************************************************)

SetVectorListableOp @ VecRot;

VecRot[vec_List ? Pos2ListOrListsQ, t_] :=
  Dot[vec, {{Cos[t], Sin[t]}, {-Sin[t], Cos[t]}}];

VecRot[vecs:ListDictP, t_] :=
  Map[VecRot[#, t]&, vecs];

VecRot[t_][vec_] := VecRot[vec, t];

(*************************************************************************************************)

DeclaredHere[VecRot45,   VecRot60,   VecRot90,   VecRot120,   VecRot180];
DeclaredHere[VecRot45CW, VecRot60CW, VecRot90CW, VecRot120CW, VecRot180CW];

setupRotFunc[sym_, angle_, cw_] := With[
  {matrix = RotationMatrix @ (If[cw, angle, -angle] * Degree)},
  {nmatrix = ToPackedReals @ N @ matrix},
  sym[vec_List]                          := Dot[vec, matrix];
  sym[vec_List] /; ArrayQ[vec, _, RealQ] := Dot[ToPacked[vec, Real], nmatrix];
];

setupRotFunc @@@ {
  {VecRot45,  45,  False}, {VecRot45CW,   45, True},
  {VecRot60,  60,  False}, {VecRot60CW,   60, True},
  {VecRot90,  90,  False}, {VecRot90CW,   90, True},
  {VecRot120, 120, False}, {VecRot120CW, 120, True},
  {VecRot180, 180, False}, {VecRot180CW, 180, True}
};

(**************************************************************************************************)

VecRotTo[vec_List ? Pos2ListOrListsQ, to_] :=
  Dot[vec, rotToTrans @ to];

VecRotTo[vecs:ListDictP, to_] :=
  Map[VecRotTo[to], vecs];

VecRotTo[t_] := DotRightOp @ rotToTrans @ t;

rotToTrans[dirx_] := ToPackedReals @ List[dirx, VecRot90 @ dirx];

(**************************************************************************************************)

(* TODO: make these properly listable! *)

SetCurry234 @ VecScaleRotTrans;

VecScaleRotTrans[points_List, scale_, angle_, trans_List] :=
  VecTrans[trans, VecScaleRot[scale, angle, points]];

(**************************************************************************************************)

SetCurry12 @ VecScaleRot;

VecScaleRot[points_List, scale_, angle_List] :=
  VecScaleRot[points, scale, PairAngle @ angle];

VecScaleRot[points_List, scale_, angle_] :=
  scale * Dot[points, rotationMatrix @ angle];

(**************************************************************************************************)

RotMatrix = CaseOf[
  Alt[ZeroP, TauP, -TauP] := {{ 1, 0}, {0,  1}};
  Alt[PiP, -PiP]          := {{-1, 0}, {0, -1}};
  angle_                  := Transpose @ ToPacked @ Chop @ RotationMatrix @ N @ angle
];

(**************************************************************************************************)

(* TODO: fix me *)
SetVectorListableOp @ VecTrans;
SetCurry2 @ VecTrans;

VecTrans[points_List, trans_List] := Threaded[trans] + points;

VecTrans[t_][points_] := VecTrans[t, points];

(**************************************************************************************************)

RotToMatrix[dirx_] :=
  ToPackedReals @ Transpose[{dirx, VecRot90 @ dirx}];

RotToMatrix[dirx_, {sx_, sy_}] :=
  ToPackedReals @ Transpose[{dirx * sx, VecRot90 @ dirx * sy}];

(**************************************************************************************************)

"VecScaleTo[vec$, d$] rescales a vector have length d$.
VecScaleTo[{vec$1, vec$2, $$}, d$] rescales a list of vectors to each have length d$.
VecScaleTo[d$] is the operator form of VecScaleTo."

SetCurry2 @ VecScaleTo;

VecScaleTo::notnumeric = "First arg `` was not a numeric vector or matrix."
VecScaleTo[x_ ? NumVecQ, r_] := Normalize[N @ x] * r;
VecScaleTo[x_ ? NumMatQ, r_] := ToPackedReals[Normalize[#] * r& /@ N[x]];
VecScaleTo[x_, r_] := (Message[VecScaleTo::notnumeric, x]; $Failed);
