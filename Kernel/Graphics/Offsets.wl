PackageExports[
  "Predicate",
    OffsetsQ,
  "Function",
    PointPlus, PointTangent, PointDist,
    RemoveOffsets, SimplifyOffsets, ResolveOffsets,
    FromOffsetNum, ToOffsetNum, FromOffsetCoord, ToOffsetCoord
];

(**************************************************************************************************)

OffsetsQ[points_] := VContainsQ[points, Offset];

(**************************************************************************************************)

RemoveOffsets[points_] := ToPackedReals[points /. Offset[_, p_] :> p];

SimplifyOffsets[points_] := points //. $simpOffRules;

$simpOffRules := $simpOffRules = Dispatch @ {
  Offset[d1_, Offset[d2_, p_]] :> RuleCondition @ Offset[d1 + d2, p],
  Offset[{ZeroP, ZeroP}, p_]   :> p
};

(*************************************************************************************************)

PointPlus[Offset[o1_, p1_], Offset[o2_, p2_]] := Offset[o1 + o2, p1 + p2];
PointPlus[Offset[o_, p1_], p2_] := Offset[o, p1 + p2];
PointPlus[p1_, Offset[o_, p2_]] := Offset[o, p1 + p2];
PointPlus[p1_, p2_]             := p1 + p2;

PointTangent[a_, Offset[_, b_]] := PointTangent[a, b];
PointTangent[Offset[_, a_], b_] := PointTangent[a, b];
PointTangent[a_, b_]            := VecTangent[b - a];

PointDist[Offset[_, a_], b_]    := PointDist[a, b];
PointDist[a_, Offset[_, b_]]    := PointDist[a, b];
PointDist[a_, b_]               := Dist[a, b];

(**************************************************************************************************)

SetCurry2[ResolveOffsets]

ResolveOffsets[e_, scale_ ? NumberQ] :=
  RepAll[e, {
    Offset[o_, p_] :> RuleCondition[p + o / scale],
    Offset[o_]     :> RuleCondition[o / scale]
  }];

ResolveOffsets[e_, _] := e;

(**************************************************************************************************)

FromOffsetNum = CaseOf[
  Offset[o:NumP]                     := {0, o};
  (* n:$numRectP                     := {n, 0}; *)
  (* Offset[o:$NumberP, n:$numRectP] := {n, o}; *)
];

ToOffsetNum = CaseOf[
  {n_, ZeroP}      := n;
  {n_, o_}         := Offset[o, n];
];

(**************************************************************************************************)

FromOffsetCoord = CaseOf[
  p:Num2P                  := {p, {0,0}};
  Offset[o:Num2P]          := {{0,0}, o};
  Offset[o:Num2P, p:Num2P] := {p, o};
];

ToOffsetCoord = CaseOf[
  {p_, Zero2P} := p;
  {p_, o_}     := Offset[o, p];
];
