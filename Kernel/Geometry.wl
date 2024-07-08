SystemExports[
  "Function",
    BezierPoints, CurvePoints, LinePoints,
    CosSin, ClockwisePoints, AnticlockwisePoints, SideToRadians,
    LineLength, PointAlongLine, TangentAlongLine,
  "Predicate",
    ContainsOffsetsQ,
  "Symbol",
    TopLeft, TopRight, BottomLeft, BottomRight
];

PackageExports[
  "Function",
    PointPlus, PointTangent, PointDist,
    RemoveOffsets, SimplifyOffsets, ResolveOffsets, FromOffsetNum, ToOffsetNum, FromOffsetCoord, ToOffsetCoord,
  "Variable",
    $SideToRadians
];

(*************************************************************************************************)

BezierPoints::badPoints = "Points `` must be a numeric matrix.";

BezierPoints[points_, numPoints_:Auto] :=
  complexToPath @ GeometricFunctions`BezierCurve[
    EnsurePackedReals[points, Message[BezierPoints::badPoints, points]; $Failed],
    GeometricFunctions`SamplePoints -> numPoints
  ];

complexToPath[GraphicsComplex[coords_List, Line[indices_List]]] :=
  ToPackedReals @ Part[coords, indices];

complexToPath[_] := $Failed;

(*************************************************************************************************)

(* used to be called DiscretizeCurve *)
"
CurvePoints[object$] supports the following existing graphics primitives:
| %Line[$$] | returns the line unchanged |
| %Circle[$$] | samples the circle as a 32-gon, or between the provided angle endpoints |
| %BezierCurve[$$] | samples the curve using %DiscretizeGraphics |
| %BSplineCurve[$$] | as above |
"

CurvePoints[e_] := ToPackedReals @ discCurve[e];

discCurve = CaseOf[

  Line[points_] := points;

  Circle[center:Num2P, radius_ ? NumericQ, {t1_ ? NumericQ, t2_ ? NumericQ}] :=
    circleVector[center, radius, angRange[t1, t2]];

  Circle[center:Num2P, radius_ ? NumericQ] :=
    circleVector[center, radius, $tau32];

  curve:(BezierCurve|BSplineCurve)[line:{_, _}] :=
    line;

  BezierCurve[points_List] := BezierPoints[points];

  (* TODO: optimize this by calling Graphics`Mesh`DiscretizeGraphicsPrimitive directly *)
  curve:(BezierCurve|BSplineCurve)[___] := Locals[
    region = DiscretizeGraphics @ MapAt[ToPacked, curve, 1];
    Catenate @ region["EdgeCoordinates"]
  ];

  spec_ := (Message[CurvePoints::badCurve, spec]; $Failed)

  (* CurvePoints[e_] := CurveToPoints[e]; *)
];

CurvePoints::badCurve = "Cannot discretize unrecognized curve ``.";

(*************************************************************************************************)

$tau32 := $tau32 = ToPackedReals @ N @ Append[0] @ Range[0, Tau - 0.01, Tau / 48];

angRange[t_, t_] := {t};
angRange[t1_, t2_] := DeleteDuplicates @ Append[t2] @ Range[t1, t2, (t2 - t1) / Ceiling[32 - Min[24, 16/Abs[t2-t1]]]];

circleVector[pos_, r_, theta_] := pos + r * CosSin[theta];
circleVector[pos_, r_, theta_List] := Threaded[pos] + r * CosSin[theta];

(*************************************************************************************************)

DeclareListable[CosSin]

CosSin[theta_] := {Cos[theta], Sin[theta]};

(**************************************************************************************************)

"ClockwisePoints[n$] gives n$ equally spaced clockwise points with the first point at {0, 1}.
ClockwisePoints[n$, side$] starts at a symbolic side like Top, Left, etc.
AnticlockwisePoints[n$] gives n$ equally spaced anti-clockwise points with the first point at {0, 1}.
AnticlockwisePoints[n$, side$] starts at a symbolic side like Top, Left, etc."

ClockwisePoints[n_, side_:Top]     := AngleVector /@ ($SideToRadians[side] - Range[0, Tau-1/n, Tau/n]);
AnticlockwisePoints[n_, side_:Top] := AngleVector /@ ($SideToRadians[side] + Range[0, Tau-1/n, Tau/n]);

(**************************************************************************************************)

SideToRadians[side:(_Symbol | _List)] := Lookup[$SideToRadians, side];

$SideToRadians = <|
  Left        ->  4/4 * Pi,
  TopLeft     ->  3/4 * Pi,
  Top         ->  2/4 * Pi,
  TopRight    ->  1/4 * Pi,
  Right       ->  0,
  BottomRight -> -1/4 * Pi,
  Bottom      -> -2/4 * Pi,
  BottomLeft  -> -3/4 * Pi
|>;

(**************************************************************************************************)

"PointAlongLine[path$, d$] returns the point at distance d$ along line path$.
PointAlongLine[path$, Scaled[f$]] takes the fraction f$ along the path.
PointAlongLine[d$] is the operator form of PointAlongLine."

(* TODO: rename XXXAlongLine to XXXAlongPath *)
DeclareCurry2[PointAlongLine]

PointAlongLine[{a_, b_}, d_ ? NumericQ] :=
  a + Normalize[b - a] * d;

PointAlongLine[coords_, Scaled[d_]] :=
  PointAlongLine[coords, LineLength[coords] * d];

PointAlongLine[coords_List, d_ ? NumericQ] :=
  First @ tangentAlongLine[coords, d];

(**************************************************************************************************)

DeclareStrict[LinePoints]

"LinePoints[path$, d$] returns a list of points sampled every distance d$.
* The initial and endpoint are always sampled.
* The sample distance can be smaller, but no smaller than necessary to sample evenly."

LinePoints[{a_, b_}, i_Into] := Lerp[a, b, i];
LinePoints[{a_, b_}, d_]     := Lerp[a, b, toEveryD[Dist[a, b], d]];

(* TODO: avoid restarting the walk many times *)
LinePoints[path_List, d_] := Locals[
  len = LineLength[path];
  PointAlongLine[path, #]& /@ Lerp[0, len, toEveryD[len, d]]
];

toEveryD[total_, d_] := Into @ Ceiling[total / d];

(**************************************************************************************************)

"TangentAlongLine[path$, d$] returns the pair {pos$, dir$} for the point distance d$ along line path$.
TangentAlongLine[path$, Scaled[f$]] takes the fraction f$ along the path.
* If f$ is less than 0 or greater than 1 the point is extrapolated from the tangent at the end of the path.
* Paths of length 2 can have Offset endpoints and these will be correctly handled, though dir$ will ignore the offset."

DeclareStrict[TangentAlongLine]

TangentAlongLine[p:{_, _}, d_ ? NumericQ] := tangentAlongSegment[p, d];

TangentAlongLine[coords_, Scaled[1|1.]] := pathTangent[coords, Len @ coords];
TangentAlongLine[coords_, Scaled[0|0.]|0|0.] := pathTangent[coords, 1];

TangentAlongLine[{a:Num2P, b:Num2P}, Scaled[d_]] :=
  {Lerp[a, b, d], N @ Normalize[b - a]};

TangentAlongLine[{a_, b_} ? ContainsOffsetsQ, Scaled[d_]] := Locals[
  {ar, aa} = FromOffsetCoord @ a;
  {br, ba} = FromOffsetCoord @ b;
  ca = Lerp[aa, ba, d];
  cr = Lerp[ar, br, d];
  {Offset[ca, cr], PointTangent[a, b]}
];

TangentAlongLine[coords_, Scaled[d_]] :=
  TangentAlongLine[coords, LineLength[coords] * d];

(* TODO: this doesn't take the length of the path into account *)
TangentAlongLine[coords_, Scaled[d_] /; d > 1] := Locals[
  {pos, dir} = pathTangent[coords, Len @ coords];
  pos = PointPlus[pos, dir * (d - 1) * LineLength[coords]];
  {pos, dir} // SimplifyOffsets
];

TangentAlongLine[coords_, Scaled[d_] /; d < 0] := Locals[
  {pos, dir} = pathTangent[coords, 1];
  pos = PointPlus[pos, dir * d * LineLength[coords]];
  {pos, dir} // SimplifyOffsets
];

TangentAlongLine[coords_List, d_ ? NumericQ] :=
  tangentAlongLine[coords, d];

(**************************************************************************************************)

tangentAlongSegment[{a_, b_}, d_] := Locals[
  delta = PointTangent[a, b];
  {a + delta * d, delta}
];

tangentAlongLine[coords_, d_] := Locals[
  prev = F @ coords; total = 0;
  n = LengthWhile[coords, curr |-> (
    total += PointDist[curr, prev];
    prev = curr; total < d
  )];
  If[n == Len[coords], Return @ pathTangent[coords, n]];
  rem = total - d;
  If[rem == 0,
    pathTangent[coords, n + 1],
    {1, -1} * tangentAlongSegment[Part[coords, {n + 1, n}], rem]
  ]
];

pathTangent[{coord_}, 1] := {coord, {0, 0}};

pathTangent[coords_, 1] := Locals[
  {p1, p2} = Part[coords, {1, 2}];
  {p1, PointTangent[p1, p2]}
]

pathTangent[coords_, n_] /; n == Len[coords] := Locals[
  {p0, p1} = Part[coords, {-2, -1}];
  {p1, PointTangent[p0, p1]}
]

pathTangent[coords_, i_] := Locals[
  {p0, p1, p2} = Part[coords, i + {-1, 0, 1}];
  {p1, N @ Normalize @ Avg[p1 - p0, p2 - p1]}
]

(**************************************************************************************************)

DeclareStrict[LineLength]

LineLength[{}] := 0;
LineLength[{a_, b_}] := Dist @@ RemoveOffsets[{a, b}];
LineLength[list_List] := Total @ ApplyWindowed[Dist, RemoveOffsets @ list];

(**************************************************************************************************)

(* TODO: expose this and other such Offset-aware functions properly *)

PointPlus[Offset[o1_, p1_], Offset[o2_, p2_]] := Offset[o1 + o2, p1 + p2];
PointPlus[Offset[o_, p1_], p2_] := Offset[o, p1 + p2];
PointPlus[p1_, Offset[o_, p2_]] := Offset[o, p1 + p2];
PointPlus[p1_, p2_] := p1 + p2;

PointTangent[a_, Offset[_, b_]] := PointTangent[a, b];
PointTangent[Offset[_, a_], b_] := PointTangent[a, b];
PointTangent[a_, b_] := N @ Normalize[b - a];

PointDist[Offset[_, a_], b_] := PointDist[a, b];
PointDist[a_, Offset[_, b_]] := PointDist[a, b];
PointDist[a_, b_] := Dist[a, b];

(**************************************************************************************************)

ContainsOffsetsQ[points_] := VContainsQ[points, Offset];

RemoveOffsets[points_] := ToPackedReals[points /. Offset[_, p_] :> p];

(**************************************************************************************************)

$simpOffRules := $simpOffRules = Dispatch @ {
  Offset[d1_, Offset[d2_, p_]] :> RuleCondition @ Offset[d1 + d2, p],
  Offset[{ZeroP, ZeroP}, p_]   :> p
};

SimplifyOffsets[points_] := points //. $simpOffRules;

(**************************************************************************************************)

DeclareCurry2[ResolveOffsets]

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
  (* {$zeroFP, $zeroP} := 0; *)
  (* {$zeroFP, o_}     := Offset[o]; *)
  {n_, ZeroP}      := n;
  {n_, o_}         := Offset[o, n];
];

(*
$zeroP = 0|0.;
$zeroVP = {$zeroP, $zeroP};
$zeroRP = Rectangular[{$zeroP,$zerop}];
$zeroFP = $zeroP | $zeroRP;
$nullSBSpecP = None | $zeroFP | Offset[$zeroP] | Offset[$zeroP, $zeroFP];
 *)
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
