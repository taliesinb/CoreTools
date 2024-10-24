SystemExports[
  "Function",
    BezierPoints, CurvePoints, LinePoints,
    PointAlongLine, TangentAlongLine,
    LineLength, LineLengthAcc
];

PackageExports[
  "SymbolicHead", VScaled
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

CurvePoints[e_] := ToPackedReals @ curvePoints @ e;

curvePoints = CaseOf[

  Line[points_] := points;

  Circle[center:Num2P, radius_:NumP, {t1_:NumP, t2_:NumP}] :=
    circleVector[center, radius, angRange[t1, t2]];

  Circle[center:Num2P, radius_:NumP] :=
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

circleVector[pos_, r_, theta_]     := pos + r * CosSin[theta];
circleVector[pos_, r_, theta_List] := Threaded[pos] + r * CosSin[theta];

(**************************************************************************************************)

SetStrict[LinePoints]

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

"PointAlongLine[path$, d$] returns the point at distance d$ along line path$.
PointAlongLine[path$, Scaled[f$]] takes the fraction f$ along the path.
PointAlongLine[d$] is the operator form of PointAlongLine."

(* TODO: rename XXXAlongLine to XXXAlongPath *)

SetCurry2[PointAlongLine]

PointAlongLine[{a_, b_}, d:NumP] :=
  a + Normalize[b - a] * d;

PointAlongLine[coords_, Scaled[d_]] :=
  PointAlongLine[coords, LineLength[coords] * d];

PointAlongLine[coords_, VScaled[d_]] :=
  vscaledPointAlongLine[coords, d];

PointAlongLine[coords_List, d:NumP] :=
  First @ tangentAlongLine[coords, d];

(**************************************************************************************************)

vscaledPointAlongLine[coords_, f_] /; f <= 0. := P1 @ coords;
vscaledPointAlongLine[coords_, f_] /; f >= 1. := PN @ coords;

vscaledPointAlongLine[{{x1_, y1_}, {x2_, y2_}}, f_] := Locals[
  ym = Lerp[y1, y2, f];
  xm = Lerp[x1, x2, (ym - y1) / (y2 - y1)];
  List[xm, ym]
];

vscaledPointAlongLine[coords_, f_] := Locals[
  ym = Lerp[P12 @ coords, PN2 @ coords, f];
  {x2, y2} = First @ coords;
  Do[
    {x1, y1} = {x2, y2};
    {x2, y2} = Part[coords, i];
    If[Sign[y2 - ym] != Sign[y1 - ym],
      xm = Lerp[x1, x2, (ym - y1) / (y2 - y1)];
      Return @ {xm, ym}
    ],
    {i, 2, Len @ coords}
  ]
];

(**************************************************************************************************)

"TangentAlongLine[path$, d$] returns the pair {pos$, dir$} for the point distance d$ along line path$.
TangentAlongLine[path$, Scaled[f$]] takes the fraction f$ along the path.
* If f$ is less than 0 or greater than 1 the point is extrapolated from the tangent at the end of the path.
* Paths of length 2 can have Offset endpoints and these will be correctly handled, though dir$ will ignore the offset."

SetStrict[TangentAlongLine]

TangentAlongLine = CaseOf[
  $[coords_, Scaled[1|1.]]             := pathTangent[coords, Len @ coords];
  $[coords_, Scaled[ZeroP]|ZeroP]      := pathTangent[coords, 1];
  $[{a:Num2P, b:Num2P}, Scaled[d_]]    := simpleTangent[a, b, d];
  $[{a_, b_} ? OffsetsQ, Scaled[d_]]   := offsetTangent[a, b, d];
  $[coords_, Scaled[d_]]               := $[coords, LineLength[coords] * d];
  $[coords_, Scaled[d_ ? OutsideUQ]]   := extrapTangent[coords, d];
  $[coords_List, d:NumP]               := tangentAlongLine[coords, d];
  $[p:{_, _}, d:NumP]                  := segmentTangent[p, d];
];

extrapTangent[a_, b_, d_] := Module[{pos, dir},
  (* TODO: this doesn't take the length of the path into account *)
  {pos, dir} = pathTangent[coords, If[d > 0, Len @ coords, 1]];
  pos = PointPlus[pos, dir * If[d > 0, (d - 1), d] * LineLength[coords]];
  SimplifyOffsets @ {pos, dir}
];

simpleTangent[a_, b_, d_] :=
  {Lerp[a, b, d], N @ Normalize[b - a]};

tangentOffset[a_, b_, d_] := Locals[
  {ar, aa} = FromOffsetCoord @ a;
  {br, ba} = FromOffsetCoord @ b;
  ca = Lerp[aa, ba, d];
  cr = Lerp[ar, br, d];
  {Offset[ca, cr], PointTangent[a, b]}
];

segmentTangent[{a_, b_}, d_] := Locals[
  delta = PointTangent[a, b];
  {a + delta * d, delta}
];

tangentAlongLine[coords_, d_] := Locals[
  prev = First @ coords; total = 0;
  n = LengthWhile[coords, curr |-> (
    total += PointDist[curr, prev];
    prev = curr; total < d
  )];
  If[n == Len[coords], Return @ pathTangent[coords, n]];
  rem = total - d;
  If[rem == 0,
    pathTangent[coords, n + 1],
    {1, -1} * segmentTangent[Part[coords, {n + 1, n}], rem]
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

"LineLengthAcc[{p$1, p$2, $$}] gives the list of accumulated lengths along a line.
* the first entry is 0."

SetStrict[LineLength, LineLengthAcc];

LineLength[{}] := 0;
LineLength[{a_, b_}] := Dist @@ RemoveOffsets[{a, b}];
LineLength[list_List] := Total @ ApplyWindowed[Dist, RemoveOffsets @ list];

LineLengthAcc[{}] := {};

LineLengthAcc[points_] :=
  ToPackedReals @ Prepend[0.] @ Accumulate @ MapWindowed[Apply @ Dist, N @ points]

