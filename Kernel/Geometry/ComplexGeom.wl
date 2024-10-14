PackageExports[
  "Function",
    LineTowards,
    PointTowards,
    ShrinkPolygon,
    ArcBetween
];

(**************************************************************************************************)

"LineTowards[{p$, $q}, t$, d$] shifts the line {p$, q$} orthogonally by distance d$ towards the point t$."
"PointTowards[p$, q$, d$] returns the point at distance d$ from p$ to q$."

SetStrict[LineTowards, PointTowards];

LineTowards[line:{a_, b_}, point_, dist_] := Module[
  {delta = Normalize[VecReject[b - point, a - b]] * dist},
  {a - delta, b - delta}
]

PointTowards[p_, q_, ZeroP]  := p;
PointTowards[p_, q_, d:NumP] := If[Dist[p, q] >= d, q, a + Normalize[b - a] * d];

(**************************************************************************************************)

"ShrinkPolygon[points$, d$] shrinks a convex polygon towards its center, moving each segment by distance d$ orthogonal to itself.
* No line will be moved beyond the center of the polygon.
* Currently a polygon wil not be reduced in arity if it is shrunk so much that a side becomes negative length."

(* TODO: fix the negative side length issue *)

SetStrict @ ShrinkPolygon;

ShrinkPolygon[{a_, b_, c_}, d_] := Locals[
  {da, db, dc} = {1, 1, 1} * d;
  ab = lineTowardsLimited[{a, b}, c, da];
  bc = lineTowardsLimited[{b, c}, a, db];
  ac = lineTowardsLimited[{a, c}, b, dc];
  a2 = LineLineInter[ab, ac]; If[FailureQ[a2], a2 = PointTowards[a, Avg[c, b], da]];
  b2 = LineLineInter[ab, bc]; If[FailureQ[b2], b2 = PointTowards[b, Avg[a, c], db]];
  c2 = LineLineInter[ac, bc]; If[FailureQ[c2], c2 = PointTowards[c, Avg[a, b], dc]];
  {a2, b2, c2}
]

ShrinkPolygon[points_, dist_] := Locals[
  center = Mean[points];
  lines = MapWindowedCyclic[lineTowardsLimited[#, center, dist]&, points];
  newPoints = ApplyWindowedCyclic[LineLineInter, lines];
  MapThread[
    If[PosAQ[#1], #1, PointTowards[#2, center, dist]]&,
    {newPoints, points}
  ]
]

lineTowardsLimited[line_, point_, dist_] :=
  LineTowards[line, point, Min[dist, LineDist[line, point] * 0.45]];

(**************************************************************************************************)

"ArcBetween[c$, {p$1, p$2}] returns a circular arc centered on c$ that begins at p$1 and ends at p$2.
ArcBetween[c$, {p$1, p$2}, towards$] ensures the arc takes the way around the circle that faces towards$.
* In 2D, a %Circle is returned, in 3D, a Line is returned by discretizing the circle."

SetStrict @ ArcBetween;

almostEqualQ[p1_, p2_] := Equal[p1, p2] || Dist[p1, p2] < 0.0001;

ArcBetween[c:Pos2P, {p1:Pos2P, p2:Pos2P}, towards_:Auto] := Locals[
  If[almostEqualQ[p1, c] || almostEqualQ[p2, c] || almostEqualQ[p1, p2],
    Return @ Line @ DelDups @ {p1, p2}];
  r = Avg[Dist[c, p1], Dist[c, p2]];
  tow = IfAuto[towards, Avg[p1, p2]];
  d1 = p1 - c; d2 = p2 - c; d3 = tow - c;
  theta1 = ArcTan @@ d1; theta2 = ArcTan @@ d2; theta3 = ArcTan @@ d3;
  If[theta2 < theta1, theta2 += Tau];
  If[theta3 < theta1, theta3 += Tau];
  If[theta3 > theta2, theta2 -= Tau];
  Circle[c, r, {theta1, theta2}]
];

(* TODO: supprot towards by lerping *outside* the line and simulating a point at infinity *)
ArcBetween[center_, {p1:Pos3P, p2:Pos3P}, towards_:Auto] := Locals[
  r = Mean[Dist[center, #]& /@ {p1, p2}];
  Line[center + Normalize[# - center] * r& /@ Lerp[p1, p2, Into[24]]]
]
