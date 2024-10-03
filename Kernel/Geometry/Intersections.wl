PackageExports[
  "Function",
    LineLineInter,
    InfLineLineInter,
    LineCircleInter,
    InfLineCircleInter,
    LineRectangleInter,
    LineNearest,
    InfLineNearest
];

(**************************************************************************************************)

"LineLineInter[line$1, line$2] gives the point where two line segments cross.
* If they do not cross exactly but cross approximately to within 1/10 their minimum length, the mean point of their closest approach will be given."

SetStrict @ LineLineInter;

LineLineInter[l1_, l2_] := IfNone[posX1, BooleanRegion[And, Line /@ {l1, l2}], approxLineLineInter];

approxLineLineInter[l1_, l2_] := Locals[
  p = P1 @ l2;
  Do[p = LineNearest[l2, LineNearest[l1, p]], 10];
  d = LineDist[l1, p];
  scale = Min[Dist @@ l1, Dist @@ l2];
  If[d > scale / 10,
    $Failed,
    Avg[p, LineNearest[l1, p]]
  ]
];

(**************************************************************************************************)

"InfLineLineInter[{a$1, a$2}, {b$1, b$2}] gives the point where two line segments cross.
* the lines are the taken to be infinite lines passing through {a$1, a$2}, etc.
* if the lines are exactly parallel, None is returned.
* if the intersection point is much further away then the length of a$, None is returned."

SetStrict @ InfLineLineInter;

InfLineLineInter[{a_, b_}, {c_, d_}] := Locals @ Quiet[
  det = Det[Chop @ {a - b, c - d}];
  If[Abs[det] < 0.0001 * Dist[a, b], None,
    (Det[Chop @ {a, b}] * (c - d) - Det[Chop @ {c, d}] * (a - b)) / det
  ]
];

(**************************************************************************************************)

"LineCircleInter[line$1, {p$, r$}] gives the point where %Line[line$] crosses %Circle[p$, r$]."
"InfLineCircleInter[{a$, b$}, {p$, r$}] gives the point where %InfiniteLine[{a$, b$}] crosses %Circle[p$, r$]."

   LineCircleInter[l_List, {p_, r_}] := posX1 @ BooleanRegion[And, {Line @ l, Circle[p, r]}];
InfLineCircleInter[l_List, {p_, r_}] := posX1 @ BooleanRegion[And, {InfiniteLine @ l, Circle[p, r]}];

(**************************************************************************************************)

posX1 = CaseOf[
  Point[p:Pos2P] := p;
  Point[p_List]  := First @ p;
  Line[p_List]   := First @ p;
  _              := None;
];

(**************************************************************************************************)

"LineRectangleInter[line$1, rectangle$] gives the point where a line and a rectangle cross.
* The rectangle should be specified as {{x$min, y$min}, {x$max, y$max}}."

SetStrict @ LineRectangleInter;

LineRectangleInter[line_List, rect:Pos2PairP] := Locals[
  points = Map[
    BooleanRegion[And, {Line @ ToPackedReals @ line, Line @ N @ #1}]&,
    RectCorners @ rect
  ];
  points = Occs[points, Pos2P];
  p1 = P1 @ l1;
  If[points === {}, p1, MinimumBy[points, Dist[#, p1]&]]
];


