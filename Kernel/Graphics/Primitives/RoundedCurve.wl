SystemExports[
  "GraphicsPrimitive", RoundedCurve,
  "Option",            Corner
];

PackageExports[
  "Function",          RoundedCurvePoints, RoundedCurveBox
];

PrivateExports[
  "Function",          RoundedCurvePointsFast
];

(*************************************************************************************************)

DeclaredHere[RoundedCurve]

"
RoundedCurve[path$] represents a curve in which line segments are connected by circular bends.

* RoundedCurve supports the following options:
| %Rounding | the radius of bends between segments |
| %Corner | what curve to connect the segments with |

* %RoundingRadius can be a list of radii, one for each bend.

* The settings of %Corner can be:
| 'Arc' | a circular bend of radius r$ (default) |
| 'Bevel' | a linear corner starting where the arc would start |
| 'Bezier' | a bezier curve with control point being the corner |
| 'Spline' | a simple spline curve |
| None | return the path unchanged |

* A circular bend will be possible for the given radius if the points themselves are too close together, in this case a smaller radius is used.
"

Options[RoundedCurve] = {
  Rounding -> 0.1,
  Corner -> "Arc"
};

DefineGPrim[RoundedCurve, "PosList", RoundedCurveBox];

RoundedCurveBox[points:Pos2ListP, opts___Rule] :=
  Make[LineBox, RoundedCurvePoints[points, opts]];

(**************************************************************************************************)

(*
TODO: allow the maximum distance from the corner to be specified, after which the radius will be decreased
TODO: fix arcs that interact with eachother badly and cause 'jumps'. *)

Options[RoundedCurvePoints] = Options[RoundedCurve];

RoundedCurvePoints[curve_, opts___Rule] := Locals @ CatchMessages[
  UnpackSymbolsAs[RoundedCurvePoints, {opts}, rounding, corner];
  RoundedCurvePointsFast[curve, rounding, corner]
];

(**************************************************************************************************)

RoundedCurvePointsFast[points_, Inf, _]  := CurvePoints @ BezierCurve @ points;
RoundedCurvePointsFast[points_, 0, _]    := points;
RoundedCurvePointsFast[points_, _, None] := points;
RoundedCurvePointsFast[points2_, radius2_, corner_] := Locals[

  radius = N @ radius2;
  points = N @ points2;

  numBends = Len[points] - 2;
  dists = ApplyWindowed[Dist, points];

  radii = ParseCyclicSpec[radius, numBends] /. Auto -> 0.1;
  radii = ZipMap[Min, ApplyWindowed[Min, dists], radii];
  triples = Partition[points, 3, 1];

  Switch[corner,
    "Spline",
      splinePoints = preApp[P1 @ points, PN @ points, ZipMap[makeSplineBend, triples, radii]];
      CurvePoints @ BSplineCurve @ splinePoints
    ,
    "Arc" | "Bevel" | "Line" | "Bezier",
      ToPackedReals @ populateSegments[corner, FirstLast @ points, triples, radii],
    _,
      ThrowOptVal[Corner, corner, {"Spline", "Arc", "Bevel", "Line", "Bezier"}];
      points
  ]
];

(**************************************************************************************************)

makeSplineBend[{a_, b_, c_}, 0|0.] := b;

makeSplineBend[{a_, b_, c_}, r_] := List[
  PointAlongLine[{b, a}, r], b,
  PointAlongLine[{b, c}, r]
];

(**************************************************************************************************)

populateSegments[shape_, {p1_, pn_}, triples_, radii_] := Locals[
  rscale = 1;
  Label[retry];
  tuples = ZipMap[
    {abc, r} |-> Which[
      {a, b, c} = abc;
      (b - a) == (c - b),
        Nothing,
      (r < Min[Dist[a, b], Dist[b, c]] / 100),
        List[b, b, b],
      True,
        l1 = {b, a}; l2 = {b, c};
        center = InfLineLineInter[
          LineTowards[l1, c, r],
          LineTowards[l2, a, r]
        ];
        If[!MatchQ[center, Pos2P], Nothing,
        List[
          center,
          {InfLineNearest[l1, center],
           InfLineNearest[l2, center]},
          b
        ]
      ]
    ],
    triples, radii * rscale
  ];

  (* detect if we had a reversal *)
  skeleton = preApp[p1, pn, Col2 @ tuples];
  deltas = ApplyWindowed[Subtract, skeleton];
  dots = ApplyWindowed[Dot, deltas];
  If[Min[dots] < 0,
    rscale *= 0.95;
    Goto[retry]
  ];

  (* create corners *)
  If[MatchQ[shape, "Bevel" | "Line"], Return @ skeleton];

  fn = hardBendHandler @ If[shape == "Arc",
    ArcBetween /* CurvePoints,
    CurvePoints @ BezierCurve[Insert[#2, #3, 2]]&
  ];

  preApp[p1, pn, Map[fn, tuples]]
];

preApp[p1_, pn_, segments_] := Prepend[p1] @ Append[pn] @ Catenate @ segments;

hardBendHandler[fn_][{b_, b_, b_}] := {b};
hardBendHandler[fn_][args_List]    := Apply[fn, args];

