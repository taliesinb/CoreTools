SystemExports[
  "Function",
    RectCorners,
    CosSin,
    ClockwisePoints,
    AnticlockwisePoints,
    SideToRadians
];

(*************************************************************************************************)

SetListable[CosSin]

CosSin[theta_] := {Cos[theta], Sin[theta]};

(**************************************************************************************************)

"RectCorners[{bl$, tr}] gives the corner points of the rectangule.";

RectCorners[{{x1_, y1_}, {x2_, y2_}}] := List[
  {{x1, y1}, {x1, y2}},
  {{x1, y2}, {x2, y2}},
  {{x2, y2}, {x2, y1}},
  {{x2, y1}, {x1, y1}}
];

(**************************************************************************************************)

"ClockwisePoints[n$] gives n$ equally spaced clockwise points with the first point at {0, 1}.
ClockwisePoints[n$, side$] starts at a symbolic side like Top, Left, etc."

"AnticlockwisePoints[n$] gives n$ equally spaced anti-clockwise points with the first point at {0, 1}.
AnticlockwisePoints[n$, side$] starts at a symbolic side like Top, Left, etc."

ClockwisePoints[n_, side_:Top]     := AngleVector /@ ($SideToRadians[side] - Range[0, Tau-1/n, Tau/n]);

AnticlockwisePoints[n_, side_:Top] := AngleVector /@ ($SideToRadians[side] + Range[0, Tau-1/n, Tau/n]);

