SystemExports[
  "GraphicsPrimitive", AngledCurve
  "Option",            JoinStyle, JoinPosition, Shortcut, Setback
];


PackageExports[
  "Option",            JoinPos,
  "GraphicsPrimitive", AngledCurve,
  "GraphicsBoxFn",     AngledCurveBox,
  "Function",          AngledCurvePoints, AngledCurvePointsFast
];


(*************************************************************************************************)

(* used to be NeatCurve in MathTools *)

DeclaredHere[AngledCurve]

"AngledCurve[{src$, tgt$}] is a curve that leaves src$ at a given angle and enters tgt$ at a given angle.

* the way the endpoints are connected depends on the setting of %JoinStyle, which can take these settings:
| dir$ | enter and leave with a given orientation |
| {dir$s, dir$t} | leave src$ with direction dir$s, enter dst$ from direction dir$t |
* individual directions can be:
| %Vertical | enter/leave vertically, with a horizontal segment in the middle |
| %Horizontal | enter/leave horizontally, with a vertical segment in the middle |
| %Axis | enter/leave either horizontally or vertically |

* if the lines extended from the endpoints will not cross, a segment is introduced to create a connection.

* additionally, a single spec side$ ensures a segment *will* be placed in a given position.
| %Top | place a horizontal connecting segment topmost |
| %Bottom | place a horizontal connecting segment bottommost|
| %Left | place a vertical connecting segment leftmost |
| %Right | place a vertical connecting segment rightmost |

* %JoinPosition -> r% specifies that any segment should be placed scaled position r% between 0 (start) and 1 (end).

* %ShortcutLength -> r% specifies that a full corner / segment will not be emitted, and instead a distance r%
will be travelled from the endpoints before a shortcut is taken.

* %Bend -> r% gives the radius of bends connecting the edges.

* the option %Setback is applied with respect to the entering and leaving angles."

(*************************************************************************************************)

DefineAliasRules[
  JoinPos -> JoinPosition
];

(*************************************************************************************************)

Options[AngledCurve] = {
  JoinStyle       -> Axis,
  JoinPos         -> 0.5,
  Shortcut        -> 0.0,
  Bend            -> 0.5,
  Setback         -> 0.0
};

DefineGPrim[AngledCurve, "PosPair", AngledCurveBox];

AngledCurveBox[coords:Pos2PairP, opts___Rule] :=
  Make[LineBox, AngledCurvePoints[coords, opts]];

(**************************************************************************************************)

Options[AngledCurvePoints] = Options[AngledCurve];

AngledCurvePoints[coords_List, opts___Rule] := Locals @ CatchMessages[
  UnpackSymbolsAs[AngledCurvePoints, {opts}, joinStyle, joinPos, shortcut, bend, setback];
  SetNone[setback, 0];
  {setback1, setback2} = EnsurePair @ setback;
  AngledCurvePointsFast[coords, joinStyle, joinPos, shortcut, bend, setback1, setback2]
];


AngledCurvePointsFast[coords_, joinStyle_, joinPos_, shortcut_, origBend_, setback1_, setback2_] := Locals[

  segment = Auto;

  {a, b} = N @ coords;
  delta = b - a;

  procJoinStyle @ joinStyle;

  (* can a and b snap on the X and Y axes? *)
  abs = Abs @ delta;
  {snap1, snap2} = ZipMap[canSnap, {setback1, setback2}, {abs, abs}];

  (* turn Horizontal, Vertical, Automatic, etc. specs into Left, Right, etc.
  we use the snapping to bias towards axial directions for Automatic and Axes *)
  dir1 = resolveHV[dir1, delta,  snap1];
  dir2 = resolveHV[dir2, -delta, snap2];

  (* we now use the setbacks to determine where endpoints starts and its initial direction *)

  {dir1, off1} = procSetbackDir[delta, setback1, dir1];
  {dir2, off2} = procSetbackDir[-delta, setback2, dir2];

  a = a + off1; b = b + off2;
  delta = b - a; dist = Norm[delta];

  (* we now decide if endpoints are within the bounds of Rectangular endpoints and able to be
  turned into straight lines *)
  snapped = applySnap[{a, b}, dir1, dir2, snap1, snap2];
  If[snapped =!= None, Return @ snapped];

  If[segment === Auto,
    ab = {a, a + dir1 * dist};
    ba = {b, b + dir2 * dist};
    m = InfLineLineInter[ab, ba];
    segment = If[m === None, If[P1[abs] < P2[abs], Hor, Ver], {m}];
  ];

  numBends = Switch[segment, Hor | Ver, 2, _, 1];
  bend = origBend;
  maxBend = Max @ bend;

  If[maxBend > 0 && joinPos != 0.5,
    (* we must leave buffer for the bend to happen *)
    dbend = Sign[delta] * maxBend / numBends;
    m = Lerp[a + dbend, b - dbend, joinPos];
  ,
    m = Lerp[a, b, joinPos];
  ];
  mids = chooseMids[a, b, m, segment];

  If[shortcut > 0,
    short = Min[shortcut, dist/2 - maxBend];
    as = PointAlongLine[{a, First @ mids}, short];
    bs = PointAlongLine[{b, Last @ mids}, short];
    bend = ThreadMin[bend, short];
    points = {a, as, bs, b};
  ,
    bend = ThreadMin[bend, Min[(Norm /@ delta)/numBends]];
    points = Join[{a}, mids, {b}];
  ];

  Which[
    origBend === Inf,  RoundedCurvePointsFast[points, Inf, None],
    Max[bend] > 0,     RoundedCurvePointsFast[points, bend, "Arc"],
    True,              ToPackedReals @ points
  ]

];

(**************************************************************************************************)

procJoinStyle = CaseOf[
  {Ver, Ver}          := Then[dir1 = dir2 = Ver, segment = Hor];
  {Hor, Hor}          := Then[dir1 = dir2 = Hor, segment = Ver];
  Above               := Then[{dir1, dir2} = If[P2[a] < P2[b], {Ver, Hor}, {Hor, Ver}]];
  Below               := Then[{dir1, dir2} = If[P2[a] > P2[b], {Ver, Hor}, {Hor, Ver}]];
  spec:$dir$          := $[{spec, spec}];
  spec:{$dir$, $dir$} := Then[{dir1, dir2} = spec];
  spec_               := Then[Message[AngledCurvePoints::badJoinStyle, spec]; $[{Ver, Ver}]];
,
  {$dir$ -> Alt[ExtSideP, Horizontal, Vertical, Pos2P, Axis]}
];

AngledCurvePoints::badJoinStyle = "JoinStyle should be one or a pair of a direction vector, Horizontal, Vertical, or Axis; it was ``.";

(**************************************************************************************************)

resolveHV = CaseOf[
  Seq[Axis, d:{x_, y_}, s_]     := $[If[Abs[x] >= Abs[y], Hor, Ver], d, s];
  Seq[Hor, {x_, y_}, {sx_, _}]  := If[sx, If[y > 0, Top, Bot], If[x > 0, Rig, Lef]];
  Seq[Ver, {x_, y_}, {_, sy_}]  := If[sy, If[x > 0, Rig, Lef], If[y > 0, Top, Bot]];
  Seq[e_, _, _]                  := e;
];

(**************************************************************************************************)

procSetbackDir[delta_, Rectangular[r:Pos2PairP], dir_] := procRectDir[delta, r, dir];
procSetbackDir[delta_, r:NumP, dir_]                   := procCircDir[delta, r, dir];
procSetbackDir[delta_, setback_, dir_]                 := (OptMsg[Setback, setback]; {{1,0}, 0});

procRectDir[delta_, r_, s:ExtSideP] := With[{v = $SideToCoords @ s}, List[Normalize @ v, v * r]];
procRectDir[delta_, r_, v:Num2P]    := With[{v2 = Normalize @ v},    List[v2, LineRectangleInter[{v2 * Norm[delta], {0,0}}, {-r, +r}]]];

procCircDir[delta_, r_, s:ExtSideP] := With[{v = $SideToCoords @ s}, List[Normalize @ v, v * r]];
procCircDir[delta_, r_, v:Num2P]    := With[{v2 = Normalize @ v},    List[v2, v2 * r]];

(**************************************************************************************************)

canSnap[Rectangular[{w_, h_}], {ax_, ay_}] := {ax < w, ay < h};
canSnap[r_ ? NumberQ,          {ax_, ay_}] := {ax < r / 32, ay < r / 32};
canSnap[_, _]                              := {False, False};

(* if either axis can snap, and the chosen direction is compatible with snapping in that axis,
then apply the snapping. so if A and B are both y-snappable, then set the y coord of the
line to be average of these. if just A is snappable, then y is set to B's value. if neither snappable,
we can't snap.
*)
applySnap[line2_, dir1_, dir2_, {s1x_, s1y_}, {s2x_, s2y_}] := Locals[
  line = line2;
  Which[
    !Or[s1x, s1y, s2x, s2y], Return @ None,
    isV[dir1] && isV[dir2],  Part[line, All, 1] //= snapCoord[s1x, s2x],
    isH[dir1] && isH[dir2],  Part[line, All, 2] //= snapCoord[s1y, s2y],
    True,                    Return @ None
  ];
  line
]

isH[{x_, y_}] := Abs[x] >= Abs[y];
isV[{x_, y_}] := Abs[y] >= Abs[x];

snapCoord[True,   True] = Mean; (* TODO: choose the narrow one to win *)
snapCoord[True,  False] = Last;
snapCoord[False,  True] = First;
snapCoord[False, False] = Id;

(**************************************************************************************************)

chooseMids[{ax_, ay_}, {bx_, by_}, {mx_, my_}] := CaseOf[
  Hor        := {{ax, my}, {bx, my}};
  Ver        := {{mx, ay}, {mx, by}};
  Center     := {{mx, my}};
  seg_List   := seg;
  None       := {};
]

(**************************************************************************************************)

(* NOTE: dead code... should I understand if the goal is still valid? *)
tooClose[u_, v_, m_:1] := (Dist[u, v]) < m * roundingRadius;

(* push v towards t until it is roundingRadius away from s *)
push[s_, v_, t_] := InfLineCircleInter[{v, t}, {s, roundingRadius}];

fixTooClose = CaseOf[
  {a_, m_, b_} /; tooClose[a, m] := {a, push[a, m, b], b};
  {a_, m_, b_} /; tooClose[m, b] := {a, push[b, m, a], b};
  {a_, m_, n_, b_} /; tooClose[a, m] := {a, n, b};
  {a_, m_, n_, b_} /; tooClose[n, b] := {a, m, b};
  {a_, m_, n_, b_} /; tooClose[m, n, 2] := {a, push[m, n, a], push[n, m, b], b};
  other_ := other;
]
