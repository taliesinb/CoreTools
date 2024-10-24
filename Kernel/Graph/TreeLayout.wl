PackageExports[
  "Function",  OrderedTreeLayout, GraphSourceIndices,
  "Option",    RootPosition, RootVertex, SharedLeaves, SplitPos, HStretch, VStretch
];

(*************************************************************************************************)

"
OrderedTreeLayout[$$] is a layout engine for ExtendedGraph that supports the following options:
| %RootPosition | one of Top or Left, Bottom, or Right |
| %RootVertex | the root vertex, Automatic means the first vertex |
| %SplitPosition | where to put the fan-out between a parent and its children |
| %RoundingRadius | bend radius of parent-to-child edges |
| %Alignment | Auto: center parent on immediate children, All: all children |
* Leaf vertices are visited in depth-first order.
* Leaf vertices occupy successive horizontal positions.
* Parent vertices use the center over the interval spanned by their descendents.
* Vertices are placed at the depth that they live from the root.
"

Options[OrderedTreeLayout] = {
  RootPosition     -> Top,
  SharedLeaves     -> {},
  SplitPos         -> Auto,
  Rounding         -> Auto,
  HStretch         -> 1.0,
  VStretch         -> 1.0,
  PMargin          -> 0.5,
  Setback          -> 0,
  Alignment        -> False
};

OrderedTreeLayout::vertexCoordsError = "Couldn't construct vertex coordinates.";
OrderedTreeLayout::badRootPosition = "Bad root position ``.";

OrderedTreeLayout[graph_, opts___Rule] := Locals @ DebugGroup["OrderedTreeLayout",

  UnpackSymbolsAs[OrderedTreeLayout, {opts},
    rootPosition, sharedLeaves,
    splitPos, rounding,
    $hStretch, $vStretch, pMargin,
    setback, alignment
  ];

  $centerGlobal = alignment === All;

  If[VertexCount[graph] === 0, Return @ {{}, {}}];
  indexGraph = vertexEdgeIndexGraph @ graph;
  edgePairs = {#1, #2}& @@@ EdgeList[indexGraph];
  vertexCount = VertexCount @ indexGraph;
  DPrint["VertexCount = ", vertexCount];

  vrange = Range @ vertexCount;
  $inDeg = VertexInDegree @ indexGraph;
  $outDeg = VertexOutDegree @ indexGraph;
  roots = Pick[vrange, $inDeg, 0];
  If[roots === {}, roots = {1}];
  DPrint["Roots = ", roots];

  $corrupt = False; retries = 0;
  Label[$retry$];

  $x = 1.;
  $depth = $ccount = $cindex = $parent = ConstList[0, vertexCount];
  $xs = $ys = ConstList[0., vertexCount];
  $bounds = ConstList[{}, vertexCount];
  $isLast = $isFirst = $discovered = $previsited = $postvisited = ConstList[False, vertexCount];
  Part[$ccount, roots] = -1;
  If[EdgeCount[indexGraph] == 0, $xs = vrange; Goto[done]];
  DebugGroup["Visiting",
    Scan[
      root |-> DepthFirstScan[indexGraph, root, {
        "DiscoverVertex"  -> discoverVertex,
        "PrevisitVertex"  -> previsitVertex,
        "PostvisitVertex" -> postvisitVertex
      }],
      roots
    ]
  ];

  If[$corrupt && retries < 3, Goto[$retry$]];
  DebugPrint["Ending scan"];

  isShared = ConstList[False, vertexCount];

  childInds = PosIndex @ ReplacePart[$parent, ConstRules[roots, 0]];
  Part[$xs, roots] = Map[Median @ Part[$xs, Lookup[childInds, #1, {#1}]]&, roots];
  isInner = ZipMap[1 < #1 < #2 && #3 > 0&, $cindex, Part[$ccount, $parent], $inDeg];
  isInnerShared = ConstList[False, vertexCount];

(*   mids = Pick[vrange, isInner];
  Part[$xs, mids] += $firstLastDelta;
  Part[$xs, roots] =
 *)
  Label[done];

  numShared = Len @ sharedLeaves;

  maxD = Max[$depth] + 1 + If[numShared > 0, 1, 0];
  If[maxD > 64, ReturnFailed[]];
  DPrint["MaxD = ", maxD];

  $firstLastDelta = 0.00125;
  $xDelta = $firstLastDelta / 128;
  DebugPrint["Done"];

  $share = UDict[];
  If[numShared > 0,
    DebugPrint["Sharing"];
    shareInds = Range[numShared] + vertexCount;
    $share = UDict @ ZipMap[ConstRules, sharedLeaves, shareInds];
    sharedXs = Parts[$xs, sharedLeaves];
    sharedLRs = MinMax /@ sharedXs;
    ZipScan[
      {inds, xs, lr} |-> Set[Part[isInnerShared, inds], Min[AbsDelta[lr, #]] > 0.01& /@ xs],
      sharedLeaves, sharedXs, sharedLRs
    ];
    Scan[Set[Part[isShared, #], True]&, sharedLeaves];
    sx = MapP[Mean[Part[$xs, #] + If[EvenQ[#2], .2, -.2]]&, sharedLeaves];
    sy = Max[$ys] + .5;
    sys = sy + Range[numShared] * .2;
    JoinTo[$xs, sx];
    JoinTo[$ys, sys];
    $maxsy = Max[sys] + .1;
  ];

  vertexCoords = EnsurePackedReals[Flip @ {$xs, maxD - $ys}, ReturnFailed["vertexCoordsError"]];
  coordFn = Switch[rootPosition, Bottom, FlipYOp, Top, Id, Left, Rot90LOp, Right, Rot90ROp,
    _, ReturnFailed["badRootPosition", rootPosition]];

  Module[{origCoords = Take[vertexCoords, vertexCount]},
    $isInner = UDictThread[origCoords, isInner];
    $isShared = UDictThread[origCoords, isShared];
    $isInnerShared = UDictThread[origCoords, isInnerShared];
  ];

  DebugPrint["Constructing edges"];
  $shrTargets = UDictThread[Part[vertexCoords, Keys @ $share], Part[vertexCoords, Vals @ $share]];
  edgePaths = ExtractIndices[vertexCoords, edgePairs];
  DPrint[Graphics[Arrow /@ edgePaths, ImageSize -> 200]];
  edgePaths = SetbackLine[setback] @ constructEdgePaths[edgePaths, rounding, splitPos];

  vertexCoords //= coordFn;
  edgePaths //= coordFn;
  DebugPrint["Calculating bounds"];
  bounds = CoordinateBounds[vertexCoords, pMargin];
  If[numShared > 0,
    KeyValueScan[Set[Part[vertexCoords, #1], Part[vertexCoords, #2, All]]&, $share];
    vertexCoords = Drop[vertexCoords, -numShared];
  ];

  vertexCoords = EnsurePackedReals[vertexCoords, ReturnFailed["vertexCoordsError"]];
  DPrint["Done with layout"];

  {vertexCoords, edgePaths, bounds, {}}
]

(*************************************************************************************************)

makeFTF = CaseOf[
  1  := {False};
  2  := {False, False};
  n_ := ToList[False, ConstList[True, n-2], False];
];

discoverVertex[t_, s_, d_] := If[Part[$discovered, t], Null,
  DPrint["Discover: ", DEdge[s, t], " at ", d];
  Part[$discovered, t] = True;
  Part[$ccount, s]++;
  Part[$cindex, t] = Part[$ccount, s];
  Part[$parent, t] = s;
  Part[$depth, t] = If[d > 512, $corrupt = True; Message[OrderedTreeLayout::corrupt, {t, s, d}]; 0, d];
];

OrderedTreeLayout::corrupt = "Hit bug in kernel: ``";

previsitVertex[v_] := If[Part[$previsited, v], Null,
  DPrint["PreVisit: ", v];
  Part[$previsited, v] = True;
  Part[$ys, v] = Part[$depth, v] * $vStretch;
  If[Part[$outDeg, v] > 0,
    If[$centerGlobal && Part[$xs, v] == 0., Part[$xs, v] = $x],
    Part[$xs, v] = $x += $hStretch;
  ];
];

postvisitVertex[v_] := If[Part[$postvisited, v], Null,
  DPrint["PostVisit: ", v];
  Part[$postvisited, v] = True;
  If[Part[$outDeg, v] > 0,
    Part[$xs, v] = If[$centerGlobal,
      Avg[Part[$xs, v], ($x-1)],
      Replace[Part[$bounds, v], {{} :> Part[$xs, v], e_ :> Mean[e]}]
    ];
  ];
  If[!$centerGlobal, Part[$bounds, Part[$parent, v]] //= MinMax[Flatten @ {#, Part[$xs, v]}]&];
];

(*************************************************************************************************)

vertexEdgeIndexGraph[g_] := IndexEdgeTaggedGraph @ IndexGraph @ g;

(*************************************************************************************************)

constructEdgePaths[edgePaths_, rounding_, splitPos_] := Locals[
  $r = rounding;
  SetAuto[$r, 0.333 * MinDist @ Join[Col1 @ edgePaths, ColN @ edgePaths]];
  Switch[splitPos,
    "Top" | Top | Scaled[0|0.],    Map[bendTop, edgePaths],
    "Bottom" | Bot | Scaled[1|1.], Map[bendBot, edgePaths],
    "Center" | Center,             Map[bendScl[0.5], edgePaths],
    Scaled[NumP],                  Map[bendScl[Last[splitPos]], edgePaths],
    "Circuit",                     Map[bendCen, edgePaths],
    NumP,                          Map[bendPos[splitPos * $vStretch], edgePaths],
    Auto | None,                   edgePaths,
    _,
      Message[OrderedTreeLayout::optVal, SplitPosition, splitPos];
      edgePaths
  ]
]

bendCen[{a:{ax_, ay_}, b:{bx_, by_}}] /; $isShared[b] := bendCenShared[a, b, .5];
bendCen[{a:{ax_, ay_}, b:{bx_, by_}}] := Locals[
  If[Min[Abs[ax - bx], Abs[ay - by]] < 0.0001, Return @ {a, b}];
  aby = (ay + by) / 2;
  abx = (ax + bx) / 2;
  c = {ax, aby}; d = {bx, aby};
  ca = ptAlong[c, a, $r];
  cd = ptAlong[c, d, $r]; Part[cd, 1] //= ClipOp[Sort @ {ax, abx}];
  dc = ptAlong[d, c, $r]; Part[dc, 1] //= ClipOp[Sort @ {bx, abx}];
  db = ptAlong[d, b, $r];
  Join[{a}, BezierPoints[{ca, c, cd}], BezierPoints[{dc, d, db}], {b}]
];

bendPos[o_][{a:{ax_, ay_}, b:{bx_, by_}}] := Locals[
  If[$trimCorner && Min[Abs[ax - bx], Abs[ay - by]] < 0.0001, Return @ {a, b}];
  aby = If[o >= 0, ay - o, by - o];
  If[isMidX[bx], Return @ {{bx, aby}, {bx, by}}];
  c = {ax, aby}; d = {bx, aby};
  ca = ptAlong[c, a, $r];
  cd = ptAlong[c, d, $r];
  dc = ptAlong[d, c, $r];
  db = ptAlong[d, b, $r];
  Join[{a, c}, BezierPoints[{dc, d, db}], {b}]
];

bendScl[f_][{a:{ax_, ay_}, b:{bx_, by_}}] /; $isShared[b] := bendCenShared[a, b, f];
bendScl[f_][{a:{ax_, ay_}, b:{bx_, by_}}] := Locals[
  If[$trimCorner && Min[Abs[ax - bx], Abs[ay - by]] < 0.0001, Return @ {a, b}];
  aby = Lerp[ay, by, f];
  If[isMidX[bx], Return @ {{bx, aby}, {bx, by}}];
  c = {ax, aby}; d = {bx, aby};
  ca = ptAlong[c, a, $r];
  cd = ptAlong[c, d, $r];
  dc = ptAlong[d, c, $r];
  db = ptAlong[d, b, $r];
  Join[{a, c}, BezierPoints[{dc, d, db}], {b}]
];

isMidX[x_] := Norm[FractionalPart[x] - $firstLastDelta] < $xDelta;

$trimCorner = True;

bendCenShared[a_, b_, f_] := Module[{c, d},
  $isShared[b] = False;
  d = $shrTargets[b];
  c = d + {0, 0.33};
  Join[
    bendScl[f][{a, b}],
    BlockFalse[$trimCorner, bendBot @ {b, c}],
    List[c, d]
  ]
];

bendTop[{a:{ax_, ay_}, b:{bx_, by_}}] /; $isShared[b] := bendShared[a, b];

bendShared[a_, b_] := Then[
  $isShared[b] = False,
  Join[
    bendTop[{a, b}],
    BlockFalse[$trimCorner, bendBot[{b, $shrTargets @ b}]]
  ]
];

bendTop[{a:{ax_, ay_}, b:{bx_, by_}}] := Locals[
  If[$trimCorner && Abs[ax - bx], Abs[ay - by] < 0.001, Return @ {a, b}];
  If[$isInner[b], Return @ {{bx, ay}, {bx, by}}];
  c = {bx, ay};
  ca = ptAlong[c, a, $r];
  cb = ptAlong[c, b, $r];
  Join[{a}, BezierPoints[{ca, c, cb}], If[$isInner[b], {b, b + {-2, -2}}, {b}]]
];

bendBot[{a:{ax_, ay_}, b:{bx_, by_}}] := Locals[
  If[$trimCorner && Min[Abs[ax - bx], Abs[ay - by]] < 0.001, Return @ {a, b}];
  If[$isInner[b], Return @ {{ax,ay}, {ax, by}}];
  c = {ax, by};
  ca = ptAlong[c, a, $r];
  cb = ptAlong[c, b, $r];
  Join[{a}, BezierPoints[{ca, c, cb}], {b}]
];

bendBot[line_] := line;
bendTop[line_] := line;
bendCen[line_] := line;
bendCenFraction[line_, _] := line;

ptAlong[a_, b_, d_] := Which[d <= 0, a, d >= Dist[a, b], b, True, PointAlongLine[{a, b}, d]];

(*************************************************************************************************)

GraphSourceIndices[graph_, Auto] := Locals[
  vrange  = Range @ VertexCount @ graph;
  inDeg  = VertexInDegree @ graph;
  outDeg = VertexOutDegree @ graph;
  roots = Pick[vrange, inDeg, 0];
  If[roots === {}, roots = {1}];
  roots
];

GraphSourceIndices[graph_, spec_] := FastQuietCheck[
  VertexIndex[graph, spec],
  ThrowMessage["badGraphRoot", spec]
];

General::badGraphRoot = "Specified root vertex `` doesn't occur in the graph.";
