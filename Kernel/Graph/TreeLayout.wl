PackageExports[
  "Function",            OrderedTreeLayout
];

(*************************************************************************************************)

"
OrderedTreeLayout[$$] is a layout engine for ExtendedGraph that supports the following options:
| %Orientation | one of Top or Left, Bottom, or Right |
| %RootVertex | the root vertex, Automatic means the first vertex |
| %SplitPosition | where to put the fan-out between a parent and its children |
| %RoundingRadius | bend radius of parent-to-child edges |
| %LayerDepths | gives a list of depths for each layer, excluding first |
| %GlobalCentering | whether to center parent on all children or just immediate children |
* %FanoutStyle can be one of Top, Center, Bottom, or 'Circuit'.
* Leaf vertices are visited in depth-first order.
* Leaf vertices occupy successive horizontal positions.
* Parent vertices use the center over the interval spanned by their descendents.
* Vertices are placed at the depth that they live from the root.
"

Options[OrderedTreeLayout] = {
  "RootPosition"     -> Top,
  "RootVertex"       -> Auto,
  "RoundingRadius"   -> Auto,
  "SplitPosition"    -> Auto,
  "LayerDepths"      -> Auto,
  "Setback"          -> 0,
  "GlobalCentering"  -> False,
  "FirstLastDelta"   -> 0.00125,
  "SharedLeaves"     -> {},
  "PlotRangePadding" -> 0.5
};

OrderedTreeLayout::vertexCoordsError = "Couldn't construct vertex coordinates.";
OrderedTreeLayout::badRootPosition = "Bad root position ``.";
OrderedTreeLayout::badRoot = "Specified root vertex `` doesn't occur in the graph.";

OrderedTreeLayout[graph_, OptionsPattern[]] := Locals @ DebugGroup["OrderedTreeLayout",

  UnpackOptions[
    rootPosition, rootVertex, plotRangePadding,
    splitPosition, roundingRadius, layerDepths,
    setback,
    layerDepths, $globalCentering, $firstLastDelta
  ];

  If[VertexCount[graph] === 0, Return @ {{}, {}}];
  indexGraph = vertexEdgeIndexGraph @ graph;
  edgePairs = {#1, #2}& @@@ EdgeList[indexGraph];
  vertexCount = VertexCount @ indexGraph;
  DPrint["VertexCount = ", vertexCount];

  rootVertex = If[rootVertex === Auto, 1, FastQuietCheck[
    VertexIndex[graph, rootVertex],
    ReturnFailed["badRoot", rootVertex]
  ]];
  DPrint["RootVertex = ", rootVertex];

  SetAuto[layerDepths, {}];
  If[NumericQ[layerDepths], layerDepths //= ToList];

  vrange = Range @ vertexCount;
  $inDeg = VertexInDegree @ indexGraph;
  $outDeg = VertexOutDegree @ indexGraph;
  roots = Pick[vrange, $inDeg, 0];
  If[roots === {}, roots = {1}];

  $corrupt = False; retries = 0;
  Label[$retry$];

  $x = 1.;
  $depth = $ccount = $cindex = $parent = ConstList[0, vertexCount];
  $xs = $ys = ConstList[0., vertexCount];
  $bounds = ConstList[{}, vertexCount];
  $isLast = $isFirst = $discovered = $previsited = $postvisited = ConstList[False, vertexCount];
  Part[$ccount, rootVertex] = -1;
  If[EdgeCount[indexGraph] == 0, $xs = vrange; Goto[done]];
  Scan[root |-> DepthFirstScan[indexGraph, root, {
    "DiscoverVertex"  -> discoverVertex,
    "PrevisitVertex"  -> previsitVertex,
    "PostvisitVertex" -> postvisitVertex
  }], roots];

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

  DebugPrint["Layout"];
  If[layerDepths =!= {},
    numDepths = Len @ layerDepths;
    PrependTo[layerDepths, 0];
    If[maxD >= numDepths,
      DebugPrint["Extending depths"];
      depthDelta = Subtract @@ Part[layerDepths, {-1, -2}];
      newDepths = Last[layerDepths, 1] + Range[maxD - numDepths] * depthDelta;
      JoinTo[layerDepths, newDepths]
    ];
    $ys = N @ Part[layerDepths, $ys + 1];
  ];
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
  edgePaths = SetbackLine[setback] @ constructEdgePaths[edgePaths, roundingRadius, splitPosition];

  vertexCoords //= coordFn;
  edgePaths //= coordFn;
  DebugPrint["Calculating bounds"];
  bounds = CoordinateBounds[vertexCoords, plotRangePadding];
  If[numShared > 0,
    KeyValueScan[Set[Part[vertexCoords, #1], Part[vertexCoords, #2, All]]&, $share];
    vertexCoords = Drop[vertexCoords, -numShared];
  ];

  vertexCoords = EnsurePackedReals[vertexCoords, ReturnFailed["vertexCoordsError"]];
  DPrint["Done with layout"];

  {vertexCoords, edgePaths, bounds}
]

makeFTF = CaseOf[
  1  := {False};
  2  := {False, False};
  n_ := ToList[False, ConstList[True, n-2], False];
];

discoverVertex[t_, s_, d_] := If[Part[$discovered, t], Null,
  Part[$discovered, t] = True;
  Part[$ccount, s]++;
  Part[$cindex, t] = Part[$ccount, s];
  Part[$parent, t] = s;
  Part[$depth, t] = If[d > 512, $corrupt = True; Message[OrderedTreeLayout::corrupt, {t, s, d}]; 0, d];
];

OrderedTreeLayout::corrupt = "Hit bug in kernel: ``";

previsitVertex[v_] := If[Part[$previsited, v], Null,
  Part[$previsited, v] = True;
  Part[$ys, v] = Part[$depth, v];
  If[Part[$outDeg, v] > 0,
    If[$globalCentering && Part[$xs, v] == 0., Part[$xs, v] = $x],
    Part[$xs, v] = $x++;
  ];
];

postvisitVertex[v_] := If[Part[$postvisited, v], Null,
  Part[$postvisited, v] = True;
  If[Part[$outDeg, v] > 0,
    Part[$xs, v] = If[$globalCentering,
      Avg[Part[$xs, v], ($x-1)],
      Replace[Part[$bounds, v], {{} :> Part[$xs, v], e_ :> Mean[e]}]
    ];
  ];
  If[!$globalCentering, Part[$bounds, Part[$parent, v]] //= MinMax[Flatten @ {#, Part[$xs, v]}]&];
];

(*************************************************************************************************)

vertexEdgeIndexGraph[g_] := IndexEdgeTaggedGraph @ IndexGraph @ g;

(*************************************************************************************************)

constructEdgePaths[edgePaths_, rounding_, splitPosition_] := Locals[
  $r = rounding;
  SetAuto[$r, 0.333 * MinDist @ Join[Col1 @ edgePaths, ColN @ edgePaths]];
  Switch[splitPosition,

    "Top" | Top | Scaled[1|1.],    Map[bendTop, edgePaths],
    "Bottom" | Bot | Scaled[0|0.], Map[bendBot, edgePaths],
    "Center" | Center,             Map[bendCenterFraction[#, 0.5]&, edgePaths],
    Scaled[NumP],                  Map[bendCenterFraction[#, Last @ splitPosition]&, edgePaths],
    "Circuit",                     Map[bendCenter, edgePaths],
    Auto | None,                   edgePaths,
    _,
      Message[OrderedTreeLayout::optVal, SplitPosition, splitPosition];
      edgePaths
  ]
]

bendCenter[{a:{ax_, ay_}, b:{bx_, by_}}] := Locals[
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

bendCenterFraction[{a:{ax_, ay_}, b:{bx_, by_}}, f_] := Locals[
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

bendCenter[{a:{ax_, ay_}, b:{bx_, by_}}] /; $isShared[b] := bendCenterShared[a, b, .5];
bendCenterFraction[{a:{ax_, ay_}, b:{bx_, by_}}, f_] /; $isShared[b] := bendCenterShared[a, b, f];

bendCenterShared[a_, b_, f_] := Module[{c, d},
  $isShared[b] = False;
  d = $shrTargets[b];
  c = d + {0, 0.33};
  Join[
    bendCenterFraction[{a, b}, f],
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
bendCenter[line_] := line;
bendCenterFraction[line_, _] := line;

ptAlong[a_, b_, d_] := Which[d <= 0, a, d >= Dist[a, b], b, True, PointAlongLine[{a, b}, d]];


(*************************************************************************************************)
