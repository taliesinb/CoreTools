SystemExports[
  "Option",              GraphScale, NodeData, NodeTooltips, NodeColor, NodeSize, NodeShape, RootPosition
];

PackageExports[
  "GraphicsDirective",   FaceEdge,
  "GraphicsFunction",    ExprTreePlot, NiceTreePlot, InsetNode, TreeNodeColor,
  "GraphicsBoxFunction", ExprTreePlotBoxes, TreeNodePlotBoxes
];

(**************************************************************************************************)

InsetNode[content_, fn_:Id, opts___][pos_, size_, color_] :=
  Inset[Style[fn[content], color, opts, Background -> GrayLevel[1,1]], pos, Center, Alignment -> Center];

(**************************************************************************************************)

Options[NiceTreePlot] = {
  GraphScale        -> 10,
  NodeTooltips      -> None,
  NodeColor         -> Auto,
  NodeSize          -> 5,
  NodeShape         -> "Disk",
  NodeData          -> None,
  FontSize          -> Automatic,
  EdgeColor         -> Auto,
  EdgeThickness     -> 1,
  RootPosition      -> Top,
  BaselinePosition  -> Root
};

$knownShapes = Dict[
  "Point"    -> pointShape,
  "Disk"     -> diskShape,
  "OpenDisk" -> circleShape,
  "Square"   -> squareShape,
  "OpenSquare" -> openSquareShape
];

SetPred1 @ extColorQ;
extColorQ[FaceEdge[ColorP, ColorP] | ColorP] := True;

$treePlotVertexSpecs = {
  ItemSpec[NodeTooltips, TrueFn, None],
  ItemSpec[NodeColor,    extColorQ, $DarkGray],
  ItemSpec[NodeSize,     NumberQ],
  ItemSpec[NodeShape,    TrueFn, Inherit, MethodResolution -> $knownShapes]
};

NiceTreePlot[graph_Graph, opts:OptionsPattern[]] := Locals @ CatchMessages[

  UnpackOptions[graphScale, edgeColor, edgeThickness, rootPosition, baselinePosition, fontSize];

  vertexCount = VertexCount @ graph;
  If[vertexCount === 0, Return @ Graphics[{}, ImageSize -> graphScale]];

  {vertexCoords, edgeCoords} = OrderedTreeLayout[graph, RootPosition -> rootPosition, $treePlotLayoutOptions];
  {{plotL, plotR}, {plotB, plotT}} = plotBounds = CoordinateBounds[vertexCoords, {.5, .5}];
  {plotW, plotH} = plotSize = Dist @@@ plotBounds;
  imageSize = plotSize * graphScale;
  $diskScale = 0.5 / graphScale;

  edgeStyle = Directive[AbsoluteThickness @ edgeThickness];

  vertexList = VertexList @ graph;
  vertexSpecs = ParseItemOptions[NiceTreePlot, $treePlotVertexSpecs, vertexList, {opts}, UseBroadcast -> Auto, ItemData -> NodeData];
  vertexSpecs = Lookup[vertexSpecs, {NodeShape, NodeSize, NodeTooltips, NodeColor}];
  vertexPrims = makePrimitives[vertexSpecs, vertexCoords];
  SetAuto[edgeColor, GrayLevel[0.5]];
  AppendTo[edgeStyle, edgeColor];
  SetAuto[fontSize, Inherited];

  vertexPrims = vertexPrims /. Circle[c_, p_] :> {Style[Disk[c, p], FaceForm @ White], Circle[c, p]};
  primitives = {Style[Line @ edgeCoords, edgeStyle], Style[vertexPrims, FontSize -> fontSize]};

  basePos = Switch[baselinePosition,
    Root, Scaled[(Part[vertexCoords, 1, 2] - plotB - $meanSize/graphScale/1.25) / plotH],
    Top | Bottom | Center, baselinePosition,
    _,    Part @ VertexIndex[graph, baselinePosition] - graphScale
  ];

  Graphics[
    primitives,
    PlotRange -> plotBounds,
    PlotRangePadding -> 0,
    BaselinePosition -> basePos,
    ImageSize -> imageSize
  ]
];

(* if they are all BCast, just do the make inside and wrap with BCast
if none are bcasts, just do Make,
if some are bcasts, turn them to lists and do a ZipMap *)

makePrimitives[{shape_, size_, tooltips_, colors_}, coords_] := Locals[
  prims = BMap[makeShape, shape, coords, size, colors];
  GlobalVar[$meanSize]; $meanSize = Mean @ FromB @ size;
  If[!BMatchQ[tooltips, None], prims = BMap[Tooltip, prims, tooltips]];
  prims
];

makeShape[fn_, pos_, size_, color_] := fn[pos, size, color];

pointShape[pos_, size_, color_]  := Style[Point @ pos, APointSize @ size, color];
diskShape[pos_, size_, color_]   := edgedStyle[color, Disk[pos, size * $diskScale]];
diskShape[pos_, size_, color_]   := edgedStyle[color, Disk[pos, size * $diskScale]];
squareShape[pos_, size_, color_] := edgedStyle[color, Rectangle[pos - size * $diskScale, pos + size * $diskScale]];
openSquareShape[pos_, size_, color_] := faceEdgeStyle[White, color, Rectangle[pos - size * $diskScale, pos + size * $diskScale]];
circleShape[pos_, size_, color_] := {noEdgeStyle[White, Disk[pos, size * $diskScale]], Style[Circle[pos, size * $diskScale], color]};

edgedStyle[FaceEdge[f_, e_], prim_] := faceEdgeStyle[f, e, prim];
edgedStyle[c_, prim_] := Style[prim, FaceForm @ c, EdgeForm @ Darker[c, .1]];
noEdgeStyle[c_, prim_] := Style[prim, FaceForm @ c, EdgeForm @ None];
faceEdgeStyle[f_, e_, prim_] := Style[prim, FaceForm @ f, EdgeForm[Directive[e, AbsoluteThickness[1.2]]]];

$treePlotLayoutOptions = Seq[
  "BendRadius"      -> 0.25,
  "FanoutStyle"     -> "Top",
  "LayerDepths"     -> 0.8,
  "GlobalCentering" -> False
];

(*************************************************************************************************)

"
OrderedTreeLayout[$$] is a layout engine for ExtendedGraph that supports the following options:
| %Orientation | one of Top or Left, Bottom, or Right |
| %RootVertex | the root vertex, Automatic means the first vertex |
| %FanoutStyle | where to put the fan-out between a parent and its children |
| %BendRadius | bend radius of parent-to-child edges |
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
  "RootVertex"      -> Auto,
  "BendRadius"      -> Auto,
  "FanoutStyle"     -> Auto,
  "LayerDepths"     -> Auto,
  "GlobalCentering" -> True,
  "FirstLastDelta"  -> 0.00125
};

OrderedTreeLayout::vertexCoordsError = "Couldn't construct vertex coordinates.";
OrderedTreeLayout::badRootPosition = "Bad root position ``.";
OrderedTreeLayout::badRoot = "Specified root vertex `` doesn't occur in the graph.";

OrderedTreeLayout[graph_, OptionsPattern[]] := Locals[

  UnpackOptions[
    rootPosition, rootVertex, fanoutStyle, bendRadius, layerDepths,
    layerDepths, $globalCentering, $firstLastDelta
  ];

  If[VertexCount[graph] === 0, Return @ {{}, {}}];
  indexGraph = vertexEdgeIndexGraph @ graph;
  edgePairs = {#1, #2}& @@@ EdgeList[indexGraph];
  vertexCount = VertexCount @ indexGraph;

  rootVertex = If[rootVertex === Auto, 1, FastQuietCheck[
    VertexIndex[graph, rootVertex],
    ReturnFailed["badRoot", rootVertex]
  ]];

  SetAuto[layerDepths, {}];
  If[NumericQ[layerDepths], layerDepths //= ToList];

  $x = 1.;
  $d = $ccount = $cindex = $parent = ConstList[0, vertexCount];
  $xs = $ys = ConstList[0., vertexCount];
  $bounds = ConstList[{}, vertexCount];
  $isLast = $isFirst = ConstList[False, vertexCount];
  $inDeg = VertexInDegree @ indexGraph;
  $outDeg = VertexOutDegree @ indexGraph;
  Part[$ccount, rootVertex] = -1;

  vrange = Range @ vertexCount;
  If[EdgeCount[indexGraph] == 0, $xs = vrange; Goto[done]];
  roots = Pick[vrange, $inDeg, 0];

  Scan[root |-> DepthFirstScan[indexGraph, root, {
    "DiscoverVertex"  -> discoverVertex,
    "PrevisitVertex"  -> previsitVertex,
    "PostvisitVertex" -> postvisitVertex
  }], roots];

  childInds = PosIndex @ ReplacePart[$parent, ConstRules[roots, 0]];
  Part[$xs, roots] = Map[Median @ Part[$xs, Lookup[childInds, #1, {#1}]]&, roots];
  isInner = ZipMap[1 < #1 < #2 && #3 > 0&, $cindex, Part[$ccount, $parent], $inDeg];

(*   mids = Pick[vrange, isInner];
  Part[$xs, mids] += $firstLastDelta;
  Part[$xs, roots] =
 *)
  Label[done];
  maxD = Max[$d] + 1;
  If[layerDepths =!= {},
    numDepths = Len @ layerDepths;
    PrependTo[layerDepths, 0];
    If[maxD >= numDepths,
      depthDelta = Subtract @@ Part[layerDepths, {-1, -2}];
      newDepths = Last[layerDepths, 1] + Range[maxD - numDepths] * depthDelta;
      JoinTo[layerDepths, newDepths]
    ];
    $ys = N @ Part[layerDepths, $ys + 1];
  ];

  $xDelta = $firstLastDelta / 128;

  vertexCoords = EnsurePackedReals[Flip @ {$xs, maxD - $ys}, ReturnFailed["vertexCoordsError"]];
  coordFn = Switch[rootPosition, Bottom, FlipYOp, Top, Id, Left, Rot90LOp, Right, Rot90ROp,
    _, ReturnFailed["badRootPosition", rootPosition]];

  $isInner = UDictThread[vertexCoords, isInner];
  edgePaths = ExtractIndices[vertexCoords, edgePairs];
  edgePaths = constructEdgePaths[edgePaths, bendRadius, fanoutStyle];

  coordFn /@ {vertexCoords, edgePaths}
]

discoverVertex[t_, s_, d_] := (
  Part[$ccount, s]++;
  Part[$cindex, t] = Part[$ccount, s];
  Part[$parent, t] = s;
  Part[$d, t] = d;
);

previsitVertex[v_] := (
  Part[$ys, v] = Part[$d, v];
  If[Part[$outDeg, v] > 0,
    If[$globalCentering && Part[$xs, v] == 0., Part[$xs, v] = $x],
    Part[$xs, v] = $x++;
  ];
);

postvisitVertex[v_] := (
  If[Part[$outDeg, v] > 0,
    Part[$xs, v] = If[$globalCentering,
      Avg[Part[$xs, v], ($x-1)],
      Mean @ Part[$bounds, v]
    ];
  ];
  If[!$globalCentering, Part[$bounds, Part[$parent, v]] //= MinMax[{#, Part[$xs, v]}]&];
)

(*************************************************************************************************)

vertexEdgeIndexGraph[g_] := IndexEdgeTaggedGraph @ IndexGraph @ g;

(*************************************************************************************************)

constructEdgePaths[edgePaths_, bendRadius_, fanoutStyle_] := Locals[
  $r = bendRadius;
  SetAuto[$r, 0.333 * MinimumDistance @ Join[Col1 @ edgePaths, ColN @ edgePaths]];
  Switch[fanoutStyle,
    "Top" | Top,       Map[bendTop, edgePaths],
    "Circuit",         Map[bendCenter, edgePaths],
    "Center" | Center, Map[bendCenterFraction[#, 0.5]&, edgePaths],
    Center -> _,       Map[bendCenterFraction[#, Last @ fanoutStyle]&, edgePaths],
    "Bottom" | Bottom, Map[bendBottom, edgePaths],
    Auto | None,       edgePaths,
    _,
      Message[OrderedTreeLayout::badopt, fanoutStyle -> fanoutStyle];
      edgePaths
  ]
]

bendCenter[{a:{ax_, ay_}, b:{bx_, by_}}] := Locals[
  If[Min[Abs[ax - bx], Abs[ay - by]] < 0.001, Return @ {a, b}];
  aby = (ay + by) / 2;
  abx = (ax + bx) / 2;
  c = {ax, aby}; d = {bx, aby};
  ca = ptAlong[c, a, $r];
  cd = ptAlong[c, d, $r]; Part[cd, 1] //= ClipOp[Sort @ {ax, abx}];
  dc = ptAlong[d, c, $r]; Part[dc, 1] //= ClipOp[Sort @ {bx, abx}];
  db = ptAlong[d, b, $r];
  coords @ Join[{a}, BezierPoints[{ca, c, cd}], BezierPoints[{dc, d, db}], {b}]
];

bendCenterFraction[{a:{ax_, ay_}, b:{bx_, by_}}, f_] := Locals[
  If[Min[Abs[ax - bx], Abs[ay - by]] < 0.001, Return @ {a, b}];
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
isInnerX[x:{bx_, by_}] := $isInner[x];

bendTop[{a:{ax_, ay_}, b:{bx_, by_}}] := Locals[
  If[Min[Abs[ax - bx], Abs[ay - by]] < 0.001, Return @ {a, b}];
  If[$isInner[b], Return @ {{bx, ay}, {bx, by}}];
  c = {bx, ay};
  ca = ptAlong[c, a, $r];
  cb = ptAlong[c, b, $r];
  Join[{a}, BezierPoints[{ca, c, cb}], If[$isInner[b], {b, b + {-2, -2}}, {b}]]
];

bendBottom[{a:{ax_, ay_}, b:{bx_, by_}}] := Locals[
  If[Min[Abs[ax - bx], Abs[ay - by]] < 0.001, Return @ {a, b}];
  c = {ax, by};
  ca = ptAlong[c, a, $r];
  cb = ptAlong[c, b, $r];
  Join[{a}, BezierPoints[{ca, c, cb}], {b}]
];

bendBottom[line_] := line;
bendTop[line_] := line;
bendCenter[line_] := line;
bendCenterFraction[line_, _] := line;

ptAlong[a_, b_, d_] := Which[d <= 0, a, d >= Dist[a, b], b, True, PointAlongLine[{a, b}, d]];
