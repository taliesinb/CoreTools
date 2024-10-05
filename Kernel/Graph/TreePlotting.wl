SystemExports[
  "Option",
    GraphScale, AspectRatio,
    NodeData, NodeTooltips, NodeColor, NodeSize, NodeShape,
    RootPosition,
    NodeGroups, GroupStyles,
    NodeBackground,
    SharedLeaves
];

PackageExports[
  "SymbolicHead",     FaceEdge,
  "GraphicsFunction", ExprTreePlot, NiceTreePlot, InsetNode, TreeNodeColor
];

(**************************************************************************************************)

InsetNode[content_, fn_:Id, opts___][pos_, size_, color_] :=
  Inset[Style[fn[content], color, opts, Background -> GrayLevel[1,1]], pos, Center, Alignment -> Center];

(**************************************************************************************************)

Options[NiceTreePlot] = {
  GraphScale        -> 10,
  AspectRatio       -> 0.8,
  NodeTooltips      -> None,
  NodeColor         -> Auto,
  NodeSize          -> 5,
  NodeShape         -> "Disk",
  NodeData          -> Auto,
  NodeGroups        -> None,
  GroupStyles       -> None,
  FontSize          -> Inherited,
  FontFamily        -> Inherited,
  FontWeight        -> Inherited,
  NodeBackground    -> None,
  EdgeColor         -> Auto,
  EdgeThickness     -> 1,
  RootPosition      -> Top,
  BaselinePosition  -> Root,
  SharedLeaves      -> {}
};

SetPred1 @ extColorQ;
extColorQ[FaceEdge[ColorP, ColorP] | ColorP] := True;

$defaultColor = RGBColor[0.4, 0.4, 0.4];

NiceTreePlot[graph_Graph, opts:OptionsPattern[]] := Locals @ CatchMessages[

  UnpackOptions[graphScale, edgeColor, edgeThickness,
    rootPosition, baselinePosition,
    nodeData, aspectRatio,
    fontSize, fontWeight, fontFamily,
    sharedLeaves,
    $nodeBackground
  ];

  vertexCount = VertexCount @ graph;
  If[vertexCount === 0, Return @ Graphics[{}, ImageSize -> graphScale]];

  {vertexCoords, edgeCoords, plotBounds} = OrderedTreeLayout[graph,
    RootPosition -> rootPosition, "LayerDepths" -> aspectRatio,
    SharedLeaves -> sharedLeaves,
    $treePlotLayoutOptions
  ];
  {{plotL, plotR}, {plotB, plotT}} = plotBounds;
  {plotW, plotH} = plotSize = Dist @@@ plotBounds;
  imageSize = plotSize * graphScale;
  $scale = 0.5 / graphScale;

  edgeStyle = Directive[AbsoluteThickness @ edgeThickness];

  SetAuto[nodeData, transposeDict @ GraphVertexAnnotations @ graph];

  vertexList = VertexList @ graph;
  vertexSpecs = ParseItemOptions[NiceTreePlot, $treePlotVertexSpecs, vertexList, {opts}, UseBroadcast -> Auto,
    ItemData -> nodeData, ItemGroups -> NodeGroups, GroupSettings -> GroupStyles
  ];
  vertexSpecs = Lookup[vertexSpecs, {NodeShape, NodeSize, NodeTooltips, NodeColor}];
  vertexPrims = makePrimitives[vertexSpecs, vertexCoords];
  SetAuto[edgeColor, GrayLevel[0.5]];
  AppendTo[edgeStyle, edgeColor];
  fontStyle = DelCases[
    List[FontSize -> fontSize, FontWeight -> fontWeight, FontFamily -> fontFamily],
    _ -> Inherited
  ];

  vertexPrims = vertexPrims /. Circle[c_, p_] :> {Style[Disk[c, p], FaceForm @ White], Circle[c, p]};
  primitives = List[
    Style[Line @ edgeCoords, edgeStyle],
    Style[vertexPrims, Seq @@ fontStyle]
  ];

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

$treePlotLayoutOptions = Seq[
  "BendRadius"      -> 0.25,
  "FanoutStyle"     -> "Top",
  "GlobalCentering" -> False
];

transposeDict[_]         := None;
transposeDict[annos_Dict] := Locals[
  {verts, dicts} = KeysVals @ Select[annos, DictQ];
  keys = Union @@ Map[Keys, dicts];
  vals = Map[key |-> Lookup[dicts, key, None], keys];
  DictThread[keys, vals]
];

(* if they are all BCast, just do the make inside and wrap with BCast
if none are bcasts, just do Make,
if some are bcasts, turn them to lists and do a ZipMap *)

(**************************************************************************************************)

makePrimitives[{shape_, size_, tooltips_, colors_}, coords_] := Locals[
  prims = BMap[makeShape, shape, coords, size, colors];
  GlobalVar[$meanSize]; $meanSize = Mean @ FromB @ size;
  If[!BMatchQ[tooltips, None], prims = BMap[Tooltip, prims, tooltips]];
  prims
];

(**************************************************************************************************)

makeShape[Framed[str_, opts___Rule], pos_, size_, color_] := Locals[
  bg = toBackCol[color, $nodeBackground, White];
  margins = If[bg == White, 0, {{2,1},{1,1}}];
  Text[
    Framed[
      str, opts, $frameOpts,
      If[color =!= $defaultColor, BaseStyle -> {FontColor -> color}, Seq @@ {}],
      Background -> bg,
      FrameStyle -> {AThickness[0], Darker[color,.1]}
    ],
    Offset[{0,2}, pos]
  ]
];

$frameOpts = Seq[
  Alignment -> Scaled[0.5], ContentPadding -> False,
  FrameMargins -> {{2,1},{1,1}}, RoundingRadius -> 2
];

(**************************************************************************************************)

makeShape[Text[str_], pos_, size_, color_] := List[
  mkText[str, Offset[{-0.75,2}, pos], White],
  mkText[str, Offset[{0.75, 2}, pos], White],
  mkText[str, Offset[{0,2}, pos], If[col === $defaultColor, Inherited, color]]
];

mkText[str_, pos_, col_, opts___] := Text[
  Style[str, opts, FontColor -> col],
  pos
];

(**************************************************************************************************)

addTextColor[col_] := If[col === $defaultColor, Id, Append[BaseStyle -> {FontColor -> col}]];

addBackCol[col_, None][tbox_]        := tbox;
addBackCol[col_, bg_][tbox_]         := Append[tbox, Background -> bg];
addBackCol[col_, Opacity[r_]][tbox_] := Append[tbox, Background -> Opacity[r, col]];

toBackCol[col_, Opacity[r_], _] := Opacity[r, col];
toBackCol[col_, bg_, def_]      := bg;
toBackCol[col_, None, def_]     := def;

(**************************************************************************************************)

makeShape[fn_, pos_, size_, color_] :=
  fn[pos, size, color];

$knownShapes = Dict[
  "Point"        -> Fn @ Style[Point @ #1, APointSize @ #2, #3],
  "Disk"         -> Fn @ edged[#3] @ Disk[#1, #2 * $scale],
  "OpenDisk"     -> Fn @ List[whiteDisk[#1, #2, #3], Style[Circle[#1, #2 * $scale], #3]],
  "Ring"         -> Fn @ List[whiteDisk[#1, #2], edged[#3] @ Annulus[#1, {.8, 1.} * #2 * $scale]],
  "Square"       -> Fn @ edged[#3] @ Rectangle[#1 - #2 * $scale, #1 + #2 * $scale],
  "OpenSquare"   -> Fn @ faceEdged[White, #3] @ Rectangle[#1 - #2 * $scale, #1 + #2 * $scale],
  "DownTriangle" -> Fn @ edged[#3] @ Polygon[Threaded[#1] + ($dtri * #2 * $scale)],
  "Lozenge"      -> Fn @ edged[#3] @ Rectangle[
    #1 - #2 * $scale * {1.5, 1}, #1 + #2 * $scale * {1.5, 1},
    RoundingRadius -> (#2*$scale*0.5)]
];

whiteDisk[p_, sz_] := unedged[White] @ Disk[p, sz * $scale];

$dtri = (Threaded[{0, .8}] + {{-1,0},{0,-1.5}, {1,0}}) * 1.5;

(**************************************************************************************************)

edged[FaceEdge[f_, e_]][pr_] := faceEdged[f, e, pr];
edged[c_][pr_]               := Style[pr, FaceForm @ c, EdgeForm @ Darker[c, .1]];
unedged[c_][pr_]             := Style[pr, FaceForm @ c, EdgeForm @ None];
faceEdged[f_, e_][pr_]       := Style[pr, FaceForm @ f, EdgeForm[Directive[e, AThickness[1.2]]]];

(**************************************************************************************************)

$treePlotVertexSpecs = {
  ItemSpec[NodeTooltips, TrueFn, None],
  ItemSpec[NodeColor,    extColorQ, $defaultColor],
  ItemSpec[NodeSize,     NumberQ],
  ItemSpec[NodeShape,    TrueFn, Inherit, MethodResolution -> $knownShapes]
};

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
  "FirstLastDelta"  -> 0.00125,
  "SharedLeaves"    -> {}
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
  isShared = ConstList[False, vertexCount];
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
  isInnerShared = ConstList[False, vertexCount];

(*   mids = Pick[vrange, isInner];
  Part[$xs, mids] += $firstLastDelta;
  Part[$xs, roots] =
 *)
  Label[done];

  numShared = Len @ sharedLeaves;

  maxD = Max[$d] + 1 + If[numShared > 0, 1, 0];
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

  $share = UDict[];
  If[numShared > 0,
    shareInds = Range[numShared] + vertexCount;
    $share = UDict @ ZipMap[ConstRules, sharedLeaves, shareInds];
    sharedXs = Parts[$xs, sharedLeaves];
    sharedLRs = MinMax /@ sharedXs;
    ZipScan[
      {inds, xs, lr} |-> Set[Part[isInnerShared, inds], Min[AbsDelta[lr, #]] > 0.01& /@ xs],
      sharedLeaves, sharedXs, sharedLRs
    ];
    Scan[Set[Part[isShared, #], True]&, sharedLeaves];
    sx = Mean[Part[$xs, #]]& /@ sharedLeaves;
    sy = Max[$ys] - 0.5;
    JoinTo[$xs, sx];
    JoinTo[$ys, sy + Range[numShared]];
  ];

  vertexCoords = EnsurePackedReals[Flip @ {$xs, maxD - $ys}, ReturnFailed["vertexCoordsError"]];
  coordFn = Switch[rootPosition, Bottom, FlipYOp, Top, Id, Left, Rot90LOp, Right, Rot90ROp,
    _, ReturnFailed["badRootPosition", rootPosition]];

  Module[{origCoords = Take[vertexCoords, vertexCount]},
    $isInner = UDictThread[origCoords, isInner];
    $isShared = UDictThread[origCoords, isShared];
    $isInnerShared = UDictThread[origCoords, isInnerShared];
  ];

  $shrTargets = UDictThread[Part[vertexCoords, Keys @ $share], Part[vertexCoords, Vals @ $share]];
  edgePaths = ExtractIndices[vertexCoords, edgePairs];
  edgePaths = constructEdgePaths[edgePaths, bendRadius, fanoutStyle];

  vertexCoords //= coordFn;
  edgePaths //= coordFn;
  bounds = CoordinateBounds[vertexCoords, {.5, .5}];
  If[numShared > 0,
    KeyValueScan[Set[Part[vertexCoords, #1], Part[vertexCoords, #2, All]]&, $share];
    vertexCoords = Drop[vertexCoords, -numShared];
  ];

  {vertexCoords, edgePaths, bounds}
]

makeFTF = CaseOf[
  1  := {False};
  2  := {False, False};
  n_ := ToList[False, ConstList[True, n-2], False];
];

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
      Replace[Part[$bounds, v], {{} :> Part[$xs, v], e_ :> Mean[e]}]
    ];
  ];
  If[!$globalCentering, Part[$bounds, Part[$parent, v]] //= MinMax[Flatten @ {#, Part[$xs, v]}]&];
)

(*************************************************************************************************)

vertexEdgeIndexGraph[g_] := IndexEdgeTaggedGraph @ IndexGraph @ g;

(*************************************************************************************************)

constructEdgePaths[edgePaths_, bendRadius_, fanoutStyle_] := Locals[
  $r = bendRadius;
  SetAuto[$r, 0.333 * MinDist @ Join[Col1 @ edgePaths, ColN @ edgePaths]];
  Switch[fanoutStyle,
    "Top" | Top,       Map[bendTop, edgePaths],
    "Circuit",         Map[bendCenter, edgePaths],
    "Center" | Center, Map[bendCenterFraction[#, 0.5]&, edgePaths],
    Center -> _,       Map[bendCenterFraction[#, Last @ fanoutStyle]&, edgePaths],
    "Bottom" | Bottom, Map[bendBot, edgePaths],
    Auto | None,       edgePaths,
    _,
      Message[OrderedTreeLayout::badopt, fanoutStyle -> fanoutStyle];
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

bendTop[{a:{ax_, ay_}, b:{bx_, by_}}] /; $isShared[b] := Then[
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

