SystemExports[
  "GraphicsOption",
    GraphScale, AspectRatio,
    SplitPosition,
    NodeData, NodeTooltips, NodeColor, NodeSize, NodeShape, NodeThickness,
    RootPosition,
    NodeGroups, GroupStyles,
    NodeBackground, NodeOffset,
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
  NodeThickness     -> 1,
  NodeData          -> Auto,
  NodeGroups        -> None,
  GroupStyles       -> None,
  NodeOffset        -> {0, 0},
  SplitPosition     -> Top,
  Rounding          -> 0.25,
  PMargin           -> 0.5,
  FontSize          -> 10,
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

SetStrict[NiceTreePlot];

NiceTreePlot[edges:{___Rule}, opts___Rule] := NiceTreePlot[Graph[edges], opts];

NiceTreePlot[graph_Graph, opts___Rule] := Locals @ CatchMessages[

  CheckOptKeys[NiceTreePlot, opts];

  UnpackSymbolsAs[
    NiceTreePlot, List @ opts,
    graphScale, edgeColor, edgeThickness,
    rootPosition, baselinePosition,
    nodeData, aspectRatio,
    fontSize, fontWeight, fontFamily,
    sharedLeaves, splitPosition, rounding, pMargin,
    nodeThickness, $nodeBackground, $nodeOffset
  ];

  AssertOptsValid[
    NodeThickness -> nodeThickness -> NumQ,
    GraphScale    -> graphScale    -> NumQ,
    EdgeColor     -> edgeColor     -> ColorQ,
    EdgeThickness -> edgeThickness -> NumQ
  ];

  vertexCount = VertexCount @ graph;
  If[vertexCount === 0, Return @ Graphics[{}, ImageSize -> graphScale]];

  {vertexCoords, edgeCoords, plotBounds} = OrderedTreeLayout[graph,
    RootPosition -> rootPosition, "LayerDepths" -> aspectRatio,
    SharedLeaves -> sharedLeaves, PMargin -> pMargin,
    SplitPosition -> splitPosition, RoundingRadius -> rounding
  ];

  {{plotL, plotR}, {plotB, plotT}} = plotBounds;
  {plotW, plotH} = plotSize = Dist @@@ plotBounds;
  imageSize = plotSize * graphScale;
  $scale = 0.5 / graphScale;

  edgeStyle = Directive @ AThickness @ edgeThickness;
  nodeStyle = EdgeForm @ AThickness @ nodeThickness;

  itemData = transposeDict @ GraphVertexAnnotations @ graph;
  If[nodeData =!= Auto, AppendTo[itemData, nodeData]];

  vertexList = VertexList @ graph;
  vertexSpecs = ParseItemOptions[NiceTreePlot,
    $treePlotVertexSpecs, vertexList, {opts}, UseBroadcast -> Auto,
    ItemData -> itemData, ItemGroups -> NodeGroups, GroupSettings -> GroupStyles
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
    Style[vertexPrims, nodeStyle, Seq @@ fontStyle]
  ];

  basePos = Switch[baselinePosition,
    Root, Scaled[(Part[vertexCoords, 1, 2] - plotB - $meanSize/graphScale/1.25) / plotH],
    Top | Bottom | Center, baselinePosition,
    _,    Part @ VertexIndex[graph, baselinePosition] - graphScale
  ];

  Graphics[
    primitives,
    PRange -> plotBounds,
    PMargin -> 0,
    BaselinePosition -> basePos,
    ImageSize -> imageSize
  ]
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
  margins = If[bg === White, {{0,0},{0,0}}, {{2,1},{1,1}}];
  rounding = If[bg === White, 0, 2];
  Text[
    Framed[
      str, opts,
      FrameMargins -> margins, RoundingRadius -> rounding,
      $frameOpts,
      If[color =!= $defaultColor, BaseStyle -> {FontColor -> color}, Seq @@ {}],
      Background -> bg,
      FrameStyle -> {AThickness[0], Darker[color,.1]}
    ],
    Offset[$nodeOffset, pos]
  ]
];

$frameOpts = Seq[
  Alignment -> Scaled[0.5], ContentPadding -> False
];

(**************************************************************************************************)

(* makeShape[Text[str_], pos_, size_, color_] := List[
  {APointSize[fontSize*.5], White, Point @ Offset[$nodeOffset, pos]},
  mkText[str, Offset[$nodeOffset + {-0.75, 0}, pos], White],
  mkText[str, Offset[$nodeOffset + { 0.75, 0}, pos], White],
  mkText[str, Offset[$nodeOffset + { 0,    0}, pos], If[col === $defaultColor, Inherited, color]]
];
 *)
makeShape[Text[str_], pos_, size_, color_] := List[
  {APointSize[fontSize*.5], White, Point @ Offset[$nodeOffset, pos]},
  DropShadowing[{0,0}, {"SoftDilation",2}, White],
  mkText[str, Offset[$nodeOffset,pos], If[col === $defaultColor, Inherited, color]]
];

mkText[str_, pos_, col_, opts___] := Text[
  Style[str, opts, FontColor -> col],
  pos
];

(**************************************************************************************************)

addTextColor[col_] := If[col === $defaultColor, Id, Append[BaseStyle -> {FontColor -> col}]];

toBackCol[col_, Opacity[r_], _] := Opacity[r, col];
toBackCol[col_, bg_, def_]      := bg;
toBackCol[col_, Auto, def_]     := def;
toBackCol[col_, None, def_]     := None;

(**************************************************************************************************)

makeShape[shape_Str, pos_, size_, color_] := $shapeFns[shape][pos, size, color];

$shapeFns = UDict[
  "Point"        -> Fn @ Style[Point @ #1, APointSize @ #2, #3],
  "Disk"         -> Fn @ List[
    facedDisk[#1, #2, #3],
    edgedCirc[#1, #2, darker @ #3]
  ],
  "OpenDisk"     -> Fn @ List[whiteDisk[#1, #2], Style[Circle[#1, #2 * $scale], #3]],
  "Ring"         -> Fn @ List[
    whiteDisk[#1, #2],
    facedRing[#1, #2, #3],
    edgedCirc[#1, #2, darker @ #3]
  ],
  "Square"       -> Fn @ edged[#3] @ Rectangle[#1 - #2 * $scale, #1 + #2 * $scale],
  "OpenSquare"   -> Fn @ faceEdged[White, #3] @ Rectangle[#1 - #2 * $scale, #1 + #2 * $scale],
  "DownTriangle" -> Fn @ edged[#3] @ Polygon[Threaded[#1] + ($dtri * #2 * $scale)],
  "Lozenge"      -> Fn @ edged[#3] @ Rectangle[
    #1 - #2 * $scale * {1.5, 1}, #1 + #2 * $scale * {1.5, 1},
    RoundingRadius -> (#2*$scale*0.5)]
];

facedRing[p_, sz_, c_] := makeFE[Annulus[p, {.5, 1} * sz * $scale], c, None];
facedDisk[p_, sz_, c_] := makeFE[Disk[p, sz * $scale], c, None];
edgedCirc[p_, sz_, c_] := makeFE[Disk[p, sz * $scale], None, c];
whiteDisk[p_, sz_]     := makeFE[Disk[p, sz * $scale], White, None];

$dtri = (Threaded[{0, .8}] + {{-1,0},{0,-1.5}, {1,0}}) * 1.5;

(**************************************************************************************************)

darker[c_] := Darker[c, .125];

edged[FaceEdge[f_, e_]][pr_] := faceEdged[f, e, pr];
edged[c_][pr_]               := makeFE[pr, c, darker @ c];
faced[c_][pr_]               := makeFE[pr, c, None];
faceEdged[f_, e_][pr_]       := makeFE[pr, f, e];

makeFE[pr_, f_, e_] := Style[pr, FaceForm @ f, EdgeForm @ e];

(**************************************************************************************************)

makeShape[fn_, pos_, size_, color_] := fn[pos, size, color];

(**************************************************************************************************)

SetPred1[validShapeFnQ, maybeGraphicsQ];

validShapeFnQ = ExtendCaseOf[
  Text[_]            := True;
  Framed[_, ___Rule] := True;
  name_Str           := KeyExistsQ[$shapeFns, name];
  fn_ ? MaybeFnQ     := FastQuietCheck[maybeGraphicsQ[fn[{0,0}, 1, Black]], False]
];

finalShapeFn[fn_] := fn;

maybeGraphicsQ = ExtendCaseOf[
  $head$[_, ___]  := True;
  sym_Sym[___]    := GPrimSymQ[sym];
  $wrap$[s_, ___] := $ @ s
,
  {$head$ -> Alt[_Text, _Inset],
   $wrap$ -> Alt[_Style, _Tooltip, _Annotation, _EventHandler, _Translate]}
];

(**************************************************************************************************)

$knownShapes = Keys @ $shapeFns;

General::badShapeFnResult = "NodeShape function `` returned invalid graphics: ``.";

shapeError[_, fn_ ? MaybeFnQ] := ThrowMsg["badShapeFnResult", fn, fn[{0,0}, 1, Black]];
shapeError[_, value_] := ErrorOptVal[NodeShape, value, LitStr @ StrJoin[
  "it should be None, a function taking pos, size, color, or one of ",
  QuotedStringList @ $knownShapes
]];


$treePlotVertexSpecs = {
  ItemSpec[NodeTooltips, TrueFn, None],
  ItemSpec[NodeColor,    extColorQ, $defaultColor],
  ItemSpec[NodeSize,     NumberQ],
  ItemSpec[NodeShape,    validShapeFnQ, Inherit, finalShapeFn, ItemMessageFn -> shapeError]
};

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
  "GlobalCentering"  -> False,
  "FirstLastDelta"   -> 0.00125,
  "SharedLeaves"     -> {},
  "PlotRangePadding" -> 0.5
};

OrderedTreeLayout::vertexCoordsError = "Couldn't construct vertex coordinates.";
OrderedTreeLayout::badRootPosition = "Bad root position ``.";
OrderedTreeLayout::badRoot = "Specified root vertex `` doesn't occur in the graph.";

OrderedTreeLayout[graph_, OptionsPattern[]] := Locals[

  UnpackOptions[
    rootPosition, rootVertex, plotRangePadding, splitPosition, roundingRadius, layerDepths,
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
    sx = MapP[Mean[Part[$xs, #] + If[EvenQ[#2], .2, -.2]]&, sharedLeaves];
    sy = Max[$ys] + .5;
    sys = sy + Range[numShared] * .2;
    JoinTo[$xs, sx];
    JoinTo[$ys, sys];
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
  edgePaths = constructEdgePaths[edgePaths, rounding, splitPosition];

  vertexCoords //= coordFn;
  edgePaths //= coordFn;
  bounds = CoordinateBounds[vertexCoords, plotRangePadding];
  If[numShared > 0,
    KeyValueScan[Set[Part[vertexCoords, #1], Part[vertexCoords, #2, All]]&, $share];
    vertexCoords = Drop[vertexCoords, -numShared];
  ];

  vertexCoords = EnsurePackedReals[vertexCoords, ReturnFailed["vertexCoordsError"]];

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

