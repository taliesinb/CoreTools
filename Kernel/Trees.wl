PackageExports[
  "Function",
    OrderedTreeLayout,
    NestedListsToTree,
    AllTreesDepth, AllTrees,
    DeclareTreeThread,
    TreeUnfold, DecoratedTreeUnfold,
    TreeMapThread, TreeThread, TreeThreaded,
    StochasticTreeUnfold,
    RemoveTreeDecorations,
    VertexEdgeIndexGraph,
  "GraphicsFunction",
    ExprTreePlot, NiceTreePlot,
  "GraphicsBoxFunction",
    ExprTreePlotBoxes,
  "PatternSymbol",
    DecoratedExprP, AnyTreeExprP,
  "Predicate",
    TreeExprQ,
  "Head",
    TreeNode, TreeLeaf, DecoratedNode, DecoratedLeaf, TreeSeed, FutureNode,
    Decorated,
  "SpecialVariable",
    $CurrentTreeDepth, $MaxTreeDepth,
  "OptionSymbol",
    GraphScale,
    NodePattern,
    NodeColorRules,
    VertexColorFunction
];

(**************************************************************************************************)

Options[NiceTreePlot] = {
  GraphScale          -> 10,
  VertexSize          -> 4,
  VertexColorFunction -> Inherited,
  VertexColors        -> Inherited,
  VertexTooltips      -> Inherited,
  EdgeColor           -> Auto,
  EdgeThickness       -> 1
};

NiceTreePlot[graph_Graph, opts:OptionsPattern[]] := Locals[
  UnpackOptions[graphScale, vertexSize, vertexColors, vertexTooltips, vertexColorFunction, edgeColor, edgeThickness];

  leafVertexSize = vertexSize;
  nodeVertexSize = vertexSize - 0.5;

  If[VertexCount @ g, Return @ Graphics[{}, ImageSize -> graphScale]];
  {vertCoords, edgeCoords} = OrderedTreeLayout[graph, $treePlotLayoutOptions];
  plotBounds = CoordinateBounds[vertCoords, {.5, .5}];
  plotSize = Dist @@@ plotBounds;
  imageSize = plotSize * graphScale;

  edgeStyle = Directive[AbsoluteThickness @ edgeThickness];
  vertexStyle = Directive[AbsolutePointSize @ vertexSize];

  {graphVertexColorFn, graphVertexColors, graphVertexTooltips} =
    GraphProperties[graph, {VertexColorFunction, VertexColors, VertexTooltips}];

  SetInherited[vertexColorFunction, graphVertexColorFn];
  SetInherited[vertexColors,        graphVertexColors];
  SetInherited[vertexTooltips,      graphVertexTooltips]

  SetNone[vertexColorFunction, None];
  If[vertexColorFunction =!= Auto,
    vertexColors = Map[vertexColorFunction, VertexList @ graph]];

  SetAuto[vertexColors, GrayLevel[0.3]];

  vertexPrims = Point @ vertCoords;
  Which[
    ColorQ @ vertexColors,
      AppendTo[vertexStyle, vertexColors];
    ,
    ColorVectorQ @ vertexColors,
      If[!SameLenQ[vertexColors, vertCoords], ReturnFailed[]];
      SetAuto[edgeColor, GrayLevel[0.8]];
      vertexPrims //= ZipPrims[Style, vertexColors],
    True,
      ReturnFailed[]
  ];
  SetAuto[edgeColor, GrayLevel[0.5]];
  If[ListQ[vertexTooltips], vertexPrims //= ZipPrims[Tooltip, vertexTooltips]];

  AppendTo[edgeStyle, edgeColor];

  primitives = List[
    Style[Line @ edgeCoords, edgeStyle],
    Style[vertexPrims, vertexStyle]
    (* List[GrayLevel[0.5], AbsolutePointSize[nodeVertexSize], Point @ Part[vertCoords, nodeIndices]] *)
  ];

  Graphics[primitives,
    PlotRange -> plotBounds,
    PlotRangePadding -> 0,
    ImageSize -> imageSize
  ]
];

(* TODO: expose this! *)
ZipPrims[f_, rest__][l_List] := ZipMap[f, l, rest];
ZipPrims[f_, rest__][prim_] := ZipMap[f, Thread @ prim, rest];

$treePlotLayoutOptions = Seq[
  "BendRadius"      -> 0.25,
  "FanoutStyle"     -> "Top",
  "LayerDepths"     -> 1,
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
  "Orientation"     -> Top,
  "RootVertex"      -> Auto,
  "BendRadius"      -> Auto,
  "FanoutStyle"     -> Auto,
  "LayerDepths"     -> Auto,
  "GlobalCentering" -> True,
  "FirstLastDelta"  -> 0.00125
};

OrderedTreeLayout::vertexCoordsError = "Couldn't construct vertex coordinates.";
OrderedTreeLayout::badRoot = "Specified root vertex `` doesn't occur in the graph.";

OrderedTreeLayout[graph_, OptionsPattern[]] := Locals[

  UnpackOptions[
    orientation, rootVertex, fanoutStyle, bendRadius, layerDepths,
    layerDepths, $globalCentering, $firstLastDelta
  ];

  If[VertexCount[graph] === 0, Return @ {{}, {}}];
  indexGraph = VertexEdgeIndexGraph @ graph;
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
  If[EdgeCount[indexGraph] == 0, $xs = vrange; Goto[Done]];
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
  Label[Done];
  $maxD = Max[$d] + 1;
  If[layerDepths =!= {},
    numDepths = Len @ layerDepths;
    PrependTo[layerDepths, 0];
    If[$maxD >= numDepths,
      depthDelta = Subtract @@ Part[layerDepths, {-1, -2}];
      newDepths = Last[layerDepths, 1] + Range[$maxD - numDepths] * depthDelta;
      JoinTo[layerDepths, newDepths]
    ];
    $ys = N @ Part[layerDepths, $ys + 1];
  ];

  $xDelta = $firstLastDelta / 128;
  vertexCoords = EnsurePackedReals[Transpose[{$xs, $maxD - $ys}], ReturnFailed["vertexCoordsError"]];
  $isInner = UAssocThread[vertexCoords, isInner];
  edgePaths = ExtractIndices[vertexCoords, edgePairs];
  edgePaths = constructEdgePaths[edgePaths, bendRadius, fanoutStyle];

  {vertexCoords, edgePaths}
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

VertexEdgeIndexGraph[g_] := IndexEdgeTaggedGraph @ IndexGraph @ g;

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
  Join[{a}, BezierPoints[{ca, c, cd}], BezierPoints[{dc, d, db}], {b}]
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

(**************************************************************************************************)

DefinePatternRules[TreeExprP      -> _TreeNode | TreeLeaf]
DefinePatternRules[DecoratedExprP -> _DecoratedNode[___] | _DecoratedLeaf]
DefinePatternRules[AnyTreeExprP   -> _DecoratedNode[___] | _DecoratedLeaf | _TreeNode | TreeLeaf];

DefinePatternPredicateRules[TreeExprQ -> AnyTreeExprP]

(**************************************************************************************************)

CoreBoxes[t_TreeSeed]          := treeNodePlotBoxes @ t;
CoreBoxes[t:FutureNode]        := treeNodePlotBoxes @ t;
CoreBoxes[t_TreeNode]          := treeNodePlotBoxes @ t;
CoreBoxes[t:TreeLeaf]          := treeNodePlotBoxes @ t;
CoreBoxes[t_DecoratedLeaf]     := treeNodePlotBoxes @ t;

DeclareCoreSubBoxes[DecoratedNode];
MakeCoreBoxes[t:_DecoratedNode[___]] := treeNodePlotBoxes @ t;

treeNodePlotBoxes[tree_] := ExprTreePlotBoxes[tree, NodePattern -> AnyTreeExprP | _TreeSeed | FutureNode];

(**************************************************************************************************)

NestedListsToTree[expr_] := expr /. {List -> TreeNode, DatumP -> TreeLeaf};

AllTrees[d_Int] := AllTrees[d, 2];
AllTrees[0,     b_Int] := {};
AllTrees[d_Int, b_Int] := AllTrees[d, b] = Join[AllTrees[d-1, b], AllTreesDepth[d, b]];

AllTreesDepth[d_Int, b_Int] := AllTreesDepth[d, b] = Map[NestedListsToTree, Groupings[d, b]];

(**************************************************************************************************)

ExprTreePlotBoxes[expr_, opts:OptionsPattern[]] := Which[
  LeafCount[expr] > 256, FnBracketRowBox[ToBoxes @ Head @ expr, List @ SkeletonBox @ NatStr @ LeafCount @ expr],
  Depth[expr] > 24,      ToBoxes @ ExprTreePlot[Replace[expr, _ -> TreeLeaf, {16}], opts],
  True,                  ToBoxes @ ExprTreePlot[expr, opts]
];

Options[ExprTreePlot] = Options[ExprTreePlotBoxes] = JoinOptions[
  {NodePattern -> Auto, NodeColorRules -> None},
  Options @ NiceTreePlot
];

ExprTreePlot[expr_, opts:OptionsPattern[]] := Locals[
  UnpackOptions[nodePattern, nodeColorRules];
  SetAuto[nodePattern, _];
  paths = FindExprPaths[expr, nodePattern];
  nodes = Extract[expr, List @@@ paths];
  vertexColors = If[nodeColorRules === None,
    Map[toNodeColor, nodes],
    VectorReplace[nodes, ToList[nodeColorRules, _ -> Black]]
  ];
  graph = PrefixGraph @ paths;
  NiceTreePlot[graph, VertexColors -> vertexColors, NarrowOptions @ opts]
];

toNodeColor = CaseOf[
  i:NatP            := Part[$MediumColorPalette, i + 1];
  c:ColorP          := c;
  TreeSeed[i_]      := $ @ i;
  DecoratedNode[i_][___] := $ @ i;
  DecoratedLeaf[i_] := $ @ i;
  _TreeNode         := $Gray;
  TreeLeaf          := Black;
  FutureNode        := White;
  None | Null       := $Gray;
  i_Real ? UnitNumberQ := GrayLevel[i];
  i_Real            := NiceHue @ i;
  e_                := HashToColor @ Hash @ Head @ e;
];

(**************************************************************************************************)

DeclareDeclare[DeclareTreeThread]

DeclareTreeThread[sym_Symbol] := (
  sym[TreeLeaf]                                 := TreeLeaf;
  sym[a_TreeNode]                               := Map[sym, a];
  sym[a_TreeNode, b_TreeNode] /; SameLenQ[a, b] := ZipMap[sym, a, b];
  sym[TreeLeaf, _]                              := TreeLeaf;
  sym[_, TreeLeaf]                              := TreeLeaf;
  sym[a_, b_TreeNode]                           := Map[z |-> sym[a, z], b];
  sym[a_TreeNode, b_]                           := Map[z |-> sym[z, b], a];
);

(**************************************************************************************************)

Decorated[s_, t_TreeNode] := DecoratedNode[s] @@ t;
Decorated[s_, TreeLeaf]   := DecoratedLeaf[s];
Decorated[s_, e_]         := e;

(**************************************************************************************************)

TreeUnfold[f_, s_, maxD_:16] := Locals[
  $unfoldFn = f; $CurrentTreeDepth = 0; $MaxTreeDepth = maxD;
  unfoldStep0 @ s
];

StochasticTreeUnfold[f_, s_, maxD_:16] := TreeUnfold[f /* RuleVectorChoice, s, maxD];

unfoldStep0[s_] /; $CurrentTreeDepth >= $MaxTreeDepth := FutureNode;
unfoldStep0[s_] := BlockIncrement[$CurrentTreeDepth, unfoldStep1 @ $unfoldFn @ s];

unfoldStep1 = CaseOf[
  l_DecoratedLeaf         := l;
  TreeLeaf                := TreeLeaf;
  t:(_DecoratedNode[___]) := Map[$, t];
  t_TreeNode              := Map[$, t];
  e_                      := unfoldStep0[e];
];

(**************************************************************************************************)

RemoveTreeDecorations[tree_] := ReplaceAll[tree, {_DecoratedNode -> TreeNode, _DecoratedLeaf -> TreeLeaf}];

(**************************************************************************************************)
(*
DecoratedTreeUnfold[f_, s_, maxD_:16] := Locals[
  $unfoldFn = f; $CurrentTreeDepth = 0; $MaxTreeDepth = maxD;
  decUnfoldStep0 @ s
];

DecoratedStochasticTreeUnfold[f_, s_, maxD_:16] := TreeUnfold[f /* RuleVectorChoice, s, maxD];

decUnfoldStep0[s_] /; $CurrentTreeDepth >= $MaxTreeDepth := DecoratedLeaf[s];
decUnfoldStep0[s_] := BlockIncrement[$CurrentTreeDepth, decUnfoldStep1 @ $unfoldFn @ s];

decUnfoldStep1 = CaseOf[
  l_DecoratedLeaf         := l;
  TreeLeaf                := TreeLeaf;
  t:(_DecoratedNode[___]) := Map[$, t];
  t_TreeNode              := Map[$, t];
  e_                      := decUnfoldStep0[e];
];
 *)
(**************************************************************************************************)

TreeMapThread[f_, args_List] := iTreeThread0[f, args];
TreeThread[f_[args___]]      := iTreeThread0[f, List @ args];
TreeThreaded[f_][args___]    := iTreeThread0[f, List @ args];

iTreeThread0[f_, {}]     := f[];
iTreeThread0[f_, {a_}]   := Locals[$threadFn = f; iTreeMap[a]];
iTreeThread0[f_, a_List] := Locals[$threadFn = f; iTreeMapThread[a]];

iTreeMap = CaseOf[
  t:(_DecoratedNode[___]) := Map[iTreeMap, t];
  t_TreeNode              := Map[iTreeMap, t];
  n_DecoratedLeaf         := n;
  TreeLeaf                := TreeLeaf;
  expr_                   := $threadFn @ expr;
];

iTreeMapThread[list_List] := Which[
  MemberQ[list, TreeLeaf],  TreeLeaf,
  MemberQ[list, _TreeNode], iTreeMapThread /@ FastQuietCheck[Thread[list, TreeNode], TreeLeaf],
  True,                     $threadFn @@ list
];

