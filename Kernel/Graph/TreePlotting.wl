SystemExports[
  "PlotOption",
    GraphScale, HStretch, VStretch,
    SplitPosition, TreeLayout,
    NodeData, ColorLegend,
    NodeTooltips, NodeClickFunction, NodeClickData,
    NodeLabel, EdgeLabel,
    EdgeTooltips, EdgeClickFunction, EdgeClickData,
    NodeColor, NodeSize, NodeShape, NodeThickness,
    RootPosition,
    NodeGroups, NodeStyles,
    NodeBackground, NodeOffset,
    SharedLeaves, ReverseEdges, EdgeFn,
    SplayDistance,
    DebugItemData,
    FrameOptions
];

PackageExports[
  "SymbolicHead", FaceEdge,
  "PlotOption",   SplitPos, NodeClickFn, EdgeClickFn,
  "PlotFn",       PolyGraphPlot, TreeGraphPlot, InsetNode
];

(**************************************************************************************************)

"TreeGraphPlot[graph$, opts$] gives customizable plotting of a DAG.

The following options can be specified to be a single global value, or can be
computed per-item based on a properties or derived properties.

For vertices:
| NodeTooltips |
| NodeClickFn |
| NodeClickData |
| NodeColor |
| NodeSize |
| NodeShape |

For edges:
| EdgeLabel     |
| EdgeTooltips  |
| EdgeClickFn   |
| EdgeClickData |

The way values are specified is as follows:
| value$ or Broadcast[value$] | a single global value |
| 'Name' | the vertex / edge itself |
| 'EdgeTag' | the tag of an edge (if present) |
| 'Index' | the integer index in order of EdgeList or VertexList |
| 'anno$' | take the per-item value of the annotation with given key |

The options NodeGroups and NodeStyles can be used to give 'themes' that
apply whenever a property has a particular value. This is useful for
discrete-valued properties.

NodeGroups can be one of the following:
| {n$1, n$2, $$} | the group(s) that each vertex is a member of |
| fn$            | a function mapping vertex names to groups |
| assoc$         | an association mapping as above (with special key DefaultValue) |
| 'key$'         | one of the vertex annotation keys |
| rules$         | use pattern matching on vertices to derive groups |

A 'group assignment' mentioned above is either a list of strings, which allows
a vertex to be a member of multiple groups (their styles are combined), or, a
single string.

Once groups are assigned, styles will be applied by looking up 'overrides' for
each group. These are contained in NodeStyles, which is an association of the
form <| 'group' -> options$ |>.

The options are the very same options (e.g. NodeTooltips etc) as mentioned above.
They simply override that *part* of the graph.

TODOs:
* allow EdgeFn to be edge-data customized
"

Options[TreeGraphPlot] = {
  GraphScale        -> 30,
  TreeLayout        -> Auto,
  HStretch          -> 1.0,
  VStretch          -> 1.0,

  NodeLabel         -> None,
  NodeTooltips      -> None,
  NodeClickFn       -> None,
  NodeClickData     -> "Name",
  NodeColor         -> Auto,
  NodeSize          -> 5,
  NodeShape         -> "Disk",
  NodeThickness     -> 1,
  NodeData          -> Auto,
  NodeGroups        -> None,
  NodeOffset        -> {0, 0},
  NodeStyles        -> None,
  NodeBCol          -> None,

  EdgeLabel         -> None,
  EdgeTooltips      -> None,
  EdgeClickFn       -> None,
  EdgeClickData     -> "Name",
  EdgeColor         -> Black,
  EdgeThickness     -> 1,
  EdgeFn            -> Line,
  Setback           -> 0,

  SplitPosition     -> Top,
  Rounding          -> 0.25,
  SplayDistance     -> 0.2,

  PMargin           -> 0.5,
  BaselinePosition  -> Root,
  FontSize          -> 10,
  FontFamily        -> Inherited,
  FontWeight        -> Inherited,
  FontSlant         -> Inherited,

  PerformanceGoal   -> Auto,
  Balancing         -> Auto,

  RootPosition      -> Top,
  SharedLeaves      -> {},
  ReverseEdges      -> False,
  DebugItemData     -> False,
  FrameOptions      -> {},
  ColorLegend       -> False
};

Options[PolyGraphPlot] = OptionValueRules[TreeGraphPlot,
  HStretch -> 1.2,
  Setback  -> {0, 0}
];

SetPred1 @ extColorQ;
extColorQ[FaceEdge[ColorP, ColorP] | ColorP] := True;

$defaultColor = RGBColor[0.4, 0.4, 0.4];

$treeLayouts = {"OrderedTree", "OrderedGraph",  "UnorderedTree", "UnorderedGraph", Automatic};

SetStrict[TreeGraphPlot];

TreeGraphPlot[edges:{___Rule}, opts___Rule]  := TreeGraphPlot[Graph[edges], opts];
PolyGraphPlot[edges:{___Rule}, opts___Rule] := PolyGraphPlot[Graph[edges], opts];

TreeGraphPlot[graph_Graph, opts___Rule]  := Locals @ CatchMessages[genericTreePlot[TreeGraphPlot, graph, opts]];
PolyGraphPlot[graph_Graph, opts___Rule] := Locals @ CatchMessages[genericTreePlot[PolyGraphPlot, graph, opts]];

General::graphNotDAG = "Input graph should be directed and acyclic."

genericTreePlot[head_, graph_, opts___Rule] := Locals[

  CheckOptKeys[PolyGraphPlot, opts];

  UnpackSymbolsAs[
    head, List @ opts,
    graphScale, treeLayout, performanceGoal, balancing,
    hStretch, vStretch, pMargin,
    nodeData, rootPosition, baselinePosition, sharedLeaves,
    fontSize, fontWeight, fontFamily, fontSlant,
    $splitPos, $rounding, $splayDistance,
    nodeThickness, $nodeBCol, $nodeOffset,
    setback, reverseEdges, debugItemData,
    edgeTooltips, edgeClickFn, edgeClickData, edgeFn,
    nodeLabel, edgeLabel, nodeColor,
    $frameOptions, colorLegend
  ];

  {$s1, $s2} = EnsurePair[setback, NumQ];

  AssertOptsValid[
    NodeThickness -> nodeThickness -> NumQ,
    GraphScale    -> graphScale    -> NumQ
  ];

  If[!(DirectedGraphQ[graph] && AcyclicGraphQ[graph]), ThrowMessage["graphNotDAG"]];
  vertexCount = VertexCount @ graph;
  {vertexList, edgeList} = VertexEdgeList @ graph;
  If[vertexCount === 0, Return @ Graphics[{}, ImageSize -> graphScale]];

  SetAuto[performanceGoal, If[vertexCount > 16, "Speed", "Quality"]];
  speed = performanceGoal === "Speed";

  AssertInQ[treeLayout, $treeLayouts];
  SetAuto[treeLayout, If[head === PolyGraphPlot, "UnorderedTree", If[speed, "UnorderedGraph", "OrderedTree"]]];

  {isTree, isGraph} = StringEndsQ[treeLayout, #]& /@ {"Tree", "Graph"};
  {isOrdered, isUnordered} = StringStartsQ[treeLayout, #]& /@ {"Ordered", "Unordered"};

  Which[
    isUnordered,
      layoutData = DAGLayout[graph,
        RootPosition  -> rootPosition,
        HStretch      -> hStretch,
        VStretch      -> vStretch,
        PMargin       -> pMargin,
        Balancing     -> IfAuto[balancing, isTree]
      ],
    isOrdered,
      Switch[$splitPos,
        Top, $splitPos = Scaled[0.0],
        Bot, $splitPos = Scaled[1.0],
        Cen, $splitPos = Scaled[0.5],
        Scaled[NumP] | NumP, Null,
        None, Null,
        _, ErrorOptVal[SplitPos, $splitPos];
        $splitPos = Scaled[0.5];
      ];
      layoutData = OrderedTreeLayout[graph,
        RootPosition  -> rootPosition,
        SharedLeaves  -> sharedLeaves,
        HStretch      -> hStretch,
        VStretch      -> vStretch,
        PMargin       -> pMargin,
        SplitPos      -> If[isTree, $splitPos, None],
        Rounding      -> $rounding,
        Setback       -> {$s1, $s2}
      ],
    _,
      ErrorOptVal[TreeLayout, treeLayout, $treeLayouts];
      ReturnFailed[];
  ];
  If[!MatchQ[layoutData, {PackedP, _List, _List, _List}], ThrowMessage["internalError"]];

  {vertexCoords, edgeCoords, plotBounds, extra} = layoutData;
  {{plotL, plotR}, {plotB, plotT}} = plotBounds;
  {plotW, plotH} = plotSize = Dist @@@ plotBounds;
  imageSize = plotSize * graphScale;
  $scale = 0.5 / graphScale;

  If[Len[extra] === 5 && isTree,
    $maxSplayDistance = 0.15;
    If[ListQ[edgeFn],
      If[Len[edgeFn] =!= 3, ReturnFailed[]];
      {pathFn, fanOFn, fanIFn} = edgeFn
    ,
      pathFn = fanOFn = fanIFn = edgeFn;
    ];
    If[$splitPos === None,
      edgePrims = Map[edgeFn, edgeCoords]
    ,
      If[reverseEdges,
        {$fwdFn, $revFn} = {Rev, Id};
        $lineFn = Id; $arrowFn = Line&;
      ,
        {$fwdFn, $revFn} = {Id, Rev};
        $lineFn = Line&; $arrowFn = Id;
      ];
      {paths, fanOs, fanIs, vertPosDict, edgePosDict} = extra;
      pathPrims = KVMap[pathLines[pathFn, vertPosDict @ P1[#1],    vertPosDict @ P2[#1],    edgePosDict /@ #2, #2]&, paths];
      fanOPrims = KVMap[fanOLines[fanOFn, vertPosDict @ #1,        vertPosDict /@ Col2[#2], edgePosDict /@ #2, #2]&, fanOs];
      fanIPrims = KVMap[fanILines[fanIFn, vertPosDict /@ Col1[#2], vertPosDict @ #1,        edgePosDict /@ #2, #2]&, fanIs];
      edgePrims = List[pathPrims, fanOPrims, fanIPrims];
    ];
  ,
    edgePrims = Map[edgeFn, edgeCoords];
  ];
  edgeStyle = Directive[$niceArrowhead];
  nodeStyle = EdgeForm @ AThickness @ nodeThickness;

  edgeLabelPrims = {};
  Block[{$DebugPrinting = TrueQ @ debugItemData},
    vertexData = transposeDict @ GraphVertexAnnotations @ graph;
    If[nodeData =!= Auto, AppendTo[vertexData, nodeData]];
    vertexSpecs = ParseItemOptions[head,
      $treePlotVertexSpecs, vertexList, {opts}, UseBroadcast -> True,
      ItemData -> vertexData, ItemGroups -> NodeGroups, GroupSettings -> NodeStyles
    ];
    vertexSpecs = Lookup[vertexSpecs, {NodeLabel, NodeShape, NodeSize, NodeTooltips, NodeColor, NodeClickFn, NodeClickData}];
    {vertexPrims, vertexLabelPrims} = makeVertexPrimitives[vertexSpecs, vertexCoords];

    If[head === TreeGraphPlot && EdgeCount[graph] > 0,
      edgeData = transposeDict @ GraphEdgeAnnotations @ graph;
      edgeSpecs = ParseItemOptions[head,
        $treePlotEdgeSpecs, edgeList, {opts}, UseBroadcast -> True,
        ItemData -> edgeData
      ];
      edgeSpecs = Lookup[edgeSpecs, {EdgeLabel, EdgeTooltips, EdgeClickFn, EdgeClickData, EdgeColor, EdgeThickness}];
      {edgePrims, edgeLabelPrims} = makeEdgeLabelPrimitives[edgeSpecs, edgePrims];
    ];
  ];

  fontStyle = DelCases[
    List[FontSize -> fontSize, FontWeight -> fontWeight, FontFamily -> fontFamily, FontSlant -> fontSlant],
    _ -> Inherited
  ];

  vertexPrims = vertexPrims /. Circle[c_, p_] :> {Style[Disk[c, p], FaceForm @ White], Circle[c, p]};

  If[NotEmptyQ[edgeLabelPrims], AppendTo[vertexPrims, edgeLabelPrims]];
  If[NotEmptyQ[vertexLabelPrims],
    AppendTo[vertexPrims, vertexLabelPrims];
    plotBounds[[1]] += {-1, 1} * hStretch * .75;
    imageSize[[1]] += 10;
  ,
    plotBounds[[1]] += {-1, 1} * hStretch * .1;
    plotBounds[[2]] += {-1, 1} * vStretch * .1;
  ];

  primitives = List[
    Style[edgePrims, edgeStyle],
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

(**************************************************************************************************)

$arrowheadCurve = Polygon[
      ToPacked @ ThreadPlus[{-0.4, 0}, 0.8*{{{-0.3166, -0.3333}, {-0.3166, 0.3333}, {0.25, 0.}}}]
    ];

$niceArrowhead = Arrowheads[{{Medium, 1.0, List[Graphics @ $arrowheadCurve, .4]}}];

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

pathLines[fn_, src_, tgt_, paths_List, edges_] :=
  fn @ $fwdFn @ SetbackLine[{$s1, $s2}] @ makeVBend[src, tgt];

fanOLines[fn_, src_, {tgt_}, paths_List, edges_] := pathLines[fn, src, tgt, paths, edges];
fanILines[fn_, {src_}, tgt_, paths_List, edges_] := pathLines[fn, tgt, src, paths, edges];

fanOLines[fn_, src_, tgts_, paths_List, edges_] :=
  makeFan[src, tgts, $fwdFn, $lineFn[fn], $arrowFn[fn], fn, $s1, $s2, True];

fanILines[fn_, srcs_, tgt_, paths_List, edges_] :=
  makeFan[tgt, srcs, $revFn, $lineFn[fn], $arrowFn[fn], fn, $s2, $s1, False];

makeFan[a_, bs_, g_, fn1_, fn2_, fn3_, s1_, s2_, bendy_] := Locals[
  {ax, ay} = a1 = a; bys = Col2 @ bs; n = Len @ bs;
  dy = Sign[Mean[bys] - ay];
  dd = List[0, dy];
  fd = $splitPos;
  SetScaled[fd, Min @ AbsDelta[ay, bys]];
  fd = Clip[If[fd >= 0, fd, 1.0 - fd], {0, 1}];
  bs1 = ThreadPlus[dd * -s2, SortBy[bs, P1]];
  If[bendy,
    If[fd <= 0 || !bendy, stub = None,
      a1 += dd * s1;
      a0 = a1;
      a1 += dd * Max[fd - s1, 0];
      stub = fn1 @ g @ List[a0, a1]
    ];
    {ax, ay} = a1;
    {b1, bn} = FirstLast @ bs1;
    p1 = makeBend[a1, b1];
    pn = makeBend[a1, bn];
    pm = makeMid[ay] /@ Part[bs1, 2;;-2];
    prims = fn2 /@ g /@ List[p1, Splice @ pm, pn];
    If[stub === None, prims, Append[prims, stub]]
  ,
    splay = Range[n]-(n/2. + .5);
    splay *= $splayDistance;
    If[Max[splay] > $maxSplayDistance, splay *= $maxSplayDistance / Max[splay]];
    a1 += dd * s1;
    ZipMap[fn3[g @ makeVBend[a1 + {#2, 0}, #1]]&, bs1, splay]
  ]
];

makeMid[ay_][b:{bx_, by_}] := {{bx, ay}, b};
makeBend[a_, b_] := RoundedCurvePointsFast[cornerPoints[a, b], $rounding, "Arc"];

makeVBend[a:{ax_, ay_}, b:{bx_, by_}] /; Dist[ax, bx] < .05 := {a, b};
makeVBend[a:{ax_, ay_}, b:{bx_, by_}] := BezierPoints[{a, {ax, Avg[ay, by]}, {bx, Avg[ay, by]}, b}];
makeVBend[a:{ax_, ay_}, b:{bx_, by_}] := AngledCurvePoints[{a, b}, JoinStyle -> Vertical, Rounding -> $rounding, Shortcut -> 1.5*$rounding];

cornerPoints[a:{ax_, ay_}, b:{bx_, by_}] := {a, {bx, ay}, b};

(**************************************************************************************************)

InsetNode[content_, fn_:Id, opts___][pos_, size_, color_] :=
  Inset[Style[fn[content], color, opts, Background -> GrayLevel[1,1]], pos, Center, Alignment -> Center];

(**************************************************************************************************)

SetStrict @ makeEdgeLabelPrimitives;

makeEdgeLabelPrimitives[{labels_, tooltips_, clickFns_, clickData_, color_, thickness_}, primitives_] := Locals[

  edgePrims = primitives;

  hasLabels   = BNotNoneQ @ labels;
  hasTooltips = BNotNoneQ @ tooltips;
  hasClicks   = BNotNoneQ @ clickFns;

  If[hasLabels,
    labelPrims = BMap[toEdgeLabel, primitives, labels];
    If[hasTooltips, labelPrims = BMap[Tooltip, labelPrims, tooltips]];
    If[hasClicks,   labelPrims = BMap[makeClicker, labelPrims, clickFns, clickData]];
    labelPrims = Discard[labelPrims, VContainsQ[#, $Failed]&];
  ,
    labelPrims = {}
  ];

  If[hasTooltips, edgePrims = BMap[Tooltip, edgePrims, tooltips]];

  If[hasClicks,   edgePrims = BMap[makeClicker, edgePrims, clickFns, clickData]];

  edgePrims = applyColorThickness[edgePrims, color, thickness];

  {edgePrims, labelPrims}
];

(**************************************************************************************************)

SetStrict[applyColorThickness];

applyColorThickness[prims_List, Broadcast[col_, _], Broadcast[thick_, _]] :=
  Style[prims, col, AbsoluteThickness[thick]];

applyColorThickness[prims_List, Broadcast[col_, _], thick_List] :=
  Style[ZipMap[Style[#1, AbsoluteThickness[#2]]&, prims, thick], col]

applyColorThickness[prims_List, col_List, Broadcast[thick_, _]] :=
  Style[ZipMap[Style, prims, col], AbsoluteThickness @ thick];

applyColorThickness[prims_List, col_List, thick_List] :=
  ZipMap[Style[#1, AbsoluteThickness[#2], #3]&, prims, thick, col]

(**************************************************************************************************)

toEdgeLabel[prim_, None]   := $Failed;
toEdgeLabel[prim_, label_] := makeEdgeLabel[prim, getEdgePos @ prim, label];

getEdgePos[prim_] := DeepFirstCase[prim, (Line|Arrow)[c_] :> chooseLinePos[c], labelPos @ DeepCases[prim, Pos2P]];

chooseLinePos[line_] := Locals[
  {mx, my} = PointAlongLine[line, VScaled[0.5]];
  {cx, cy} = Mean @ FirstLast @ line;
  {mx, cy}
];

makeEdgeLabel[prim_, pos_, Framed[str_, opts___Rule]] := Locals[
  framed = Framed[
    str, opts,
    ToSeq @ $frameOptions,
    FrameMargins -> {{0,0},{0,0}},
    ContentPadding -> False,
    $defaultFrameOpts,
    Background -> White
  ];
  Text[framed, Offset[{0,2}, pos], {0,0}]
];

makeEdgeLabel[prim_, pos_, label_] := List[
  DropShadowing[{0,0}, {"SoftDilation",2}, White], FontColor -> Black,
  Text[label, pos, {0,0}]
];

labelPos[list_] := N @ Map[DelDups /* Median, Round[Flip @ list, .1]];

(**************************************************************************************************)

SetStrict @ makeVertexPrimitives;

makeVertexPrimitives[{labels_, shape_, size_, tooltips_, colors_, clickFns_, clickData_}, coords_] := Locals[

  vertexPrims = BMap[makeShape, shape, coords, size, colors];

  GlobalVar[$meanSize]; $meanSize = Mean @ FromB @ size;

  labelPrims = {};

  If[BNotNoneQ @ tooltips, vertexPrims = BMap[Tooltip, vertexPrims, tooltips]];

  If[BNotNoneQ @ clickFns, vertexPrims = BMap[makeClicker, vertexPrims, clickFns, clickData]];

  If[BNotNoneQ @ labels, labelPrims = BMap[makeLabel, labels, size, coords]];

  {vertexPrims, labelPrims}
];

makeClicker[prim_, None | Auto | Inherit, _] := prim;
makeClicker[prim_, fn_, data_] := Button[prim, fn @ data];

makeLabel[None | Null, _, _] := Nothing;
makeLabel[label_, size_, pos_] := Inset[
  Style[label, Background -> GrayLevel[1,1]],
  Offset[{0, 1} * size, pos], Baseline, Alignment -> Center
];

(**************************************************************************************************)

makeShape[Framed[str_, opts___Rule], pos_, size_, color_] := Locals[
  bg = toBackCol[color, $nodeBCol, White];
  margins = If[bg === White, {{0,0},{0,0}}, {{2,1},{1,1}}];
  rounding = If[bg === White, 0, 2];
  fstyle = If[bg === White, None, Directive @ {AThickness[1], Darker[color,.1]}];
  Text[
    Framed[
      str, opts,
      ToSeq @ $frameOptions,
      FrameMargins -> margins,
      RoundingRadius -> rounding,
      $defaultFrameOpts,
      If[color =!= $defaultColor, BaseStyle -> {FontColor -> color}, Seq @@ {}],
      Background -> bg,
      FrameStyle -> fstyle
    ],
    Offset[$nodeOffset, pos]
  ]
];

$defaultFrameOpts = Seq[
  Alignment -> Scaled[0.5],
  ContentPadding -> False
];

(**************************************************************************************************)

makeShape[None, _, _, _] := {};

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

makeShape[Sized[shape_Str, size_], pos_, _, color_] := $shapeFns[shape][pos, size, color];
makeShape[shape_Str, pos_, size_, color_] := $shapeFns[shape][pos, size, color];

$shapeFns = UDict[
  "Point"         -> Fn @ Style[Point @ #1, APointSize @ #2, #3],
  "Disk"          -> Fn @ List[
    facedDisk[#1, #2, #3],
    edgedCirc[#1, #2, darker @ #3]
  ],
  "OpenDisk"      -> Fn @ List[whiteDisk[#1, #2], Style[Circle[#1, #2 * $scale], #3]],
  "Ring"          -> Fn @ List[
    whiteDisk[#1, #2],
    facedRing[#1, #2, #3],
    edgedCirc[#1, #2, darker @ #3]
  ],
  "Square"        -> Fn @ edged[#3] @ Rectangle[#1 - #2 * $scale, #1 + #2 * $scale],
  "OpenSquare"    -> Fn @ faceEdged[White, #3] @ Rectangle[#1 - #2 * $scale, #1 + #2 * $scale],
  "DownTriangle"  -> Fn @ edged[#3] @ Polygon[Threaded[#1] + ($dtri * #2 * $scale)],
  "Lozenge"       -> Fn @ edged[#3] @ Rectangle[
    #1 - #2 * $scale * {1.5, 1}, #1 + #2 * $scale * {1.5, 1},
    RoundingRadius -> (#2*$scale*0.5)],
  "OpenLozenge" -> Fn @ faceEdged[White, #3] @ Rectangle[
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
  None               := True;
  Text[_]            := True;
  Framed[_, ___Rule] := True;
  name_Str           := KeyExistsQ[$shapeFns, name];
  Sized[name_Str, _] := $ @ name;
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
  ItemSpec[NodeLabel,     None, None],
  ItemSpec[NodeTooltips,  None, None],
  ItemSpec[NodeClickFn,   OrOp[MaybeFnQ, NoneQ], None],
  ItemSpec[NodeClickData, None, None],
  ItemSpec[NodeColor,     extColorQ, $defaultColor],
  ItemSpec[NodeSize,      NumberQ],
  ItemSpec[NodeShape,     validShapeFnQ, Inherit, finalShapeFn, ItemMessageFn -> shapeError]
};

$treePlotEdgeSpecs = {
  ItemSpec[EdgeLabel,     None, None],
  ItemSpec[EdgeColor,     extColorQ, Black],
  ItemSpec[EdgeThickness, NumQ, 1],
  ItemSpec[EdgeTooltips,  None, None],
  ItemSpec[EdgeClickFn,   OrOp[MaybeFnQ, NoneQ], None],
  ItemSpec[EdgeClickData, None, None]
};
