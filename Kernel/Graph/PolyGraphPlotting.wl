SystemExports[
  "GraphicsOption",
    GraphScale, HStretch, VStretch,
    SplitPosition,
    NodeData, NodeTooltips, NodeColor, NodeSize, NodeShape, NodeThickness,
    RootPosition,
    NodeGroups, GroupStyles,
    NodeBackground, NodeOffset,
    SharedLeaves, EdgeFn,
    FanDistance, ReverseEdges
];

PackageExports[
  "GraphicsFunction", PolyGraphPlot,
  "Function", EdgeArityGroups
];

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
  a1 = a; bys = Col2 @ bs; n = Len @ bs;
  dy = Sign[Mean[bys] - P2[a1]];
  dd = List[0, dy];
  fd = $fanDistance;
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
    a1 += dd * s1;
    ZipMap[fn3[g @ makeVBend[a1 + {#2, 0}, #1]]&, bs1, $rounding/3 * splay]
  ]
];

makeMid[ay_][b:{bx_, by_}] := {{bx, ay}, b};
makeBend[a_, b_] := RoundedCurvePointsFast[cornerPoints[a, b], $rounding, "Arc"];

makeVBend[a:{ax_, ay_}, b:{bx_, by_}] /; Dist[ax, bx] < .05 := {a, b};
makeVBend[a:{ax_, ay_}, b:{bx_, by_}] := BezierPoints[{a, {ax, Avg[ay, by]}, {bx, Avg[ay, by]}, b}];
makeVBend[a:{ax_, ay_}, b:{bx_, by_}] := AngledCurvePoints[{a, b}, JoinStyle -> Vertical, Rounding -> $rounding*.75, Shortcut -> $rounding/2];

cornerPoints[a:{ax_, ay_}, b:{bx_, by_}] := {a, {bx, ay}, b};

(**************************************************************************************************)

Options[PolyGraphPlot] = {
  GraphScale        -> 30,
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
  FanDistance       -> 0,
  Rounding          -> 0.25,
  PMargin           -> 0.5,
  FontSize          -> 10,
  HStretch          -> 1.2,
  VStretch          -> 1,
  AspectRatio       -> 1,
  FontFamily        -> Inherited,
  FontWeight        -> Inherited,
  FontSlant         -> Inherited,
  NodeBackground    -> None,
  EdgeColor         -> Black,
  EdgeThickness     -> 1,
  RootPosition      -> Top,
  BaselinePosition  -> Root,
  SharedLeaves      -> {},
  ReverseEdges      -> False,
  EdgeFn            -> Line,
  Setback           -> {0, 0}
};

blendXs[coords_][{x_, y_}] := {N @ Median @ Col1 @ coords, y};

SetPred1 @ extColorQ;
extColorQ[FaceEdge[ColorP, ColorP] | ColorP] := True;

$defaultColor = RGBColor[0.4, 0.4, 0.4];

SetStrict[PolyGraphPlot];

PolyGraphPlot[edges:{___Rule}, opts___Rule] := PolyGraphPlot[Graph[edges], opts];

PolyGraphPlot[graph_Graph, opts___Rule] := Locals @ CatchMessages[

  CheckOptKeys[PolyGraphPlot, opts];

  UnpackSymbolsAs[
    PolyGraphPlot, List @ opts,
    graphScale, edgeColor, edgeThickness,
    rootPosition, baselinePosition,
    nodeData, hStretch, vStretch,
    fontSize, fontWeight, fontFamily, fontSlant,
    sharedLeaves, $rounding, pMargin,
    nodeThickness, $nodeBackground, $nodeOffset,
    edgeFn, setback, $fanDistance, reverseEdges
  ];

  AssertOptsValid[
    NodeThickness -> nodeThickness -> NumQ,
    GraphScale    -> graphScale    -> NumQ,
    EdgeColor     -> edgeColor     -> ColorQ,
    EdgeThickness -> edgeThickness -> NumQ
  ];

  {$s1, $s2} = EnsurePair[setback, NumQ];
  If[reverseEdges,
    {$fwdFn, $revFn} = {Rev, Id};
    $lineFn = Id; $arrowFn = Line&;
  ,
    {$fwdFn, $revFn} = {Id, Rev};
    $lineFn = Line&; $arrowFn = Id;
  ];
  vertexCount = VertexCount @ graph;
  vertexList = VertexList @ graph;
  If[vertexCount === 0, Return @ Graphics[{}, ImageSize -> graphScale]];

  {vertexCoords, pathsDict} = DAGLayout[graph, HStretch -> 1*hStretch, VStretch -> vStretch];

  stretch = ThreadTimesOp[{1.2, 1}];
  plotBounds = CoordinateBounds[vertexCoords, pMargin];

  {{plotL, plotR}, {plotB, plotT}} = plotBounds;
  {plotW, plotH} = plotSize = Dist @@@ plotBounds;
  imageSize = plotSize * graphScale;
  $scale = 0.5 / graphScale;

  If[ListQ[edgeFn],
    If[Len[edgeFn] =!= 3, ReturnFailed[]];
    {pathFn, fanOFn, fanIFn} = edgeFn;
  ,
    pathFn = fanOFn = fanIFn = edgeFn;
  ];

  vertsDict = UDictThread[VertexList @ graph, vertexCoords];
  {paths, fanOs, fanIs} = EdgeArityGroups[graph];
  $y1s = UDict[];
  Do[
    KeyValueMap[ApplyTo[vertsDict[P1 @ #1], blendXs[Lookup[vertsDict, Col2 @ #2]]]&, paths];
    KeyValueMap[ApplyTo[vertsDict[#1], blendXs[Lookup[vertsDict, Col2 @ #2]]]&, fanOs];
    KeyValueMap[ApplyTo[vertsDict[#1], blendXs[Lookup[vertsDict, Col1 @ #2]]]&, fanOs];
  ,
    (* KeyValueMap[Set[vertsDict[#1], Mean @ Col1 @ #2]&, fanOs], *)
    {3}
  ];
  pathPrims = KeyValueMap[pathLines[pathFn, vertsDict @ P1[#1], vertsDict @ P2[#1], pathsDict /@ #2, #2]&, paths];
  fanOPrims = KeyValueMap[fanOLines[fanOFn, vertsDict @ #1, vertsDict /@ Col2[#2], pathsDict /@ #2, #2]&, fanOs];
  fanIPrims = KeyValueMap[fanILines[fanIFn, vertsDict /@ Col1[#2], vertsDict @ #1, pathsDict /@ #2, #2]&, fanIs];
  edgePrims = List[pathPrims, fanOPrims, fanIPrims];
  vertexCoords = Lookup[vertsDict, vertexList];

  edgeStyle = Directive[AThickness @ edgeThickness, $niceArrowhead];
  nodeStyle = EdgeForm @ AThickness @ nodeThickness;

  itemData = transposeDict @ GraphVertexAnnotations @ graph;
  If[nodeData =!= Auto, AppendTo[itemData, nodeData]];

  vertexSpecs = ParseItemOptions[PolyGraphPlot,
    $treePlotVertexSpecs, vertexList, {opts}, UseBroadcast -> Auto,
    ItemData -> itemData, ItemGroups -> NodeGroups, GroupSettings -> GroupStyles
  ];
  vertexSpecs = Lookup[vertexSpecs, {NodeShape, NodeSize, NodeTooltips, NodeColor}];
  vertexPrims = makePrimitives[vertexSpecs, vertexCoords];
  SetAuto[edgeColor, GrayLevel[0.5]];
  AppendTo[edgeStyle, edgeColor];
  fontStyle = DelCases[
    List[FontSize -> fontSize, FontWeight -> fontWeight, FontFamily -> fontFamily, FontSlant -> fontSlant],
    _ -> Inherited
  ];

  vertexPrims = vertexPrims /. Circle[c_, p_] :> {Style[Disk[c, p], FaceForm @ White], Circle[c, p]};
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

$arrowheadCurve = Polygon[
      ToPacked @ ThreadPlus[{-0.4, 0}, 0.8*{{{-0.3166, -0.3333}, {-0.3166, 0.3333}, {0.25, 0.}}}]
    ];

(*   FilledCurve[
    {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
    ToPacked @ ThreadPlus[{-0.4, 0}, {{{-0.3166, -0.25}, {-0.1833, 0.}, {-0.3166, 0.25}, {0.25, 0.}}}]
  ]
 *)
$niceArrowhead = Arrowheads[{{Medium, 1.0, List[Graphics @ $arrowheadCurve, .4]}}];

(**************************************************************************************************)

EdgeArityGroups[graph_Graph] := Locals[
  verts = VertexList @ graph;
  edges = EdgeList @ graph;
  toDegDict = Function[UDictThread[verts] @ Clip[#, {0, 2}]];
  odeg = toDegDict @ VertexOutDegree[graph];
  ideg = toDegDict @ VertexInDegree[graph];
  paths = UDict[]; (* {src, tgt} -> edges *);
  fanOs = UDict[]; (* src -> edges *);
  fanIs = UDict[]; (* tgt -> edges *);
  procEdge = Function[Switch[{odeg[P1 @ #1], ideg[P2 @ #]},
    {2, 1}, KeyAppendTo[fanOs, P1 @ #, #],
    {_, 2}, KeyAppendTo[fanIs, P2 @ #, #],
    _,      KeyAppendTo[paths, {P1 @ #, P2 @ #}, #]]];
  Scan[procEdge, edges];
  {paths, fanOs, fanIs}
];

(**************************************************************************************************)

transposeDict[_]         := None;
transposeDict[annos_Dict] := Locals[
  {verts, dicts} = KeysVals @ Select[annos, DictQ];
  keys = Union @@ Map[Keys, dicts];
  vals = Map[key |-> Lookup[dicts, key, None], keys];
  DictThread[keys, vals]
];

(**************************************************************************************************)

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
  fstyle = If[bg === White, None, Directive @ {AThickness[1], Darker[color,.1]}];
  Text[
    Framed[
      str, opts,
      FrameMargins -> margins, RoundingRadius -> rounding,
      $frameOpts,
      If[color =!= $defaultColor, BaseStyle -> {FontColor -> color}, Seq @@ {}],
      Background -> bg,
      FrameStyle -> fstyle
    ],
    Offset[$nodeOffset, pos]
  ]
];

$frameOpts = Seq[
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
  None               := True;
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

