SystemExports[
  "GraphicsOption",
    GraphScale, HStretch, VStretch,
    SplitPosition,
    NodeData,
    NodeTooltips, NodeClickFunction, NodeClickData,
    EdgeTooltips, EdgeClickFunction, EdgeClickData,
    NodeColor, NodeSize, NodeShape, NodeThickness,
    RootPosition,
    NodeGroups, NodeStyles,
    NodeBackground, NodeOffset,
    SharedLeaves, EdgeFn
];

PackageExports[
  "SymbolicHead",     FaceEdge,
  "GraphicsOption",   SplitPos, NodeClickFn, EdgeClickFn,
  "GraphicsFunction", ExprTreePlot, NiceTreePlot, InsetNode, TreeNodeColor
];

(**************************************************************************************************)

InsetNode[content_, fn_:Id, opts___][pos_, size_, color_] :=
  Inset[Style[fn[content], color, opts, Background -> GrayLevel[1,1]], pos, Center, Alignment -> Center];

(**************************************************************************************************)

Options[NiceTreePlot] = {
  GraphScale        -> 30,
  HStretch          -> 1.0,
  VStretch          -> 1.0,

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

  EdgeClickFn       -> None,
  EdgeClickData     -> "Name",
  EdgeColor         -> Black,
  EdgeThickness     -> 1,
  EdgeFn            -> Line,
  Setback           -> 0,

  SplitPosition     -> Top,
  FanDistance       -> None,
  Rounding          -> 0.25,

  PMargin           -> 0.5,
  BaselinePosition  -> Root,
  FontSize          -> 10,
  FontFamily        -> Inherited,
  FontWeight        -> Inherited,
  FontSlant         -> Inherited,

  RootPosition      -> Top,
  SharedLeaves      -> {},
  ReverseEdges      -> False
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
    graphScale, hStretch, vStretch,
    edgeColor, edgeThickness,
    rootPosition, baselinePosition,
    nodeData,
    fontSize, fontWeight, fontFamily, fontSlant,
    sharedLeaves, splitPosition, rounding, pMargin,
    nodeThickness, $nodeBCol, $nodeOffset,
    edgeFn, setback
  ];

  AssertOptsValid[
    NodeThickness -> nodeThickness -> NumQ,
    GraphScale    -> graphScale    -> NumQ,
    EdgeColor     -> edgeColor     -> ColorQ,
    EdgeThickness -> edgeThickness -> NumQ
  ];

  vertexCount = VertexCount @ graph;
  If[vertexCount === 0, Return @ Graphics[{}, ImageSize -> graphScale]];

  aspectRatio = 0.66*vStretch / hStretch;
  {vertexCoords, edgeCoords, plotBounds} = OrderedTreeLayout[graph,
    RootPosition -> rootPosition, "LayerDepths" -> aspectRatio,
    SharedLeaves -> sharedLeaves, PMargin -> pMargin,
    Setback -> setback,
    SplitPosition -> splitPosition, RoundingRadius -> rounding
  ];

  {{plotL, plotR}, {plotB, plotT}} = plotBounds;
  {plotW, plotH} = plotSize = Dist @@@ plotBounds;
  imageSize = plotSize * graphScale;
  $scale = 0.5 / graphScale;

  edgeStyle = Directive[AThickness @ edgeThickness, $niceArrowhead];
  nodeStyle = EdgeForm @ AThickness @ nodeThickness;

  itemData = transposeDict @ GraphVertexAnnotations @ graph;
  If[nodeData =!= Auto, AppendTo[itemData, nodeData]];

  vertexList = VertexList @ graph;
  vertexSpecs = ParseItemOptions[NiceTreePlot,
    $treePlotVertexSpecs, vertexList, {opts}, UseBroadcast -> True,
    ItemData -> itemData, ItemGroups -> NodeGroups, GroupSettings -> NodeStyles
  ];
  vertexSpecs = Lookup[
    vertexSpecs,
    {NodeShape, NodeSize, NodeTooltips, NodeColor, NodeClickFn, NodeClickData}
  ];
  vertexPrims = makePrimitives[vertexSpecs, vertexCoords];

  SetAuto[edgeColor, GrayLevel[0.5]];
  AppendTo[edgeStyle, edgeColor];
  fontStyle = DelCases[
    List[FontSize -> fontSize, FontWeight -> fontWeight, FontFamily -> fontFamily, FontSlant -> fontSlant],
    _ -> Inherited
  ];

  vertexPrims = vertexPrims /. Circle[c_, p_] :> {Style[Disk[c, p], FaceForm @ White], Circle[c, p]};
  primitives = List[
    Style[edgeFn @ edgeCoords, edgeStyle],
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

SetStrict @ makePrimitives;

makePrimitives[{shape_, size_, tooltips_, colors_, clickFns_, clickData_}, coords_] := Locals[
  prims = BMap[makeShape, shape, coords, size, colors];
  GlobalVar[$meanSize]; $meanSize = Mean @ FromB @ size;
  If[!BMatchQ[tooltips, None], prims = BMap[Tooltip, prims, tooltips]];
  If[!BMatchQ[clickFns, None], prims = BMap[makeClicker, prims, clickFns, clickData]];
  prims
];

makeClicker[prim_, None | Auto | Inherit, _] := prim;
makeClicker[prim_, fn_, data_] := Button[prim, fn @ data];

(**************************************************************************************************)

makeShape[Framed[str_, opts___Rule], pos_, size_, color_] := Locals[
  bg = toBackCol[color, $nodeBCol, White];
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
  ItemSpec[NodeTooltips,  None, None],
  ItemSpec[NodeClickFn,   OrOp[MaybeFnQ, NoneQ], None],
  ItemSpec[NodeClickData, None, None],
  ItemSpec[NodeColor,     extColorQ, $defaultColor],
  ItemSpec[NodeSize,      NumberQ],
  ItemSpec[NodeShape,     validShapeFnQ, Inherit, finalShapeFn, ItemMessageFn -> shapeError]
};


