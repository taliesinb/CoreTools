PackageExports[
  "GraphicsFunction", MultigraphVertexColor, MultigraphEdgeColor,
  "Variable",         $MultigraphDisplayForm
];

(**************************************************************************************************)

SetInitial[$MultigraphDisplayForm, "Multiedges"]

CoreBoxes[mgraph:HoldP[Multigraph[InternalData[data_Dict]] ? SealedQ]] :=
  With[{res = mgraphBoxes[mgraph]}, If[res === $Failed, fallbackBoxes @ mgraph, res]];

mgraphBoxes[mgraph_] := Switch[$MultigraphDisplayForm,
  "BipartiteIncidenceGraph", incidenceIconBoxes[mgraph, True],
  "IncidenceGraph",          incidenceIconBoxes[mgraph, False],
  "Multiedges",              multiedgeRowBoxes @ mgraph,
  _,                         fallbackBoxes @ mgraph
];

incidenceIconBoxes[mgraph_, dupTargets_] := Module[
  {igraph, graphics},
  igraph = MultigraphIncidenceGraph[mgraph, ImageSize -> {200, {50, 200}},
    ImagePadding -> {{10, 10},{30,30}},
    EdgeShapeFunction -> "Line", DuplicateTargets -> dupTargets];
  If[GraphQ[igraph] && HasHeadQ[graphics = CustomGraphDrawing @ igraph, Graphics],
    (* Construct[FrameBox, ToBoxes @ graphics, FrameStyle -> $LightPink], *)
    NiceObjectBoxes["Multigraph", ToBoxes @ graphics, .2],
    $Failed, $Failed
  ]
];

fallbackBoxes[mgraph:MGraphP] :=
  NiceObjectBoxes["Multigraph",
    {RBox[MVertexCount @ mgraph, " vertices"], RBox[MEdgeCount @ mgraph, " multiedges"]}, .2,
    BaseStyle -> FontFamily -> "Source Code Sans"
  ];

intToStr[i_Int] := IntStr @ i;
intToStr[_]     := "?";

(**************************************************************************************************)

multiedgeRowBoxes[mgraph_] := Locals[
  vertices = MVertexList @ mgraph;
  vertexColors = MultigraphVertexColor /@ vertices;
  vertexItems = ZipMap[StyleBox, ToBoxes /@ vertices, vertexColors];
  vertexBoxes = ColumnBox @ ZipMap[StyleBox, ToBoxes /@ vertices, vertexColors];
  edges = MEdgeList @ mgraph;
  $MultiedgeIconScale = 0.8;
  If[Len[edges] < 8,
    edgeBoxes = widthLimitedRow @ SortBy[edges, MultiedgeArity];
  ,
    edgeGroups = Values @ KeySort @ GroupBy[edges, MultiedgeArity];
    If[Len[edgeGroups] > 8, edgeGroups = Append[{"\[Ellipsis]"}] @ Take[edgeGroups, 8]];
    edgeBoxes = ColumnBox[widthLimitedRow /@ edgeGroups, Left]
  ];
  NiceObjectBoxes["Multigraph", {vertexBoxes, edgeBoxes}, .2, ColumnLines -> True]
];

widthLimitedRow[edges_, maxWidth_:800] := Locals[
  width = 0;
  boxes = {};
  i = 0; num = Len @ edges;
  While[++i <= num && width <= maxWidth,
    itemBox = ToBoxes @ Part[edges, i];
    itemSize = ToImageSize @ RawBoxes @ itemBox;
    width += P1 @ itemSize;
    AppendTo[boxes, itemBox];
  ];
  If[i < num, AppendTo[boxes, ToBoxes @ Overscript["\[Ellipsis]", num - i]]];
  RiffledRowBox[SpacerBox[2]] @ boxes
];

