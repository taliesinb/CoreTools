SystemExports[
  "Function",     MultigraphIncidenceGraph, MultigraphPlot,
  "OptionSymbol", DuplicateTargets
];

(**************************************************************************************************)

MultigraphPlot[m_Multigraph] :=
  MultigraphIncidenceGraph[m, DuplicateTargets -> True];

(**************************************************************************************************)

Options[MultigraphIncidenceGraph] = JoinOptions[
  {DuplicateTargets -> False,
  ImageSize -> Automatic},
  Options @ Graph
];

MultigraphIncidenceGraph[mg:MGraphDataP[dict_], opts:OptionsPattern[]] := Locals[

  UnpackOptions[duplicateTargets];

  {origVertices, multiSrcs, multiTgts, multiLabels} = Lookup[dict, {
    MVertexList, IEdgeInputs, IEdgeOutputs, MEdgeNameIndex
  }];

  vcount = Len @ origVertices;
  ecount = Len @ multiSrcs;
  origEdges = MEdgeList @ mg;

  targetVertexHead = If[duplicateTargets, IndexedVertex[#, "Output"]&, IndexedVertex];
  indexedSources = Map2[IndexedVertex, multiSrcs];
  indexedTargets = Map[targetVertexHead, multiTgts];
  vertexColors = Map[MultigraphVertexColor, origVertices];
  edgeColors = Map[MultigraphEdgeColor, origEdges];
  indexedVertices = Map[IndexedVertex, Range @ vcount];
  indexedMultiedges = Map[IndexedMultiedge, Range @ ecount];

  srcEdges = ZipMap[
    {srcList, indexedEdge} |-> MapP[
      {src, i} |-> DirectedEdge[src, indexedEdge, i],
      srcList
    ],
    indexedSources, indexedMultiedges
  ];
  tgtEdges = ZipMap[DirectedEdge, indexedMultiedges, indexedTargets];

  vertexLabels = Join[
    RuleThread[indexedVertices, Part[origVertices, Col1 @ indexedVertices]],
    KeyValueMap[{label, int} |-> IndexedMultiedge[int] -> label, multiLabels]
  ];

  vertexPartition = {Len @ indexedVertices, Len @ indexedMultiedges};
  finalVertices = Join[indexedVertices, indexedMultiedges];
  finalEdges = FlatList[srcEdges, tgtEdges];

  If[duplicateTargets,
    uniqueTargets = Union @ indexedTargets;
    JoinTo[finalVertices, uniqueTargets];
    uniqueTargetIds = Col1 @ uniqueTargets;
    JoinTo[vertexLabels, RuleThread[uniqueTargets, Part[origVertices, uniqueTargetIds]]];
    JoinTo[vertexColors, Part[vertexColors, uniqueTargetIds]];
    AppendTo[vertexPartition, Len @ uniqueTargets];
  ];
  With[{vcolors = vertexColors, ecolors = edgeColors},
  vertexStyle = List[
      (* IndexedVertex[_, _] :> $DarkGreen, IndexedVertex[_] -> $DarkRed,  *)
      IndexedMultiedge[i_]   :> Part[ecolors, i],
      IndexedVertex[i_, ___] :> Part[vcolors, i]
  ]];
  (* TODO: highlight duplicates *)
  graphLayout = {"MultipartiteEmbedding", "Rotation" -> 0, "VertexPartition" -> DelCases[vertexPartition, 0]};
  graph = Graph[
    finalVertices, Map[edge |-> Annotation[edge, "DomainIndex" -> Third[edge]], finalEdges],
    NarrowOptions @ opts,
    PlotTheme    -> "CoreMultigraphIncidenceGraph",
    GraphLayout  -> graphLayout,
    (* EdgeLabels   -> Placed[EdgeAnnotation["DomainIndex"], "Middle"], *)
    EdgeStyle    -> {$Gray},
    ImagePadding -> {{25, 25}, {25, 25}},
    VertexStyle  -> vertexStyle,
    VertexLabels -> vertexLabels
  ]
];

DefineGraphTheme["CoreMultigraphIncidenceGraph", {
  ThemeParent -> "Core",
  EdgeShapeFunction -> {"ShortUnfilledArrow", "ArrowSize" -> Medium, "ArrowPositions" -> 0.75},
  Options -> {BaseStyle -> {AbsolutePointSize[10]}},
  EdgeStyle -> {$Gray}
}];
