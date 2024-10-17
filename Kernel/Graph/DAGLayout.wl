PackageExports[
  "Function", DAGLayout, PolyDagLayout,
  "GraphicsOption", HStretch, VStretch
];

(*************************************************************************************************)

Options[DAGLayout] = {
  VStretch -> 1,
  HStretch -> 1
}

DAGLayout[graph_Graph, opts___Rule] := Locals[
  UnpackSymbolsAs[DAGLayout, {opts}, vStretch, hStretch];
  vertexCoords = UDict[];
  edgeCoords = UDict[];
  fn = If[hStretch == 1 && vStretch == 1, Id, ThreadTimesOp[N @ {hStretch, vStretch}]];
  saveVertexCoords = Function[vertexCoords[#2] = fn @ #1];
  saveEdgeCoords = Function[edgeCoords[#2] = ToPackedReals[fn @ #1]];
  edgeTagged = EdgeTaggedGraph[graph,
    VertexShapeFunction -> saveVertexCoords, EdgeShapeFunction -> saveEdgeCoords,
    GraphLayout -> "LayeredDigraphEmbedding", PerformanceGoal -> "Speed"
  ];
  GraphComputation`GraphDrawing[edgeTagged];
  List[
    ToPackedReals @ Lookup[vertexCoords, VertexList @ edgeTagged],
    UDictThread[EdgeList @ graph, Lookup[edgeCoords, EdgeList @ edgeTagged]]
  ]
];

