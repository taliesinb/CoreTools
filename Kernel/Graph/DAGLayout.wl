PackageExports[
  "Function",   DAGLayout, EdgeArityGroups,
  "PlotOption", HStretch, VStretch, Balancing, FixOverlaps, EnsureSpaced
];

(*************************************************************************************************)

Options[DAGLayout] = {
  VStretch      -> 1,
  HStretch      -> 1,
  PMargin       -> 0,
  RootPosition  -> Auto,
  FixOverlaps   -> True,
  Balancing     -> True
}

DAGLayout[graph_Graph, opts___Rule] := Locals[

  UnpackSymbolsAs[DAGLayout, {opts}, vStretch, hStretch, pMargin, balancing, fixOverlaps];

  vertPosDict = Dict[];
  edgePosDict = Dict[];
  coordFn = If[hStretch == 1 && vStretch == 1, Id, ThreadTimesOp[N @ {hStretch, vStretch}]];
  saveVertPos = Function[vertPosDict[#2] = coordFn @ #1];
  saveEdgePos = Function[edgePosDict[#2] = ToPackedReals[coordFn @ #1]];

  edgeTagged = EdgeTaggedGraph[graph,
    VertexShapeFunction -> saveVertPos, EdgeShapeFunction -> saveEdgePos,
    GraphLayout -> "LayeredDigraphEmbedding", PerformanceGoal -> "Speed"
  ];
  {verts, edges} = VertexEdgeList @ edgeTagged;
  GraphComputation`GraphDrawing @ edgeTagged;
  extra = {};

  vertPosList = ToPackedReals  @ Lookup[vertPosDict, verts];

  If[TrueQ @ balancing,
    {paths, fanOs, fanIs} = extra = EdgeArityGroups @ edgeTagged;
    dx = {.75, 0} * hStretch;
    Do[
      KVMap[ApplyTo[vertPosDict[#1],      blendXs[Lookup[vertPosDict, Col2 @ #2]]]&, fanOs];
      KVMap[ApplyTo[vertPosDict[P1 @ #1], blendXs[Lookup[vertPosDict, Col2 @ #2]]]&, paths];
      If[fixOverlaps, vertPosDict = DThread[verts, NudgeOverlapping[Values @ vertPosDict, dx]]];
    ,
      {4}
    ];
    edgePosDict = Dict @ Map[edge |-> edge -> Lookup[vertPosDict, {Part[edge, 1], Part[edge, 2]}], edges];
    JoinTo[extra, {vertPosDict, edgePosDict}];
  ];

  vertPosList = ToPackedReals  @ Lookup[vertPosDict, verts];
  edgePosList = ToPackedReals /@ Lookup[edgePosDict, edges];
  plotBounds = CoordinateBounds[vertPosList, pMargin];

  List[vertPosList, edgePosList, plotBounds, extra]
];

blendXs[coords_][{x_, y_}] := With[{x2 = N @ Median @ Col1 @ coords}, List[Lerp[x2, x, .25], y]];

(**************************************************************************************************)

EdgeArityGroups[graph_Graph] := Locals[
  {verts, edges} = VertexEdgeList @ graph;
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
