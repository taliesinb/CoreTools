SystemExports[
  "Function",
    PrefixEdges, PrefixGraph,
    EdgeTagIndex,
    FromIndexGraph,
    VertexRange, VertexEdgeList, IndexVertexEdgeList,
    EdgePairs, EdgePairsT,
    FromVertexOutLists,
    VertexOutLists, IndexVertexOutLists,
    VertexInLists, IndexVertexInLists,
    IndexVertexSources, IndexVertexSinks, VertexSources, VertexSinks,
    GraphFold, GraphFoldList,
  "Predicate",
    IndexGraphQ
];

PackageExports[
  "Function",
    GraphVertexData, GraphEdgeData,
    IVertexList, IMEdgeList,
  "SymbolicHead",
    DebugRules
];

(*************************************************************************************************)

DefineAliasRules[
  IVertexList -> IntegerVertexList,
  IMEdgeList   -> IntegerEdgeList
];


(*************************************************************************************************)

PrefixEdges[vertices_] := Locals[
  verts = stripAnnos /@ vertices;
  $verts = TrueDict @ verts;
  makeFirstParentEdge /@ verts
];

stripAnnos = CaseOf[
  Annotation[v_, _] := v;
  v_                := v
];

makeFirstParentEdge = CaseOf[
  expr_ ? EmptyQ := Nothing;
  expr_ := Locals[
    parent = Most @ expr;
    i = -1;
    While[!KeyExistsQ[$verts, parent],
      If[Len[parent] === 0, FunctionReturn @ Nothing];
      parent //= Most;
      i--;
    ];
    DirectedEdge[parent, expr, Part[expr, Span[i, -1]]]
  ]
];

(*************************************************************************************************)

PrefixGraph[vertices_, opts___Rule] := Locals[
  edges = PrefixEdges @ vertices;
  ExtGraph[vertices, edges, opts]
];

(*************************************************************************************************)

DeclarePredicate1[IndexGraphQ]

IndexGraphQ[g_Graph] := PermutedRangeQ @ VertexList @ g;

(*************************************************************************************************)

DeclareStrict[FromIndexGraph]

FromIndexGraph::notIndexedGraph = "First argument was not an indexed graph."
FromIndexGraph::badVertexCount = "Wrong number of provided vertices."

FromIndexGraph[igraph_, vertexList_List, opts___Rule] := Locals[
  If[!IndexGraphQ[igraph], ReturnFailed["notIndexedGraph"]];
  {iverts, iedges} = VertexEdgeList @ igraph;
  If[!SameLenQ[iverts, vertexList], ReturnFailed["badVertexCount"]];
  vertices = Part[vertexList, iverts];
  edges = MapPart[PartOfOp[vertexList], {All, 1;;2}, iedges];
  Graph[
    vertices, edges,
    ToList[Options[igraph], opts]
  ]
];

(*************************************************************************************************)

DeclareStrict[EdgeTagIndices, VertexRange, VertexEdgeList, IndexVertexEdgeList]

"VertexEdgeList[graph$] returns {vertices$, edges$}."

BlockUnprotect[VertexIndex,
VertexIndex[graph_Graph] := UDictRange @ VertexList @ graph
];

EdgeTagIndices[graph_Graph]      := DelCases[Null] @ UDictRange @ EdgeTags @ graph;
VertexRange[graph_Graph]         := Range @ VertexCount @ graph;
VertexEdgeList[graph_Graph]      := List[VertexList @ graph, EdgeList @ graph];
IndexVertexEdgeList[graph_Graph] := VertexEdgeList @ IndexGraph @ graph;

DeclareStrict[EdgePairs, EdgePairsT]

EdgePairs[graph_Graph]                := Flip @ Col12 @ EdgeList @ graph;
EdgePairsT[graph_Graph]               := Col12 @ EdgeList @ graph;

DeclareStrict[VertexOutLists, VertexInLists]

VertexInLists[graph_Graph]  := padWithEmpty[edgeDictI @ graph, graph];
VertexOutLists[graph_Graph] := padWithEmpty[edgeDictO @ graph, graph];

edgeDictI[graph_] := Merge[ReverseRules @ EdgeRules @ graph, Id];
edgeDictO[graph_] := Merge[EdgeRules @ graph, Id];

padWithEmpty[dict_, g_] /; Len[dict] == VertexCount[g] := g;
padWithEmpty[dict_, g_] := Join[ConstUDict[VertexList @ g, {}], dict];

DeclareStrict[IndexVertexInLists, IndexVertexOutLists]

IndexVertexInLists[g_Graph]  := Lookup[edgeDictI @ IndexGraph @ g, VertexRange @ g, {}];
IndexVertexOutLists[g_Graph] := Lookup[edgeDictO @ IndexGraph @ g, VertexRange @ g, {}];

DeclareStrict[IndexVertexSources, IndexVertexSinks, VertexSources, VertexSinks]

IndexVertexSources[g_Graph] := Pick[VertexRange @ g, VertexInDegree @ g, 0];
IndexVertexSinks[g_Graph]   := Pick[VertexRange @ g, VertexOutDegree @ g, 0];
VertexSources[g_Graph]      := Pick[VertexList @ g, VertexInDegree @ g, 0];
VertexSinks[g_Graph]        := Pick[VertexList @ g, VertexOutDegree @ g, 0];

(*************************************************************************************************)

DeclareStrict[FromVertexOutLists]

FromVertexOutLists[dict_Dict, opts___Rule] := ExtGraph[
  DelDups @ Join[Keys @ dict, Catenate @ dict],
  Flatten @ KeyValueMap[{src, tgts} |-> Map[tgt |-> DirectedEdge[src, tgt], tgts], dict],
  opts
];

(*************************************************************************************************)

General::graphDataAnnoMissing = "One or more `2` do not have annotation `1`."
General::graphDataLen = "Provided data is length `1`, but graph has `2` `3`."
General::graphDataAssoc = "The `2` `1` in the graph are missing from the data association."
General::graphDataSpec = "Unknown specification `` for data for ``. It can be a list, association, Broadcast, or ``[...]."

GraphVertexData[graph_Graph, spec_] := genericGraphData[{graph, VertexCount, VertexList, VertexAnnotation, $PrintLiteral @ "vertices"}, spec];
GraphEdgeData[graph_Graph, spec_]   := genericGraphData[{graph, EdgeCount, EdgeList, EdgeAnnotation, $PrintLiteral @ "edges"}, spec];

genericGraphData[fns_, spec_] := Locals[
  {$graph, countFn, $itemsFn, $annoH, $name} = fns;
  $count = countFn @ $graph;
  parseGraphDataSpec @ spec
];

parseGraphDataSpec = CaseOf[

  Broadcast[val_] := ConstList[val, $count];

  "Index"         := Range @ $count;
  "Name"          := $itemsFn @ $graph;
  k_String        := $ @ $annoH @ k;

  $[(h:VertexAnnotation|EdgeAnnotation)[key_] /; h === $annoH] := Locals[
    rules = GraphAnnotationRules @ $graph;
    annos = VectorReplace[$itemsFn @ $graph, Append[rules, _ -> {}]];
    LookupOrThrow[annos, key, "graphDataAnnoMissing", $name]
  ];

  $[list_List /; SameQOrThrow[Len @ list, $count, "graphDataLen", $name]] := list;

  $[dict_Dict /; SameQOrThrow[Len @ dict, $count, "graphDataLen", $name]] :=
    LookupListOrThrow[dict, $itemsFn @ $graph, "graphDataAssoc", $name];

  spec_ := ThrowMsg["graphDataSpec", spec, $name, $annoH];
];

(*************************************************************************************************)

DeclareStrict[GraphFold]

GraphFold::badArguments = "Expected a graph, function, initial vertex data, and iteration steps.";

GraphFold::usage =
"GraphFold[graph$, fn$, vdata$, n$] iterates fn$ on vertex data vdata$ up to n$ times, where each time \
the vertex data is sent according to the edges of the graph.
* fn$ is called for each vertex, and obtains its own prior data, and the incoming data in a list.
  * edge order in the graph determines the order of the list passed to fn$.
* vdata$ should be a list in vertex order.
* DebugRules[fn$, rules$] will also print an animation that shows stages of the fold, using GraphAnimate:
  * VertexLabelFunction -> fl$ shows the data on each vertex.
  * EdgeLabelFunction -> fe$ shows the data on flowing along each edge."

GraphFold[g_Graph, fn_, vdata_, n_Int] :=
  CatchMessages @ genericGraphFold[g, fn, vdata, n, True];

GraphFoldList[g_Graph, fn_, vdata_, n_Int] :=
  CatchMessages @ Flip @ genericGraphFold[g, fn, vdata, n, False];

General::graphFoldMessages = "Messages occurred during ``'th iteration."

genericGraphFold[graph_, DebugRules[fn_, opts_List], vdataSpec_, n_, last_] := Locals[
  rev = Negative @ n;
  vdata = GraphVertexData[graph, vdataSpec];
  outIL = If[rev, IndexVertexOutLists @ graph, IndexVertexInLists @ graph];
  dstIL = If[rev, Col2, Col1] @ EdgeList @ IndexGraph @ graph;
  Collecting[{vDebug, eDebug},
  vDebug[vdata];
  eDebug[ConstList[None, Len @ dstIL]];
  folder = v |-> Check[it++;
    eDebug[Part[v, dstIL]];
    res = ZipMap[fn, v, ExtractIndices[v, outIL]];
    vDebug[res]; res, ThrowMsg["graphFoldMessages", it]
  ];
  result = If[last, Nest, NestList][it = 0; folder, vdata, Abs @ n];
  ];
  PrintOutputCell @ GraphAnimate[graph, Flip @ vDebug, Flip @ eDebug, Seq @@ opts];
  result
];

genericGraphFold[graph_, fn_, vDataSpec_, n_, last_] := Locals[
  rev = Negative @ n;
  vData = GraphVertexData[graph, vDataSpec];
  outIL = If[rev, IndexVertexOutLists @ graph, IndexVertexInLists @ graph];
  folder = v |-> Check[it++; ZipMap[fn, v, ExtractIndices[v, outIL]], ThrowMsg["graphFoldMessages", it]];
  If[last, Nest, NestList][it = 0; folder, vData, Abs @ n]
];
