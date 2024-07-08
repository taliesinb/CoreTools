SystemExports[
  "OptionSymbol",
    LabelFunction, VertexLabelFunction, EdgeLabelFunction, PostGraphicsFunction,
    EdgeColor, EdgeThickness,

  "Head",
    VertexAnnotation, EdgeAnnotation,

  "Function",
    PrefixGraph, GraphVertexAnnotations, GraphEdgeAnnotations, GraphAnnotationRules, AddSelfAnnotations
];

PackageExports[
  "SpecialVariable",
    $CurrentGraph,
    $CurrentVertexLabels, $CurrentVertexAnnotations, $UserVertexLabelFunction,
    $CurrentEdgeLabels,   $CurrentEdgeAnnotations,   $UserEdgeLabelFunction,

  "GraphicsFunction",
    CustomGraphDrawing, CustomGraphMakeBoxes, CustomVertexLabelFunction, CustomEdgeLabelFunction
];

(*************************************************************************************************)

PrefixGraph[vertices_, opts___Rule] := Locals[
  $verts = ConstUAssoc[vertices, True];
  Graph[vertices, makeFirstParentEdge /@ vertices, opts]
];

makeFirstParentEdge = CaseOf[
  expr_ ? ZeroLenQ := Nothing;
  expr_ := Locals[
    parent = Most @ expr;
    While[!KeyExistsQ[$verts, parent],
      If[Len[parent] === 0, FunctionReturn @ Nothing];
      parent //= Most;
    ];
    DirectedEdge[parent, expr]
  ]
];

(*************************************************************************************************)

patchMakeGraphBoxes[] := With[
  {gmb := GraphComputation`GraphMakeBoxes,
    gd := GraphComputation`GraphDrawing,
    mb := GraphComputation`GraphBoxesDump`makeBoxes,
    ivlf = GraphComputation`GraphLabelsDump`vertexLabelingFunction,
    ielf = GraphComputation`GraphLabelsDump`edgeLabelingFunction,
    vlf = GraphComputation`VertexLabelingFunction,
    elf = GraphComputation`EdgeLabelingFunction},
  Unprotect[gmb, vlf, elf, mb];
  DownValues[vlf] = Prepend[
    DownValues[vlf] /. ivlf -> CustomVertexLabelFunction,
    HoldP[vlf[Placed[VertexAnnotation[k_], pos_, f___], expr___]] :> Block[{res},
      res = CustomVertexLabelFunction[f][VertexAnnotation[#5, k], pos, expr];
      res /; UnsameQ[res, $Failed]
    ]
  ];
  DownValues[elf] = Prepend[
    DownValues[elf] /. ielf -> CustomEdgeLabelFunction,
    HoldP[elf[Placed[EdgeAnnotation[k_], pos_, f___], expr___]] :> Block[{res},
      res = CustomEdgeLabelFunction[f][EdgeAnnotation[#3, k], pos, expr];
      res /; UnsameQ[res, $Failed]
    ]
  ];
  DownValues[gmb] = DownValues[gmb] /. {gd -> CustomGraphDrawing, mb -> CustomGraphMakeBoxes};
  Protect[gmb, vlf, elf, mb];
]

With[
  {ivlf = GraphComputation`GraphLabelsDump`vertexLabelingFunction,
   ielf = GraphComputation`GraphLabelsDump`edgeLabelingFunction},
CustomVertexLabelFunction[]   := ivlf[$UserVertexLabelFunction];
CustomVertexLabelFunction[f_] := ivlf[f];
CustomEdgeLabelFunction[]     := ielf[$UserEdgeLabelFunction];
CustomEdgeLabelFunction[f_]   := ielf[f];
];

(*************************************************************************************************)

SetInitial[$UserVertexLabelFunction, Id];
SetInitial[$UserEdgeLabelFunction,   Id];

CustomGraphMakeBoxes[expr_, _, fmt_] := DisableCoreBoxInteractivity @ ToBoxes[expr, fmt];

toLabelFn[None | $Failed | Null] := Id;
toLabelFn[fn_] := passNull[fn];

passNull[fn_][Null] := Null
passNull[fn_][e_] := fn[e];

$emptyGraphGraphics := Graphics[
  {}, ImageSize -> {50, 50},
  Frame -> True, FrameStyle -> Directive[{GrayLevel[0.5], Dotted}], FrameTicks -> None
];

CustomGraphDrawing[graph_] := Module[{vl, el, vlf, elf, pgf},
  If[VertexCount[graph] === 0, Return @ $emptyGraphGraphics];
  {vl, el, lf, vlf, elf, pgf} = AnnotationValue[graph,
    {VertexLabels, EdgeLabels, LabelFunction, VertexLabelFunction, EdgeLabelFunction, PostGraphicsFunction}];
  lf //= toLabelFn; vlf //= toLabelFn; elf //= toLabelFn;
  vl = UAssoc @ If[RuleVectorQ[vl], vl, {}];
  el = UAssoc @ If[RuleVectorQ[el], el, {}];
  Block[{
    $CurrentGraph = graph, $CurrentVertexLabels = vl, $CurrentEdgeLabels = el,
    $UserVertexLabelFunction = vlf /* lf, $UserEdgeLabelFunction = elf /* lf,
    $CurrentVertexAnnotations := $CurrentVertexAnnotations = AddSelfAnnotations @ GraphVertexAnnotations[$CurrentGraph],
    $CurrentEdgeAnnotations   := $CurrentEdgeAnnotations   = AddSelfAnnotations @ GraphEdgeAnnotations[$CurrentGraph]},
   If[pgf =!= $Failed, pgf, Identity] @ GraphComputation`GraphDrawing @ graph
  ]
];

RegisterPackagePatchFunctions[
  "Network`GraphBoxes`",
  "MakeGraphBoxes" -> patchMakeGraphBoxes
];

(*************************************************************************************************)

VertexAnnotation[v_, k_]                  := VertexAnnotation[v, k, Null];
VertexAnnotation[v_Int, k_, d_]           := Lookup[PartOr[$CurrentVertexAnnotations, v, {}], k, d];
VertexAnnotation[v:Except[_Slot], k_, d_] := Lookup[Lookup[$CurrentVertexAnnotations, v, {}], k, d];

EdgeAnnotation[e_, k_]                  := EdgeAnnotation[e, k, Null];
EdgeAnnotation[e_Int, k_, d_]           := Lookup[PartOr[$CurrentEdgeAnnotations, e, {}], k, d];
EdgeAnnotation[e:Except[_Slot], k_, d_] := Lookup[Lookup[$CurrentEdgeAnnotations, e, {}], k, d];

(*************************************************************************************************)

DeclareStrict[GraphAnnotationRules];

graphAnnos[graph_] := Part[Options[graph, AnnotationRules], 1, 2];

GraphAnnotationRules[graph_Graph] := graphAnnos @ graph;
GraphAnnotationRules[graph_Graph ? EdgeTaggedGraphQ] := Locals[
  annos = graphAnnos @ graph;
  missingEdges = Complement[EdgeList @ graph, Keys @ annos];
  If[!ZeroLenQ[missingEdges], JoinTo[annos, ConstRules[missingEdges, {}]]];
  addEdgeTags /@ annos
];

addEdgeTags = CaseOf[
  Rule[edge:(DirectedEdge|UndirectedEdge)[_, _, tag_], annos_] :=
    Rule[edge, Append[annos, "EdgeTag" -> tag]];
  rule_ := rule
];

(*************************************************************************************************)

DeclareStrict[GraphVertexAnnotations, GraphEdgeAnnotations, AddSelfAnnotations];

GraphVertexAnnotations[graph_Graph]                  := graphItemAnnos[graph, VertexList @ graph];
GraphVertexAnnotations[graph_Graph, key_, def_:None] := graphItemAnnos[graph, VertexList @ graph, key, def];

GraphEdgeAnnotations[graph_Graph]                  := graphItemAnnos[graph, EdgeList @ graph];
GraphEdgeAnnotations[graph_Graph, key_, def_:None] := graphItemAnnos[graph, EdgeList @ graph, key, def];
GraphEdgeAnnotations[graph_Graph, "EdgeTag", def_:None] := VectorReplace[EdgeTags[graph], Null -> def];

AddSelfAnnotations[annos_] := Locals[
  MapP[
    i = 1; {v, k} |-> Append[v, {"Name" -> k, "Index" -> (i++)}],
    annos
  ]
];

(*************************************************************************************************)

graphItemAnnos[graph_, items_] := Locals[
  rules = GraphAnnotationRules @ graph;
  annos = VectorReplace[items, Append[rules, _ -> {}]];
  AssocThread[items, Assoc /@ annos]
];

graphItemAnnos[graph_, items_, key_, def_] := Locals[
  rules = GraphAnnotationRules @ graph;
  annos = VectorReplace[items, Append[rules, _ -> {}]];
  Lookup[annos, key, def]
];