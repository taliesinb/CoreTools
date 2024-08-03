SystemExports[
  "OptionSymbol",
    LabelFunction, VertexLabelFunction, EdgeLabelFunction, PostGraphicsFunction,
    EdgeColor, EdgeThickness, ThemeParent,
    VertexColorFunction, VertexTooltips,

  "Head",
    VertexAnnotation, EdgeAnnotation, TimePart,

  "Function",
    GraphVertexAnnotations, GraphEdgeAnnotations, GraphAnnotationRules, AddSelfAnnotations,
    GraphProperties,

  "GraphicsFunction",
    GraphAnimate
];

PackageExports[
  "SpecialVariable",
    $CurrentGraph,
    $CurrentVertexLabels, $CurrentVertexAnnotations, $UserVertexLabelFunction,
    $CurrentEdgeLabels,   $CurrentEdgeAnnotations,   $UserEdgeLabelFunction,
    $CustomGraphThemeData, $SystemGraphThemeNames,

  "MutatingFunction",
    DefineGraphTheme,

  "Function",
    ExtGraph,

  "Predicate",
    CustomGraphThemeQ,

  "GraphicsFunction",
    CustomGraphDrawing, CustomGraphMakeBoxes,
    CustomSetGraphStyle,
    GraphVertexAnimate
];

(*************************************************************************************************)

DeclarePredicate1[CustomGraphThemeQ]

SetInitial[$SystemGraphThemeNames, GraphComputation`SetGraphStyle[]];
SetInitial[$CustomGraphThemeData, UAssoc[]];

DeclareStrict[DefineGraphTheme]

DefineGraphTheme[name_String, rules_List] := (
  CustomGraphThemeQ[name] = True;
  $CustomGraphThemeData[name] = UAssoc[rules];
);

(*************************************************************************************************)

DefineGraphTheme["Core", {
  EdgeShapeFunction -> {"ShortUnfilledArrow", "ArrowSize" -> Medium, "ArrowPositions" -> 0.5},
  VertexShapeFunction -> (Point[#]&),
  VertexSize -> 0.2, ImageSize -> 200,
  EdgeStyle -> Directive[{GrayLevel[0.7, 1.0]}],
  VertexStyle -> Directive[{AbsolutePointSize[4], EdgeForm[None], FaceForm[GrayLevel[0.2, 1]], GrayLevel[0.2, 1]}],
  Options -> {ImagePadding -> 20}
}];

(*************************************************************************************************)

$CustomGraphOptions = {
  LabelFunction, VertexLabelFunction, EdgeLabelFunction,
  PostGraphicsFunction, VertexColors,
  VertexColorFunction, VertexTooltips
};
customGraphOptionQ = ConstTrueAssoc @ $CustomGraphOptions;

DeclareStrict[ExtGraph]

ExtGraph[graph_Graph, opts___Rule] := iExtGraph[graph, List @ opts];
ExtGraph[verts_List, edges_List, opts___Rule] := iExtGraph[Seq[verts, edges], List @ opts];
ExtGraph[edges_List, opts___Rule] := iExtGraph[edges, List @ opts];

SetAttributes[iExtGraph, SequenceHold];
iExtGraph[graph_, opts2_List] := Locals[
  {annos, opts} = SelectDiscard[opts2, First /* customGraphOptionQ];
  Graph[graph, ToList[opts, AnnotationRules -> {"GraphProperties" -> annos}]]
];

(*************************************************************************************************)

(* CustomSetGraphStyle[themeData_, VertexLabels | EdgeLabels, args___] := FailEval; *)
CustomSetGraphStyle[themeData_, prop_Symbol, args___] := Lookup[themeData, prop, fallbackGraphStyle[themeData, prop, args]];
fallbackGraphStyle[themeData_, prop_, args___] := GraphComputation`SetGraphStyle[Lookup[themeData, ThemeParent, Automatic], prop, args];

(*
  GraphComputation`SetGraphStyle[Lookup[themeData, ThemeParent, Automatic], prop, args]
]; *)

(*************************************************************************************************)

patchCustomGraphLabeling[] := Then[
  patchGraphLabelFn[GraphComputation`VertexLabelingFunction,        GraphComputation`GraphLabelsDump`vertexLabelingFunction,        GraphComputation`CustomVertexLabelFunction,        $UserVertexLabelFunction, $newVertexLabelDVs],
  patchGraphLabelFn[GraphComputation`EdgeLabelingFunction,          GraphComputation`GraphLabelsDump`edgeLabelingFunction,          GraphComputation`CustomEdgeLabelFunction,          $UserEdgeLabelFunction,   $newEdgeLabelDVs],
  patchGraphLabelFn[GraphComputation`DynamicVertexLabelingFunction, GraphComputation`GraphLabelsDump`dynamicVertexLabelingFunction, GraphComputation`CustomDynamicVertexLabelFunction, $UserVertexLabelFunction, $newVertexLabelDVs],
  patchGraphLabelFn[GraphComputation`DynamicEdgeLabelingFunction,   GraphComputation`GraphLabelsDump`dynamicEdgeLabelingFunction,   GraphComputation`CustomDynamicEdgeLabelFunction,   $UserEdgeLabelFunction,   $newEdgeLabelDVs]
];

DeclareHoldAllComplete[patchGraphLabelFn];
patchGraphLabelFn[gcFn_, internalFn_, customFn_, userFnVar_, rules_] := Then[
  gcFn, internalFn,
  Unprotect[gcFn],
  DownValues[gcFn] = Join[rules /. {$1 -> gcFn, $2 -> customFn}, DownValues[gcFn] /. internalFn -> customFn],
  customFn[]   := internalFn @ postProcLabelWith @ userFnVar,
  customFn[f_] := internalFn @ postProcLabelWith @ f,
  Protect[gcFn]
];

postProcLabelWith[f_][None|Null | Style[None|Null, ___]] := Null;
postProcLabelWith[f_][e_]                                := Replace[f[e], None -> Null];

$newVertexLabelDVs = {
  HoldP[$1[Placed[VertexAnnotation[k_], pos_, f___], expr___]] :> Block[{res},
    res = $2[f][VertexAnnotation[#5, k], pos, expr];
    res /; UnsameQ[res, $Failed]
  ],
  HoldP[$1[VertexAnnotation[k_], expr___]] :> Block[{res},
    res = $2[][VertexAnnotation[#5, k], Automatic, expr];
    res /; UnsameQ[res, $Failed]
  ]
};

$newEdgeLabelDVs = {
  HoldP[$1[Placed[EdgeAnnotation[k_], pos_, f___], expr___]] :> Block[{res},
    res = $2[f][EdgeAnnotation[#2, k], pos, expr];
    res /; UnsameQ[res, $Failed]
  ],
  HoldP[$1[EdgeAnnotation[k_], expr___]] :> Block[{res},
    res = $2[][EdgeAnnotation[#2, k], Automatic, expr];
    res /; UnsameQ[res, $Failed]
  ]
};

(*************************************************************************************************)

patchCustomGraphDrawing[] := With[
  {gcGMb := GraphComputation`GraphMakeBoxes},
  Unprotect[gcGMb];
  DownValues[gcGMb] = DownValues[gcGMb] /. {
    GraphComputation`GraphDrawing -> CustomGraphDrawing,
    GraphComputation`GraphBoxesDump`makeBoxes -> CustomGraphMakeBoxes
  };
  Protect[gcGMb];
];

(*************************************************************************************************)

patchCustomGraphPlotThemes[] := With[
  {gcSGS := GraphComputation`SetGraphStyle},
  Unprotect[gcSGS];
  gcSGS[] := Join[$SystemGraphThemeNames, Keys @ $CustomGraphThemeData];
  gcSGS[name_String ? CustomGraphThemeQ, prop_Symbol, args___] :=
    MaybeEval @ CustomSetGraphStyle[$CustomGraphThemeData @ name, prop, args];
  Protect[gcSGS];
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

(*************************************************************************************************)

patchMakeGraphBoxes[] := Then[patchCustomGraphPlotThemes[], patchCustomGraphLabeling[], patchCustomGraphDrawing[]];

RegisterPackagePatchFunctions[
  "Network`GraphBoxes`",
  "MakeGraphBoxes" -> patchMakeGraphBoxes
];

(*************************************************************************************************)

VertexAnnotation::usage =
"VertexAnnotation[key$], when provided via VertexLabels -> VertexAnnotation[$$], will use \
the vertex annotation named key$ as a label. It can also be wrapped in a Placed.
";

VertexAnnotation[v_, k_]                  := VertexAnnotation[v, k, Null];
VertexAnnotation[v_Int, k_, d_]           := Lookup[PartOr[$CurrentVertexAnnotations, v, {}], k, d];
VertexAnnotation[v:Except[_Slot], k_, d_] := Lookup[Lookup[$CurrentVertexAnnotations, v, {}], k, d];

EdgeAnnotation::usage =
"EdgeAnnotation[key$], when provided via EdgeLabels -> EdgeAnnotation[$$], will use \
the edge annotation named key$ as a label. It can also be wrapped in a Placed.
";

EdgeAnnotation[e_, k_]                  := EdgeAnnotation[e, k, Null];
EdgeAnnotation[e_Int, k_, d_]           := Lookup[PartOr[$CurrentEdgeAnnotations, e, {}], k, d];
EdgeAnnotation[e:Except[_Slot], k_, d_] := Lookup[Lookup[$CurrentEdgeAnnotations, e, {}], k, d];

(*************************************************************************************************)

DeclareStrict[GraphAnnotationRules, GraphProperties]

GraphProperties[graph_Graph] := Lookup[graphAnnos @ graph, "GraphProperties", Assoc[]];
GraphProperties[graph_Graph, syms_] := Lookup[GraphProperties @ graph, syms, Automatic];

graphAnnos[graph_] := Part[Options[graph, AnnotationRules], 1, 2];

GraphAnnotationRules[graph_Graph] := graphAnnos @ graph;
GraphAnnotationRules[graph_Graph ? EdgeTaggedGraphQ] := Locals[
  annos = graphAnnos @ graph;
  missingEdges = Complement[EdgeList @ graph, Keys @ annos];
  If[NonEmptyListQ[missingEdges], JoinTo[annos, ConstRules[missingEdges, {}]]];
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

GraphEdgeAnnotations[graph_Graph]                    := graphItemAnnos[graph, EdgeList @ graph];
GraphEdgeAnnotations[graph_Graph, key_, def_:None]   := graphItemAnnos[graph, EdgeList @ graph, key, def];
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

(*************************************************************************************************)

Options[GraphAnimate] = Options[Graph];
(* {
  VertexPrimitiveFunction -> Id,
  EdgePrimitiveFunction   -> Id
};
 *)
(* LabeledBy[v_, fn_]  *)

GraphAnimate[g_, v:Except[_Rule], opts___Rule] := GraphAnimate[g, v, None, opts];

(* idea:
have CustomGraphDrawing natively support this, by having a TimeFunction[...]
and we embed a TimeRange as a custom option that gives the entire range.
*)

GraphAnimate::badAnimationData = "Data provided for `` was not a matrix: ``.";
GraphAnimate::animationTimeMismatch = "Vertex time base `` =!= edge time base ``.";
GraphAnimate[graph_Graph, vertexDataSpec_, edgeDataSpec_, opts:OptionsPattern[]] := Locals @ CatchError[
  vTime = eTime = None;
  vLabelFn = If[NoneQ[vertexDataSpec], None,
    data = GraphVertexData[graph, vertexDataSpec];
    If[!AnyMatrixQ[data], ReturnFailed["badAnimationData", "vertices", data]];
    vTime = Len2 @ data;
    DictThread[VertexList @ graph, PartOfOp /@ data]
  ];
  eLabelFn = If[NoneQ[edgeDataSpec], None,
    data = GraphEdgeData[graph, edgeDataSpec];
    If[!AnyMatrixQ[data], ReturnFailed["badAnimationData", "edges", data]];
    eTime = Len2 @ data;
    DictThread[EdgeList @ graph, PartOfOp /@ data]
  ];
  SetNone[vTime, eTime]; SetNone[eTime, vTime];
  SameQOrThrow[vTime, eTime, "animationTimeMismatch"];
  If[time1 === None, Return @ graph];
  baseGraph = ExtGraph[graph,
    opts,
    VertexLabelFunction -> vLabelFn, VertexLabels -> If[vLabelFn === None, None, "Name"],
    EdgeLabelFunction -> eLabelFn, EdgeLabels -> If[eLabelFn === None, None, "Name"],
    PlotTheme -> "GraphAnimate"
  ];
  graphics = CustomGraphDrawing @ baseGraph;
  boxes = Map[
    t |-> RawBoxes @ ToBoxes @ ReplaceAll[
      graphics, p_PartOfOp :> RuleEval @ p @ t
    ],
    Range @ vTime
  ];
  ListView @ boxes
];

DefineGraphTheme["GraphAnimate", {
  ThemeParent -> "Core",
  Options -> {ImagePadding -> 25},
  EdgeShapeFunction -> {"ShortUnfilledArrow", "ArrowSize" -> Medium, "ArrowPositions" -> 0.7}
}];


