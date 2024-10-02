SystemExports[
  "Option",
    LabelFunction, VertexLabelFunction, EdgeLabelFunction, PostGraphicsFunction,
    EdgeColor, EdgeThickness, ThemeParent,
    VertexColorFunction, VertexTooltips,

  "Head",
    VertexAnnotation, EdgeAnnotation, TimePart,

  "Function",
    AddVertexAnnotations,
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
    ExtGraph, GraphThemeData,

  "Predicate",
    CustomGraphThemeQ,

  "GraphicsFunction",
    CustomGraphDrawing, CustomGraphMakeBoxes,
    CustomSetGraphStyle,
    GraphVertexAnimate,

    GraphDiskFn, GraphTooltipFn
];

(*************************************************************************************************)

SetPred1[CustomGraphThemeQ]

Initially[
  $SystemGraphThemeNames = GraphComputation`SetGraphStyle[];
  $CustomGraphThemeData  = UDict[]
];

SetStrict[DefineGraphTheme]

DefineGraphTheme[name_String, rules_List] := (
  CustomGraphThemeQ[name] = True;
  $CustomGraphThemeData[name] = UDict[rules];
);

(*************************************************************************************************)

GraphThemeData[name_Str] := Locals[
  dict = Lookup[$CustomGraphThemeData, name, None];
  Which[
    !DictQ[dict], $Failed,
    StringQ[parent = dict[ThemeParent]], Join[GraphThemeData @ parent, dict],
    True, dict
  ]
];

GraphThemeData[name_Str, key_] :=
  GraphThemeData[Lookup[$CustomGraphThemeData, name, Dict[]], key];

GraphThemeData[dict_Dict, key_] := IfInherited[
  Lookup[dict, key, Inherited],
  If[!StringQ[dict, ThemeParent], None,
    GraphThemeData[dict @ ThemeParent, key]]
];

(*************************************************************************************************)

DefineGraphTheme["Core", {
  EdgeShapeFunction -> {"ShortUnfilledArrow", "ArrowSize" -> Medium, "ArrowPositions" -> 0.5},
  VertexShapeFunction -> "Point",
  VertexSize -> 0.2, ImageSize -> 200,
  EdgeStyle -> Directive[{GrayLevel[0.7, 1.0]}],
  VertexTooltips -> "Name",
  VertexStyle -> Directive[{AbsolutePointSize[5], EdgeForm[None], FaceForm[GrayLevel[0.2, 1]], GrayLevel[0.2, 1]}],
  Options -> {ImagePadding -> 20}
}];

(*************************************************************************************************)

$CustomGraphOptions = {
  LabelFunction, VertexLabelFunction, EdgeLabelFunction,
  PostGraphicsFunction, VertexColors,
  VertexColorFunction, VertexTooltips
};
customGraphOptionQ = TrueDict @ $CustomGraphOptions;

SetStrict[ExtGraph]

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

SetHoldC[patchGraphLabelFn];
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
  {gcGMb := GraphComputation`GraphMakeBoxes,
   gcS2Dq := GraphComputation`GraphElementDataDump`Shape2DVertexQ,
   gcRNGD := GraphComputation`GraphElementDataDump`RawNetworkGraphData,
   gcS2D  := GraphComputation`GraphElementDataDump`shape2d},
  Unprotect[gcGMb, gcRNGD];
  gcS2Dq["Disk"] = True;
  (* With[{shapes = gcRNGD["VertexShapeFunction"]}, gcRNGD["VertexShapeFunction"] := Union[shapes, {"Disk"}]]; *)
  gcS2D["Disk"][pos_, ex_]    := GraphDiskFn[pos, None, ex];
  gcS2D["Disk"][_, ex_, pos_] := GraphDiskFn[pos, None, ex];
  DownValues[gcGMb] = DownValues[gcGMb] /. {
    GraphComputation`GraphDrawing -> CustomGraphDrawing,
    GraphComputation`GraphBoxesDump`makeBoxes -> CustomGraphMakeBoxes
  };
  Protect[gcGMb, gcRNGD];
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

patchCustomGraphPlotThemes[];

(*************************************************************************************************)

Initially[
  $UserVertexLabelFunction = Id;
  $UserEdgeLabelFunction   = Id;
];

CustomGraphMakeBoxes[expr_, _, fmt_] := BlockInteractive @ ToBoxes[expr, fmt];

toLabelFn[None | $Failed | Null] := Id;
toLabelFn[fn_] := passNull[fn];

passNull[fn_][Null] := Null
passNull[fn_][e_] := fn[e];

$emptyGraphGraphics := Graphics[
  {}, ImageSize -> {50, 50},
  Frame -> True, FrameStyle -> Directive[{GrayLevel[0.5], Dotted}], FrameTicks -> None
];

CustomGraphDrawing[graph2_] := Locals[
  vertexCount = VertexCount @ graph2;
  If[vertexCount === 0, Return @ $emptyGraphGraphics];
  UnpackAnnotations[graph2,
    vertexLabels, edgeLabels, labelFn, vertexLabelFn, edgeLabelFn,
    vertexColors, vertexColorFn, vertexTooltips,
    postGraphicsFn
  ];
  vertexShapeFn = LookupOptions[graph2, VertexShapeFunction];
  plotTheme = LookupOptions[graph2, PlotTheme];
  If[StringQ @ plotTheme,
    plotThemeData = GraphThemeData @ plotTheme;
    SetAuto[vertexShapeFn,  Lookup[plotThemeData, VertexShapeFunction, Auto]];
    SetNone[labelFn,        Lookup[plotThemeData, LabelFunction, None]];
    SetNone[vertexLabelFn,  Lookup[plotThemeData, VertexLabelFunction, None]];
    SetNone[edgeLabelFn,    Lookup[plotThemeData, EdgeLabelFunction, None]];
    SetNone[vertexColors,   Lookup[plotThemeData, VertexColors, None]];
    SetNone[vertexColorFn,  Lookup[plotThemeData, VertexColorFunction, None]];
    SetNone[vertexTooltips, Lookup[plotThemeData, VertexTooltips, None]];
  ];
  origShapeFn = vertexShapeFn;

  labelFn //= toLabelFn;
  vertexLabelFn //= toLabelFn;
  edgeLabelFn //= toLabelFn;
  vertexLabels = UDict @ If[RuleVectorQ[vertexLabels], vertexLabels, {}];
  edgeLabels = UDict @ If[RuleVectorQ[edgeLabels], edgeLabels, {}];
  newOptions = {};
  vertexStyle = Which[
    !MatchQ[vertexColorFn, None | Automatic], With[{vcf = vertexColorFn}, FmV :> vcf[FmV]],
    ColorQ[vertexColors],       vertexColors,
    ColorVectorQ[vertexColors], makeVertexColorRules[graph2, vertexColors],
    NoneQ[vertexColors],        None,
    True,                       GraphVertexData[graph2, vertexStyle]
  ];
  If[vertexStyle =!= None, AppendTo[newOptions, VertexStyle -> vertexStyle]];

  SetAuto[vertexShapeFn, "Disk"];
  If[vertexTooltips =!= None,
    vertexTooltips = GraphVertexData[graph2, vertexTooltips];
    vertexShapeFn = GraphTooltipFn[vertexShapeFn, UDictThread[VertexList @ graph2, vertexTooltips]]
  ];

  If[origShapeFn =!= vertexShapeFn, AppendTo[newOptions, VertexShapeFunction -> vertexShapeFn]];

  With[{graph = If[newOptions === {}, graph2, Graph[graph2, Seq @@ newOptions]]},
  Block[{
    $CurrentGraph = graph, $CurrentVertexLabels = vertexLabels, $CurrentEdgeLabels = edgeLabels,
    $UserVertexLabelFunction = vertexLabelFn /* labelFn, $UserEdgeLabelFunction = edgeLabelFn /* labelFn,
    $CurrentVertexAnnotations := $CurrentVertexAnnotations = AddSelfAnnotations @ GraphVertexAnnotations[$CurrentGraph],
    $CurrentEdgeAnnotations   := $CurrentEdgeAnnotations   = AddSelfAnnotations @ GraphEdgeAnnotations[$CurrentGraph]},
    If[optSetQ[postGraphicsFn], postGraphicsFn, Id] @ GraphComputation`GraphDrawing @ graph
  ]]
];

GraphDiskFn[pos_, _, size_] := Style[Disk[pos, Min @ size], EdgeForm @ GrayLevel[0, 0.3]];
GraphTooltipFn[fn_, assoc_][pos_, vert_, size_] :=
  graphicsTooltip[toShapeFn[dispatchVert[fn, vert, GraphDiskFn]][pos, vert, size], assoc[vert]];

toShapeFn[str_Str] := Fn[GraphComputation`GraphElementDataDump`shape2d[str][#1, #3]];
toShapeFn[fn_] := fn;

dispatchVert[{str_Str}, vert_, def_] := str;
dispatchVert[list_List, vert_, def_] := subDef[def] @ Replace[vert, Append[Select[list, RuleLikeQ], _ :> Auto]];
dispatchVert[expr_, vert_, def_] := subDef[def] @ expr;

subDef[def_][e_] := If[optSetQ[e], e, def];

optSetQ[None | Auto | Inherited | $Failed] := False;
optSetQ[_] := True;



makeVertexColorRules[graph_, colorList_] := ZipMap[
  {v, c} |-> Rule[v, c],
  Take[VertexList @ graph, UpTo @ Len @ colorList],
  Take[vertexColors, UpTo @ VertexCount @ graph]
]

graphicsTooltip[prim_, _Missing] := prim;
graphicsTooltip[prim_, label_] := Tooltip[prim,
  Pane[label,
    ImageMargins -> {{5, 5}, {5, 5}},
    ImageSize -> {{10, All}, {10, All}}
  ],
  TooltipStyle -> {Background -> GrayLevel[1], CellFrameColor -> None, CellFrame -> 0},
  TooltipDelay -> 0
];

(*************************************************************************************************)

patchMakeGraphBoxes[] := Then[
  patchCustomGraphLabeling[],
  patchCustomGraphDrawing[]
];

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

SetStrict[GraphAnnotationRules, GraphProperties]

GraphProperties[graph_Graph] := Lookup[graphAnnos @ graph, "GraphProperties", Dict[]];
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

SetStrict[AddVertexAnnotations]

AddVertexAnnotations[graph_Graph, EmptyP] := graph;
AddVertexAnnotations[graph_Graph, rule__Rule] := AddVertexAnnotations[graph, Dict[rule]];
AddVertexAnnotations[graph_Graph, dict_Dict] := Locals @ CatchMessages[
  $verts = VertexList @ graph;
  vrules = OutermostToInnermost @ KeyValueMap[toVRules, dict];
  Graph[graph, AnnotationRules -> RuleThread[$verts, vrules]]
];

toVRules = CaseOf[
  $[key_, list_List] := Then[SameLenQOrThrow[list, $verts, "badAnnoLen", key], Thread[key -> list]];
  $[key_, dict_Dict] := Thread[key -> Lookup[dict, $verts]];
  $[key_, expr_]     := ThrowMsg["badAnnoData", key, expr];
];

AddVertexAnnotations::badAnnoLen = "Length of list `1` for key `3` doesn't match number of vertices `2`.";
AddVertexAnnotations::badAnnoData = "Data for key `` should be a list or dict, not ``.";

(*************************************************************************************************)

SetStrict[GraphVertexAnnotations, GraphEdgeAnnotations, AddSelfAnnotations];

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
  DictThread[items, Dict /@ annos]
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
GraphAnimate[graph_Graph, vertexDataSpec_, edgeDataSpec_, opts:OptionsPattern[]] := Locals @ CatchMessages[
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
  VertexTooltips -> None,
  EdgeShapeFunction -> {"ShortUnfilledArrow", "ArrowSize" -> Medium, "ArrowPositions" -> 0.7}
}];


