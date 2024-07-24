SystemExports[
  "OptionSymbol",
    LabelFunction, VertexLabelFunction, EdgeLabelFunction, PostGraphicsFunction,
    EdgeColor, EdgeThickness, ThemeParent,

  "Head",
    VertexAnnotation, EdgeAnnotation, TimePart,

  "Function",
    GraphVertexAnnotations, GraphEdgeAnnotations, GraphAnnotationRules, AddSelfAnnotations,

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
    CustomVertexLabelFunction, CustomEdgeLabelFunction,
    CustomDynamicVertexLabelFunction, CustomDynamicEdgeLabelFunction,
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

$CustomGraphOptions = {LabelFunction, VertexLabelFunction, EdgeLabelFunction, PostGraphicsFunction};
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

patchMakeGraphBoxes[] := With[
  {gmb := GraphComputation`GraphMakeBoxes,
    gd := GraphComputation`GraphDrawing,
    mb := GraphComputation`GraphBoxesDump`makeBoxes,
    ivlf = GraphComputation`GraphLabelsDump`vertexLabelingFunction,
    ielf = GraphComputation`GraphLabelsDump`edgeLabelingFunction,
    idvlf = GraphComputation`GraphLabelsDump`dynamicVertexLabelingFunction,
    idelf = GraphComputation`GraphLabelsDump`dynamicEdgeLabelingFunction,
    vlf = GraphComputation`VertexLabelingFunction,
    elf = GraphComputation`EdgeLabelingFunction,
    dvlf = GraphComputation`DynamicVertexLabelingFunction,
    delf = GraphComputation`DynamicEdgeLabelingFunction
  },
  Unprotect[gmb, vlf, elf, dvlf, delf, mb, setGStyle];

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
  DownValues[dvlf] = Prepend[
    DownValues[dvlf] /. idvlf -> CustomDynamicVertexLabelFunction,
    HoldP[vlf[Placed[VertexAnnotation[k_], pos_, f___], expr___]] :> Block[{res},
      res = CustomDynamicVertexLabelFunction[f][VertexAnnotation[#5, k], pos, expr];
      res /; UnsameQ[res, $Failed]
    ]
  ];
  DownValues[delf] = Prepend[
    DownValues[delf] /. idelf -> CustomDynamicEdgeLabelFunction,
    HoldP[elf[Placed[EdgeAnnotation[k_], pos_, f___], expr___]] :> Block[{res},
      res = CustomDynamicEdgeLabelFunction[f][EdgeAnnotation[#3, k], pos, expr];
      res /; UnsameQ[res, $Failed]
    ]
  ];
  DownValues[gmb] = DownValues[gmb] /. {gd -> CustomGraphDrawing, mb -> CustomGraphMakeBoxes};
  Protect[gmb, vlf, elf, mb, gThemes, setGStyle];
]

With[
  {ivlf = GraphComputation`GraphLabelsDump`vertexLabelingFunction,
   ielf = GraphComputation`GraphLabelsDump`edgeLabelingFunction,
   idvlf = GraphComputation`GraphLabelsDump`dynamicVertexLabelingFunction,
   idelf = GraphComputation`GraphLabelsDump`dynamicEdgeLabelingFunction,

   (* gThemes := GraphComputation`$GraphThemes, *)
   sgs := GraphComputation`SetGraphStyle},

  Unprotect[sgs];
  sgs[] := Join[$SystemGraphThemeNames, Keys @ $CustomGraphThemeData];
  sgs[name_String ? CustomGraphThemeQ, prop_Symbol, args___] :=
    TryEval @ CustomSetGraphStyle[$CustomGraphThemeData @ name, prop, args];
  Protect[sgs];

  CustomVertexLabelFunction[]   := ivlf[wrapUL @ $UserVertexLabelFunction];
  CustomVertexLabelFunction[f_] := ivlf[wrapUL @ f];
  CustomEdgeLabelFunction[]     := ielf[wrapUL @ $UserEdgeLabelFunction];
  CustomEdgeLabelFunction[f_]   := ielf[wrapUL @ f];
  CustomDynamicVertexLabelFunction[]   := idvlf[wrapUL @ $UserVertexLabelFunction];
  CustomDynamicVertexLabelFunction[f_] := idvlf[wrapUL @ f];
  CustomDynamicEdgeLabelFunction[]     := idelf[wrapUL @ $UserEdgeLabelFunction];
  CustomDynamicEdgeLabelFunction[f_]   := idelf[wrapUL @ f];
];

wrapUL[f_][Null|None] := Null;
wrapUL[f_][e_]        := Replace[f[e], None -> Null];

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

DeclareStrict[GraphAnnotationRules]

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
      graphics, p_PartOfOp :> TryEval @ p @ t
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


