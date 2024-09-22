PackageExports[
  "GraphicsFunction", MultiedgeTreePlot, MultiedgeIcon,
  "BoxFunction", MultiedgeIconBoxes
];

PrivateExports[
  "Variable",      $MultiedgeIconScale, $MultiedgeAutoColoring,
  "CacheVariable", $MultiedgeTreePlotCache
];

(**************************************************************************************************)

Initially[
  $MultiedgeIconScale = 1;
  $MultiedgeTheme = None;
  $MultiedgeAutoColoring = True;
];

(**************************************************************************************************)

CoreBoxes[m:Multiedge[ListDictP, _, ___] ? MultiedgeQ] := MaybeEval @ ClickBox[
  MultiedgeIconBoxes @ m,
  PrintInputCell @ m
];

(**************************************************************************************************)

MultiedgeInputs[Multiedge[ins_, __]] := ins;
MultiedgeOutputs[Multiedge[_, out_, _]] := out;
MultiedgeName[Multiedge[_, _, name_]] := name;
MultiedgeArity[Multiedge[ins_, __]] := Len[ins];

(**************************************************************************************************)

$MultiedgeTreePlotCache = UDict[];

MultiedgeTreePlot[m_Multiedge, opts___Rule] := CachedTo[$MultiedgeTreePlotCache, Hash[{m, opts}], iMultiedgeTreePlot[m, opts]];

iMultiedgeTreePlot[m_Multiedge, opts___Rule] := Locals[
  tree = MultiedgePathGraph @ m;
  exprs = GraphVertexData[tree, VertexAnnotation[Expression]];
  nodeColors = Map[MultigraphEdgeColor, exprs];
  plot = NiceTreePlot[tree,
    opts,
    NodeShape     -> Map[nodeShapeFn, toNodeSort /@ exprs],
    NodeColor     -> nodeColors,
    NodeSize      -> 3,
    EdgeThickness -> 1.5, EdgeColor -> GrayLevel[.2,.3],
    NodeTooltips  -> Map[toNodeTooltip /* Unformatted, exprs],
    GraphScale    -> 15
  ]
];

nodeShapeFn = CaseOf[
  domain_Symbol ? DomainQ := InsetNode @ CodeStyle @ domain;
  True                    := InsetNode @ "\[FilledCircle]";
  False                   := InsetNode @ "\[EmptyCircle]";
  sym_Symbol              := InsetNode @ CodeStyle[StringTake[SymbolName @ sym, 1], FontWeight -> "Plain"];
  Style[e_, _]            := $ @ e;
  other_                  := "Disk"
];

toNodeSort = CaseOf[
  Multiedge[i_, o_]     := o;
  Multiedge[i_, o_, n_] := o;
  Style[e_, _]          := $ @ e;
  other_                := other;
];

toNodeTooltip = CaseOf[
  Multiedge[i_, o_]     := Multiedge[toNodeSort /@ i, o];
  Multiedge[i_, o_, n_] := Multiedge[toNodeSort /@ i, o, n];
  Style[e_, _]          := $ @ e;
  other_                := other;
];

toNodeColor = CaseOf[
  m_Multiedge        := If[$MultiedgeAutoColoring, MultigraphEdgeColor @ m, $Gray];
  v_                 := If[$MultiedgeAutoColoring, MultigraphVertexColor @ v, $Gray];
  Style[_, c:ColorP] := c;
];

(**************************************************************************************************)

MultiedgeIcon[m_Multiedge, scale_:1] := RawBoxes @ MultiedgeIconBoxes[m, scale];

MultiedgeIconBoxes[m_Multiedge, scale_:1.] := Block[
  {$MultiedgeAutoColoring = $MultiedgeAutoColoring && VFreeQ[m, Keyed],
   $MultiedgeIconScale = scale},
  iconBoxes @ If[$MultiedgeAutoColoring, m, ColorDuplicates[m, Keyed[_, k_] :> k]]
];

(**************************************************************************************************)

iconBoxes = CaseOf[
  m:Multiedge[{___Multiedge}, _Multiedge, ___] := twoDimMultiedgeIconBoxes @ m;
  m:Multiedge[_, _]        := edgeTreeBoxes[m, $MultiedgeIconScale];
  m:Multiedge[_, _, name_] := ColumnBox[
    {edgeNameBoxes[name, $MultiedgeIconScale],
     edgeTreeBoxes[Take[m, 2], $MultiedgeIconScale]},
    Center, 0.  5, FrameStyle -> $Gray, RowLines -> True,
    BaselinePosition -> {{2,1}, Baseline}
  ];
];

edgeNameBoxes[m_, _] := StyleBox[SmallBox @ MakeBoxes @ m, MultigraphEdgeColor @ m];
edgeNameBoxes[m_Multiedge ? MultiedgeQ, scale_] :=
  ToBoxes @ MultiedgeTreePlot[m,
    RootPosition -> Bottom,
    GraphScale -> 10 * scale, NodeSize -> 4 * scale,
    EdgeThickness -> 1 * scale, FontSize -> 8 * scale
  ];

edgeTreeBoxes[m_Multiedge, scale_] :=
  ToBoxes @ MultiedgeTreePlot[m, RootPosition -> Top,
    GraphScale -> 15 * scale,
    NodeSize -> 6 * scale, FontSize -> 10 * scale, EdgeThickness -> 1.2 * scale
  ];

(**************************************************************************************************)

twoDimArrowBox[None] := "\[LeftTeeArrow]";
twoDimArrowBox[name_] := OverscriptBox["\[LeftTeeArrow]",
  HMarginBox[edgeNameBoxes[name, $MultiedgeIconScale], {.5, .5}]
];

twoDimMultiedgeIconBoxes = CaseOf[
  Multiedge[doms_, cod_, name_:None] := RBox[
    PaneBox[iconBoxes @ cod, ImageSize -> {{50, Automatic}, All}, Alignment -> Center],
    twoDimArrowBox @ name,
    If[doms === {}, "\[CenterDot]", Splice @ Riffle[MapVals[iconBoxes, doms], " "]]
  ];
];

(**************************************************************************************************)

SetPred1 @ MultiedgeQ;

MultiedgeQ[Multiedge[ListDictP ? (AllTrue[checkNestedQ]), Blank12]] := True;

checkNestedQ[m_Multiedge] := MultiedgeQ @ m;
checkNestedQ[_] := True;

(**************************************************************************************************)

MultiedgeLeafCount[_] := 0;
MultiedgeLeafCount[me_Multiedge] := leafCount @ me;

leafCount = CaseOf[
  Multiedge[ins_, ___] := Total @ MapVals[leafCount, ins];
  _ := 1;
];

(**************************************************************************************************)

MultiedgeDepth[_] := 0;
MultiedgeDepth[me_Multiedge] := edgeDepth @ me;

edgeDepth = CaseOf[
  Multiedge[ins_, ___]   := 1 + Max[MapVals[edgeDepth, ins], 0];
  _ := 0;
];

(**************************************************************************************************)

MultiedgePaths[m_Multiedge] := First @ nestedMEdgePathData @ m;

MultiedgePathGraph[m_Multiedge, opts___Rule] := Locals[
  {paths, exprs} = nestedMEdgePathData @ m;
  PrefixGraph[ZipMap[Annotation[#1, Expression -> #2]&, paths, exprs], opts]
];

nestedMEdgePathData[me_Multiedge] := Locals[
  $pathStack = GraphPath[];
  Collecting[{$pathNodes, $pathExprs}, visitNode @ me];
  {$pathNodes, $pathExprs}
];

visitNode = CaseOf[
  leaf_ := addNode @ leaf;
  medge:Multiedge[inputs_, output_, tag___] := MapP[
    addNode @ medge;
    {in, key} |-> BlockAppend[
      $pathStack, {output, key, tag},
      visitNode @ in
    ],
    inputs
  ]
];

addNode[expr_] := Then[
  $pathNodes @ $pathStack;
  $pathExprs @ expr;
];
