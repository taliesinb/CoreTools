SystemExports[
  "Function",
    MultigraphIncidenceGraph,
    MultigraphCompose,
    MultigraphPower
]

(**************************************************************************************************)

MultigraphPower[mg_MGraph, 0]  := Multigraph[MultigraphVertexList @ mg, {}];
MultigraphPower[mg_MGraph, 1]  := mg;
MultigraphPower[mg_MGraph, n_] := MultigraphCompose @@ ConstList[mg, n];

MultigraphCompose::usage = "
MultigraphCompose[mgraph$1, mgraph$2] composes two multigraphs."

MultigraphCompose[g_MGraph] := g;

MultigraphCompose[g__MGraph] := Fold[MultigraphCompose, {g}];

MultigraphCompose[gl_MGraph, gr_MGraph] := Locals[
  $lOutGroups = MultiedgeOutputGroups @ gl;
  rEdges = MultiedgeList @ gr;
  Multigraph @ Sort @ Flatten @ Map[toCompositeEdges, rEdges]
];

toCompositeEdges[Multiedge[ins_, out_, name_]] :=
  MapTuples[makeCompositeEdge[out, name], Lookup[$lOutGroups, ins, {}]];

makeCompositeEdge[rightOut_, rightName_][leftEdges_List] := With[
  {leftInputs = Col1 @ leftEdges, leftNames = Col3 @ leftEdges},
  Multiedge[Catenate @ leftInputs, rightOut, Multiedge[leftNames, rightName]]
];
