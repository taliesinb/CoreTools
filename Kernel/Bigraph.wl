SystemExports[
  "Head",
    BipartiteGraph,
  "Function",
    GraphCompose
];

(**************************************************************************************************)

BipartiteGraph[edges_List] := Locals[
  arr = SparseArray @ Normal @ Merge[Total] @ MapApply[{s,t} |-> {s,t} -> 1, edges];
  BipartiteGraph @ arr
];

CoreBoxes[BipartiteGraph[adj_SparseArray]] := Locals[
  {ns, nt} = Dims @ adj;
  rules = ArrayRules[adj];
  srcPos = Thread @ {0, Range[ns]};
  tgtPos = Thread @ {3, Range[nt]};
  arrows = MapApply[{st, w} |-> (
    p1 = Part[srcPos, P1 @ st]; p2 = Part[tgtPos, P2 @ st];
    {If[w =!= 1, Text[Style[w, GrayLevel[0,1]], Offset[{0,5},(p1 + p2)/2]], Nothing], Arrow[{p1,p2}]}),
    Most @ rules
  ];
  ToBoxes @ Graphics[{
    {Opacity[0.5], Gray, Arrowheads[{{.06, 0.7}}], arrows},
    {AbsolutePointSize[8], $Blue, Point @ srcPos, $Red, Point @ tgtPos}
  }, ImageSize -> 150]
];

GraphCompose[a_BipartiteGraph, b_BipartiteGraph] :=
  BipartiteGraph[Dot[First @ a, First @ b]]
