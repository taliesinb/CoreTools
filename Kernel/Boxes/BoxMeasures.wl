PrivateExports[
  "Predicate",     SingleLineBoxArgsQ, SingleLineBox2Q, ExtraLinesBoxQ, SimpleRowBoxArgsQ
]

(*************************************************************************************************)

SetHoldC[MultilineBoxArgsQ, SingleLineBoxArgsQ, MultilineBoxArgsQ, SingleLineBox2Q, ExtraLinesBoxQ, ComplexRowBoxQ];

SingleLineBoxArgsQ[{boxes_}] /; VFreeQ[NoEval @ boxes, "\n"] := True;
SingleLineBoxArgsQ[boxes_] := And[
  LeafCount[NoEval @ boxes] < 128,

  VFreeQ[NoEval @ boxes, {RowBox, GridBox}] || FreeQ[NoEval @ boxes, $needsLineP]
];

(*************************************************************************************************)

SetHoldC @ MultilineBoxArgsQ;

MultilineBoxArgsQ[boxes_] := Or[
  MultilineBoxArgsQ[boxes, LeafCount],
  MultilineBoxArgsQ[boxes, GraphicsBox],
  MultilineBoxArgsQ[boxes, GridBox],
  MultilineBoxArgsQ[boxes, RowBox]
];

MultilineBoxArgsQ[boxes_, LeafCount] :=
  LeafCount[NoEval @ boxes] > 128;

MultilineBoxArgsQ[boxes_, GraphicsBox]  := And[
  VContainsQ[NoEval @ boxes, GraphicsBox],
  VContainsQ[ReplaceAll[HoldC @ boxes, TagBox[_, "ColorSwatch"] :> Null], GraphicsBox]
];

MultilineBoxArgsQ[boxes_, GridBox] := And[
  VContainsQ[NoEval @ boxes, GridBox],
   ContainsQ[NoEval @ boxes, GridBox[_List ? MultilineGridBoxQ, ___]]
];

MultilineBoxArgsQ[boxes_, RowBox] := And[
  VContainsQ[NoEval @ boxes, RowBox],
   ContainsQ[NoEval @ boxes, RowBox[$dataRowBoxP] ? MultilineRowBoxQ]
];

(*************************************************************************************************)

SetHoldC @ ComplexGridBoxQ;

$dataRowBoxP = {"{", __, "}"} | {_, "[", __, "]"} | {LAssoc, __, RAssoc};

MultilineRowBoxQ[RowBox[list_List]] := !StrVecQ[NoEval @ list] || HLen[list] > 12;
MultilineRowBoxQ[_] := False;

(*************************************************************************************************)

SetHoldC @ MultilineRowBoxQ;

$dataRowBoxP = {"{", __, "}"} | {_, "[", __, "]"} | {LAssoc, __, RAssoc};

MultilineRowBoxQ[RowBox[list_List]] := !StrVecQ[NoEval @ list] || HLen[list] > 12;
MultilineRowBoxQ[_] := False;
