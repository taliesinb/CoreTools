PackageExports[
  "FormHead", DataForm
];

(*************************************************************************************************)

CoreBoxes[DataForm[e_]] := PaneBox[
  DataFormBoxes[e],
  FrameStyle -> GrayLevel[0.9],
  DefaultBaseStyle -> ToList[AutoIndent -> False, LineBreakWithin -> False, $CodePaneBaseStyle]
];

$maxRowWidth = 100;
$maxArrayElems = 200;
$maxListLen = 50;
$maxStrLen = 64;
DataFormBoxes[e_, n_Int:80] := Check[
  DisableCoreBoxFormatting @ LineFlowedBoxes[dataBox @ e, n],
  StyleBox["(MessagesDuringFormatting)", $Red, Bold]
];

(*************************************************************************************************)

LineFlowedBoxes::usage = "LineFlowedBoxes[expr] evaluates box-creation code expr such that higher-level
boxes like BraceRowBox, and DelimitedRowBox, behave differently, resulting in a line-broken result.
";

DeclareHoldFirst[LineFlowedBoxes];

LineFlowedBoxes[body_, w_:80] := Block[
  {$x = 0, $xl = 0, $xr = w, DelimitedRowBox},
  boxes = body;
  boxes = boxes /. $embedWidthRules;
  (* boxes = boxes /. d_DelimitedRowBox :> layoutDR[d] *)
  boxes
];

$embedWidthRules = d_DelimitedRowBox[l_List] :> d[Map[addBoxW, l /. $embedWidthRules]];
addBoxW[b_] := BoxW[b, toInfW @ getW @ b];

getW = CaseOf[
  b_Str             := If[StrContainsQ[s, "\n"], Inf, StrLen @ b];
  RowBox[b_List]    := Total @ Map[$, b];
  DelimitedRowBox[l_, m_, r_][e_List] := $[l] + $[r] + ($[m]+1)*(Len[e]-1) + $[RowBox[e]];
  StyleBox[b_, ___] := $ @ b;
  TagBox[b_, ___]   := $ @ b;
  BoxW[b_, w_]      := w;
  _                 := Inf;
];
toInfW[w_] := If[w > $xr, Inf, w];

layoutDR[DelimitedRowBox[l_, m_, r_][e_]] := Locals[
  ws = getW /@ e;
  If[InfQ @ Max[ws], Null];
  InheritVar[$xl];
  $rif = m; $rifW = getW @ m; $xl += getW @ l;
  RowBox @ FlatList[l, MapFirstRest[flowFirst, flowRest, list], r]
];

(*
attribute grammar??
each item decides: am i on a fresh line, or after my younger sibling


*)

(* strategy: preprocess, enriching all delimited rows with their item widths. this happens from
bottom up.

how will this help us? well, ok, all indenting occurs because of delimited rows.
there is big indenting, which happens if we put the L and R on the their own row, everything in
between is indented by one tab spaces.
{
  {1,2,3},
  {4,5,6}
}

then there is flowing, where we insert newlines only as necessary:
{1, 2, 3, 4
 5, 6, 7, 8,
 9}

i think the obvious rule is that a delimited row must either be the first item in another delimited row,
OR start on a fresh line.

we want to avoid having this kind of thing:
{{0, 1, 0, 0, 1}, {1, 1, 1, 0,
   1}, {1, 1, 0, 0, 0}


is that the only source of indent? i think so.

*)

lineFlow = CaseOf[
  RowBox[list_List]         := RowBox[$ /@ list];
  StyleBox[b_, s___]        := StyleBox[$ @ b, s];
  s_Str                     := ($x += StrLen[s]; s);
  d_DelimitedRowBox[e_List] := delimFlow[d, l];
];

delimFlow[DelimitedRowBox[l_, m_, r_], list_] := Locals[
  InheritVar[$xl];
  (* If[$x === $xl, $xl += getW[l]; $x = $xl]; *)
  $rif = m; $rifW = getW @ m; $xl += getW @ l;
  RowBox @ FlatList[l, MapFirstRest[flowFirst, flowRest, list], r]
];

delimFlow1[d_DelimitedRowBox[a_]] := Locals[
  $xl += 1;
  z = newlineAndIndent[];
  {" ", z}
];

delimFlow[b_] := Locals[
  $canFlow = $x = b2 = lineFlow @ b; $x += $rifW;
  List[
    If[$x > $xr, newlineAndIndent[]],
    b2,
    If[$di++ != $dlen, $rif]
  ]
];

newlineAndIndent[] := {"\n", $x = $xl; mkIndent @ $xl};
(* mkIndent[n_] := mkIndent[n] = StrRepeat["\t", Floor[n / 4]] <> StrRepeat[" ", Mod[n, 4, 0]]; *)
mkIndent[n_] := mkIndent[n] = StrRepeat[" ", n];

getW = CaseOf[
  b_Str             := StrLen[b];
  RowBox[b_List]    := Total @ Map[$, b];
  StyleBox[b_, ___] := $ @ b;
  TagBox[b_, ___]   := $ @ b;
  _                 := 1;
];

(*************************************************************************************************)

DeclareHoldAllComplete[
  dataBox, atomDataBox,
  packedArrayBox, listDataBox, rawListDataBox,
  assocDataBox, assocEntryDataBox, exprBox
];

dataBox = CaseOf[
  e_ ? PackedArrayQ            := packedArrayBox @ e;
  e_List                       := listDataBox @ e;
  e_Dict ? HoldAtomQ          := assocDataBox @ e;
  DirectedInfinity[1]|Infinity := "\[Infinity]";
  DirectedInfinity[-1]         := "-\[Infinity]";
  e_ ? HoldAtomQ               := atomDataBox @ e;
  e_                           := exprBox @ e;
];

(*************************************************************************************************)

atomDataBox = CaseOf[
  i_Int      := ToString @ i;
  r_Real     := RealString[r, 3];
  r_Rational := RBox[IntStr @ #1, "/", IntStr @ #2]& @@ NumeratorDenominator[r];
  Infinity   := "\[Infinity]";
  s_Symbol   := HoldSymbolName @ s;
  s_Str      := If[StrLen[s] <= $maxStrLen, s, StrRep[s, dropSpec[StrLen @ s, $maxStrLen] -> "\[Ellipsis]"]];
  e_         := exprBox @ e;
];

(*************************************************************************************************)

packedArrayBox[a_ ? VectorQ] := listDataBox @ a;
packedArrayBox[a_] := elideDimsBox[Dims @ Unevaluated @ a,
  strArrayBox[ArrayDepth @ a][arrayStrings @ a]];

strArrayBox[1][a_]  := rawListDataBox @ a;
strArrayBox[n_][a_] := ApplyIndentBox[BraceRowBox @ Map[strArrayBox[n-1], a], 1];

arrayStrings[a_] := Locals[
  fn = If[PackedRealsQ[a], RealString[#, 3]&, ToString];
  strs = MapLeaves[fn, a];
  maxLen = Max @ StrLen @ Flatten @ strs;
  MapLastAxis[StringPadLeft[#, maxLen]&, strs]
];

(*************************************************************************************************)

DeclareHoldRest[elideDimsBox, applyLong];
DeclareHoldFirst[truncateList];

elideDimsBox[dims_, body_] := If[
  Apply[Times, dims] > $maxArrayElems,
  BraceRBox @ RiffledRowBox["\[Times]"] @ Map[NatStr, dims],
  body
];

applyLong[fn_, e_] :=
  fn @@ truncateList[e, dropSpec[Length @ Unevaluated @ e, $maxListLen]]

truncateList[e_, _] := Hold[e];
truncateList[e_, d:{d1_, d2_}] := Insert[Drop[Hold[e], None, d], $dropped[d2 - d1], {1, d1}];
dropSpec[len_, max_] /; len <= max := None;
dropSpec[len_, max_] := {Floor[1/2 * max], Ceiling[len - 1/2 * max]};

(*************************************************************************************************)

listDataBox[e_] := applyLong[rawListDataBox, e];

rawListDataBox[e_] := applyDataIndent @ BraceRowBox @ Map[dataBox, e];

assocDataBox[e_] := applyDataIndent @ AssocRowBox @ KeyValueMap[assocEntryDataBox, e];
assocEntryDataBox[k_, v_] := ArrowRowBox[dataBox @ k, dataBox @ v];
(* TODO: handle newlines that appear in key and val*)

(*************************************************************************************************)

rowBoxWidth[boxes_] :=
  Total @ Occurences[boxes, s_Str :> If[StrContainsQ[s, "\n"], Inf, StrLen @ s]];

applyDataIndent[boxes_] := If[
  rowBoxWidth[boxes] <= $maxRowWidth, boxes,
  ApplyIndentBox @ boxes
];

(*************************************************************************************************)

exprBox = CaseOf[
  $dropped[n_]     := StyleBox[RBox["«", NatStr @ n, "»"], $Red];
  head_Symbol[___] := RBox[HoldSymbolName @ head, BracketRBox["[", "\[Ellipsis]", "]"]];
  _                := "?"
];

(*************************************************************************************************)
(*
DeclareHoldAllComplete[dShortQ, dShallowQ, dSmallQ];

dShortQ[e_] := InputFormStringLength[CoreToolsHold[e], PrintPrecision -> 3];
dShallowQ[e_] := VectorQ[e, dSmallQ];
dSmallQ[_Symbol[e_ ? dSmallQ]] := True;
dSmallQ[e_Symbol ? HoldSymbolQ] := True;
dSmallQ[ExtNumP ? HoldSymbolQ] := True;
 *)
