PackageExports[
  "Head",
    ExprDelta,
    DeltaPos,
    NotEqualDelta, NotSameDelta, ArrayNotEqualDelta, LenthDelta, DepthDelta, DimensionsDelta, KeysAddDelta, KeysRemovedDelta, KeysReorderedDelta, KeysChangedDelta, ArgumentsAddedDelta, ArgumentsRemovedDelta, SetLargerDelta, SetSmallerDelta, SetDelta,
  "Head",
    StochasticFunction
];

(*************************************************************************************************)

CoreBox[ed:ExprDelta[pos_List, crumbs_List, diff_]] :=
  GridBox[
    List @ Map[ToBoxes, Append[crumbs, diff]],
    FrameStyle -> $LightGray,
    Dividers -> All,
    ColumnSpacings -> 2,
    GridBoxDividers -> {"Columns" -> {False, {True}, False}, "Rows" -> {{}}},
    GridFrameMargins -> {{5, 5}, {3, 10}}
  ];

CoreBox[d_DeltaPos] := dposBoxes @@ d;
With[{hp = _Str | _HoldC},
dposBoxes[Head]            := dposSubBox["?", "head"];
dposBoxes[Args, h:hp]      := dposSubBox[h, "args"];
dposBoxes[PartOp[n], h:hp] := dposSubBox[h, n];
dposBoxes["Array"]             := StyleBox["\[DottedSquare]", Bold];
dposBoxes["Array", n_Int]      := dposSubBox["\[DottedSquare]", n];
dposBoxes["Opt", h:hp]        := dposSubBox[h, "keys"];
dposBoxes[OptionRules, h:hp]   := dposSubBox[h, "key", k]
dposBoxes[OptionKey[k], h:hp]  := dposSubBox[h, "key", k]
dposBoxes[Key[k], h:hp, k_]    := dposSubBox[h, k]
dposBoxes["Keys", h:hp]        := dposSubBox[h, "keys"];
dposBoxes["Keys", h:hp, k_]    := dposSubBox[h, k]
];

dposSubBox[head_, sub_] := UnderscriptBox[
  StyleBox[dposHeadStr @ head, Bold],
  StyleBox[dposLabelStr @ sub, Italic]
]

dposHeadStr = CaseOf[
  s_Str              := s;
  HoldC[List]        := "{\[CenterEllipsis]}";
  HoldC[Dict]        := "\[LeftAssociation]\[CenterEllipsis]\[RightAssociation]";
  HoldC[head:SymP]   := codeBox @ HoldSymbolName @ head;
  HoldC[head_[___]]  := RBox[% @ HoldC @ head, "[\[CenterEllipsis]]"];
  HoldC[s_Str]       := codeBox @ StrJoin["\"", s, "\""];
  hc_HoldC           := codeBox @ hc;
];

dposLabelStr = CaseOf[
  i_Int          := IntStr @ i;
  s_Str          := StyleBox[s, $Gray, FontFamily -> "Palantir"];
  h_HoldC        := codeBox @ h;
]

(*************************************************************************************************)

Format[ExprDelta[pos:{___Int}, crumbs_List, diff_], OutputForm] :=
  exprDeltaOutputForm[pos, crumbs, diff];

exprDeltaOutputForm[pos_, crumbs_, diff_] :=
  Row[Join[Riffle[crumbs, " | "], {" --> ", diff}]] /. HoldC[h_] :> HoldForm[h];

Format[$HeadCrumb,              OutputForm] := Underscript["?", "head"];
Format[$ArgsCrumb[h:hp],        OutputForm] := Underscript[h, "args"];
Format[$ArgsCrumb[h:hp, n_Int], OutputForm] := Underscript[h, n];
Format[$ArrayCrumb[],           OutputForm] := "-array-";
Format[$ArrayCrumb[n_Int],      OutputForm] := Underscript["-array-", n];
Format[$KeysCrumb[h:hp],        OutputForm] := Underscript[h, "keys"];
Format[$KeysCrumb[h:hp, k_],    OutputForm] := Underscript[h, k];

(**************************************************************************************************)

PrivateHead[NotEqualDelta, NotSameDelta, ArrayNotEqualDelta, LenthDelta, DepthDelta, DimensionsDelta, KeysAddDelta, KeysRemovedDelta, KeysReorderedDelta, KeysChangedDelta, ArgumentsAddedDelta, ArgumentsRemovedDelta, SetLargerDelta, SetSmallerDelta, SetDelta]

DefineStandardTraditionalForm[{
  NotEqualDelta[e1_HoldC, e2_HoldC]      :> diffBox @ notEqualBox[codeBox @ e1, codeBox @ e2],
  NotSameDelta[e1_HoldC, e2_HoldC]  :> diffBox @ notSameBox[codeBox @ e1, codeBox @ e2],
  ArrayNotEqualDelta[e1_HoldC, e2_HoldC] :> diffBox @ notSameBox[minMaxBox @ e1, minMaxBox @ e2],
  NotEqualDelta[prop
  LenthDelta[n1_Int, n2_Int]         :> diffBox @ changeBox["len", intBox @ n1, intBox @ n2],
  DepthDelta[d1_Int, d2_Int]          :> diffBox @ changeBox["depth", intBox @ d1, intBox @ d2],
  DimensionsDelta[d1_Int, d2_Int]           :> diffBox @ changeBox["dims", dimsBox @ d1, dimsBox @ d2],
  KeysAddDelta[k1_HoldC, k2_HoldC]      :> diffBox @ plusBox[keysBox @ Comp[k2, k1]],
  KeysRemovedDelta[k1_HoldC, k2_HoldC]    :> diffBox @ minusBox[keysBox @ Comp[k1, k2]],
  KeysReorderedDelta[k1_HoldC, k2_HoldC]  :> diffBox @ notSameBox[keysBox @ k1, keysBox @ k2],
  KeysChangedDelta[k1_HoldC, k2_HoldC]    :> diffBox @ notSameBox[keysBox @ k1, keysBox @ k2],
  ArgumentsAddedDelta[n1_Int, n2_Int]          :> diffBox @ changeBox["len", intBox @ n1, intBox @ n2],
  ArgumentsRemovedDelta[n1_Int, n2_Int]        :> diffBox @ changeBox["len", intBox @ n1, intBox @ n2],
  SetDelta[_, _, vals_HoldC]              :> diffBox @ plusBox[codeBox @ vals],
  SetSmallerDelta[vals_HoldC]             :> diffBox @ minusBox[codeBox @ vals],
  SetDelta[p_HoldC, m_HoldC]          :> diffBox @ plusMinusBox[codeBox @ p, codeBox @ m]
}];

diffBox[e_] := e;

$changeArrow = changeStyle @ "\[RightArrow]";

notEqualBox[a_, b_]       := compBox["\[NotEqual]", a, b];
notSameBox[a_, b_]        := compBox["\[NotCongruent]", a, b];
changeStyle[e_]           := OrangeBox @ BoldBox @ e;
propBox[e_]               := TypewriterBox @ StyleBox[e, $Gray];
minMaxBox[arr_]           := GridBox[{{PinkBox @ RealDigitsString[Max @ arr, 2]}, {TealBox @ RealDigitsString[Min @ arr, 2]}}, RowSpacings -> 0];

compBox[cmp_, a_, b_]     := RBox[a, changeStyle @ cmp, b];
changeBox[prop_, a_, b_]  := RBox[propBox @ prop, ":", notSameBox[a, b]];
plusBox[a_]               := RBox[GreenBox @ BoldBox @ "+", " ", a];
minusBox[a_]              := RBox[RedBox @ BoldBox @ "-", " ", a];
plusMinusBox[a_, b_]      := GridBox[{{GreenBox @ BoldBox @ "+", " ", a}, {RedBox @ BoldBox @ "-", " ", b}}];

codeBox[a_]               := StyleBox[a, "Code", Background -> None, FontColor -> Black];
codeBox[HoldC[e_]]        := codeBox @ ToPrettifiedString[InternalHoldForm[e], MaxLength -> 20, MaxDepth -> 2, CompactRealNumbers -> 4];

keysBox[list_List]        := RowBox @ Riffle[Map[keyBox, list], ","];
keyBox[HoldC[s_Symbol]]   := ItalicBox @ HoldSymbolName @ s;
keyBox[h_]                := codeBox[h];
intBox[i_Int]             := IntStr[i];
dimsBox[dims_List]        := RowBox @ Riffle[Map[intBox, dims], "\[Times]"];

(**************************************************************************************************)

PublicFunction[FindExprDeltas]

Options[FindExprDeltas] = {
  MaxItems -> 3
};

FindExprDeltas[a_, b_, OptionsPattern[]] := Locals[
  UnpackOptions[maxItems];
  $diffs = Bag[]; $count = 0; $crumbs = {}; $pos = {};
  Catch[
    diffExpr[HoldC[a], HoldC[b]],
    $fedTag
  ];
  BagPart[$diffs, All]
];

emitDiff[tag_] := If[$count++ < maxItems,
  StuffBag[$diffs, ExprDelta[$pos, $crumbs, tag]]; True,
  Throw[Null, $fedTag];
];

emitAtomDiff[a_, b_] /; a === b := False;
emitAtomDiff[a_, b_] := emitDiff @ If[TrueQ[a == b], UnsameDelta[a, b], UnequalDelta[a, b]];

_emitDiff := BadArguments[];

(**************************************************************************************************)

holdPart[e_HoldC, p_] := Extract[e, p, HoldC];
holdPart[e_HoldC, p__] := Extract[e, {p}, HoldC];

_holdPart := BadArguments[];

holdKeys[HoldC[]] := {};
holdKeys[HoldC[args__]] := Keys[NoEval[{args}], HoldC];
_holdKeys := BadArguments[];

(**************************************************************************************************)

SetHoldR[withCrumb, withPos, withPosCrumb, withSubCrumb];

withCrumb[c_, body_] := Block[{$crumbs = Append[$crumbs, c]}, body];
withPos[p_, body_] := Block[{$pos = Append[$pos, p]}, body];
withPosCrumb[p_, c_, body_] := Block[{$pos = Append[$pos, p], $crumbs = Append[$crumbs, c]}, body];
withSubCrumb[p_, c_, body_] := Block[{$pos = Append[$pos, p], $crumbs = Insert[$crumbs, c, {-1, -1}]}, body];

(**************************************************************************************************)

diffExpr[a_HoldC, b_HoldC] /; a === b := False;

diffExpr[a:HoldC[_Dict ? HAtomQ], b:HoldC[_Dict ? HAtomQ]] :=
  diffAssoc[a, b];

diffExpr[a:HoldC[_ ? HAtomQ], b:HoldC[_ ? HAtomQ]] :=
  emitAtomDiff[a, b];

diffExpr[a:HoldC[_List ? setLikeListQ], b:HoldC[_List ? setLikeListQ]] :=
  diffSet[a, b];

diffExpr[a:HoldC[_List ? HPackedQ], b:HoldC[_List ? HPackedQ]] :=
  diffArray[a, b];

diffExpr[
  HoldC[(h1_[Shortest[args1___], opts1:((_Symbol|_Str) -> _)...]) ? UnsealedQ],
  HoldC[(h2_[Shortest[args2___], opts2:((_Symbol|_Str) -> _)...]) ? UnsealedQ]] := With[
    {head = HoldC @ h1},
    Or[
      withPosCrumb[0, $HeadCrumb,  diffExpr[HoldC @ h1, HoldC @ h2]],
      withCrumb[$ArgsCrumb @ head, diffArgs[HoldC @ args1, HoldC @ args2]],
      withCrumb[$KeysCrumb @ head, diffKeys[HSeqLen @ args1, HoldC @ opts1, HoldC @ opts2]]
    ]
  ];

diffExpr[a_HoldC, b_HoldC] :=
  emitAtomDiff[a, b];

_diffExpr := BadArguments[];

(**************************************************************************************************)

SetHoldF[setLikeListQ]

setLikeListQ[{}] := False;
setLikeListQ[e_List] := VecQ[NoEval @ e, HAtomQ] && OrderedQ[NoEval @ e];

diffSet[HoldC[{a__}], HoldC[{b__}]] :=
  diffSet2[HoldC[a], HoldC[b]];

diffSet2[a_, b_] := Which[
  SubsetOfQ[NoEval @ a, NoEval @ b],
    emitDiff @ SetDelta[a, b, toListHC @ Comp[b, a], {}],
  SupersetOfQ[NoEval @ a, NoEval @ b],
    emitDiff @ SetDelta[a, b, {}, toListHC @ Comp[a, b]],
  IntersectsQ[NoEval @ a, NoEval @ b],
    emitDiff @ SetDelta[a, b, toListHC @ Comp[b, a], toListHC @ Comp[a, b]],
  True,
    withCrumb[$ArgsCrumb @ HoldC[List], diffArgs[a, b]]
];

toListHC[HoldC[a___]] := HoldC @ List[a];

(**************************************************************************************************)

diffArray[a_HoldC, b_HoldC] /; a === b := False;

(* these are packed and so we don't care about evaluation *)
diffArray[ha:HoldC[a_List], hb:HoldC[b_List]] := Module[
  {dims1, dims2, depth1, depth2, counts},
  dims1 = Dims @ a; depth1 = Len @ dims1;
  dims2 = Dims @ b; depth2 = Len @ dims2;
  withCrumb[$ArrayCrumb[],
    Which[
      depth1 =!= depth2,
        emitDiff[ha, hb, Depth, UnequalDelta[depth1, depth2]],
      dims1 =!= dims2,
        emitDiff[ha, hb, Length, UnequalDelta[dims1, dims2]],
      True,
        counts = Counts @ Flatten @ MapThread[SameQ, {a, b}, depth1];
        If[counts[False] >= 3 || counts[True] == 0,
          (* don't try find differing entries if there are a lot or no entries are equal! *)
          emitDiff @ UnequalDelta[a, b, Array],
          Catch @ MapIndexed[diffScalar, TupleArray @ {a, b}, depth1]
        ];
    ]
  ];
  True
];

_diffArray := BadArguments[];

diffScalar[{a_, b_}, pos_] /; a === b := Null;
diffScalar[{a_, b_}, pos_] := withSubCrumb[
  Splice @ pos, pos,
  emitAtomDiff[HoldC @ a, HoldC @ b]
];

(**************************************************************************************************)

diffArgs[args1_HoldC, args2_HoldC] /; args1 === args2 := False;

diffArgs[args1_HoldC, args2_HoldC] := Module[
  {len1, len2},
  len1 = Len @ args1;
  len2 = Len @ args2;
  Which[

    len1 <= len2 && (args1 === Take[args2, len1]),
      emitDiff @ ArgumentsAddedDelta[len1, len2],

    len2 <= len1 && (args2 === Take[args1, len2]),
      emitDiff @ ArgumentsRemovedDelta[len1, len2],

    len1 =!= len2,
      emitDiff @ LenthDelta[len1, len2],

    True,
      Do[
        withSubCrumb[i, i, diffExpr[holdPart[args1, i], holdPart[args2, i]]],
        {i, 1, len1}
      ]
  ];
  True
];

_diffArgs := BadArguments[];

(**************************************************************************************************)

(* these are valid associations and so don't need to be held *)

diffAssoc[a_HoldC, b_HoldC] /; a === b := False;

diffAssoc[HoldC[a_Dict], HoldC[b_Dict]] :=
  withCrumb[
    $KeysCrumb @ HoldC @ Dict,
    diffKeys[1, assocToEntries @ a, assocToEntries @ b]
  ];

assocToEntries[assoc_Dict] := Module[
  {pairs},
  pairs = HoldC @@ KVMap[HoldC, assoc];
  VectorReplace[pairs, HoldC[k_, v_] :> (k -> v)]
];

_diffAssoc := BadArguments[];

(**************************************************************************************************)

diffKeys[startIndex_, entries1_HoldC, entries2_HoldC] := Module[
  {len1, len2, keys1, keys2},

  len1 = Len @ entries1; keys1 = holdKeys @ entries1;
  len2 = Len @ entries2; keys2 = holdKeys @ entries2;

  Which[

    len1 < len2 && SubsetQ[keys2, keys1],
      emitDiff @ KeysAddDelta[keys1, keys2],

    len2 < len1 && SubsetQ[keys1, keys2],
      emitDiff @ KeysRemovedDelta[keys1, keys2],

    keys1 =!= keys2,
      If[Sort[keys1] === Sort[keys2],
        emitDiff @ KeysReorderedDelta[keys1, keys2],
        emitDiff @ KeysChangedDelta[keys1, keys2]
      ],

    True,
      Null
  ];

  ScanIndex1[{key, i1} |->
    If[IntQ[i2 = IndexOf[keys2, key, None]],
      withSubCrumb[startIndex + i1 - 1, key,
        diffExpr[holdPart[entries1, i1, 2], holdPart[entries2, i2, 2]]
      ]
    ],
    keys1
  ];

  True
];

(**************************************************************************************************)

PublicFunction[ShowSequenceAlignment]

PublicOption[ElideUnchanged]

Options[ShowSequenceAlignment] = {
  ElideUnchanged -> False
}

lineSplit[e_] := SSplit[e, "\n"];
lineJoin[e_] := SRiffle[e, "\n"];

ShowSequenceAlignment[a_Str, b_Str, OptionsPattern[]] := Locals[
  UnpackOptions[$elideUnchanged];
  a = lineSplit @ a;
  b = lineSplit @ b;
  Grid[
    toSARow /@ SequenceAlignment[a, b],
    BaseStyle -> {FontFamily -> "Fira Code", FontSize -> 12},
    Alignment -> Left
  ]
];

toSARow = Case[
  a:{__Str} := If[$elideUnchanged, Nothing, {lineJoin @ a, SpanFromLeft}];
  {{a_Str}, {b_Str}} :=
    If[EditDistance[a, b] > Max[SLen[{a, b}] / 4],
      redGreen[a, b],
      {Row[toSAInline /@ SequenceAlignment[a, b]], SpanFromLeft}
    ];
  {a:{___Str}, b:{___Str}} := redGreen[lineJoin @ a, lineJoin @ b];
];

toSAInline := Case[
  a_Str := a;
  {a_Str, b_Str} := Row @ redGreen2[a, b];
];

redGreen[a_, b_] := {Style[a, FontColor -> $Red], Style[b, FontColor -> $Green]};

redGreen2[a_, b_] := {Style[a, Background -> $LightRed], Style[b, Background -> $LightGreen]};
