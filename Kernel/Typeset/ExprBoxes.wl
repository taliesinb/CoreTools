PackageExports[
  "FormHead",     ExprForm, HoldExprForm,
  "IOFunction",   MakeExprBoxes, ToExprBoxes,
  "OptionSymbol", MaxLength, MaxBytes, MaxDepth, MaxStrLen,
  "Predicate",    LongExprQ, BigExprQ, LongBigExprQ,
  "Variable",     $ExprOpts
];

(*************************************************************************************************)

SetHoldSeq @ SetHoldF @ MakeExprBoxes;

SetHoldC @ HoldExprForm;

SystemBoxes[ExprForm[e_, opts___Rule]]     := MakeExprBoxes[e, opts];
SystemBoxes[HoldExprForm[e_, opts___Rule]] := MakeExprBoxes[e, opts];

(*************************************************************************************************)

$maxLength = {16, 8, 4, 2};
$maxBytes = 2048;
$maxDepth = 4;
$maxStrLen = 32;
$printPrecision = 4;

SetHoldSeq @ SetHoldC @ SetPred1[LongExprQ, BigExprQ, LongBigExprQ];

$curDepth = 1;
$curLen = 16;

LongExprQ[e_]           := HLen[e] > $curLen;
BigExprQ[e_]            := HByteCount[e] > $maxBytes;
BigExprQ[e_ ? HDictQ]   := HByteCount[e] > $maxBytes * 2;
BigExprQ[e_ ? HPackedQ] := (HByteCount[e] * 3) < $maxBytes;
LongBigExprQ[e_]        := LongExprQ[e] || BigExprQ[e];

(*************************************************************************************************)

SetHoldC[blockDepth, decDepth];

blockDepth[e_, _] /; $curDepth > $maxDepth :=
  HoldElidedBox @ e;

blockDepth[_, body_] := decDepth[body];

decDepth[body_] := Block[
  {$curDepth = $curDepth + 1,
   $curLen = PartOr[$maxLength, $curDepth, Last @ $maxLength]},
  body
];

(*************************************************************************************************)

$exprFormOptions = {
  MaxLength   -> $maxLength,
  MaxBytes    -> $maxBytes,
  MaxDepth    -> $maxDepths,
  MaxStrLen   -> $maxStrLen,
  PrintPrecision -> $printPrecision
};

Options[ExprForm] = $exprFormOptions;
Options[MakeExprBoxes] = $exprFormOptions;

(*************************************************************************************************)

ToExprBoxes[expr_, opts___Rule] := MakeExprBoxes[expr, opts];

(*************************************************************************************************)

SetHoldSeq @ SetHoldC @ MakeExprBoxes;

MakeExprBoxes[expr_] := exprBox @ expr;

MakeExprBoxes[expr_, opts__Rule] := Locals[
  BindSymbols[{opts}, $maxLength, $maxBytes, $maxDepth, $maxStrLen, $printPrecision];
  $curLen = First @ $maxLength;
  exprBox @ expr
];

(*************************************************************************************************)

SetHoldSeq @ SetHoldC @ exprBox;

exprBox = CaseOf[
  l:PackedP := packedBox @ l;
  l_List    := blockDepth[l, listBox @ l];
  d:DictP   := blockDepth[d, dictBox @ d];
  s:StrP    := strBox @ s;
  s:SymP    := MakeBoxes @ s;
  d:DatumP  := datumBox @ d;
  s:ASetP   := blockDepth[s, setBox @ s];
  r:RuleLP  := ruleBox @ r;
  HoldForm[e_] := exprBox @ e;
  e_        := blockDepth[e, arbBox @ e];
];

SetHoldC[datumBox, strBox];

strBox[s_] := If[StrLen[s] > $maxStrLen, HoldElidedBox @ s, MakeBoxes @ s];

datumBox[r_Real] := RealString[r, $printPrecision];
datumBox[d_]     := MakeBoxes @ d;

(*************************************************************************************************)

SetHoldC[argBoxes, commaArgsBox, rawExprBox, shortQ];

shortQ[e_] := HoldLen[e] <= $curLen;

argBoxes = CaseOf[
  $[EmptyP, _, _]             := {};
  $[_[e1_], b_, _]            := {b @ e1};
  $[_[e1_, e2_], b_, d_]      := If[$curLen >= 2, {b @ e1, b @ e2}, addDots[2, d] @ {b @ e1}];
  $[e_List ? shortQ, b_, d_]  := HoldMap[b, e];
  $[e_List, b_, d_]           := addDots[HLen @ e, d] @ HTakeArgs[e, $curLen, b];
  $[e_Dict ? shortQ, b_, d_]  := KeyValueMap[Fn[Null, b[Rule[#1, #2]], HoldAllComplete], NoEval @ e];
  $[e_Dict, b_, d_]           := addDots[HLen @ e, d] @ HTakeArgs[e, $curLen, b];
];

addDots[n_, dotsFn_][boxes_] := Append[boxes, dotsFn[n - Len[boxes], Last @ boxes]];

commaArgsBox[list_] := Riffle[argBoxes[list, exprBox, CDotsBox[#]&], ","];

rawExprBox[e_] := RawBoxes @ exprBox @ e;

(*************************************************************************************************)

packedBox[p_] := If[LeafCount[p] < $curLen,
  BraceRowBox[datumBox /@ p],
  BraceRBox @ HMarginBox[GrayBox @ packedSummary @ p, {0.2,0.2}]
];

packedSummary[p_] := GrayBox[SubscriptBox[
  Replace[PackedType @ p, $typeToLetter],
  RiffBox["\[Times]"] @ Map[SysIntStr, Dims @ p]
]];

$typeToLetter = {Real -> "\[DoubleStruckCapitalR]", Int -> "\[DoubleStruckCapitalZ]", Complex -> "\[DoubleStruckCapitalC]"};

(*************************************************************************************************)

SetHoldC[listBox]

listBox[{}]        := RBox[LBrace, RBrace];
listBox[list_List] := multilineBox[LBrace, commaArgsBox @ list, RBrace]

(*************************************************************************************************)

SetHoldC[ruleBox, simpleBoxes];

ruleBox = CaseOf[
  RuleD[k:DatumP|SymP, v_] := RBox[exprBox @ k, "\[RuleDelayed]", exprBox @ v];
  Rule[k:DatumP|SymP, v_]  := RBox[exprBox @ k, "\[Rule]", exprBox @ v];
  head_[k_, v_]            := ToBoxes @ head[RawBoxes @ simpleBoxes[k], RawBoxes @ exprBox @ v];
];

simpleBoxes = CaseOf[
  {}                              := RBox[LBrace, RBrace];
  l:DatumP|SymP                   := datumBox @ l;
  l:{PatRep[DatumP|SymP, {1, 3}]} := BraceRowBox[datumBox /@ l];
  e_                              := HoldElidedBox[e];
];

(*************************************************************************************************)

SetHoldC[headExprBox];

headExprBox = CaseOf[
  sym_Sym[]        := RBox[SymName @ sym, "[", "]"];
  sym_Sym[args___] := multilineHeadBox[SymName @ sym, commaArgsBox @ {args}];
];

(*************************************************************************************************)

SetHoldC[dictBox, dictItemBox];

dictBox = CaseOf[
  EmptyODict := RBox[LAssoc, RAssoc];
  EmptyUDict := RBox["UDict", "[", "]"];
  dict_Dict ? UDictQ := multilineHeadBox["UDict", commaArgsBox @ dict];
  dict_Dict := multilineBox[LAssoc, commaArgsBox @ dict, RAssoc];
];

(*************************************************************************************************)

setBox = CaseOf[
  s_ ? LongBigExprQ := HoldElidedBox @ s;
  s_                := blockDepth[e, MakeBoxes @ s];
];

(*************************************************************************************************)

multilineBox[l_, boxes_, r_] := Which[
  singleLineQ[boxes],
    RowBox @ ToList[l, boxes, r],
  requiresExtraNewlinesQ[boxes],
    RowBox @ ToList[l, "\n",  addTab["\t"] /@ addNewline /@ boxes, "\n", r],
  True,
    RowBox @ ToList[l, MapRest[addTab["\[NonBreakingSpace]"], addNewline /@ boxes], r]
];

multilineHeadBox[head_Str, boxes_] := If[
  And[singleLineQ[boxes], VFreeQ[boxes, GraphicsBox]] || And[SingleQ[boxes], VFreeQ[boxes, "\n"]],
  RowBox @ ToList[head, "[", boxes, "]"],
  RowBox @ ToList[head, "[", "\n", addTab["\t"] /@ addNewline /@ boxes, "\n", "]"]
];

$needsLineP = Alt[
  RowBox[{"{", __, "}"} | {_Str, "[", __, "]"} ? notSimpleArgsQ | {LAssoc, __, RAssoc}],
  TemplateBox[_, "RowDefault" | "RowWithSeparators"],
  _GridBox
];

singleLineQ[boxes_] := VFreeQ[boxes, {RowBox, GridBox}] || FreeQ[boxes, $needsLineP];

notSimpleArgsQ[args_] := !StrVecQ[args];

$needsExtraLineP = Alt[
  RowBox[{_String, "[", __, "]"} ? notSimpleArgsQ],
  GridBox[{BlankSeq2}, ___]
];

requiresExtraNewlinesQ[boxes_] := ContainsQ[boxes, $needsExtraLineP];

addNewline[","] := Splice[{",", "\n"}];
addNewline[e_] := e;

addTab[_][e:(","|"\n")] := e;
addTab[t_][e_]         := Splice[{t, $add = False; subtabs[t] @ e}];

t_subtabs[r_RowBox] := Map[t, r];
t_subtabs[l_List]   := Map[t, l];
t_subtabs["\n"]     := ($add = True; "\n");
t_subtabs[s_Str]    := If[$add, $add = False; Splice[{First @ t, s}], s];
t_subtabs[e_]       := e;

makeTab[n_] := makeTab[n] = StringRepeat["\[NonBreakingSpace]", n];

(*************************************************************************************************)

SetHoldC[arbBox, compoundFormBox, atomFormBox, truncate]

arbBox = CaseOf[
  RawBoxes[boxes_]                    := boxes;
  arb:(Row | Column)[_List, ___]      := rowColBoxes @ arb;
  arb:(Grid | MatrixForm | TableForm)[_List, ___] := arrayFormBoxes @ arb;
  arb:Image ? ValidFlagQ              := imageBox @ img;
  arb:Graph ? NoEntryFlagQ            := graphBox @ grp;
  arb_Graphics                        := graphicsBox @ arb;
  arb:(_Sym ? CompoundFormHeadQ)[___] := compoundFormBox @ arb;
  arb:(_Sym ? AtomFormHeadQ)[___]     := atomFormBox @ arb;
  arb:(_Sym ? HasCoreBoxesQ)[___]     := MakeBoxes @ arb;
  arb:(SymP)[___] ? HEntryFlagQ       := headExprBox @ arb;
  arb_ ? LongBigExprQ                 := HoldElidedBox @ arb;
  arb_                                := MakeBoxes @ arb;
];

(*************************************************************************************************)

SetHoldC[imageBox, graphBox, graphicsBox];

imageBox[img_Image] /; !Image`ValidImageQ[NoEval @ img] := HoldElidedBox @ img;

$targetSize := 600 / ($curDepth)^2;

imageBox[img_Image] := Locals[
  size = Max @ ImageDimensions @ img;
  maxSize = $targetSize;
  ToBoxes @ If[size <= maxSize, img, Thumbnail[img, UpTo @ size]]
];

graphBox[g_Graph] := ToBoxes @ If[$curDepth === 1, g, Graph[g, ImageSize -> $targetSize]];

graphicsBox[g_Graphics] := ToBoxes @ If[$curDepth === 1, g,
  Append[
    DelCases[g, ImageSize -> _],
    ImageSize -> With[{t = Ceiling @ $targetSize}, UpTo[t]]
  ]
];

(*************************************************************************************************)

SetHoldC[rowColBoxes, arrayFormBoxes, gridRowBoxes];

rawInnerBoxes[HoldC[e_]] := exprBox @ e;

rowColBoxes[head_[list_List, opts___]] := With[
  {elemBoxes = Map[RawBoxes] @ ListDictMakeBoxes1D[list, head === Row, rawInnerBoxes, None, 500, $curLen]},
  MakeBoxes @ head[elemBoxes, opts]
];

arrayFormBoxes[head_[list_List, opts___]] := With[
  {rows = argBoxes[list, gridRowBoxes, dotsRow[If[head === Grid, LSpan, ""]]]},
  MakeBoxes @ head[rows, opts]
];

gridRowBoxes[row_List] := decDepth @ argBoxes[row, rawExprBox, RawBoxes[CDotsBox[#1]]&];
gridRowBoxes[other_] := decDepth @ exprBoxes @ other;

dotsRow[pad_][n_, last_] := PadRight[
  List @ RawBoxes @ ItemBox[
    CDotsBox[n, False],
    Alignment -> Center
  ],
  Len @ last, RawBoxes @ LSpan
];

(*************************************************************************************************)

SetHoldC @ SetHoldC;

atomFormBox[e_] := MakeBoxes @ e;

(*************************************************************************************************)

SetHoldC[compoundFormBox, compoundForm1Box, compoundForm2Box, compoundFormNBox];

compoundFormBox[e:(sym_[___])] := Switch[
  CompoundFormArity @ sym,
  1, compoundForm1Box[e],
  2, compoundForm2Box[e],
  All, blockDepth[e, compoundFormNBox[e]]
];

compoundForm1Box[sym_[a1_, aN___]] := finalBoxes[{rawExprBox @ a1}, sym, aN];
compoundForm2Box[sym_[a1_, a2_, aN___]] := finalBoxes[{rawExprBox @ a1, rawExprBox @ a2}, sym, aN];
compoundFormNBox[sym_[aN___]] := finalBoxes[HoldMap[rawExprBox, {aN}], sym];

SetHoldR @ SetStrict @ finalBoxes;

finalBoxes[{left___}, sym_, right___] := MakeBoxes @ sym[left, right];
