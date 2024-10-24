SystemExports[
  "Option",       MaxLength, MaxBytes, MaxDepth, MaxStrLen, ElidePacked, Multiline, SpecialChars
];

PackageExports[
  "FormHead",      SystemForm, CodeForm, DataForm, ExprForm, EchoForm, HoldExprForm, Unlimited, Limited, RichElidedForm, HoldAtForm,
  "BoxFunction",   MakeExprBoxes, MakeCodeBoxes, MakeEchoBoxes, ToExprBoxes, HoldRichElidedBox, SwatchBox,
  "Predicate",     LongExprQ, BigExprQ, LongBigExprQ
];

(*************************************************************************************************)

DeclaredHere[SystemForm, ExprForm, HoldExprForm, RichElidedForm, Unlimited, Limited];

SetHoldF[HoldExprForm];

SystemBox[SystemForm[e_]]                := MakeBoxes @ e;
SystemBox[ExprForm[e_, opts___Rule]]     := MakeExprBoxes[e, opts];
SystemBox[HoldExprForm[e_, opts___Rule]] := MakeExprBoxes[e, opts];
SystemBox[RichElidedForm[e_]]            := HoldRichElidedBox[e];
SystemBox[Unlimited[e_, d_Int:1]]            := blockUnlimited[MakeBox @ e, d];
SystemBox[Limited[e_, n:IntOrVecP, d_Int:0]] := blockLimited[MakeBox @ e, n, d];

(*************************************************************************************************)

DeclaredHere[CodeForm, DataForm, EchoForm];

SetHoldC[CodeForm];

SystemBox[CodeForm[e_, n:PatRep[ExtPosIntP, {0, 2}]]] := CodeStyleBox @ MakeCodeBoxes[e, n];
SystemBox[DataForm[e_, n:PatRep[ExtPosIntP, {0, 2}]]] := CodeStyleBox @ MakeCodeBoxes[e, n];
SystemBox[EchoForm[e_]]                               := MakeEchoBoxes[e];

(*************************************************************************************************)

SystemBox[h_HoldAtForm] := coreHoldAtBox @ h;

SetHoldC[coreHoldAtBox];

coreHoldAtBox[HoldAtForm[a_, args___]] := FnBox[MakeBox @ a, CommaRowBox @ MapMakeBox @ {args}];
coreHoldAtBox[HoldAtForm[a_, arg_]]    := RBox[MakeBox @ a, "@", MakeBox @ arg];
coreHoldAtBox[e_]                      := $Failed;

(*************************************************************************************************)

$lenStack = {16, 8, 6, 4, 2};
$maxBytes = 2048;
$maxDep = 4;
$maxStrLen = 48;
$printPrecision = 4;
$elidePacked = True;
$multiline = True;
$formatType = StandardForm;
$isStd = True;
$isEcho = False;
$specialChars = True;

SetHoldSeq @ SetHoldC @ SetPred1[LongExprQ, BigExprQ, LongBigExprQ];

$dep = 1;
$len = 16;

LongExprQ[e_]           := HLen[e] > $len;
BigExprQ[e_]            := HByteCount[e] > $maxBytes;
BigExprQ[e_ ? HDictQ]   := HByteCount[e] > $maxBytes * 2;
BigExprQ[e_ ? HPackedQ] := (HByteCount[e] * 3) < $maxBytes;
LongBigExprQ[e_]        := LongExprQ[e] || BigExprQ[e];

(*************************************************************************************************)

SetHoldC[blockUnlimited, blockLimitDepth, blockLimitLen, blockLimited];

blockUnlimited[body_, d_:1] := Block[
  {$len = Inf, $lenStack = Join[ConstList[Inf, d], $lenStack], $maxStrLen = 2000,
   $maxDep = Max[$maxDep, $dep + d], $elidePacked = False},
  body
];

blockLimitLen[body_, n_Int] := Block[
  {$len = n, $lenStack = {n}},
  body
];

blockLimitLen[body_, n_Int, d_Int] := Block[
  {$len = n, $lenStack = {n}, $maxDep = d},
  body
];

blockLimitDepth[body_, d_Int] := Block[
  {$maxDep = $dep + d},
  body
];

blockLimited[body_, n_, d_Int] := Block[
  {$len = First[n, n], $lenStack = Join[ToList @ n, $lenStack], $maxStrLen = 32,
  $maxDep = If[d <= 0, $maxDep - d, $dep + d], $elidePacked = False},
  body
];

(*************************************************************************************************)

SetHoldC[blockDepth, decDepth];

$noLenQ := $len <= 0;
$deepQ := $dep > $maxDep;

blockDepth[e_, _] /; $noLenQ := elidedBox @ e;
blockDepth[e_, _] /; $deepQ := elidedBox @ e;
blockDepth[_, body_] := decDepth[body];

decDepth[body_] := Block[
  {$dep = $dep + 1,
   $len = PartOr[$lenStack, $dep, Last @ $lenStack]},
  body
];

(*************************************************************************************************)

$exprFormOptions = {
  MaxLength      -> $lenStack,
  MaxBytes       -> $maxBytes,
  MaxDepth       -> $maxDep,
  MaxStrLen      -> $maxStrLen,
  ElidePacked    -> $elidePacked,
  PrintPrecision -> $printPrecision,
  FormatType     -> $formatType,
  Multiline      -> $multiline,
  SpecialChars   -> $specialChars
};

Options[ExprForm] = $exprFormOptions;
Options[MakeExprBoxes] = $exprFormOptions;

Options[CodeForm] = $exprFormOptions;

(*************************************************************************************************)

ToExprBoxes[expr_, opts___Rule] := MakeExprBoxes[expr, opts];

(*************************************************************************************************)

SetHoldC[MakeExprBoxes, MakeCodeBoxes, MakeEchoBoxes];

MakeCodeBoxes[expr_, d_:Inf, l_:Inf] := Locals[
  $isStd = False;
  $maxDep = d;
  $len = $maxStrLen = l * 4; $lenStack = {l};
  $specialChars = False;
  exprBox @ expr
];

MakeEchoBoxes[expr_] := Locals[
  $isStd = False;
  $maxDep = 3;
  $len = $maxStrLen = 48; $lenStack = {16};
  $specialChars = False;
  $isEcho = True;
  CodeStyleBox @ exprBox @ expr
];

MakeExprBoxes[expr_]     := exprBox @ expr;
MakeExprBoxes[expr_, {}] := exprBox @ expr;
MakeExprBoxes[expr_, opts__Rule | {opts___Rule}] := ILocals[
  BindSymbols[{opts},
    $lenStack -> MaxLength, $maxDep -> MaxDepth,
    $maxBytes, $maxStrLen, $elidePacked, $printPrecision,
    $multiline, $formatType, $specialChars
  ];
  $isStd = $formatType === StandardForm;
  $lenStack //= ToList;
  $len = First @ $lenStack;
  exprBox @ expr
];

(*************************************************************************************************)

SetHoldC[exprBox, stdInpBox];

exprBox = CaseOf[
  s_Seq          := seqBox @ s;
  s_PrivSeq      := seqBox @ s;
  l:PackedP      := If[$elidePacked, packedBox @ l, listBox @ l];
  l_List         := listBox @ l;
  d:DictP        := dictBox @ d;
  s:StrP         := strBox @ s;
  s:SymP         := symBox @ s;
  d:DatumP       := datumBox @ d;
  s:ASetP        := setBox @ s;
  r:RuleLP       := ruleBox @ r;
  p:PatternP     := patternBox @ p;
  h_PrivHold     := holdBox @ h;
  h_PrivHoldSeq  := holdBox @ h;
  l_LitStr       := literalBox @ l;
  l_LitStrRow    := literalBox @ l;
  h_HoldAtForm   := holdAtBox @ h;
  s_Slot         := slotBox @ s;
  e_             := stdInpBox @ e;
];

(*************************************************************************************************)

SetHoldC[symBox, stdSymBox, rawSymBox];

symBox = CaseOf[
  Int   := "Int";
  Real  := "Real";
  Sym   := "Sym";
  List  := "List";
  Dict  := "Dict";
  Str   := "Str";
  Rule  := "Rule";
  RuleD := "RuleD";
  s_    := (stdSymBox[s] = stdSymBox[s]) /; $isStd;
  s_    := (stdSymBox[s] = stdSymBox[s]) /; $isDebug;
  s_    := (rawSymBox[s] = rawSymBox[s]);
  s_    := AliasSymName @ s;
];

rawSymBox[s_] := AliasSymName @ s;

stdSymBox[r:Alt[Primes, Integers, Rationals, Reals, Algebraics, Complexes, Booleans,
  PositiveReals, NonNegativeReals, NonPositiveReals, NegativeReals,
  PositiveRationals, NonNegativeRationals, NonPositiveRationals, NegativeRationals,
  PositiveIntegers, NonNegativeIntegers, NonPositiveIntegers, NegativeIntegers
]] := MakeBoxes @ r;

stdSymBox[s_] /; Context[s] === "System`" := AliasSymName @ s;
stdSymBox[s_] := niceSymBox[SymPath @ s, AliasSymName @ s, None];

niceSymBox[path_, name_, type_] := Locals[
  Make[InterpBox, If[StrHasQ[path, "`Private`"], TooltipBox[StyleBox[name, $Orange], path], name], path]
];

(* niceSymBox[path_, name_, type_] := Locals[
  style = Seq @@ FlatList @ SymbolTypeStyle @ type;
  Make[InterpBox, StyleBox[name, style], path]
];
 *)
(*************************************************************************************************)

SetHoldC[patternBox];

patternBox = CaseOf[
  VBlank[]                   := "_";
  VBlankSeq[]                := "__";
  VBlankNullSeq[]            := "___";
  p_Pattern                  := patSymBox @ p;
  VBlank[p:SymP]             := StrJoin["_", symBox @ p];
  VBlankSeq[p:SymP]          := StrJoin["__", symBox @ p];
  VBlankNullSeq[p:SymP]      := StrJoin["___", symBox @ p];
  VPatternTest[p_, t:SymP]   := RBox[exprBox @ p,  "?", symBox @ t];
  VPatternTest[p_, t_]       := RBox[exprBox @ p,  "?", ParenBox @ exprBox @ t];
  VCondition[p_, c_]         := RBox[exprBox @ p, "/;", parenify @ exprBox @ c];
  a_Alt                      := altBox @ a;
  expr_                      := genericBox @ expr;
];

SetHoldC[patSymBox, altBox, altArgBox];

patSymBox = CaseOf[
  VPattern[p:SymP, e_] := patColon[symBox @ p, exprBox @ e];
  expr_                := genericBox @ expr;
];

patColon[s_Str, t_Str] /; StrStartsQ[t, "_"] := StrJoin[s, t];
patColon[s_, t_]                             := RBox[s, ":", t];

altBox = CaseOf[
  VAlt[]       := FnBox["Alt"];
  VAlt[a_]     := FnBox["Alt", exprBox @ a];
  VAlt[args__] := RiffBox["|"] @ argBoxes[{args}, altArgBox, CDotsBox[#]&];
  other_       := genericBox @ other;
];

altArgBox = CaseOf[
  a_VAlt := ParenBox @ patternBox @ a;
  e_     := parenify @ exprBox @ e;
];

(*************************************************************************************************)

SetHoldC[slotBox];

slotBox = CaseOf[
  Slot[i:NatP] := "#" <> IntegerString[i];
  Slot[s_Str]  := "#" <> s;
  expr_        := genericBox @ expr;
];

(*************************************************************************************************)

SetHoldC[holdAtBox, holdAtHeadBox0, holdAtHeadBox1];

holdAtBox[HoldAtForm[f_, arg_]]    := RBox[holdAtHeadBox0 @ f, "@", exprBox @ arg];
holdAtBox[HoldAtForm[f_, args___]] := headBox2[holdAtHeadBox0 @ f, commaArgsBox @ {args}];
holdAtBox[e_]                      := genericBox @ e;

holdAtHeadBox0[f_] /; $isEcho := holdAtHeadBox1 @ f;
holdAtHeadBox0[f_]            := genericBox @ f;

holdAtHeadBox1 = CaseOf[
  Fn           := "Fn";
  e_Symbol     := symBox @ e;
  d:DatumP     := datumBox @ d;
  _Dict        := RBox[LAssoc, CDotsS, RAssoc];
  h_[]         := FnBox[$ @ h];
  h_[d:DatumP] := FnBox[$ @ h, exprBox @ d];
  h_[___]      := FnBox[$ @ h, CDotsS];
  _            := CDotsS;
];

(*************************************************************************************************)

stdInpBox[e_] := If[$isStd, stdBox @ e, inpBox @ e];

(*************************************************************************************************)

SetHoldC[datumBox, strBox, litBox, symBox];

strBox[""] := "\"\"";
strBox[s_] := Which[
  StrLen[s] > $maxStrLen,       If[StrLen[s] < 512, InterpBox[#, s]&, Id] @ HoldElidedBox @ s,
  !$specialChars && !ASCIIQ[s], MakeBoxes @ Style[s, ShowSpecialCharacters -> False],
  True,                         MakeBoxes @ s
];

datumBox = CaseOf[
  r_Real := RealString[r, $printPrecision];
  r_Rat  := inlineFracs @ MakeBoxes @ r;
  d_     := MakeBoxes @ d;
];

literalBox = CaseOf[
  LitStr[s:StrP]                    := LitStrBox @ s;
  LitStrRow[s:{StrP...}, f_Str:","] := LitStrRowBox[s, f];
  e_                                := genericBox @ e;
];

symBox[s_Sym] := MakeBoxes @ s;

(*************************************************************************************************)

SetHoldC @ seqBox;

seqBox = CaseOf[
  _[]          := RBox["Seq", "[", "]"];
  PrivSeq[e__] := $ @ Seq[e];
  s_Seq        := genericBox @ s;
];

(*************************************************************************************************)

SetHoldC[listBox]

listBox[list_List] := delimBox[list, LBrace, list, RBrace];

(*************************************************************************************************)

SetHoldC @ dictBox;

dictBox = CaseOf[
  dict_Dict ? UDictQ := genericBox @ dict;
  dict_Dict          := delimBox[dict, LAssoc, dict, RAssoc];
];

(*************************************************************************************************)

setBox = CaseOf[
  USet[_[]]                   := RBox["USet", "[", "]"];
  OSet[_[]]                   := RBox["OSet", "[", "]"];
  MSet[_[]]                   := RBox["MSet", "[", "]"];
  set:(h_Sym[_Dict ? HAtomQ]) := blockDepth[set, headBox2[symBox @ h, At[commaArgsBox, SetElems[set, PrivHold]]]];
  expr_                       := genericBox @ expr;
];

(*************************************************************************************************)

SetHoldC[packedBox]

packedBox[p_] := With[{n = PackedSize[p]}, Which[
  $noLenQ,        packedSummaryBox[p],
  n == 1,         RBox["{", datumBox @ First @ p, "}"],
  n <= 4,         listBox @ p,
  n <= $len < 8,  BraceRowBox[datumBox /@ p],
  True,           packedSummaryBox[p]
]];

packedSummaryBox[p_] :=
  BraceBox @ HPadBox[GrayBox @ packedSummary @ p, 0.2];

packedSummary[p_] := GrayBox[SubscriptBox[
  Replace[PackedType @ p, $typeToLetter],
  RiffBox["\[Times]"] @ Map[SysIntStr, Dims @ p]
]];

$typeToLetter = {Real -> "\[DoubleStruckCapitalR]", Int -> "\[DoubleStruckCapitalZ]", Complex -> "\[DoubleStruckCapitalC]"};

(*************************************************************************************************)

SetHoldC[ruleBox, simpleBoxes];

ruleBox = CaseOf[
  RuleD[k:DatumP|SymP, v_] := RBox[exprBox @ k, "\[RuleDelayed]", exprBox @ v];
  Rule[k:DatumP|SymP, v_]  := RBox[exprBox @ k, "\[Rule]", exprBox @ v];
  head_[k_ ? simpleQ, v_]  := ToBoxes @ head[RawBoxes @ simpleBoxes[k], RawBoxes @ exprBox @ v];
  expr_                    := stdInpBox @ expr;
];

simpleQ = CaseOf[
  {}                            := True;
  DatumP|SymP                   := True;
  {PatRep[DatumP|SymP, {1, 3}]} := True;
  _                             := False
];

simpleBoxes = CaseOf[
  {}                              := RBox[LBrace, RBrace];
  l:DatumP|SymP                   := datumBox @ l;
  l:{PatRep[DatumP|SymP, {1, 3}]} := BraceRowBox[datumBox /@ l];
  e_                              := elidedBox @ e;
];

(*************************************************************************************************)

SetHoldC[holdBox];

holdBox = CaseOf[
  PrivHold[e_]      := exprBox @ e;
  PrivHoldSeq[e_]   := exprBox @ e;
  PrivHold[]        := RBox["Seq", "[", "]"];
  PrivHold[es__]    := RBox["Seq", "[", RowBox @ Riffle[HoldMap[exprBox, {es}], ","], "]"];
  PrivHoldSeq[]     := "";
  PrivHoldSeq[es__] := RowBox @ Riffle[HoldMap[exprBox, {es}], ","];
  e_                := stdInpBox @ e;
];

(*************************************************************************************************)

SetHoldC[delimBox];

delimBox = CaseOf[
  $[_,  l_, _[], r_] := RBox[l, r];
  $[_,  l_, _,   r_] /; $noLenQ := RBox[l, CDotsS, r];
  $[e_, l_, a_,  r_] := blockDepth[e, delimBox2[l, commaArgsBox @ a, r]];
];

delimBox2[l_, boxes_, r_] := Which[
  !$multiline || singleLine2Q[boxes],
    RowBox @ ToList[l, boxes, r],
  requiresExtraNewlinesQ[boxes],
    RowBox @ ToList[l, "\n",  addTab["\t"] /@ addNewline /@ boxes, "\n", r],
  True,
    RowBox @ ToList[l, MapRest[addTab["\[NonBreakingSpace]"], addNewline /@ boxes], r]
];

(*************************************************************************************************)

SetHoldC @ genericBox;

genericBox = CaseOf[
  sym_Sym[]               := RBox[symBox @ sym, "[", "]"];
  _ /; $noLenQ            := CDotsS;
  e:(sym_Sym[args___])    := blockDepth[e, headBox[sym, {args}]];
  e:head_[args___]        := bigHeadBox[head, {args}];
  e_                      := elidedBox @ e; (* safety *)
];

(*************************************************************************************************)

SetHoldC[bigHeadBox];

bigHeadBox[head_, args_] := headBox2[parenify @ exprBox @ head, blockDepth[e, commaArgsBox @ args]];

parenify[b_] := If[parenifyQ @ b, ParenBox @ b, b];

parenifyQ = CaseOf[
  nowrap$                := False;
  RBox[_, "[", ___, "]"] := False;
  s_Str                  := !StrFreeQ[s, SpaceC];
  CDotsS                 := False;
  rec1$[b_, ___]         := $ @ b;
  _                      := True;
,
  {rec1$ -> Alt[StyleBox, TagBox, TooltipBox, AdjBox],
   nowrap$ -> Alt[_GridBox, _G2DBox, _SubBox, _SuperBox, _SubsuperBox]}
];

(*************************************************************************************************)

SetHoldC[headBox];

headBox = CaseOf[
  _ /; $noLenQ                        := CDotsS;
  $[head_Sym, expr_]                  := With[{name = symBox[head]}, headBox[name, expr]];
  $[head_Str | head_InterpBox, _[]]   := RBox[head, "[", "]"];
  $[head_Str | head_InterpBox, expr_] := headBox2[head, commaArgsBox @ expr];
];


headBox2[head_, boxes_] := If[singleLineQ @ boxes, headBox1Line, headBoxNLine][head, boxes];

headBox1Line[head_, {boxes_}]   := RowBox @ ToList[head, "[", boxes, "]"]
headBox1Line[head_, boxes_List] := RowBox @ ToList[head, "[", RowBox @ boxes, "]"]
headBox1Line[head_, str_Str]    := RowBox @ ToList[head, "[", str, "]"]
headBoxNLine[head_, boxes_]     := RowBox @ ToList[head, "[", "\n", addTab["\t"] /@ addNewline /@ boxes, "\n", "]"];

$needsLineP = Alt[
  RowBox[({"{", __, "}"} | {_Str, "[", __, "]"} | {LAssoc, __, RAssoc}) ? notSimpleArgsQ],
  TemplateBox[_, "RowDefault" | "RowWithSeparators"],
  GridBox[{BlankSeq2}, ___]
];

ClearAll @ singleLineQ;
SetHoldC @ singleLineQ;

singleLineQ[_] /; Or[!$multiline, $dep > 8] := True;

singleLineQ[{boxes_}] /; VFreeQ[NoEval @ boxes, "\n"] := True;

singleLineQ[boxes_] := And[
  LeafCount[NoEval @ boxes] < 128,
  VFreeQ[NoEval @ boxes, GraphicsBox],
  VFreeQ[NoEval @ boxes, {RowBox, GridBox}] || FreeQ[NoEval @ boxes, $needsLineP]
];


(* singleLineQ[boxes_] := Or[!$multiline, $dep > 8,
  And[singleLine2Q[boxes], VFreeQ[boxes, GraphicsBox]],
  And[SingleQ[boxes], VFreeQ[boxes, "\n"]]
];
 *)
singleLine2Q[boxes_] := And[LeafCount[boxes] < 128, Or[
  VFreeQ[boxes, {RowBox, GridBox}],
  FreeQ[boxes, $needsLineP]
]];

notSimpleArgsQ[args_] := !StrVecQ[args] || Len[args] > 12;

$needsExtraLineP = Alt[
  RowBox[{_String, "[", __, "]"} ? notSimpleArgsQ],
  GridBox[{BlankSeq2}, ___]
];

requiresExtraNewlinesQ[boxes_] := ContainsQ[boxes, $needsExtraLineP];
headBoxNLine[head_, boxes_]     := RowBox @ ToList[head, "[", "\n", addTab["\t"] /@ addNewline /@ boxes, "\n", "]"];

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

SetHoldC[argBoxes, commaArgsBox, rawExprBox, shortQ];

commaArgsBox[_] /; $noLenQ := {CDotsS};
commaArgsBox[list_] := Riffle[argBoxes[list, exprBox, CDotsBox[#]&], ","];

argBoxes = CaseOf[
  $[_, _, _] /; $noLenQ       := {};
  $[EmptyP, _, _]             := {};
  $[_[e1_], b_, _]            := {b @ e1};
  $[_[e1_, e2_], b_, d_]      := If[$len >= 2, {b @ e1, b @ e2}, addDots[2, d] @ {b @ e1}];
  $[e_List ? shortQ, b_, d_]  := HoldMap[b, e];
  $[e_List, b_, d_]           := addDots[HLen @ e, d] @ HTakeArgs[e, Max[$len-1,1], b];
  $[e_Dict ? shortQ, b_, d_]  := KeyValueMap[Fn[Null, b[Rule[#1, #2]], HoldAllComplete], NoEval @ e];
  $[e_Dict, b_, d_]           := addDots[HLen @ e, d] @ HTakeArgs[e, Max[$len-1,1], b];
];

shortQ[e_] := HLen[e] <= $len;

addDots[n_, dotsFn_][boxes_] := Append[boxes, dotsFn[n - Len[boxes], Last @ boxes]];

rawExprBox[e_] := RawBoxes @ exprBox @ e;

(*************************************************************************************************)

SetHoldC @ stdBox;

stdBox = CaseOf[
  RawBoxes[b_]     := With[{d = $maxDep - $dep}, DepthTruncateBoxes[b, d]];
  HoldForm[e_]     := exprBox @ e;
  SystemForm[s_]   := MakeBoxes @ s;
  InputForm[f_]    := Block[{$isStd = False, $maxDep = 32, $maxStrLen = 512, $len = 256, $lenStack = {128}, $elidePacked = False}, StyleBox[exprBox @ f, ShowStrChars -> True]];
  StandardForm[f_] := Block[{$isStd = True}, exprBox @ f];
  c_CodeForm       := MakeBoxes @ c;
  d_DataForm       := MakeBoxes @ d;
  f_FullForm       := MakeBoxes @ f;
  c_CodePane       := MakeBoxes @ c;
  f_MsgArgForm     := MakeBoxes @ f;
  o_OutExprForm    := MakeBoxes @ o;
  l:directive$     := directiveBoxes @ l;
  e_               := blockDepth[e, stdBox2 @ e];
,
  {directive$ -> Alt[_Style, _ExprForm, _Shallow, _HoldExprForm, _Unlimited, _Limited, _Unformatted]}
];

(*************************************************************************************************)

SetHoldC @ stdBox2;

stdBox2 = CaseOf[
  _ /; $noLenQ                        := CDotsS;
  m:(math$)[__]                       := mathBox @ m;
  DirInf[] | DirInf[1]                := "\[Infinity]";
  DirInf[-1]                          := RBox["\[Minus]", "\[Infinity]"];
  arb:Column[_List, ___]              := rowColBoxes[arb, False];
  arb:Row[_List, ___]                 := rowColBoxes[arb, True];
  arb:array$[_List, ___]              := arrayFormBoxes @ arb;
  arb:script$[__]                     := scriptBoxes @ arb;
  arb_Image ? ValidFlagQ              := imageBox @ arb;
  arb_Graph ? NoEntryFlagQ            := graphBox @ arb;
  arb_Graphics                        := graphicsBox @ arb;
  col:ColorP ? HoldColorQ             := SwatchBox @ col;
  m_Manipulate                        := MakeBoxes @ m;
  m_DynamicModule                     := MakeBoxes @ m;
  File[path:StrP]                     := PathBox @ path;
  arb:(_Sym ? OperatorFormHeadQ[___][___]) := operatorFormBox @ arb;
  arb:(_Sym ? CompoundFormHeadQ[___])      := compoundFormBox @ arb;
  arb:(_Sym ? AtomFormHeadQ[___])          := atomFormBox @ arb;
  arb:(_Sym ? HasCoreBoxQ[___])            := MaybeEval @ coreBox @ arb;
  arb:(_Sym ? HasCoreSubBoxQ[___][___])    := MaybeEval @ coreBox @ arb;
  arb_                                := stdBox3 @ arb;
,
  {array$ -> Alt[Grid, MatrixForm, TableForm],
   script$ -> Alt[Superscript, Subscript, Subsuperscript, Overscript, Underscript],
   math$ -> Join[MathSymP, ExtMathSymP]}
];

(*************************************************************************************************)

SetBoxFn @ SwatchBox;

SwatchBox[color:ColorP] := Locals[
  border = ColorConvert[Darker @ color, Head @ color];
  squares = List[
    {Black, RectangleBox[{0, 0}], RectangleBox[{1, -1}]},
    {color, RectangleBox[{0, -1}, {2, 1}]}
  ];
  TagBox[Make[GraphicsBox, squares, $colorBoxOpts], "ColorSwatch"]
];

$colorBoxOpts = Seq[
  ImageSize        -> {Automatic, 10},
  DefaultBaseStyle -> "ColorSwatchGraphics",
  BaselinePosition -> Scaled[0.1],
  ImageMargins     -> {{2,2},{1,1}},
  AspectRatio      -> 1,
  Frame            -> True,
  FrameStyle       -> border,
  FrameTicks       -> None,
  PMargin          -> None
]

(*************************************************************************************************)

SetHoldC @ coreBox;

coreBox[e_] /; TrueQ[$CoreFormatting] := IfFailed[MakeCBox @ e, FailEval];
coreBox[e_]                           := FailEval;

(*************************************************************************************************)

SetHoldC @ stdBox3;

stdBox3 = CaseOf[
  arb:(_Sym ? CoreBoxSubHeadQ[___][___]) := MaybeEval @ coreBox @ arb;
  arb:(sym_Sym[___] ? HNoEntryFlagQ) /; HasFormatRulesQ[sym] := MakeBoxes @ arb;
  HoldP[if:GeneralUtilities`InternalFailure[___]] := MakeBoxes @ if;
  arb:(_[___] ? HEntryFlagQ)             := genericBox @ arb;
  arb_ ? LongBigExprQ                    := elidedBox @ arb;
  arb_                                   := MakeBoxes @ arb;
];

(*************************************************************************************************)

SetHoldC @ scriptBoxes;

scriptBoxes = CaseOf[
  h_[a_, b_, o___Rule] :=
    Replace[h, formToBox][exprBox @ a, blockScript @ exprBox @ b, o];
  Subsuperscript[a_, b_, c_, o___] := SubsuperBox[
    exprBox @ a,
    blockScript @ exprBox @ b, blockScript @ exprBox @ c, o
  ];
  other_ := genericBox @ other;
];

formToBox = {Subscript -> SubBox, Superscript -> SuperBox, Overscript -> OverBox, Underscript -> UnderBox};

(*************************************************************************************************)

SetHoldC[mathBox, mathBox2]

mathBox[e_] := postProcMath @ Apply[
  MakeBoxes, At[HoldC, mathBox2 @ e] /. decapMath[s_] :> s
];

mathBox2 = CaseOf[
  (h:Plus|Times)[a_]       := mathLeaf @ FnBox[SymName @ h, exprBox @ a];
  Power[a_, b_]            := decapMath[Power][$ @ a, blockScript[$ @ b, mathLeaf @ CDotsS]];
  (h:math$)[args__]        := decapMath[h][Map[$, NoEval @ args]];
  n:NumP                   := n;
  e_                       := mathLeaf @ exprBox @ e;
,
  {math$ -> Join[MathSymP, ExtMathSymP]}
];

mathLeaf[e_] := RawBoxes @ $mathWrap$ @ e;

(*************************************************************************************************)

SetHoldF @ blockScript;

$scriptDepth = 0;
blockScript[body_] := blockScript[body, CDotsS];
blockScript[body_, dots_] /; $scriptDepth > 1 := dots;
blockScript[body_, _] := Block[
  {$scriptDepth = $scriptDepth + 1, $len = Min[3, $len], $lenStack, $multiline},
  $lenStack = {$len}; $multiline = False;
  body
];

spaceRifQ = CaseOf[
  {_, " ", r__} := $[{r}];
  {_}           := True;
  _             := False;
];

addTimes = CaseOf[
  " " := "\[Times]";
  b_  := postProcMath @ b;
];

postProcMath = CaseOf[
  RowBox[list_List ? spaceRifQ] := RowBox[addTimes /@ list];
  FractionBox[a_, b_]    := iFrac[$ @ a, $ @ b];
  (h:rec2)[a_, b_, o___] := h[$ @ a, $ @ b, o];
  (h:recN)[a___]         := h[$ @ a, $ @ b];
  RowBox[bs_List]        := RowBox[$ /@ bs];
  $mathWrap$[box_]       := box;
  str_Str                := str;
  box_                   := box /. $mathWrap$[e_] :> e (* shouldn't happen, slow *);
,
  {rec2 -> Alt[SubBox, SuperBox, FractionBox], recAll -> Alt[SqrtBox, RadicalBox]}
];

iFrac[a_Str, b_Str] /; (!$multiline || $dep > 2) :=
  StyleBox[RBox[a, "/", b], AutoSpacing -> False];

iFrac[a_, b_] := FractionBox[a, b];

inlineFracs[boxes_] := boxes;
inlineFracs[boxes_] /; (!$multiline || $dep > 2) && VContainsQ[boxes, FractionBox]  :=
  ReplaceAll[boxes, FractionBox[a_Str, b_Str] :> StyleBox[RBox[a, "/", b], AutoSpacing -> False]];

(*************************************************************************************************)

SetHoldC @ inpBox;

inpBox = CaseOf[
  _ /; $noLenQ           := CDotsS;
  m:(math$)[__]          := inpMathBox @ m;
  DirInf[] | DirInf[1]   := "Inf";
  DirInf[-1]             := RBox["\[Minus]", "Inf"];
  g_Graph ? NoEntryFlagQ := MakeBoxes @ InputForm @ g; (* TODO: why not HNoEntryFlag? *)
  e_                     := genericBox @ e;
,
  {math$ -> Join[MathSymP, ExtMathSymP]}
];

SetHoldC @ inpMathBox;

inpMathBox[e_] := With[{mb2 = mathBox2[e]},
  Apply[
    MakeBoxes, Make[HoldC, mb2] /. decapMath[s_] :> s
  ] // inpPostProcMath
];

inpPostProcMath = CaseOf[
  RowBox[list_List ? spaceRifQ] := RowBox[inpAddTimes /@ list];
  FractionBox[a_, b_]    := inpFrac[$ @ a, $ @ b];
  SuperBox[a_, b_]       := RBox[parenify @ $ @ a, "^", parenify @ $ @ b];
  SqrtBox[a_]            := FnBox["Sqrt", $ @ a];
  RadicalBox[a_, n_]     := $ @ SuperBox[a, RBox["1", "/", n]];
  RowBox[bs_List]        := RowBox[$ /@ bs];
  $mathWrap$[box_]       := box;
  str_Str                := str;
  box_                   := box /. $mathWrap$[e_] :> e (* shouldn't happen, slow *);
];

inpFrac[a_, b_] := StyleBox[RBox[parenify @ a, "/", parenify @ b], AutoSpacing -> False];

inpAddTimes = CaseOf[
  " " := "*";
  b_  := inpPostProcMath @ b;
];

(*************************************************************************************************)

SetHoldC @ directiveBoxes;

directiveBoxes = CaseOf[
  Style[e_, opts___]                 := StyleBox[exprBox @ e, opts];
  exprForm$[e_, opts___Rule]         := MakeExprBoxes[e, opts];
  Shallow[e_, d_Int]                 := blockLimitDepth[exprBox @ e, d];
  Unlimited[e_, d_Int:1]             := blockUnlimited[exprBox @ e, d];
  Limited[e_, n_Int, d_Int:0]        := blockLimited[exprBox @ e, n, d];
  Unformatted[e_]                    := BlockFormatting @ exprBox @ e;
  Uninteractive[e_]                  := BlockInteractive @ exprBox @ e;
  expr_                              := genericBox @ expr;
,
  {exprForm$ -> Alt[ExprForm, HoldExprForm]}
];

(*************************************************************************************************)

SetHoldC[imageBox, graphBox, graphicsBox];

imageBox[img_Image] /; !Image`ValidImageQ[NoEval @ img] := HoldElidedBox @ img;

$targetSize := 600 / ($dep/2)^2;

imageBox[img_Image] := Locals[
  size = Max @ ImageDimensions @ img;
  maxSize = $targetSize;
  ToBoxes @ If[size <= maxSize, img, Thumbnail[img, UpTo @ size]]
];

graphBox[g_Graph] := ToBoxes @ If[$dep === 1, g, Graph[g, ImageSize -> UpTo[Ceiling[0.5*$targetSize]]]];
graphBox[e_]      := BoxFnErrBox[graphBox, e];

graphicsBox[g_Graphics] := If[$dep === 1 || MemberQ[NoEval @ g, ImageSize -> _],
  MakeBoxes @ g,
  Append[
    DelCases[g, ImageSize -> _],
    ImageSize -> With[{t = Ceiling @ $targetSize}, UpTo[t]]
  ]
];

(*************************************************************************************************)

SetHoldC[rowColBoxes, arrayFormBoxes, gridRowBoxes];

rawInnerBoxes[HoldC[e_]] := exprBox @ e;

rowColBoxes[e_, _] := genericBox @ e;
rowColBoxes[head_[list_List, opts___], isRow_] := With[
  {boxes = Map[RawBoxes] @ ListDictMakeBoxes1D[list, isRow, rawInnerBoxes, False, None, 1000, $len]},
  If[EmptyQ[boxes] || AllTrue[boxes, EmptyQ], CDotsS, ToBoxes @ head[boxes, opts]]
];

arrayFormBoxes[e_] := genericBox @ e;
arrayFormBoxes[head_[list_List, opts___]] := With[
  {boxes = argBoxes[list, gridRowBoxes, dotsRow[If[head === Grid, LSpanS, ""]]]},
  If[EmptyQ[boxes] || AllTrue[boxes, EmptyQ], CDotsS, MakeBoxes @ head[boxes, opts]]
];

gridRowBoxes[row_List] := decDepth @ argBoxes[row, rawExprBox, RawBoxes[CDotsBox[#1]]&];
gridRowBoxes[other_] := decDepth @ exprBox @ other;

dotsRow[pad_][n_, last_] := PadRight[
  List @ RawBoxes @ ItemBox[
    CDotsBox[n, False],
    Alignment -> Center
  ],
  Len @ last, RawBoxes @ LSpanS
];

(*************************************************************************************************)

SetHoldC @ atomFormBox;

atomFormBox[e_] := MakeBoxes @ e;

(*************************************************************************************************)

SetHoldC[compoundFormBox, compoundForm1Box, compoundForm2Box, compoundForm3Box, compoundFormNBox];

compoundFormBox[e:(sym_[___])] := Switch[
          CompoundFormArity @ sym,
  1,      compoundForm1Box[e],
  2,      compoundForm2Box[e],
  3,      compoundForm3Box[e],
  All,    compoundFormNBox[e],
  Row,    rowColBoxes[e, True],
  Column, rowColBoxes[e, False],
  Grid,   arrayFormBoxes[e]
];

(*************************************************************************************************)

compoundForm1Box[sym_[a_, r___]]         := finalBoxes[{rawExprBox @ a}, sym, r];
compoundForm2Box[sym_[a_, b_, r___]]     := finalBoxes[{rawExprBox @ a, rawExprBox @ b}, sym, r];
compoundForm3Box[sym_[a_, b_, c_, r___]] := finalBoxes[{rawExprBox @ a, rawExprBox @ b, rawExprBox @ c}, sym, r];
compoundFormNBox[sym_[r___]]             := finalBoxes[HoldMap[rawExprBox, {r}], sym];

SetHoldR @ SetStrict @ finalBoxes;

finalBoxes[{left___}, sym_, right___] := MakeBoxes @ sym[left, right];

(*************************************************************************************************)

SetHoldC[operatorFormBox]

operatorFormBox[e_] := compoundForm1Box[e];

(*************************************************************************************************)

SetHoldC @ elidedBox;

elidedBox[d:Unlimited[_, ___Int]] /; $isStd := directiveBoxes @ d;
elidedBox[e_ ? HNoEntryFlagQ]               := HoldElidedBox @ e;
elidedBox[(s_Sym ? CompoundFormHeadQ)[___]] := CDotsS;
elidedBox[e_]                               := HoldElidedBox @ e;
_elidedBox := CDotsS;

(*************************************************************************************************)

SetHoldC @ HoldRichElidedBox;

HoldRichElidedBox = CaseOf[
  e:PackedP := packedSummaryBox @ e;
  e_List    := richListBox @ e;
  e:DictP   := richDictBox @ e;
  e_        := HoldElidedBox @ e;
];

SetHoldC[richListBox, richDictBox];

(* richListBox = CaseOf[


];



 *)