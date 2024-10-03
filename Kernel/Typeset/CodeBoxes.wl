PackageExports[
  "BoxFunction",     GuessInputStrLen, MakeTruncatedBoxes
];

(*************************************************************************************************)

MakeTruncatedBoxes::usage =
"MakeTruncatedBoxes[expr, limit] is like MakeBoxes but attempts to introduces truncation when the number of approximate characters exceeds expr.
MakeTruncatedBoxes[..., slimit] truncates strings that are longer than slimit.
MakeTruncatedBoxes[..., ..., dlimit] truncates elements deeper than dlimit.
* symbols will not be printed with their full contexts.
* symbols outside system will be tinted red if they have definitions and orange if not.
"

(* todo: make the character limit apply to both width and height, and make it image size based instead! *)
MakeTruncatedBoxes[e_, charLimit_, strTrunc_:Inf, maxDepth_:5] := Block[
  {$cmMax = charLimit, $cmLeft = charLimit, $cmStrMax = strTrunc, $cmDepth = maxDepth},
  trimNothing @ cmAny @ e
];

trimNothing[Nothing] = RowBox[{}];
trimNothing[e_] := e;

(*************************************************************************************************)

SetHoldC[cmAny, cmFinal, holdBigQ];

cmAny[None]                := chowing[4, "None"];
cmAny[True]                := chowing[4, "True"];
cmAny[False]               := chowing[5, "False"];
cmAny[Infinity]            := chowing[1, "\[Infinity]"];
cmAny[DirectedInfinity[1]] := chowing[1, "\[Infinity]"];

cmAny[e_Sym ? HAtomQ]      := cmSymbol @ e;
cmAny[e_Int ? HAtomQ]      := chowStr @ cmInt @ e;
cmAny[e_Real ? HAtomQ]     := cmReal @ e;
cmAny[e_Str ? HAtomQ]      := cmString @ e;

cmAny[e_Rule]              := cmRule[e];
cmAny[e_RuleD]             := cmRule[e];
cmAny[e_Pattern]           := cmPattern[e];
cmAny[e_Alt]               := cmAlternatives[e];

cmAny[e_Set]               := cmSet[e];
cmAny[e_SetD]              := cmSet[e];
cmAny[e_Then]              := cmCE[e];

cmAny[Verbatim[_]]         := chowing[1, "_"];
cmAny[Verbatim[__]]        := chowing[2, "__"];
cmAny[Verbatim[___]]       := chowing[3, "___"];

cmAny[s:((USet|OSet|MSet)[_] ? SealedQ)] := cmSetObj[s];
cmAny[EmptyUDict]          := chowing[7, "UDict[]"];
cmAny[EmptyODict]          := chowing[7, "UDict[]"];
cmAny[e_List]              := chowing[2, decStrLen @ rbox["{",  cmListSeq @ e, "}"]];
cmAny[e_Dict ? HUDictQ]    := chowing[7, decStrLen @ rbox["UDict[", cmAssocSeq @ e, "]"]];
cmAny[e_Dict ? HODictQ]    := chowing[2, decStrLen @ rbox[LAssoc, cmAssocSeq @ e, RAssoc]]; (* TODO: doesn't limit keys *)

cmAny[e_]                  := cmFinal[e];

(*************************************************************************************************)

cmInt[0]             := "0";
cmInt[1]             := "1";
cmInt[i_ ? Negative] := "-" <> IntegerString[i];
cmInt[i_]            := IntegerString[i];

(*************************************************************************************************)

(* NOTE: the FE actually does scientific formatting itself for numbers with a final `. it's not customizable somehow. *)
(* use scientific form after 1000, which somehow you can't turn on in the FE by default *)
cmReal[0.] := chowStr @ "0.";
cmReal[r_] := If[0 < Abs[r] < 1000, chowing[5, Internal`MRealToString[r] <> "`"], chowing[10, scientificBoxes @ r]];

(* this fixes a default pathology with the FE in which it doesn't use scientific form sometimes like for 999999.1` but it does for 111111.1`. *)
scientificBoxes[r_] := ReplaceAll[
  ToBoxes[ScientificForm[r, DefaultPrintPrecision -> 3, ExponentStep -> 3]],
  RowBox[{s_String ? (StringEndsQ[".\""]), a1_, a2_}] :> RuleCondition @ RowBox[{StringDelete[s, "."], a1, a2}]
];

(*************************************************************************************************)

SetHoldC[cmSetObj];

cmSetObj[set_] := Make[cmSymHead, HoldHead @ set, SetElems[NoEval @ set, PrivHold]];

(*************************************************************************************************)

SetHoldC[cmSymbol];

cmSymbol[s_] := cmSymbol2[s, HoldSymbolNameAlias @ s, Context @ s];

SetHoldF[cmSymbol2];

cmSymbol2[_, name_, "System`" | "Global`" | "GeneralUtilities`"] := chowStr @ name;

cmSymbol2[sym_, name_, context_] :=
  cmSymbolTB[context <> name, chowStr @ name, If[System`Private`HasAnyEvaluationsQ[sym], $ladenSymbolColor, $freeSymbolColor]];

$ladenSymbolColor = RGBColor[0.71, 0.02, 0.];
$freeSymbolColor = RGBColor[0.68, 0.35, 0.];

cmSymbolTB[fullSymName_, symName_, color_] := TemplateBox[
  {fullSymName, symName, color}, "ColoredSymbol",
  DisplayFunction :> Function[DynamicBox @ FEPrivate`If[FrontEnd`CurrentValue["CellStyleName"] === "Input", #1, StyleBox[#2, FontColor -> #3]]],
  CopyFunction    :> Automatic,
  InterpretationFunction :> (Function[#1]),
  Selectable -> False
];

mergeSymTemplates[RowBox[{TemplateBox[{fn_, n_, c_}, a___], s__String}]] :=
  TemplateBox[{fn <> s, n <> s, c}, a];

mergeSymTemplates[RowBox[{s__String, TemplateBox[{fn_, n_, c_}, a___]}]] :=
  TemplateBox[{s <> fn, s <> n, c}, a];

mergeSymTemplates[RowBox[{TemplateBox[{fn1_, n1_, c1_}, a1___], s_String, TemplateBox[{fn2_, n2_, c2_}, a2___]}]] :=
  TemplateBox[{fn1 <> s <> fn2, n1 <> s <> n2, c1}, a];

mergeSymTemplates[other_] := other;

(*************************************************************************************************)

SetHoldR[holdMap];

holdMap[f_, a_] := Map[f, NoEval @ a];
holdMap[f_, a_, n_] := First @ Map[f, Take[Hold @ a, All, n], {2}];

(*************************************************************************************************)

SetHoldR[cmPattern, cmPatternRHS];

cmPattern[VPattern[a:Sym, b_]] :=
  mergeSymTemplates @ RowBox @ Prepend[cmSymbol @ a] @ cmPatternRHS[b];

cmPattern[p_] := cmFinal @ p;

(* TODO: chow *)
cmPatternRHS[Verbatim[_]]     := {"_"};
cmPatternRHS[VBlank[b:SymP]]  := {"_", cmSymbol @ b};
cmPatternRHS[Verbatim[__]]    := {"__"};
cmPatternRHS[Verbatim[___]]   := {"___"};
cmPatternRHS[a_Alt]           := {":", cmAlternatives @ a};
cmPatternRHS[other_]          := {":", parenBox @ cmAny @ other, ")"};

(*************************************************************************************************)

SetHoldC[cmAlternatives, cmAlternativesEntry];

cmAlternatives[VAlt[a_, b__]] := riffMapList[cmAlternativesEntry, "|", {a, b}];
cmAlternatives[e_]            := cmFinal @ e;

cmAlternativesEntry[e:(_Alternatives | _Rule | _RuleDelayed)] := parenBox @ cmAny @ e;
cmAlternativesEntry[e_] := cmAny @ e;

(*************************************************************************************************)

SetHoldC[cmSet, cmSetEntry];

cmSet[Set[a_, b_]]  := rbox[cmAny @ a, "=", cmSetEntry @ b];
cmSet[SetD[a_, b_]] := rbox[cmAny @ a, ":=", cmSetEntry @ b];
cmSet[e_]           := cmFinal @ e;

cmSetEntry[b:(_SetD | _Then)] := parenBox @ cmAny @ b;
cmSetEntry[b_] := cmAny @ b;

(*************************************************************************************************)

SetHoldC[cmCE, cmCEEntry, riffMapList];

cmCE[Then[a__]]       := riffMapList[cmCEEntry, ";", {a}];
cmCE[Then[a__, Null]] := RowBox @ Append[";"] @ First @ cmCE[Then[a]];
cmCE[e_] := cmFinal @ e;

cmCEEntry[e_Then] := parenBox @ cmAny @ e;
cmCEEntry[e_] := cmAny @ e;

riffMapList[fn_, str_, a_] := RowBox @ Riffle[holdMap[fn, a], str];

(*************************************************************************************************)

SetHoldC[cmFinal, cmFinal2, holdBigQ, recHeadQ, recHeadQ2, cmSymHead];

DefinePatternRules[
  boxRecurse1P -> _Pane | _Tooltip | _CodePane | _CodeTooltip | _Annotation,
  boxTypesetP  -> HoldP[_Image] | _Graph | _Grid | _NiceGrid | _Column | _NiceMulticolumn | _PlainGrid
];

(* TODO: assume thing is square, and so typeset width and height will generate that (width * height) / font size chars *)

cmFinal[PrivSeq[a___]]           := cmFinal[PrivHoldSeq[a]];
cmFinal[PrivHoldSeq[]]           := "";
cmFinal[PrivHoldSeq[a_]]         := cmAny[a]; (* TODO: decstringlen *)
cmFinal[PrivHoldSeq[a__]]        := cmListSeq[{a}]; (* TODO: decstringlen *)
cmFinal[PrivHold[a_]]            := cmAny[a];
cmFinal[PrivHold[a___]]          := cmSymHead[Sequence, {a}];
cmFinal[(h:SymP)[PrivSeq[a___]]]     := cmAny[h[a]];
cmFinal[(h:SymP)[PrivHoldSeq[a___]]] := cmAny[h[a]];
cmFinal[a:boxRecurse1P]          := ToBoxes @ MapAt[cmAny /* RawBoxes, NoEval @ a, 1];
cmFinal[Style[a_, rest___]]      := StyleBox[cmAny @ a, rest];
cmFinal[Row[a_List]]             := RowBox @ Riffle[holdMap[cmAny, a], ""];
cmFinal[Row[a_List, b_]]         := Construct[cmFinal, Row @ Riffle[holdMap[PrivHold, a], PrivHold @ b]];
cmFinal[a:boxTypesetP]           := "\[EmptySquare]";
cmFinal[RawBoxes[b_]]            := b;
cmFinal[a_]                      := cmFinal2[a];

cmFinal2[(s_Symbol ? recHeadQ)[a___]] := cmSymHead[s, List @ a];
cmFinal2[_ExternalSessionObject]      := "ExternalSessionObject[\[Ellipsis]]";
cmFinal2[a_]                          := chowing[HByteCount[a] / 4, cleanupManualBoxes @ MakeBoxes[a, StandardForm]];

cmSymHead[s_Sym, a_List] := chowing[2, decStrLen @ rbox[cmSymbol @ s, "[", cmListSeq @ a, "]"]];

cleanupManualBoxes[e_] := e //. {
  FractionBox[a_, b_] :> RowBox[{a, "/", b}],
  RowBox[{TagBox[head_String, "SummaryHead"], ___}] :> RuleEval[head <> "[\[Ellipsis]]"],
  InterpretationBox[b_, ___] :> b
};

(* TODO: find way of disabling fraction box *)

recHeadQ2[s_] := Hold[s] @ recHeadQ @ s;
recHeadQ[Hold | HoldComplete | Splice | Sequence] := True;
recHeadQ[_ ? HasPrintCodeQ] := False;
recHeadQ[_ ? HasUpDefsQ] := MemberQ[$whitelistEntry, SymName @ s];
recHeadQ[SymP] := True;

$whitelistEntry = {"Scope", "ModuleScope", "Case"};

$bigSize = 2000;
holdBigQ[e_]      := HByteCount[e] > $bigSize;

(*************************************************************************************************)

SetHoldR[chowing, chowing2];

chowing[i_, e_]   := If[($cmLeft -= i) >= 0, e, $cmEll];
chowing[i_, e_]   := If[($cmLeft -= i) >= 0, e, $cmEll];

chowing2[i_, e_, f_] := If[($cmLeft -= i) >= 0, e, f];

SetHoldC[cmAnyComma];
cmAnyComma[e_] /; $cmLeft <= 0 := If[$trig, $trig = False; $cmEll, Nothing];
cmAnyComma[e_] := Splice[{cmAny[e], $cmLeft -= 2; ","}];

commaRowBox[a:{___, ","}]            := commaRowBox @ Most @ a;
commaRowBox[a:{___, $cmEll, $cmEll}] := commaRowBox @ Most @ a;
commaRowBox[a_List]                  := RowBox[a];

safeMost[{}] := {};
safeMost[list_] := Most @ list;

chowStr[e_Str] := chowing[StrLen @ e, e];

leftFor[n_]       := Floor[$cmLeft / n];

(*************************************************************************************************)

rbox[b___]        := RowBox[{b}];

commasBox[b_]     := RowBox @ Riffle[b, ","];

parenBox[b_]      := chowing[2, rbox["(", b, ")"]];

(*************************************************************************************************)

SetHoldF[decStrLen];

decStrLen[body_] := Block[{$cmStrMax = Max[Floor[$cmStrMax * .75], 4]}, body];

cmString[s_] /; StrLen[s] > $cmStrMax := cmString[StringTake[s, $cmStrMax-1] <> $cmEll];

cmString[s_] := ToBoxes @ With[{left = $cmLeft}, chowing2[StrLen[s] + 2, s, StringTake[s, UpTo @ Max[left-3, 0]] <> $cmEll]];

$cmEll = "\[Ellipsis]";

(*************************************************************************************************)

SetHoldC[cmRule, ruleBox];

cmRule[head_[a:(_Rule | _RuleDelayed), b_]] := ruleBox[head, parenBox @ cmRule @ a, cmAny @ b];
cmRule[head_[a_, b_]]                       := ruleBox[head, cmAny @ a, cmAny @ b];
cmRule[e_]                                  := cmFinal @ e;

ruleBox[Rule,        a_, b_] := chowing[4, rbox[a, "\[Rule]", b]];
ruleBox[RuleDelayed, a_, b_] := chowing[4, rbox[a, "\[RuleDelayed]", b]];

(*************************************************************************************************)

SetHoldC[cmListSeq, cmListSeqEntries];

cmListSeq[{}]       := Nothing;
cmListSeq[{a_}]     := cmAny @ a;
cmListSeq[{a_, b_}] := chowing[2, rbox[cmAny @ a, ",", cmAny @ b]];
cmListSeq[l_List]   := With[
  {n = Length @ NoEval @ l},
  $trig = True; commaRowBox @ cmListSeqEntries[l, n, leftFor[3]]
];

cmListSeqEntries[l_, n_, m_] /; n <= m :=
  holdMap[cmAnyComma, l];

cmListSeqEntries[l_, n_, m_] :=
  Append[$cmEll] @ holdMap[cmAnyComma, l, m];

(*************************************************************************************************)

SetHoldC[cmAssocSeq, cmAssocEntries, cmAssocEntry, cmKVRule];

cmAssocSeq[a_] := With[
  {n = Length @ NoEval @ a},
  Which[
    n == 0, Nothing,
    n == 1, First @ KeyValueMap[cmKVRule, NoEval @ a],
    True,   $trig = True; commaRowBox @ cmAssocEntries[a, n, leftFor[7]]
  ]
];

cmAssocEntries[a_, n_, m_] /; n < m :=
  KeyValueMap[cmKVRule, NoEval @ a];

cmAssocEntries[a_, n_, m_] := Block[
  {$i = 1, $m = m, $cmbag = Internal`Bag[]},
  Association`ScanWhile[NoEval @ a, cmAssocEntry];
  Internal`StuffBag[$cmbag, $cmEll];
  Internal`BagPart[$cmbag, All]
];

cmKVRule[k_, v_] := cmRule[k -> v];

cmAssocEntry[r_] := (Internal`StuffBag[$cmbag, cmAnyComma @ r]; ++$i <= $m);

(*************************************************************************************************)

Options[GuessInputStrLen] = {
  PrintPrecision -> Infinity
};

GuessInputStrLen[e_, opts___] := chars0[e, opts];
GuessInputStrLen[CodePane[e_, ___], opts___] := chars0[e, opts];

GuessInputStrLen::invalidOptions = "Non-rules provided as options to GuessInputStrLen.";

$pp = Infinity;
chars0[e_, opts___Rule] := Block[
  {$pp = Lookup[{opts}, PrintPrecision, Infinity]},
  chars[e]
];

chars0[e_, ___] := chars0[Message[GuessInputStrLen::invalidOptions]; e];

(*************************************************************************************************)

SetHoldC[chars, ruleChars, runChars, commaChars, chars2, manualChars];

chars[0]                    := 1;
chars[True]                 := 4;
chars[None]                 := 4;
chars[False]                := 5;
chars[a_Str ? HAtomQ]       := StrLen[a] + 2;
chars[a_Sym ? HAtomQ]       := StrLen @ SymName @ a;
chars[a_Int ? HAtomQ]       := intChars @ a;
chars[a_Real ? HAtomQ]      := realChars @ a;
chars[{}]                   := 2;
chars[a_List]               := 2 + runChars[a];
chars[(Rule|RuleD)[a_, b_]] := ruleChars[a, b];
chars[a_Dict ? HAtomQ]      := 4 + Total[KeyValueMap[ruleChars, NoEval @ a]];
chars[a_ ? SealedQ]         := Inf;
chars[VCondition[a_, b_]]   := chars[a] + chars[b] + 4;
chars[VPattern[a_, b_]]     := chars[a] + chars[b] + 3;
chars[Verbatim[_]]          := 1;
chars[Verbatim[__]]         := 2;
chars[Verbatim[___]]        := 3;
chars[RawBoxes[b_]]         := 4; (* i'm using this in NiceEcho, don't want to drop these guys *)
chars[a_]                   := If[holdBigQ[a], Inf, chars2 @ a];
chars[MathTools`MsgPath[p_String ? HAtomQ]] := Min[StrLen[p], 40] + 2;


(*************************************************************************************************)

chars2[a:boxRecurse1P]        := MapAt[chars, NoEval @ a, 1];
chars2[boxTypesetP ? HAtomQ]  := Inf;
chars2[a_]                    := manualChars @ a;

manualChars[a_]               := StrLen[HToInputStr @ a];

(*************************************************************************************************)

(* the extra 5 is for * 10^n *)
realChars[r_ ? MachineRealQ] := Min[StrLen @ ToInputStr @ r, If[Abs[r] >= 1000, 5, 0] + 1 + $pp];
realChars[r_]                :=     StrLen @ ToInputStr @ r;

(*************************************************************************************************)

intChars[i_] := Which[
    -9 < i < 9,   1,
   -99 < i < 99,  2,
  -999 < i < 999, 3,
  True,           If[Negative[i], 1, 0] + Ceiling[Log10[Abs[i] + 1/10]]
];

ruleChars[a_, b_] := chars[a] + chars[b] + 4;

runChars[{}]           := 0;
runChars[{a_}]         := chars[a];
runChars[{a_, b_}]     := 2 + chars[a] + chars[b];
runChars[{a_, b_, c_}] := 4 + chars[a] + chars[b] + chars[c];
runChars[a_List]       := Plus[Total @ HoldMap[chars, a], commaChars[a]];

commaChars[a_List]     := 2 * (HLen[a] - 1);
