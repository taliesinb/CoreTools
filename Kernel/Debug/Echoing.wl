SystemExports[
  "DebuggingFunction",
    NiceEcho,
    Echo1, Echo2, Echo3, Echo4, Echo5,
    EchoBy, EchoLen, EchoDims, EchoOpts, EchoKeys, EchoHead,
    EchoTest,
    EchoReplaceAll, EchoReplaceRepeated,
    EchoingRules, EchoingSetDelayed,
    EchoH, EchoH0, EchoH1,
    EchoArrow,
    EchoF, EchoFL, EchoFH, EchoFLH, AttachEchos,
  "SpecialVariable",
    $EchoIndent, $EchoLHS,
  "FormHead",
    NicePaster
];

PackageExports[
  "DebuggingFunction",
    EchoSet, EchoSetH, EchoSymbol, EchoBody,
  "MessageFunction",
    EchoBindingsBody, EchoBindingsCondition,
  "BoxFunction",
    NicePasterBoxes
];

(*************************************************************************************************)

Protect[$EchoLHS];

NiceEcho[arg_]    := (printEchoPane @ CoreToolsHold @ arg; arg);
NiceEcho[args___] := (printEchoPane @ CoreToolsSequence @ args; Construct[Sequence, args]);

(*************************************************************************************************)

Echo1[arg_] := Then[echoLabel["1:", arg], arg];
Echo2[arg_] := Then[echoLabel["2:", arg], arg];
Echo3[arg_] := Then[echoLabel["3:", arg], arg];
Echo4[arg_] := Then[echoLabel["4:", arg], arg];
Echo5[arg_] := Then[echoLabel["5:", arg], arg];

SetHoldC @ echoLabel;

echoLabel[lab_]         := labeledEchoPane[lab];
echoLabel[lab_, arg_]   := labeledEchoPane[lab, CoreToolsHold @ arg];
echoLabel[lab_, args__] := labeledEchoPane[lab, CoreToolsSequence @ args];

(*************************************************************************************************)

EchoBy[fn_, parts__][arg_] := With[{res = arg}, printEchoPane @ Apply[fn] @ FastQuietCheck[Part[Hold @ arg, All, parts], Missing]; res];
EchoBy[fn_][arg_]          := With[{res = arg}, printEchoPane @ fn @ res; res];
EchoBy[fn_][args___]       := Construct[Sequence, args];

EchoLen[arg_]     := With[{res = arg}, printEchoPane @ Length @ res; res];
EchoLen[args___]  := Construct[Sequence, args];

EchoDims[arg_]    := With[{res = arg}, printEchoPane @ Dimensions @ res; res];
EchoDims[args___] := Construct[Sequence, args];

EchoOpts[arg_]    := With[{res = arg}, printEchoPane @ Options @ res; res];
EchoOpts[args___] := Construct[Sequence, args];

EchoKeys[arg_]    := With[{res = arg}, printEchoPane @ Keys @ res; res];
EchoKeys[args___] := Construct[Sequence, args];

EchoHead[arg_]    := With[{res = arg}, printEchoPane @ Head @ res; res];
EchoHead[args___] := Construct[Sequence, args];

(*************************************************************************************************)

DeclareHoldAllComplete[EchoTest]
EchoTest[arg_] := (printEchoPane @ CoreToolsHold @ arg; True);

(*************************************************************************************************)

EchoReplaceAll[body_, rules_]      := ReplaceAll[body, EchoingRules @ rules];
EchoReplaceRepeated[body_, rules_] := ReplaceRepeated[body, EchoingRules @ rules];

(*************************************************************************************************)

(* TODO: how to deal with need to echo a RuleDelayed that *isn't* RuleEval? we might
change the semantics. one option would be to *only* rewrite ruledelayes with a RuleEval! *)

EchoingRules[rule_]      := ReleaseHold @ toEchoingRule[rule, 0];
EchoingRules[rules_List] := ReleaseHold @ MapP[toEchoingRule, rules];

EchoingRules::badRule = "Can't attach to ``.";
toEchoingRule[rule:RuleLP, i_]  := attachBindingBody[rule, i];
toEchoingRule[rule_, i_] := Then[Message[EchoingRules::badRule, rule], rule];

DeclareHoldAllComplete[EchoingSetDelayed]

EchoingSetDelayed[lhs_, rhs_] := ReleaseHold @ attachBindingBody[SetD, lhs, rhs, HoldSymbolName @@ PatternHeadSymbol @ lhs];

(*************************************************************************************************)

DeclareHoldAllComplete[attachBindingCondition, makeBindingCondition];

attachBindingCondition[head_, lhs_, rhs_, label_] := attachBindingCondition[head[lhs, rhs], label];
attachBindingCondition[expr:(_[lhs_, _]), label_] := attachCondition[HoldC @ expr, makeBindingCondition[lhs, label]];
attachCondition[HoldC[head_[lhs_, rhs_]], HoldC[cond_]] := HoldC[head[lhs /; cond, rhs]];
(* TODO: make AttachCondition a proper utility *)

makeBindingCondition[lhs_, label_] := With[
  {syms = HoldC @@@ PatternBoundSymbols[lhs]},
  {names = HoldSymbolName @@@ syms},
  {label2 = label},
  HoldC @ EchoBindingsCondition[label2, names, syms]
];

(* names: list of strings, values: list of HoldCs with values subbed in *)
EchoBindingsCondition[label_, names_, values_] := EchoPrint @ RawBoxes @ namesValuesBoxes[label, names, values];

(*************************************************************************************************)

DeclareHoldAllComplete[attachBindingBody, makeBindingBody]

attachBindingBody[head_, lhs_, rhs_, label_] := attachBindingBody[head[lhs, rhs], label];
attachBindingBody[expr:(_[lhs_, rhs_]), label_] := replaceBody[HoldC @ expr, makeBindingBody[lhs, rhs, label]];

(* TODO: handle With[{}, ... /; ] idiom *)
replaceBody[HoldC[(head:Rule|RuleDelayed)[lhs_, _]], HoldC[body:Except[_RuleEval]]] := HoldC[head[lhs, RuleEval @ body]];
replaceBody[HoldC[head_[lhs_, _]], HoldC[body_]] := HoldC[head[lhs, body]];
(* TODO: make ReplaceBody a proper utility *)

makeBindingBody[lhs_, rhs_, label_] := With[
  {syms = HoldC @@@ PatternBoundSymbols[lhs]},
  {names = HoldSymbolName @@@ syms},
  {label2 = label},
  HoldC @ EchoBindingsBody[label2, names, syms, rhs]
];

DeclareHoldAllComplete[EchoBindingsBody]

(* names: list of strings, values: list of HoldCs with values subbed in *)
EchoBindingsBody[label_, names_, values_, body_] := Module[
  {res = $Aborted},
  WithLocalSettings[
    $RawPrintIndent++,
    res = body,
    $RawPrintIndent--;
    With[{res2 = res}, printEchoArrowRaw["\[RuleDelayed]",
      namesValuesBoxes[label, names, values],
      NicePasterBoxes[CodePaneBoxes[res2, {UpTo @ 500, UpTo @ 100}], res2]
    ]]
  ]
];

(*************************************************************************************************)

namesValuesBoxes[label_, names_, values_] := TightBox @ FontSizeBox[12] @ RBox[
  StyleBox[MakeBoxes @ label, Bold], StyleBox[": ", Gray],
  If[names === {}, "", GridBox[
    List @ ZipMap[nameValueEntryBox, names, values],
    $namesValuesGridOpts
  ]]
];

nameValueEntryBox[name_, HoldC[value___]] := RBox[
  StyleBox[name, $DarkGreen], ":",
  NicePasterBoxes[CodePaneBoxes[CoreToolsHold @ value, {UpTo[150], UpTo[30]}], Hold[value]]
];

$namesValuesGridOpts = Seq[
  GridBoxDividers -> {"Columns" -> {False, {True}, False}, "Rows" -> {{None}}},
  FrameStyle -> GrayLevel[0.5],
  BaselinePosition -> {{1, 1}, Baseline},
  RowMinHeight -> 1.2, ColumnSpacings -> 1.2
];

(*************************************************************************************************)

AttachEchos[sym_Symbol] := (DownValues[sym] = Map[attachEchoRule, DownValues @ sym];);

attachEchoRule[rule_] /; VContainsQ[rule, $EchoLHS] := rule;
attachEchoRule[lhs_ :> rhs_] := RuleDelayed[$EchoLHS:lhs, EchoBody[$EchoLHS, rhs]];

(*************************************************************************************************)

DeclareHoldAllComplete[EchoBody];

EchoBody[lhs_, body_] := Module[
  {res = $Aborted},
  WithLocalSettings[
    $RawPrintIndent++
  ,
    res = body
  ,
    $RawPrintIndent--;
    With[{res2 = res}, EchoArrow[lhs, res2]]
  ]
];


(*************************************************************************************************)

DeclareHoldAllComplete[EchoH];

EchoH[expr_] := Module[
  {res = $Aborted},
  WithLocalSettings[
    $RawPrintIndent++
  ,
    res = expr
  ,
    $RawPrintIndent--;
    With[{res2 = res}, EchoArrow[expr, res2]]
  ]
];

(*************************************************************************************************)

(* if fn is held, EchoH0 should use EchoH *)
DeclareHoldAllComplete[EchoH0];

EchoH0[fn_[args___]] := Module[
  {res = $Aborted, inner = $Aborted},
  WithLocalSettings[
    $RawPrintIndent++
  ,
    inner = CoreToolsSequence[args];
    res = Apply[fn, inner]
  ,
    $RawPrintIndent--;
    With[{lhs2 = CoreToolsHold[fn[$inner]] /. $inner -> inner, res2 = res}, EchoArrow[lhs2, res2]]
  ]
];

(*************************************************************************************************)

(* if fn is held, EchoH0 should use EchoH *)
DeclareHoldAllComplete[EchoH1];

EchoH1[fn_[args___]] := Module[
  {res = $Aborted, inner = $Aborted},
  WithLocalSettings[
    $RawPrintIndent++
  ,
    inner = Construct[CoreToolsSequence, args];
    res = Apply[fn, inner]
  ,
    $RawPrintIndent--;
    With[{lhs2 = CoreToolsHold[fn[$inner]] /. $inner -> inner, res2 = res}, EchoArrow[lhs2, res2]]
  ]
];

(*************************************************************************************************)

DeclareHoldAllComplete[EchoArrow]

EchoArrow[a_, b_]                                 := printEchoArrow[a, b, True];
EchoArrow[a_, b_, fill_]                          := printEchoArrow[a, b, fill];

With[{patt = Apply[Alternatives, Blank /@ $MutatingSymbols]},
EchoArrow[a:patt, b_] := printEchoArrow[a, b, False]];

(*************************************************************************************************)

printEchoPane[e_] /; $EchoPrinting := EchoPrint @ CodePane[e, {{200, 300}, UpTo @ 100}];

labeledEchoPane[l_, e_] /; $EchoPrinting := EchoPrint[l, CodePane[e, {{200, 300}, UpTo @ 100}]];

(*************************************************************************************************)

DeclareHoldAllComplete[printEchoArrow, printEchoArrowRaw]

printEchoArrow[lhs2_, rhs_, fill_] := With[
  {lhs = If[fill, fillImmediateVals, Identity] @ CoreToolsHold @ lhs2},
  printEchoArrowRaw["\[Function]",
    NicePasterBoxes[CodePaneBoxes[lhs, {{200, 300}, UpTo @ 100}], lhs],
    NicePasterBoxes[CodePaneBoxes[rhs, {UpTo @ 500, UpTo @ 100}], rhs]
  ]
];

(* avoid filling just a single value or list of these *)
fillImmediateVals[e:CoreToolsHold[_Symbol | {__Symbol}]] := e;
fillImmediateVals[e_] := ReplaceAll[e, s_Symbol ? System`Private`HasImmediateValueQ :> RuleCondition[s]];

printEchoArrowRaw[arrow_, lhs_, rhs_] := EchoPrint @ RawBoxes @ GridBox[
  List @ {lhs, arrow, rhs},
  GridFrameMargins -> 0,
  GridBoxItemSize  -> {"Columns" -> {30 - $RawPrintIndent*0.9, 3, 500}},
  GridBoxAlignment -> {"Columns" -> {Left, Left, Left}, "Rows" -> {Baseline}}
]

(*************************************************************************************************)

EchoF[fn_] := EchoFL[getFLabel @ fn, fn];

DeclareHoldAllComplete[getFLabel];

getFLabel = CaseOf[
  Fn        := "Fn";
  e_Symbol  := SymbolName @ e;
  _Dict     := "\[LeftAssociation]\[Ellipsis]\[RightAssociation]";
  h_[]      := getFLabel[h] <> "[]";
  h_[i_Int] := getFLabel[h] <> "[" <> IntegerString[i] <> "]";
  h_[___]   := getFLabel[h] <> "[\[Ellipsis]]";
  _         := "\[FilledSquare]";
];

EchoFL[fnl_, fn_][args___] := Module[
  {res = $Aborted},
  WithLocalSettings[
    $RawPrintIndent++
  ,
    res = fn[args]
  ,
    $RawPrintIndent--;
    With[{res2 = res}, EchoArrow[RawBoxes[StyleBox[fnl, Bold]][args], res2]]
  ]
];

(*************************************************************************************************)

EchoFH[fn_] := With[{label = getFLabel @ fn},
  Function[Null, EchoFLH[label, fn, SlotSequence[]], HoldAllComplete]
];

DeclareHoldAllComplete[EchoFLH];

EchoFLH[fnl_, fn_, args___] := Module[
  {res = $Aborted},
  WithLocalSettings[
    $RawPrintIndent++
  ,
    res = fn[args]
  ,
    $RawPrintIndent--;
    With[{res2 = res}, EchoArrow[RawBoxes[StyleBox[fnl, Bold]][args], res2, False]]
  ]
];

(*************************************************************************************************)

NicePaster::usage =
"NicePaster[display$, paste$] displays as display$ but when clicked pastes the held expression paste$.
";

DeclareHoldRest[NicePaster, NicePasterBoxes];

MakeBoxes[NicePaster[display_, paste_], _] := NicePasterBoxes[MakeBoxes @ display, paste];

(* f[display_, CoreToolsHold[paste_]] := nicePasterBoxes[display, paste]; *)

NicePasterBoxes[boxes_, paste_] := With[
  {comp = Compress @ Defer @ paste},
  TagBox[
    TagBox[boxes, EventHandlerTag[{{"MouseClicked", 1} :> pasteExpr[comp], Method -> "Preemptive", PassEventsDown -> Automatic, PassEventsUp -> True}]],
    MouseAppearanceTag["LinkHand"]
  ]
];

(* TODO: switch to PrintInputCell? *)
pasteExpr[expr_String] := (
  (* the GeneratedCell -> False prevents the printed cell from mixing with echos and causing problems on re-evaluation *)
  CellPrint @ ExpressionCell[Uncompress @ expr, "Input", GeneratedCell -> False];
);
