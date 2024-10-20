SystemExports[
  "DebuggingFunction",
    NiceEcho, PEcho, EchoL,
    Echo1, Echo2, Echo3, Echo4, Echo5,
    EchoBy, EchoLen, EchoDims, EchoOpts, EchoKeys, EchoHead,
    EchoTest,
    EchoReplaceAll, EchoReplaceRepeated,
    EchoingRules, EchoingSetDelayed,
    EchoH, EchoHC, PEchoH,
    EchoArrow, PEchoArrow,
    EchoF, EchoFL, EchoFH, EchoFLH, AttachEchos,
    EchoProgress, EchoProgressFn,
  "SpecialVariable", $EchoIndent, $EchoLHS,
  "FormHead",        NicePaster,
  "DataHead",        StoredPaste
];

PackageExports[
  "DebuggingFunction", EchoBody,
  "MessageFunction",   EchoBindingsBody, EchoBindingsCondition,
  "BoxFunction",       PasterBox
];

PrivateExports[
  "Function",      StoredPasteGet, StoredPasteAdd,
  "CacheVariable", $StoredPasteStore
];

SessionExports[
  "TransientVariable", $ProgressCounter, $ProgressCounterMax
];

(*************************************************************************************************)

SetHoldC[EchoProgress, EchoProgressFn];

EchoProgressFn[fn_] := Module[{dynamicBox, cell},
  $ProgressCounter = 0;
  dynamicBox = DynamicBox[IntStr[$ProgressCounter], TrackedSymbols :> {$ProgressCounter}];
  cell = CellPrint @ Cell[BoxData @ dynamicBox, "Print"];
  Fn[Null, $ProgressCounter++; fn[##]]
];

$ProgressCounter = $ProgressCounterMax = 0;

EchoProgress[(head:Map|Scan|MapP|ScanP|ZipMap|ZipScan|MapApply|KeyValueMap|KeyValueScan)[fn_, arg1_, argN___]] := Module[
  {$arg1 = arg1, dynamicBox, cell, result},
  $ProgressCounter = 0;
  $ProgressCounterMax = Len[$arg1];
  result = $Failed;
  dynamicBox = DynamicBox[RowBox @ {IntStr[$ProgressCounter], "/", IntStr[$ProgressCounterMax]}, TrackedSymbols :> {$ProgressCounter}];
  cell = CellPrint @ Cell[BoxData @ dynamicBox, "Print"];
  result = Check[
    head[Fn[Null, $ProgressCounter++; fn[##]], $arg1, argN],
    $Failed
  ];
  If[result =!= $Failed, NotebookDelete @ cell];
  $ProgressCounterMax = $ProgressCounter = 0;
  result
];

(*************************************************************************************************)

Protect[$EchoLHS];

NiceEcho[args___]    := With[{eval = args}, eval /; echoCode[eval]];
PEcho[args___]       := With[{eval = args}, eval /; echoPrint[eval]];
EchoL[lbl_][args___] := With[{eval = args}, eval /; echoLabel[lbl, eval]];

(*************************************************************************************************)

Echo1[args___] := With[{eval = args}, eval /; echoLabel["1", eval]];
Echo2[args___] := With[{eval = args}, eval /; echoLabel["2", eval]];
Echo3[args___] := With[{eval = args}, eval /; echoLabel["3", eval]];
Echo4[args___] := With[{eval = args}, eval /; echoLabel["4", eval]];
Echo5[args___] := With[{eval = args}, eval /; echoLabel["5", eval]];

SetHoldC[echoLabel, echoPrint, echoCode];

echoLabel[lab_]         := (labeledEchoPane[lab]; True);
echoLabel[lab_, args__] := (labeledEchoPane[lab, PrivHoldSeq @ args]; True);
echoEval[arg_]          := (printEchoPane[arg]; True);
echoCode[args___]       := (printEchoPane[PrivHold @ args]; True);
echoPrint[args___]      := (EchoPrint[args]; True);

(*************************************************************************************************)

EchoBy[fn_][args___]          := iEchoBy[fn, args];
EchoBy[fn_, parts__][args___] := iEchoBy[PartOp[parts] /* fn, args];

EchoLen[args___]  := iEchoBy[Len, args];
EchoDims[args___] := iEchoBy[Dims, args];
EchoOpts[args___] := iEchoBy[Options, args];
EchoKeys[args___] := iEchoBy[Keys, args];
EchoHead[args___] := iEchoBy[Head, args];

SetHoldA[iEchoBy];

iEchoBy[fn_, args__] := With[{eval = args}, eval /; echoEval[fn @ Seq1[eval]]];
iEchoBy[fn_]         := With[{res = Seq[]}, res /; (EchoPrint["EchoBy: nothing to apply ", fn, " to!"]; True)];

(*************************************************************************************************)

SetHoldC[EchoTest]

EchoTest[args___] := echoCode[args];

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

SetHoldC[EchoingSetDelayed]

EchoingSetDelayed[lhs_, rhs_] := ReleaseHold @ attachBindingBody[SetD, lhs, rhs, SymName @@ PatternHead @ lhs];

(*************************************************************************************************)

SetHoldC[attachBindingCondition, makeBindingCondition];

attachBindingCondition[head_, lhs_, rhs_, label_] := attachBindingCondition[head[lhs, rhs], label];
attachBindingCondition[expr:(_[lhs_, _]), label_] := attachCondition[HoldC @ expr, makeBindingCondition[lhs, label]];
attachCondition[HoldC[head_[lhs_, rhs_]], HoldC[cond_]] := HoldC[head[lhs /; cond, rhs]];
(* TODO: make AttachCondition a proper utility *)

makeBindingCondition[lhs_, label_] := With[
  {syms = HoldC @@@ PatSyms[lhs]},
  {names = SymName @@@ syms},
  {label2 = label},
  HoldC @ EchoBindingsCondition[label2, names, syms]
];

(* names: list of strings, values: list of HoldCs with values subbed in *)
EchoBindingsCondition[label_, names_, values_] := EchoPrint @ RawBoxes @ namesValuesBoxes[label, names, values];

(*************************************************************************************************)

SetHoldC[attachBindingBody, makeBindingBody]

attachBindingBody[head_, lhs_, rhs_, label_] := attachBindingBody[head[lhs, rhs], label];
attachBindingBody[expr:(_[lhs_, rhs_]), label_] := replaceBody[HoldC @ expr, makeBindingBody[lhs, rhs, label]];

(* TODO: handle With[{}, ... /; ] idiom *)
replaceBody[HoldC[(head:Rule|RuleDelayed)[lhs_, _]], HoldC[body:Except[_RuleEval]]] := HoldC[head[lhs, RuleEval @ body]];
replaceBody[HoldC[head_[lhs_, _]], HoldC[body_]] := HoldC[head[lhs, body]];
(* TODO: make ReplaceBody a proper utility *)

makeBindingBody[lhs_, rhs_, label_] := With[
  {syms = HoldC @@@ PatSyms[lhs]},
  {names = SymName @@@ syms},
  {label2 = label},
  HoldC @ EchoBindingsBody[label2, names, syms, rhs]
];

SetHoldC[EchoBindingsBody]

(* names: list of strings, values: list of HoldCs with values subbed in *)
EchoBindingsBody[label_, names_, values_, body_] := Module[
  {res = $Aborted},
  WithLocalSettings[
    $PrintIndent++,
    res = body,
    $PrintIndent--;
    With[{res2 = res}, printEchoArrowRaw["\[RuleDelayed]",
      namesValuesBoxes[label, names, values],
      PasterBox[MakeEchoBoxes[res2], res2]
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
  PasterBox[MakeEchoBoxes[PrivHold @ value], Hold[value]]
];

$namesValuesGridOpts = Seq[
  GridBoxDividers -> {"Columns" -> {False, {True}, False}, "Rows" -> {{None}}},
  FrameStyle -> GrayLevel[0.5],
  BaselinePosition -> {{1, 1}, Baseline},
  RowMinHeight -> 1.2, ColumnSpacings -> 1.2
];

(*************************************************************************************************)

AttachEchos[sym_Sym] := (DownValues[sym] = Map[attachEchoRule, DownValues @ sym];);

attachEchoRule[rule_] /; VContainsQ[rule, $EchoLHS] := rule;
attachEchoRule[lhs_ :> rhs_] := RuleDelayed[$EchoLHS:lhs, EchoBody[$EchoLHS, rhs]];

(*************************************************************************************************)

SetHoldC[EchoBody];

EchoBody[lhs_, body_] := Module[
  {res = $Aborted},
  WithLocalSettings[
    $PrintIndent++
  ,
    res = body
  ,
    $PrintIndent--;
    With[{res2 = res}, EchoArrow[lhs, res2]]
  ]
];


(*************************************************************************************************)

SetHoldC[EchoHC];

EchoHC[expr_] := Module[
  {res = $Aborted},
  WithLocalSettings[
    $PrintIndent++
  ,
    res = expr
  ,
    $PrintIndent--;
    With[{res2 = res}, EchoArrow[expr, res2]]
  ]
];

(*************************************************************************************************)

SetHoldC[PEchoH, mkPEchoH];

PEchoH[fn_[args___]] := With[{in = Unsequence[args]}, mkPEchoH[fn, in]];

mkPEchoH[fn_, Unsequence[in___]] := With[{out = fn[in]},
  Then[
    PEchoArrow[fn[in], out, False],
    out
  ]
];

(*************************************************************************************************)

(* if fn is held, EchoH0 should use EchoH *)
SetHoldC[EchoH];

EchoH[fn_[args___]] := Module[
  {res = $Aborted, inner = $Aborted},
  WithLocalSettings[
    $PrintIndent++
  ,
    inner = Make[PrivHoldSeq, args];
    res = Apply[fn, inner]
  ,
    $PrintIndent--;
    With[{lhs2 = PrivHold[fn[$inner]] /. $inner -> inner, res2 = res}, EchoArrow[lhs2, res2]]
  ]
];


(*************************************************************************************************)

SetHoldC[EchoArrow, PEchoArrow]

EchoArrow[a_, b_]         := printEchoArrow[a, b, True];
EchoArrow[a_, b_, fill_]  := printEchoArrow[a, b, fill];
PEchoArrow[a_, b_]        := printPEchoArrow[a, b, True];
PEchoArrow[a_, b_, fill_] := printPEchoArrow[a, b, fill];

With[{patt = Apply[Alternatives, Blank /@ $MutatingSymbols]},
EchoArrow[a:patt, b_]     := printEchoArrow[a, b, False];
PEchoArrow[a:patt, b_]    := printPEchoArrow[a, b, False]
];

(*************************************************************************************************)

printEchoPane[e_] /; $EchoPrinting := EchoPrint @ RawBoxes @ MakeEchoBoxes @ e;

labeledEchoPane[l_, e_] /; $EchoPrinting := EchoPrint[l, ": ", RawBoxes @ MakeEchoBoxes @ e];

(*************************************************************************************************)

SetHoldC[printPEchoArrow, printEchoArrow, printEchoArrowRaw]

printPEchoArrow[lhs2_, rhs_, fill_] := With[
  {lhs = If[fill, fillImmediateVals, Identity] @ HoldForm @ lhs2},
  printEchoArrowRaw["\[Function]",
    PasterBox[PaneBox[MakeBoxes @ lhs, ImageSize -> {{200, 300}, UpTo @ 100}], lhs],
    PasterBox[PaneBox[MakeBoxes @ rhs, ImageSize -> {UpTo @ 500, UpTo @ 100}], rhs]
  ]
];

printEchoArrow[lhs2_, rhs_, fill_] := With[
  {lhs = If[fill, fillImmediateVals, Identity] @ PrivHold @ lhs2},
  printEchoArrowRaw["\[Function]",
    PasterBox[PaneBox[MakeEchoBoxes @ lhs, ImageSize -> {{200, 300}, UpTo @ 100}], lhs],
    PasterBox[PaneBox[MakeEchoBoxes @ rhs, ImageSize -> {UpTo @ 500, UpTo @ 100}], rhs]
  ]
];

(* avoid filling just a single value or list of these *)
fillImmediateVals[e:PrivHold[_Sym | {__Sym}]] := e;
fillImmediateVals[e_] := ReplaceAll[e, s_Sym ? HasIValueQ :> RuleCondition[s]];

printEchoArrowRaw[arrow_, lhs_, rhs_] := EchoPrint @ RawBoxes @ GridBox[
  List @ {lhs, arrow, rhs},
  GridFrameMargins -> 0,
  GridBoxItemSize  -> {"Columns" -> {30 - $PrintIndent*0.9, 3, 500}},
  GridBoxAlignment -> {"Columns" -> {Left, Left, Left}, "Rows" -> {Baseline}}
]

(*************************************************************************************************)

EchoF[fn_] := EchoFL[fn, fn];

EchoFL[fnl_, fn_][args___] := Module[
  {res = $Aborted},
  WithLocalSettings[
    $PrintIndent++
  ,
    res = fn[args]
  ,
    $PrintIndent--;
    With[{res2 = res}, EchoArrow[HoldAtForm[fnl, args], res2]]
  ]
];

(*************************************************************************************************)

EchoFH[fn_] := Function[Null, EchoFLH[label, fn, SlotSequence[]], HoldAllComplete];

SetHoldC[EchoFLH];

EchoFLH[fnl_, fn_, args___] := Module[
  {res = $Aborted},
  WithLocalSettings[
    $PrintIndent++
  ,
    res = fn[args]
  ,
    $PrintIndent--;
    With[{res2 = res}, EchoArrow[HoldAtForm[fnl, args], res2, False]]
  ]
];

(*************************************************************************************************)

NicePaster::usage =
"NicePaster[display$, paste$] displays as display$ but when clicked pastes the held expression paste$.
";

SetForm0[NicePaster];
SetHoldR[NicePaster, PasterBox];

MakeBoxes[NicePaster[display_, paste_], _] :=
  PasterBox[MakeBoxes @ display, paste];

PasterBox[boxes_, paste_] := With[
  {sexp = Most @ StoredPasteAdd[HoldC @ paste]},
  ClickBox[boxes, sexp[]]
];


(*************************************************************************************************)

$StoredPasteStore = StoreNew[];

StoredPasteGet[hash_] := StoreGet[$StoredPasteStore, hash];

StoredPasteAdd[expr_] := Module[{hash, comp, size, sexp},
  hash = Hash @ expr;
  If[StoreKeyQ[$StoredPasteStore, hash], Return @ StoredPasteGet @ hash];
  size = ByteCount @ expr;
  comp = Compress @ expr;
  sexp = StoredPaste[hash, size, comp];
  StoreSet[$StoredPasteStore, hash, sexp];
  sexp
];

StoredPaste[id_Int, _][]  := iStoredPaste @ StoreGet[$StoredPasteStore, id];

iStoredPaste[_] := Beep[];
iStoredPaste[StoredPaste[_, _, str_Str]] := PrintInputCell @ Uncompress @ str;

(* the GeneratedCell -> False prevents the printed cell from mixing with echos and causing problems on re-evaluation *)

(*************************************************************************************************)

SetForm0[NicePaster];

MakeBoxes[StoredPaste[_, size_Int, ___], _] := RBox["\[LeftSkeleton]", IntStr @ size, "\[RightSkeleton]"];

