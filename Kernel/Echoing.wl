SystemExports[
  "DebuggingFunction",
    NiceEcho, EchoTest, EchoArrow,
    EchoBy, EchoLen, EchoDims, EchoKeys, EchoHead,
    EchoH, EchoH0, EchoH1, EchoF, EchoFL, EchoFH, EchoFLH, AttachEchos,
  "SpecialVariable",
    $EchoIndent,
  "FormHead",
    NicePaster
];

PackageExports[
  "DebuggingFunction",
    EchoSet, EchoSetH, EchoSymbol, EchoBody,
  "BoxFunction",
    NicePasterBoxes
];

(*************************************************************************************************)

DeclareHoldAllComplete[EchoTest]
EchoTest[arg_] := (niceEchoPane @ CoreToolsHold @ arg; True);

(*************************************************************************************************)

EchoBy[fn_, parts__][arg_] := With[{res = arg},
  niceEchoPane @ Apply[fn] @ FastQuietCheck[Part[Hold @ arg, All, parts], Missing]; res];

EchoBy[fn_][arg_] := With[{res = arg}, niceEchoPane @ fn @ res; res];
EchoBy[fn_][args___] := Construct[Sequence, args];

(*************************************************************************************************)

EchoDims[arg_] := With[{res = arg}, niceEchoPane @ Dimensions @ res; res];
EchoDims[args___] := Construct[Sequence, args];

(*************************************************************************************************)

EchoHead[arg_] := With[{res = arg}, niceEchoPane @ Head @ res; res];
EchoHead[args___] := Construct[Sequence, args];

(*************************************************************************************************)

EchoLen[arg_] := With[{res = arg}, niceEchoPane @ Length @ res; res];
EchoLen[args___] := Construct[Sequence, args];

(*************************************************************************************************)

EchoKeys[arg_] := With[{res = arg}, niceEchoPane @ Keys @ res; res];
EchoKeys[args___] := Construct[Sequence, args];

(*************************************************************************************************)

NiceEcho[arg_] := (niceEchoPane @ CoreToolsHold @ arg; arg);
NiceEcho[args___] := (niceEchoPane @ CoreToolsHold @ CoreToolsSequence[args]; Construct[Sequence, args]);

(*************************************************************************************************)

niceEchoPane[e_] /; $shouldPrint := printRawEchoCell @ ToBoxes @ CodePane[e, {{200, 300}, UpTo @ 100}];

$shouldPrint := !TrueQ[$SessionCurrentEvaluationPrintCount >= $SessionMaxEvaluationPrintCount];

printRawEchoCell[boxes_] /; $shouldPrint := (
  $SessionCurrentEvaluationPrintCount++;
  CellPrint @ Cell[
    BoxData @ boxes, "Print",
    CellMargins -> {{66 + $RawPrintIndent * $EchoIndent, 0}, {1, 1}},
    "GeneratedCell" -> True, CellAutoOverwrite -> True,
    CellBracketOptions -> {"Color" -> Orange, "Thickness" -> 1.5}
  ];
);

SetInitial[$EchoIndent, 12];

(*************************************************************************************************)

AttachEchos[sym_Symbol] := (
  DownValues[sym] = Map[attachEchoRule, DownValues @ sym];
);

attachEchoRule[rule_] /; VContainsQ[rule, $EchoLHS$] := rule;
attachEchoRule[lhs_ :> rhs_] := RuleDelayed[$EchoLHS:lhs, EchoBody[$EchoLHS, rhs]];

(*************************************************************************************************)

DeclareHoldAllComplete[EchoBody];

EchoBody[lhs_, body_] := Module[
  {res = $Aborted},
  Internal`WithLocalSettings[
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
  Internal`WithLocalSettings[
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
  Internal`WithLocalSettings[
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
  Internal`WithLocalSettings[
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

EchoArrow[a_, b_]                                 := echoArrow[a, b, True];
EchoArrow[a_, b_, fill_]                          := echoArrow[a, b, fill];

With[{patt = Apply[Alternatives, Blank /@ $MutatingSymbols]},
EchoArrow[a:patt, b_] := echoArrow[a, b, False]];

(*************************************************************************************************)

DeclareHoldAllComplete[echoArrow]

echoArrow[lhs2_, rhs2_, fill_] := With[
  {lhs = If[fill, fillImmediateVals, Identity] @ CoreToolsHold @ lhs2,
   rhs = CoreToolsHold @ rhs2, width = 30 - $RawPrintIndent*0.9},
  printRawEchoCell @ GridBox[{{
    ToBoxes @ NicePaster[CodePane[lhs, {{200, 300}, UpTo @ 100}], lhs],
    "\[Function]",
    ToBoxes @ NicePaster[CodePane[rhs, {UpTo @ 500, UpTo @ 100}], rhs]
  }},
    GridFrameMargins -> 0,
    GridBoxItemSize -> {"Columns" -> {width, 3, 500}},
    GridBoxAlignment -> {"Columns" -> {Left, Left, Left}, "Rows" -> {Baseline}}
]];

(* avoid filling just a single value or list of these *)
fillImmediateVals[e:CoreToolsHold[_Symbol | {__Symbol}]] := e;
fillImmediateVals[e_] := ReplaceAll[e, s_Symbol ? System`Private`HasImmediateValueQ :> RuleCondition[s]];

(*************************************************************************************************)

EchoF[fn_] := EchoFL[getFLabel @ fn, fn];

DeclareHoldAllComplete[getFLabel];

getFLabel[Function]       := "Fn";
getFLabel[e_Symbol]       := SymbolName @ e;
getFLabel[_Association]   := "\[LeftAssociation]\[Ellipsis]\[RightAssociation]";
getFLabel[h_[]]           := getFLabel[h] <> "[]";
getFLabel[h_[i_Integer]]  := getFLabel[h] <> "[" <> IntegerString[i] <> "]";
getFLabel[h_[___]]        := getFLabel[h] <> "[\[Ellipsis]]";
getFLabel[_]              := "\[FilledSquare]";

EchoFL[fnl_, fn_][args___] := Module[
  {res = $Aborted},
  Internal`WithLocalSettings[
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
  Internal`WithLocalSettings[
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
"NicePaster[display, paste] displays as display but when clicked pastes the held expression paste.
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

pasteExpr[expr_String] := (
  (* the GeneratedCell -> False prevents the printed cell from mixing with echos and causing problems on re-evaluation *)
  CellPrint @ ExpressionCell[Uncompress @ expr, "Input", GeneratedCell -> False];
);
