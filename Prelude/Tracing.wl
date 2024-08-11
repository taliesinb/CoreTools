BeginPackage["Prelude`Tracing`"]

System`PackageExports[
"DebuggingFunction",
System`BlockPrint,
System`TraceAutoloads,
System`Capture,
System`RawPrintBoxes,
System`CountUnpackings,
System`TraceUnpackings,
System`EnableDebugPrinting,
System`DisableEchoPrinting,
System`FailureString,

"IOFunction",
System`CustomizedPrint,
System`ErrorPrint,
System`LogPrint,
System`RawPrint,
System`LabeledPrint,
System`DPrint,
System`EchoPrint,
System`WithRawPrintIndent,

"SpecialVariable",
System`$Captured,
System`$RawPrintIndent,
System`$RawPrintMaxRate,
System`$CellPrintLabel,
System`$DebugPrinting,
System`$EchoPrinting,
System`$CurrentlyTracingAutoloads,
System`$CurrentPackageFile,
System`$CurrentPackageExprCount,
System`$CurrentPackageExpr,
System`$ShouldPrint,
System`$LastTraceback,
System`$Disabled
]

(*************************************************************************************************)

Begin["`Private`"]

Protect[$Disabled];

(*************************************************************************************************)

SetAttributes[{BlockPrint, EnableDebugPrinting, DisableEchoPrinting, WithRawPrintIndent}, HoldAll];

BlockPrint[body_] := Block[{CellPrint = Hold, Print = Hold}, body];

$DebugPrinting = False;
$EchoPrinting = True;

EnableDebugPrinting[body_] := Block[{$DebugPrinting = True}, body];
DisableEchoPrinting[body_] := Block[{$EchoPrinting = False}, body];

WithRawPrintIndent[body_] := Block[{$RawPrintIndent = $RawPrintIndent + 1}, body];

(*************************************************************************************************)

SetAttributes[{RawPrint, LogPrint, ErrorPrint, EchoPrint, DPrint}, HoldAllComplete];

(*************************************************************************************************)

$errorPrintOpts = {
  CellStyle -> "Message",
  FontSize -> 13, FontColor -> Orange,
  CellLabel -> "Error", CellLabelStyle -> Orange
};

$logPrintOpts = {FontSize -> 13, FontColor -> GrayLevel[0.5], CellLabel -> "Log"};
$echoDingbat = StyleBox["» ", FontSize -> 15, FontFamily -> "Roboto", FontColor -> Orange];
$echoPrintOpts = {FontSize -> 13, CellDingbat -> $echoDingbat};
$DPrintOpts = {FontSize -> 13, FontColor -> Pink, CellLabel -> "Debug", CellLabelStyle -> Pink};

RawPrint[args___]   := CustomizedPrint[{}, args];
LogPrint[args___]   := CustomizedPrint[$logPrintOpts, args];
ErrorPrint[args___] := CustomizedPrint[$errorPrintOpts, args];
EchoPrint[args___]  := If[$EchoPrinting, CustomizedPrint[$echoPrintOpts, args], $Disabled, $Disabled];
DPrint[args___] := If[$DebugPrinting, CustomizedPrint[$DPrintOpts, args], $Disabled, $Disabled];

(*************************************************************************************************)

$RawPrintIndent = 0;
$RawPrintMaxRate = 50;

(*************************************************************************************************)

$rawPrintCount = 0;
$rawPrintResetTime = 0;
$rawPrintThisTime = 0;

$ShouldPrint := (
  If[
    ($rawPrintThisTime = SessionTime[]) >= $rawPrintResetTime,
    $rawPrintResetTime = $rawPrintThisTime + .5;
    $rawPrintCount = 0
  ];
  $rawPrintCount <= $RawPrintMaxRate
);

(*************************************************************************************************)

RawPrintBoxes[boxes_, style_, opts___Rule] := If[!$ShouldPrint, $TimedOut,
  CellPrint @ Cell[
    BoxData @ boxes, style,
    Sequence @@ generateRawPrintOptions[{opts}],
    LineSpacing -> {1, 1}
  ]
];

generateRawPrintOptions[opts_] := Flatten @ {
  If[KeyExistsQ[opts, CellLabel],
    List[
      ShowCellLabel  -> True,
      CellLabel      -> StringJoin[Lookup[opts, CellLabel], ConstantArray["       ", $RawPrintIndent]],
      CellLabelStyle -> Directive["CellLabel", Lookup[opts, CellLabelStyle, GrayLevel[0.5]]],
      DeleteCases[opts, CellLabelStyle | CellLabel -> _]
    ],
    opts
  ],
  If[StringQ[$CurrentPackageFile],
    "CellFrameLabels" -> {{None, trimPath @ $CurrentPackageFile}, {None, None}},
    {}
  ],
  If[$rawPrintCount++ >= $RawPrintMaxRate,
    List[
      Beep[];
      CellFrame -> {{False, False}, {True, False}},
      CellFrameMargins -> {{0, 0}, {0, 0}}
    ],
    {}
  ],
  If[$RawPrintIndent > 0,
    List[
      CellDingbatMargin -> 5 + $RawPrintIndent * 20,
      CellGroupingRules -> "GraphicsGrouping",
      (* CellGroupingRules -> {"GraphicsGrouping", $RawPrintIndent}, *)
      CellMargins -> {{66 + $RawPrintIndent * 20, 3}, {0, 0}},
      CellLabelMargins -> {{12 + $RawPrintIndent * 20, Inherited}, {Inherited, Inherited}}
    ],
    List[
      CellMargins -> {{Inherited, 1}, {1, 1}},
      CellGroupingRules -> "GraphicsGrouping"
    ]
  ],
  If[StringQ[$CellPrintLabel],
    CellFrameLabels -> {{None, $CellPrintLabel}, {None, None}},
    {}
  ]
};

(*************************************************************************************************)

(*
TODO: investigate CellGroupingRules
TODO: investigate CellDingbat as an alternative to using CellLabel
*)

SetAttributes[CustomizedPrint, HoldRest];

CustomizedPrint[opts_List, args___] := If[!$ShouldPrint, $TimedOut,
  RawPrintBoxes[
    toPrintBoxes[args],
    Lookup[opts, CellStyle, "Print"],
    Sequence @@ DeleteCases[opts, CellStyle -> _]
  ]
];

toPrintBoxes[RawBoxes[boxes_]] := boxes;
toPrintBoxes[args___] := printSeqBoxes @@ Map[printArgBoxes, {args}];

printSeqBoxes[e___] := RowBox @ Riffle[{e}, "\[InvisibleSpace]"];
printSeqBoxes[e_] := e;

printArgBoxes[e_] := ToBoxes @ e;
printArgBoxes[HoldPattern[f:Failure[_, _Association ? Developer`HoldAtomQ]]] := ToBoxes @ FailureString[f];

(*************************************************************************************************)

FailureString[_] := "???";

FailureString[HoldPattern[f:Failure[___]]] :=
  "\[LeftGuillemet][\[Ellipsis]]\[RightGuillemet]";

FailureString[HoldPattern[f:Failure[tag_, assoc_Association ? Developer`HoldAtomQ]]] := StringJoin[
  "\[LeftGuillemet][",
  TextString @ tag, fmtErrorCode @ assoc @ "ErrorCode", ": ",
  msgPayloadStr @ f,
  fmtTraceback[System`$LastTraceback = assoc @ "Traceback"],
  "]\[RightGuillemet]"
];

msgPayloadStr[HoldPattern[Failure[_, <|"MessageTemplate" -> msg_String|>]]] := msg;
msgPayloadStr[f_] := TextString[f];

(*************************************************************************************************)

fmtErrorCode[_Missing] := "";
fmtErrorCode[sub_]     := TextString @ sub;

fmtTraceback[_] := "";
fmtTraceback[HoldPattern[stack_OpenerView]] := List[
  " @ ", fmtFinalFrame @ getLastFrame @ stack
];

getLastFrame[stack_] := Module[{pos},
  pos = Position[stack, _OpenerView, {5, Infinity}];
  If[pos === {}, None, Extract[stack, Last @ pos]]
];

fmtFinalFrame[_] := "??";
fmtFinalFrame[HoldPattern[frame_OpenerView]] := fmtFileLoc[
  getFileLoc @ Part[frame, 1, 1],
  FirstCase[frame, Item[line_String, ___] :> fmtFrameLine[line], None, Infinity]
];

getFileLoc[TemplateBox[{boxes_,  "\" in \"", fn_}, "RowDefault"]] := {getFileLoc2 @ boxes, ToExpression @ fn};
getFileLoc[boxes_] := {getFileLoc2 @ boxes, None};
getFileLoc2[path_String] := path;
getFileLoc2[boxes_] := FirstCase[boxes, HoldPattern[FileExistsQ[path_String]] :> path, None, Infinity];

fmtFileLoc[{path_String, fnName_String}, line_String] := {path, ":", line, ":", fnName};
fmtFileLoc[{path_String, None}, line_String] := {path, ":", line};
fmtFileLoc[_, _] := "???"

fmtFrameLine[line_] := First[
  StringCases[line, d:DigitCharacter.. ~~ ". " :> d, 1],
  "???"
];

(*************************************************************************************************)

SetAttributes[Capture, HoldAll];
Capture[e_] := Block[
  {res = $Captured = HoldComplete[e]},
  $Captured = $Captured /. sym_Symbol ? System`Private`HasImmediateValueQ :> RuleCondition[sym];
  ReleaseHold[res]
];

(*************************************************************************************************)

SetAttributes[{withUnpackMsgHandler}, HoldFirst];

withUnpackMsgHandler[expr_, handler_] :=
  Internal`HandlerBlock[{"Message", handleUnpackMsg[handler]},
    Quiet[
      Internal`WithLocalSettings[On["Packing"], expr, Off["Packing"]],
      "Packing"
    ]
  ];

handleUnpackMsg[handler_][Hold[Message[MessageName[_, "unpack"|"punpack1"], dims_], _]] := handler[dims];
handleUnpackMsg[handler_][Hold[Message[MessageName[_, "punpack"|"punpackl1"], _, dims_], _]] := handler[dims]

(*************************************************************************************************)

SetAttributes[{CountUnpackings, TraceUnpackings}, HoldFirst];

CountUnpackings[expr_] := Block[
  {$unpackCounts = Association[]},
  withUnpackMsgHandler[expr, countUnpack];
  $unpackCounts
];
countUnpack[dims_] := Set[$unpackCounts[dims], Lookup[$unpackCounts, dims, 0] + 1];

TraceUnpackings[expr_] := withUnpackMsgHandler[expr, printUnpack];
printUnpack[dims_] := LogPrint[Row[dims, "\[Times]"]];

(*************************************************************************************************)

SetAttributes[TraceAutoloads, HoldFirst];
SetAttributes[{$hookAutoload, $autoLoadHandler, $getHandler}, HoldAllComplete];

$loadDepth = 0;
tlPrint[args__] :=
  CellPrint @ Cell[
    BoxData @ RowBox[{args}], "Message",
    "CellMargins" -> {{65 + $loadDepth * 20, 0}, {0, 0}},
    "GeneratedCell" -> True, "CellAutoOverwrite" -> True,
    "LineBreakWithin" -> False,
    "ShowStringCharacters" -> True,
    "CellFrameLabels" -> {{None, trimPath @ $currentPath}, {None, None}},
    "CellBracketOptions" -> {"Color" -> RGBColor[1, 0.5, 0], "Thickness" -> 1}
  ];

$currentPath := If[StringQ[$CurrentPackageFile],
  $CurrentPackageFile <> ":" <> IntegerString[$CurrentPackageExprCount],
  $InputFileName
];

$sysDir = FileNameJoin[{$InstallationDirectory, "SystemFiles"}];
$compDir = FileNameJoin[{$sysDir, "Components"}];

trimPath[""] := None;
trimPath[str_String] := StringReplace[str, {
  $HomeDirectory -> "~",
  $compDir -> "$Comp",
  $sysDir -> "$Sys",
  $InstallationDirectory -> "$Inst"
}];

tlPrintLn[args__] := tlPrint[args];

$autoLoadHandler[sym_Symbol, context_String] := (
  tlPrint[StyleBox[SymbolName[Unevaluated @ sym], RGBColor[1, 0.5, 0]], "\[Implies]", "\"" <> context <> "\""];
  $loadCallbackFn[HoldComplete[sym] -> context, Symbol];
);

$getHandler[HoldComplete[context_String, _, First]] := (
  tlPrintLn["<<" <> trimPath[context]];
  $loadDepth++;
  $loadCallbackFn[context, Begin];
);

$getHandler[HoldComplete[context_String, _, Last]] := (
  $loadDepth--;
  $loadCallbackFn[context, End];
);

$msgHandler[Hold[msg_Message, True]] := tlPrintLn[InputForm @ msg];

mxGet[fn_, file_] := Block[{$hookAutoload = False},
  tlPrintLn[SymbolName[fn] <> "[\"" <> trimPath[file] <> "\"]"];
  $loadCallbackFn[file, Begin];
  With[{res = fn @ file}, $loadCallbackFn[file, End]; res]
];

TraceAutoloads[expr_, callback_:None] := Block[
  {$CurrentlyTracingAutoloads = True, $hookAutoload = True, $loadCallbackFn = callback},
  Internal`WithLocalSettings[
    Unprotect[Package`ActivateLoad, System`Dump`AutoLoad, Get, DumpGet];
    Package`ActivateLoad[sym_, _, context_String, _] /; $autoLoadHandler[sym, context] := Null;
    System`Dump`AutoLoad[Hold[sym_], Hold[__], context_] /; $autoLoadHandler[sym, context] := Null;
    Internal`AddHandler["GetFileEvent", $getHandler];
    Internal`AddHandler["Message", $msgHandler];
    Get[s_String] /; TrueQ[$hookAutoload && StringEndsQ[s, ".mx"]] := mxGet[Get, s];
    DumpGet[s_String] /; TrueQ[$hookAutoload && StringEndsQ[s, ".mx"]] := mxGet[DumpGet, s];
    Protect[Package`ActivateLoad, System`Dump`AutoLoad, Get];
  ,
    CheckAbort[expr, $Aborted]
  ,
    Unprotect[Package`ActivateLoad, System`Dump`AutoLoad, Get];
    Get[s_String] /; TrueQ[$hookAutoload && StringEndsQ[s, ".mx"]] =.;
    Internal`RemoveHandler["GetFileEvent", $getHandler];
    Internal`RemoveHandler["Message", $msgHandler];
    Package`ActivateLoad[sym_, _, context_String, _] /; $autoLoadHandler[sym, context] =.;
    System`Dump`AutoLoad[Hold[sym_], Hold[__], context_] /; $autoLoadHandler[sym, context] =.;
    DumpGet[s_String] /; TrueQ[$hookAutoload && StringEndsQ[s, ".mx"]] =.;
    Protect[Package`ActivateLoad, System`Dump`AutoLoad, Get, DumpGet];
  ]
];

(*************************************************************************************************)

End[]

EndPackage[]