BeginPackage["Prelude`Tracing`"]

System`PackageExports[
"DebuggingFunction",
System`BlockPrint,
System`TraceAutoloads,
System`Capture,
System`RawPrintBoxes,
System`CountUnpackings,
System`TraceUnpackings,
System`WithDebugPrinting,

"IOFunction",
System`ErrorPrint,
System`LogPrint,
System`RawPrint,
System`DebugPrint,
System`WithRawPrintIndent,

"SpecialVariable",
System`$Captured,
System`$RawPrintIndent,
System`$CellPrintLabel,
System`$DebugPrinting
System`$CurrentlyTracingAutoloads,
System`$CurrentPackageFile,
System`$CurrentPackageExprCount,
System`$CurrentPackageExpr
]

(*************************************************************************************************)

Begin["`Private`"]

(*************************************************************************************************)

SetAttributes[{BlockPrint, WithRawPrintIndent, WithDebugPrinting}, HoldAll];

BlockPrint[body_] := Block[{CellPrint = Hold, Print = Hold}, body];
WithRawPrintIndent[body_] := Block[{$RawPrintIndent = $RawPrintIndent + 1}, body];
WithDebugPrinting[body_] := Block[{$DebugPrinting = True}, body];

(*************************************************************************************************)

$RawPrintIndent = 0;

RawPrintBoxes[boxes_, style_, opts___Rule] /; TrueQ[$shouldPrintReset; $rawPrintCount <= $rawPrintMaxRate; True] :=
  CellPrint @ Cell[
    BoxData @ ToString[{$rawPrintCount, $rawPrintMaxRate, $rawPrintThisTime, $rawPrintResetTime}, InputForm];
    BoxData @ boxes
    , style,
    CellMargins -> {{66 + $RawPrintIndent * 20, 0}, {0, 0}},
    CellAutoOverwrite -> True,
    Sequence @@ If[$rawPrintCount < $rawPrintMaxRate, {},
      Beep[];
      {CellFrame -> {{False, False}, {True, False}}, CellFrameMargins -> {{0, 0}, {0, 0}}}
    ],
    Sequence @@ If[StringQ[$CellPrintLabel],
      {CellFrameLabels -> {{None, $CellPrintLabel}, {None, None}}}, {}
    ],
    opts,
    LineSpacing -> {1, 1}
  ];

(*************************************************************************************************)

$rawPrintMaxRate = 50;
$rawPrintCount = 0;
$rawPrintResetTime = 0;
$rawPrintThisTime = 0;

$shouldPrintReset := (
  $rawPrintThisTime = SessionTime[];
  If[$rawPrintThisTime >= $rawPrintResetTime,
    $rawPrintCount = 0;
    $rawPrintResetTime = $rawPrintThisTime + .5;
  ];
);

$shouldPrintQ := (
  $shouldPrintReset;
  If[$rawPrintCount <= $rawPrintMaxRate, True, False]; True
);

(*************************************************************************************************)

ErrorPrint[args___] /; $shouldPrintQ := RawPrintBoxes[
  ToBoxes[SequenceForm[args], StandardForm],
  "Message",
  FontSize -> 13, FontColor -> Orange,
  CellBracketOptions -> {"Color" -> Orange, "Thickness" -> 1.5}
];

(*************************************************************************************************)

LogPrint[args___] /; $shouldPrintQ := RawPrintBoxes[
  ToBoxes[SequenceForm[args], StandardForm],
  "Print",
  FontSize -> 13, FontColor -> GrayLevel[0.5],
  CellBracketOptions -> {"Color" -> GrayLevel[0.5], "Thickness" -> 1.5}
];

(*************************************************************************************************)

DebugPrint[args___] /; $DebugPrinting && $shouldPrintQ := RawPrintBoxes[
  ToBoxes[SequenceForm[args], StandardForm],
  "Print",
  FontSize -> 13, FontColor -> GrayLevel[0.5],
  CellBracketOptions -> {"Color" -> GrayLevel[0.5], "Thickness" -> 1.5}
];

(*************************************************************************************************)

RawPrint[args___] /; $shouldPrintQ := RawPrintBoxes[
  ToBoxes[SequenceForm[args], StandardForm],
  "Print"
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