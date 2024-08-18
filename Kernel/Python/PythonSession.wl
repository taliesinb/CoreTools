SystemExports[
  "Head",
    PythonVariable,
  "IOFunction",
    PythonEvaluators,
    PythonRestart,
    PythonRun,
    PythonScriptRun,
    PythonScriptRunDynamic,
    PrintPythonFile,
    PythonReload,
  "Head",
    TorchTensor, NamedTuple, PythonScriptResult, PythonResult, PyObject, PyObjectRef, PyFunction,
  "IOFunction",
    SetupTorchSerialization, PythonSessionPrint, PythonSessionError,
  "Function",
    SplitOnAxis,
  "Variable",
    $PythonSession,
    $PythonPath
];

PackageExports[
  "IOFunction",
    DefaultPythonSession,
    InitializeTorch,
  "Function",
    PythonObjectGraph,
  "Variable",
    $PythonPrintCallback,
    $PythonErrorCallback,
    $PythonResultPostProcessor,
    $PythonSessionParameters
];

PrivateExports[
  "IOFunction",
    PythonPrint,
    PythonRawPrint,
    PythonErrorPrint,
    PythonSessionInfo,
  "SpecialFunction",
    CustomExternalEvaluateCatch,
    ExternalEvaluatePostProcessor,
  "SpecialVariable",
    $PythonSessionName,
    $TorchInitialized,
    $WolframClientPath,
    $PathToNotebookCache
];

(*************************************************************************************************)

PythonObjectGraph[expr_] := Locals[
  paths = ToExprPaths @ Position[expr, _PyObject | _PyObjectRef];
  paths //= ReplaceAll[ExprPath[e___, 0] :> ExprPath[e]];
  exprs = ExtractExprPaths[expr, paths];
  exprs = Replace[exprs, PyObject[n_, h_][___] :> PyObjectRef[n, h], {1, Infinity}];
  igraph = IndexGraph @ PrefixGraph @ paths;
  FromIndexGraph[igraph, exprs,
    PlotTheme -> "CorePythonGraph",
    GraphLayout -> {"LayeredDigraphEmbedding", "Orientation" -> Left}
  ]
];

DefineGraphTheme["CorePythonGraph", {
  ThemeParent -> "Core",
  EdgeShapeFunction -> {"ShortUnfilledArrow", "ArrowSize" -> Medium, "ArrowPositions" -> 0.7},
  VertexLabels -> "Name",
  EdgeLabels -> "EdgeTag",
  Options -> {ImagePadding -> 60}
}];

(*************************************************************************************************)

PythonSessionInfo[args___] :=
  ExternalEvaluate`Private`getSessionOpts[First @ $PythonSession, args];

(* "StartupCommand" -> {"/opt/homebrew/bin/python3.12", "-u",
  "/Applications/Mathematica.app/Contents/SystemFiles/Components/\
ExternalEvaluate_Python/Resources/cli.py", "start_externalevaluate",
  "--path",
  "/Applications/Mathematica.app/Contents/SystemFiles/Components/\
WolframClientForPython", "--installpath",
  "/Applications/Mathematica.app/Contents"}
 *)
(*************************************************************************************************)

SetInitial[$PythonPath, None];
SetInitial[$PythonSessionName, "Python"];
SetInitial[$PythonSessionParameters, Dict[
  "Name"                   :> $PythonSessionName,
  "System"                 -> "Python",
  "StandardOutputFunction" -> PythonSessionPrint,
  "StandardErrorFunction"  -> PythonSessionError,
  "SessionProlog"          -> File @ DataPath["Python", "ct_prelude.py"],
  "ID"                     :> $PythonSessionName
]];
SetInitial[$PythonPrintCallback, PythonRawPrint];
SetInitial[$PythonErrorCallback, PythonErrorPrint];
SetInitial[$PythonResultPostProcessor, Id];

SetDelayedInitial[$PythonSession, DefaultPythonSession[]];
SetDelayedInitial[$WolframClientPath, $WolframClientPath = First[PacletFind["WolframClientForPython"]]["Location"]];

(*************************************************************************************************)

$pythonPrintOpts = {
  CellLabel -> "Python", CellLabelStyle -> $DarkGreen
};

PythonRawPrint[]        := CustomizedPrint[$pythonPrintOpts];
PythonRawPrint[arg_]    := CustomizedPrint[$pythonPrintOpts, arg];
PythonRawPrint[args___] := CustomizedPrint[$pythonPrintOpts, Row[{args}, " "]];

$pythonErrorPrintOpts = {
  CellStyle -> "Message",
  CellLabel -> "Python", CellLabelStyle -> $DarkGreen,
  FontSize -> 13, FontColor -> $DarkOrange
};

PythonErrorPrint[args___] := CustomizedPrint[$pythonErrorPrintOpts, args];

(*************************************************************************************************)

PythonEvaluators[query___] := Select[
  Normal @ FindExternalEvaluators["Python"],
  dictMatchQ[Dict[query]]
];

dictMatchQ[query_][dict_] := AllAreTrueQ @ Merge[{query, dict}, valMatchQ];
valMatchQ[{_}] := True;
valMatchQ[{q_Str, v_}] := StringContainsQ[v, q];
valMatchQ[{a_, b_}] := a === b;

(*************************************************************************************************)

DefaultPythonSession[] := SelectFirst[
  ExternalSessions[],
  SameQ[#["ID"], $PythonSessionName]&, PythonRestart[]
];

PythonSessionPrint[e___] := $PythonPrintCallback[e];
PythonSessionError[e___] := $PythonErrorCallback[e];

PythonReload[] := PythonRun["reload_prelude()"];

PythonRestart[] := Module[{path},
  path = If[StrQ @ $PythonPath, $PythonPath <> ":" <> DataPath["Python"]];
  DPrint["Initializing $PythonSession with ", path];
  SetEnvironment["PYTHONPATH" -> path];
  If[HasIValueQ[$PythonSession], Quiet @ DeleteObject[$PythonSession]];
  $TorchInitialized = False;
  $PythonSession = StartExternalSession[
    addBestEval @ $PythonSessionParameters
  ]
];

addBestEval[params_] :=
  Join[params, KeyDrop["Registered"] @ First[PythonEvaluators["Registered" -> True], Assoc[]]];

(*************************************************************************************************)

DeclareHoldFirst[CustomExternalEvaluateCatch];

CustomExternalEvaluateCatch[expr_] := Catch[ExternalEvaluatePostProcessor @ expr, "__externalevaluate__"];

ExternalEvaluatePostProcessor[f_Failure] := $PythonErrorCallback @ f;
ExternalEvaluatePostProcessor[e_]        := $PythonResultPostProcessor @ e;

RegisterPackagePatchFunctions["ExternalEvaluate`",
"OverrideDefaultExternalSession" -> Function[
  ExternalEvaluateCommon`ExternalEvaluateCatch[expr_] := CustomExternalEvaluateCatch[expr];
  ExternalEvaluate`Private`$SessionNormalizationRules //= ReplaceAll[$sessionNormUpdate];
  ExternalEvaluate`GetDefaultExternalSession["Python"] := DefaultPythonSession[]
]];

$sessionNormUpdate = HoldP[s:Switch[#Name, ___]] :> If[StringQ[#Name] && StringQ[#Evaluator], {#Name, #Evaluator}, s];

(*************************************************************************************************)

RegisterDynamicAliasFunction["P`", SymbolNameSetDelayed[#1, PythonVariable[#3]]&];

PythonVariable[name_String] := Block[{$NewSymbol, res},
  res = ExternalEvaluate[$PythonSession, name];
  If[FailureQ[res], None, res]
];

(*************************************************************************************************)

PrintPythonFile[path_String] := Locals[
  cell = makeDynamicPythonCodeCell[path];
  If[FailureQ[cell], ReturnFailed[]];
  preprocessPython @ First @ cell;
  next = NextCell[];
  (* unfortunately, cells that are Evaluatable ignore CellAutoOverwrite *)
  If[MatchQ[NotebookRead[next], Cell[___, CellAutoOverwrite -> True, ___]],
    NotebookDelete[next]];
  CellPrint @ cell;
  printed = First @ Cells[CellTags -> path];
  SelectionMove[printed, All, CellContents];
  SelectionEvaluateCreateCell[EvaluationNotebook[]];
];

makeDynamicPythonCodeCell[path_] := Locals[
  str = preprocessPython @ ImportUTF8[path];
  If[!StringQ[str], ReturnFailed[]];
  str //= StringTrim;
  str = StringJoin["\<", ToString[str], "\>"];
  With[{date = UnixTime @ FileDate @ path},
  Cell[str,
    "ExternalLanguage",
    CellAutoOverwrite -> True,
    CellTags -> path,
     CellFrameColor -> RGBColor[0.7, 0.5, 0.5],
    CellEventActions -> {{"MenuCommand","Save"} :> saveCurrentCell[EvaluationCell[], path]},
    CellDynamicExpression :> Refresh[
      If[UnixTime[FileDate @ path] > date,
      NotebookWrite[EvaluationCell[], makeDynamicPythonCodeCell @ path]],
      UpdateInterval -> 1]
  ]]
];

saveCurrentCell[cell_CellObject, path_String] := Locals[
  str = NotebookRead[cell];
  ExportUTF8[path, First @ str];
  Beep[];
];

(*************************************************************************************************)

(*f`ExternalEvaluate`Private`$LanguageInformations["Python"]//Keys*)

PythonRun[cmd_String] := ExternalEvaluate[$PythonSession, cmd];

(*************************************************************************************************)

PythonResult /: Normal[PythonResult[res_]] := res;

compactPythonResultBoxes = CaseOf[
  o_ExternalObject |
  o_ExternalFunction             := o["Command"];
  f_Failure                      := compactPythonFailureBoxes @ f;
  e:{__TorchTensor}              := RowBox @ Riffle[smallTensors @ e, " "];
  e_TorchTensor                  := smallTensors @ e;
  e_                             := Which[
    AtomQ[e] && ByteCount[e] < 20, e,
    LeafCount[e] < 20,             CodePaneBoxes[e],
    True,                          Head[e]
  ];
];

compactPythonFailureBoxes[f_] :=
  NiceObjectBoxes[
    StyleBox[f["FailureCode"], "Message"],
    Construct[NicePasterBoxes,
      StyleBox[ToBoxes @ First @ f["StyledMessage"], FontWeight -> Bold, ShowStringCharacters -> True],
      formatTraceback @ f["Traceback"]
    ]
  ]

smallTensors[expr_] := ReplaceAll[expr,
  TorchTensor[na_NumericArray] :>
    ToBoxes @ SmartArrayPlot[Normal @ na, PixelConstrained->{2,4},ImageSize->{50,50}]
];

errorBoxes[e_] := StyleBox[RowBox[{"\[LeftSkeleton]", e, "\[RightSkeleton]"}], $Red];

MakeBoxes[PythonResult[e_], StandardForm] :=
  NiceObjectBoxes["PythonResult", compactPythonResultBoxes @ e];

(*************************************************************************************************)

PythonScriptRunDynamic[path_] :=
  PythonScriptRun[path, UpdateInterval -> 1];

Options[PythonScriptRun] = {UpdateInterval -> None};

PythonScriptRun[path_String, OptionsPattern[]] := Locals[
  UnpackOptions[updateInterval];
  $PythonSession;
  If[!FileExistsQ[path], ReturnFailed[]];
  Module[{var = runScriptData[path, updateInterval, 0]},
    If[updateInterval === None, Construct[PythonScriptResult, var], PythonScriptResult[var]]
  ]
];

preprocessPython[code2_String] := Locals[
  If[StringContainsQ[code2, {"import torch", "from torch"}], InitializeTorch[]];
  code = code2;
  code //= StringReplace["namedtuple(" -> "wlnamedtuple("];
  code //= StringReplace[StartOfLine ~~ "exit(" ~~ ___ -> ""];
  code = StringTrimRight[code, Whitespace];
  last = Max @ StringPosition[code, StartOfLine];
  If[StringMatchQ[StringTake[code, {last, last}], WordCharacter | "{" | "("],
    code = StringInsert[code, "__final = ", last];
  ];
  code
];

(*************************************************************************************************)

(* TODO: check python syntax *)
runScriptData[path_, updateInterval_, runCount_] := Locals[
  If[!FileExistsQ[path], ReturnFailed[]];
  fileTime = UnixTime @ FileDate @ path;
  startTime = SessionTime[];
  dynamic = updateInterval =!= None;
  $path = path;
  tmpPath = NewTemporaryFilename @ StrJoin[Base36Hash @ path, ".py"];
  ExportUTF8[tmpPath, preprocessPython @ ImportUTF8 @ path];
  notebook = Lookup[$PathToNotebookCache, path, None];
  If[notebook =!= None, NotebookDelete @ Cells @ notebook];
  Block[{$PythonPrintCallback = PythonScriptPrint[path]},
    result = ExternalEvaluate[$PythonSession, "runpy.run_path('" <> tmpPath <> "', init_globals=globals()).get('__final', None)"];
    If[FailureQ[result],
      result = ReplaceAll[result, s_String :> RuleCondition @ StringReplace[s, tmpPath -> path]];
      $PythonPrintCallback[result]];
  ];
  endTime = SessionTime[];
  runTime = endTime - startTime;
  PackAssociation[
    fileTime, startTime, endTime, runTime,
    updateInterval, path, dynamic, result, runCount
  ]
];

DeclareHoldFirst[PythonScriptPrint];

SetInitial[$PathToNotebookCache, UAssoc[]];

PythonScriptPrint[path_String][args___] := Module[{cell, notebook},
  cell = argsToPrintCell[args];
  notebook = Lookup[$PathToNotebookCache, path, None];
  If[notebook === None || Options[notebook] === $Failed,
    $PathToNotebookCache[path] = CreateDocument[cell,
      Saveable -> False, WindowTitle -> path,
      WindowSize -> {1000, Scaled[1]},
      WindowMargins -> {{Auto, 50}, {Auto, Auto}}
    ];
  ,
    NotebookWrite[notebook, cell, After]
  ]
];

argsToPrintCell[f_Failure] := Cell[
  BoxData @ ToBoxes @ formatTraceback @ f["Traceback"], "Print",
  Background -> Darker[$DarkRed, .4]
];

argsToPrintCell[args___] := Cell[BoxData @ RowBox[ToBoxes /@ {args}], "Output"];
argsToPrintCell[args___] := Cell[BoxData @ RowBox[ToBoxes /@ {args}], "Output"];

(*************************************************************************************************)

formatTraceback[expr_] := Locals[
  e = expr;
  e //= ReplaceAll[$tracebackColorRules];
  e //= ReplaceAll[$tracebackFinalRules];
  (* e //= MapAt[Select[notInternalFrameQ], {1,2,1,1}]; *)
  e
];

$tracebackColorRules = {
  RGBColor[1, 1, 0.85] -> $DarkRed,
  GrayLevel[0.95] -> GrayLevel[0.2],
  GrayLevel[1] -> GrayLevel[0],
  GrayLevel[0.85] -> GrayLevel[0]
}

$tracebackFinalRules = {
  Column[b___] :> Column[b /. SystemOpen -> SublimeOpen, BaseStyle -> {FontColor -> GrayLevel[0.8]}]
};

notInternalFrameQ[e_] := FreeQ[e, s_String /; StringContainsQ[s, $InstallationDirectory | "runpy"]];

(*************************************************************************************************)

DeclareHoldFirst[PythonScriptResult, scriptResultObjectBoxes];

PythonScriptResult[var_][field_] := Lookup[var, field];

PythonScriptResult /: Normal[PythonScriptResult[var_]] := var["Result"];

MakeBoxes[PythonScriptResult[var_], StandardForm] :=
  scriptResultObjectBoxes[var];

scriptResultObjectBoxes[var_] := Locals[
  If[!AssociationQ[var], Return @ $invalidScriptBoxes];
  argBoxes = runSummaryArgsFn @ var;
  UnpackAssociation[var, dynamic, path, fileTime, updateInterval];
  If[dynamic, argBoxes = makeDynamicScriptBox[
    Hold[var], path, fileTime, updateInterval, argBoxes
  ]];
  NiceObjectBoxes["PythonScriptResult", argBoxes]
];

$invalidScriptBoxes := NiceObjectBoxes["PythonScriptResult", errorBoxes @ "invalid"];

makeDynamicScriptBox[Hold[var_], path_, fileTime_, interval_, argBoxes_] :=
  DynamicModuleBox[{curBoxes = argBoxes, curFileTime = fileTime, runCount = 1},
    DynamicBox[
      If[UnixTime[FileDate @ path] > curFileTime,
        var = runScriptData[path, rate, runCount++];
        curFileTime = var["FileTime"];
        curBoxes = runSummaryArgsFn @ var;
      ];
      curBoxes,
      UpdateInterval -> interval,
      TrackedSymbols :> {}
    ]
  ];

runSummaryArgsFn = Function @ GridBox[{
  {fieldBox["file"], FileNameTake @ #Path, realign @ compactPythonResultBoxes @ #Result},
  {fieldBox["time"], RealString[#RunTime, 2] <> "s", $spanAbove},
  {fieldBox["count"], #RunCount, $spanAbove}
}, GridBoxAlignment -> {"Columns" -> {Right, Left, Right, Left}},
  GridBoxSpacings -> {"Rows" -> {{0.2}}, "Columns" -> {{.3, .3, 1}}},
  BaseStyle -> {FontSize -> 12},
  GridBoxDividers -> {"Columns" -> {False, {False, True, False}}},
  RowMinHeight -> 1.5];

realign[e_] := If[FreeQ[e, GraphicsBox], e, ItemBox[e, Alignment -> Top]];

$spanAbove = "\[SpanFromAbove]";

(*************************************************************************************************)

SetInitial[$TorchInitialized, False];

InitializeTorch[] := If[!$TorchInitialized,
  $TorchInitialized = True;
  LogPrint["Initializing PyTorch."];
  ExternalEvaluate[$PythonSession, File @ DataPath["Python", "PyTorch.py"]];
];

(*************************************************************************************************)

pyStyleBox[None][s_] := StyleBox[ToBoxes @ s, FontColor -> $DarkGreen];
pyStyleBox[hash_][s_] := StyleBox[ToBoxes @ s, FontColor -> HashToColor[Hash @ hash]];

(* CoreBoxes[PyFunction[name_String, args_List]] :=
  NiceObjectBoxes[
    pyStyleBox @ "PyFunction",
    {ItalicBox @ BraceRowBox[ToBoxes /@ args], SkeletonBox @ pyStyleBox @ name},
    .2
  ];
 *)
CoreBoxes[PyFunction[name_String, args_List]] :=
  NiceObjectBoxes[
    pyStyleBox[None] @ "PyFunction",
    {FnParenRowBox[ToBoxes @ name, ItalicBox /@ ToBoxes /@ args]},
    .2
  ];

PyFunction[name_String, _][in___] :=
  ExternalFunction[Assoc["System" -> "Python", "Session" -> $PythonSession, "Command" -> name]][in];

(*************************************************************************************************)

DeclareCoreSubBoxes[PyObject]

MakeCoreBoxes[PyObject[name_String, hash_Integer][assoc_Association]] := pyObjectBoxes[name, hash, assoc];
MakeCoreBoxes[PyObject[name_String, hash_Integer][]]                  := pyObjectBoxes[name, hash, Assoc[]];

CoreBoxes[PyObjectRef[name_String, hash_Integer]]  := RBox[SemiBoldBox @ pyStyleBox[hash] @ name, "[", "\[Ellipsis]", "]"];

pyObjectBoxes[name_, hash_, assoc_] := NiceObjectBoxes[pyStyleBox[hash] @ name, objectEntryBoxes @ assoc, .3];

fieldBox[f_, opts___Rule] := StyleBox[
  RBox[ToBoxes @ f, ":"], FontColor -> GrayLevel[.5], FontFamily -> "Source Code Sans", opts
];

objectEntryBoxes[<||>] := "";
objectEntryBoxes[assoc_Association] := Locals[
  grid = KeyValueMap[{fieldBox @ #1, ToBoxes[#2]}&, assoc];
  GridBox[
    grid,
    GridBoxDividers -> {"Rows" -> {False, {True}, False}},
    FrameStyle -> GrayLevel[0.8],
    RowMinHeight -> 1.2,
    BaseStyle -> {FontFamily -> "Source Code Pro"},
    GridBoxAlignment -> {"Rows" -> {{Baseline}}, "Columns" -> {{Right, Left}}},
    GridBoxSpacings -> {"Rows" -> {1, {.5, .5}}, "Columns" -> {0, {.5}}},
    BaselinePosition -> {{1, 2}, Baseline}
  ]
];

PyObject[name_String, _][assoc_Association][key_] := assoc[key];

(*************************************************************************************************)

namedTupleBoxes[name_, assoc_] := NiceObjectBoxes[name, assocBoxes @ assoc];

assocBoxes[<||>] := {};
assocBoxes[assoc_Association] := Locals[
  entryLists = Partition[Normal @ assoc, UpTo @ maxLen];
  If[Len[assoc] > maxLen,
    entryLists //= Map[PadRight[#, maxLen, "" -> ""]&];
  ];
  grid = Catenate @ Map[
    entries |-> Flip @ MapApply[{ToBoxes[#2], fieldBox[#1, FontSize -> 10]}&, entries],
    entryLists
  ];
  GridBox[
    grid,
    GridBoxDividers -> {"Columns" -> {False, {True}, False}},
    FrameStyle -> GrayLevel[0.8],
    RowMinHeight -> 1,
    GridBoxAlignment -> {"Rows" -> {{Baseline}}, "Columns" -> {{Right, Left}}},
    GridBoxSpacings -> {"Rows" -> {1, {.25, .5}}, "Columns" -> {1, {1.5}}}
  ]
];

(*************************************************************************************************)

SplitOnAxis[TorchTensor[na_NumericArray], i_Integer] := Locals[
  dims = Dims @ na;
  rank = Len @ dims;
  If[!(1 <= Abs[i] <= rank), ReturnFailed[]];
  alls = ConstList[All, rank];
  dim = Part[dims, i];
  Map[TorchTensor[Extract[na, ReplacePart[alls, i -> #]]]&, Range @ dim]
];

MakeBoxes[TorchTensor[na_NumericArray], StandardForm] := torchTensorBoxes[na];

torchTensorBoxes[na_] := NiceObject["Tensor", SmartArrayPlot[Normal @ na]];
torchTensorBoxes[na_] := ToBoxes @ SmartArrayPlot[Normal @ na];

tensorData[TorchTensor[na_NumericArray]] := Normal @ na;
flatTensorData[TorchTensor[na_NumericArray]] := Flatten @ Normal @ na;

TorchTensor /: Normal[t_TorchTensor] := tensorData @ t;

TorchTensor /: Part[TorchTensor[na_], p___] := TorchTensor[Part[na, p]];

setupNormalizing1[head_] :=
  TagSetDelayed[TorchTensor, head[t_TorchTensor, rest___], head[tensorData @ t, rest]];

Scan[setupNormalizing1, {Normal, SmartArrayPlot, CompactArrayPlot, Min, Max, MinMax}];

setupFlatNormalizing1[head_] :=
  TagSetDelayed[TorchTensor, head[t_TorchTensor, rest___], head[flatTensorData @ t, rest]];

(* Scan[setupFlatNormalizing1, {Histogram}]; *)


(*
externalValueFast[session_, input_String] := Lookup[1] @ ExternalEvaluate`Private`externalEvaluateLink[
  "Message", session,
  {1 -> Association["input" -> input,"constants" -> Association[], "return_type"->"expression"]},
  Association[#1]&,
  BinarySerialize,BinaryDeserialize
]
 *)