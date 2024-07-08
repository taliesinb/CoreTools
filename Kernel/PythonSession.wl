SystemExports[
  "Variable",
    $PythonSession, $PythonSessionParameters,
  "Head",
    PythonVariable,
  "IOFunction",
    PythonRestart, PythonRun, PythonScriptRun, PythonScriptRunDynamic, PythonRestart, PrintPythonFile,
  "Head",
    TorchTensor, NamedTuple, PythonScriptResult, PythonResult,
  "IOFunction",
    SetupTorchSerialization, PythonSessionPrint, PythonSessionError,
  "Function",
    SplitOnAxis
];

PrivateExports[
  "SpecialVariable",
    $TorchInitialized,
    $PythonPrintCallback,
    $PythonErrorCallback,
    $PathToNotebookCache
];

(*************************************************************************************************)

RegisterPackagePatchFunctions["ExternalEvaluate`",
"OverrideDefaultExternalSession" -> Function[
  ExternalEvaluate`GetDefaultExternalSession["Python"] := $PythonSession;
]];

RegisterDynamicAliasFunction["P`", SymbolNameSetDelayed[#1, PythonVariable[#3]]&];

(*************************************************************************************************)

PythonSessionPrint[e___] := $PythonPrintCallback[e];
PythonSessionError[e____] := $PythonErrorCallback[e];

SetInitial[$PythonPrintCallback, LogPrint];
SetInitial[$PythonErrorCallback, ErrorPrint[#1]&];

$prolog =
"from wolframclient.language import wl
from wolframclient.language.side_effects import wl_side_effect
def wlPrint(*args):
  wl_side_effect(wl.PythonSessionPrint(*args))
print = wlPrint

def is_namedtuple(obj):
  return isinstance(obj, tuple) and hasattr(obj, '_fields')

from wolframclient.serializers import wolfram_encoder
from wolframclient.language import wl
from wolframclient.serializers import export

from collections import namedtuple, OrderedDict
def wlnamedtuple(name, *args):
  x = namedtuple(name, *args)
  @wolfram_encoder.dispatch(x)
  def named_tuple_encoder(serializer, t):
    return serializer.encode(wl.NamedTuple(name)(t._asdict()))
  return x

import runpy
"

SetInitial[$PythonSessionParameters, Assoc[
  "Name"->"DefaultPythonSession", "System" -> "Python",
  "StandardOutputFunction" -> PythonSessionPrint,
  "StandardErrorFunction" -> PythonSessionError,
  "SessionProlog" -> $prolog
]];

SetDelayedInitial[$PythonSession, PythonRestart[]];

PythonRestart[] := (
  LogPrint["Initializing $PythonSession."];
  If[SymbolImmediateValueQ[$PythonSession], Quiet @ DeleteObject[$PythonSession]];
  $TorchInitialized = False;
  $PythonSession = StartExternalSession[$PythonSessionParameters]
);

(*************************************************************************************************)

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

PythonRun[cmd_String] := PythonResult[ExternalEvaluate[$PythonSession, cmd]];

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

fieldBox[f_, opts___Rule] := StyleBox[f, FontColor -> GrayLevel[.5], FontFamily -> "Source Code Sans", opts];

(*************************************************************************************************)

SetInitial[$TorchInitialized, False];

InitializeTorch[] := If[!$TorchInitialized,
  $TorchInitialized = True;
  LogPrint["Initializing PyTorch."];
  ExternalEvaluate[$PythonSession, File @ DataPath["Python", "PyTorch.py"]];
];

(*************************************************************************************************)

MakeBoxes[NamedTuple[name_String][assoc_Assoc], StandardForm] := namedTupleBoxes[name, assoc];

namedTupleBoxes[name_, assoc_] := NiceObjectBoxes[name, assocBoxes @ assoc];

assocBoxes[assoc_Association, maxLen_:2] := Locals[
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
    GridBoxAlignment -> {"Rows" -> {{Top}}, "Columns" -> {{Left}}},
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