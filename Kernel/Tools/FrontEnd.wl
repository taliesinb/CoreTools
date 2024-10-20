SystemExports[
  "FrontendFunction",
    AvailableWindowSize,
    AvailableScreenSize,
    UserNotebooks
];

PackageExports[
  "FrontendFunction",
    CallFrontEnd,
    CodeCellData,
    SetFrontEndDefaults,
    ToggleFrontEndDebugFlag,
    PrintInputCell, PrintOutputCell,
    PrintNextCellBoxData, PrintPreviousCellBoxData,
    DeleteNextGeneratedCells,
    ExpandNotebookTemplateBox
];

(*************************************************************************************************)

ExpandNotebookTemplateBox = CaseOf[
  tb:TemplateBox[_List, _Str] := BoxForm`TemplateBoxToDisplayBoxes[tb];
];

(*************************************************************************************************)

(* this allows MakeImage etc to work from the terminal *)
CallFrontEnd[e_] := If[$Notebooks, MathLink`CallFrontEnd[e], System`ConvertersDump`Utilities`GetFromFE[e]];

(*************************************************************************************************)

UserNotebooks[] := DelCases[Notebooks[], MessagesNotebook[]];

(*************************************************************************************************)

ToggleFrontEndDebugFlag[] := FrontEndTokenExecute["ToggleDebugFlag"];

(*************************************************************************************************)

SetFrontEndDefaults[] := SetOptions[$FrontEnd, $frontendDefaults];

$frontendDefaults = List[
  CodeAssistOptions   -> {
    "FloatingElementEnable" -> False,
    "AutoDetectHyperlinks"  -> False,
    "MaxContextMatches"     -> 10,
    "MaxGlobalMatches"      -> 10,
    "CompletionInclusions"  -> {}
  },
  "DisplayImagePixels"         -> True,
  EvaluationCompletionAction   -> "ShowTiming",
  InitializationCellWarning    -> False,
  OutputSizeLimit              -> 4048576,
  "NotebooksMenuHistoryLength" -> 50,
  "AllowDataUpdates"           -> False,
  "AllowDownloads"             -> False,
  DelimiterAutoMatching        -> False,
  InitializationCellEvaluation -> False,
  "MaxMessageCount"            -> 5,
  ShowAutoSpellCheck           -> False
];

(*************************************************************************************************)

AvailableWindowSize[] := CurrentValue[WindowSize] - {120, 10};
AvailableScreenSize[] := calculateScreenSize[];

calculateScreenSize[] := EuclideanDistance @@@ Lookup[First @ $screenInfo, "ScreenArea", {{0, 2048}, {0, 1536}}];
$screenInfo := $screenInfo = SystemInformation["Devices", "ScreenInformation"];

(*************************************************************************************************)

DeleteNextGeneratedCells[] := Locals[
  toDelete = {};
  cell = EvaluationCell[];
  i = 1;
  While[i++ < 100,
    cell = NextCell[cell];
    If[Head[cell] =!= CellObject, Break[]];
    opts = Options[cell, GeneratedCell];
    If[!ListQ[opts], Break[]];
    isGenerated = TrueQ @ Lookup[opts, GeneratedCell, False];
    If[isGenerated || shouldDelete @ NotebookRead @ cell,
      AppendTo[toDelete, cell],
      Break[]];
  ];
  NotebookDelete[toDelete];
];

shouldDelete[Cell[_, "Output" | "Print" | "Message" | {"Message", ___}, ___]] := True;
shouldDelete[_] := False;

(*************************************************************************************************)

PrintNextCellBoxData[] := printCellBoxData[NextCell[]];
PrintPreviousCellBoxData[] := printCellBoxData[PreviousCell[]];

printCellBoxData[_] := $Failed;
printCellBoxData[cell_CellObject] := Locals[
  cellData = NotebookRead[cell];
  If[Head[cellData] =!= Cell, ReturnFailed[]];
  expr = Replace[First @ cellData, BoxData[b_] :> b];
  CellPrint[ExpressionCell[expr, "Input"]];
];

(*************************************************************************************************)

CodeCellData[n_Int:1] := Locals[
  cells = Cells[CellStyle -> "ExternalLanguage"];
  cell = PartOr[cells, n, $Failed];
  If[Head[cell] =!= CellObject, ReturnFailed[]];
  cell = NotebookRead[cell];
  If[Head[cell] =!= Cell, ReturnFailed[]];
  boxes = First @ cell;
  If[StringQ[boxes], boxes, $Failed]
];

(*************************************************************************************************)

PrintInputCell[e_]        := iPrintInput @ e;
PrintInputCell[Hold[e_]]  := iPrintInput @ PrivHold @ e;
PrintInputCell[HoldC[e_]] := iPrintInput @ PrivHold @ e;

PrintOutputCell[e_]       := iPrintOutput @ e;

(*************************************************************************************************)

iPrintInput[e_]  := CellPrint @ Cell[BoxData @ MakeExprBoxes @ InputForm @ e, "Input", GeneratedCell -> False];
iPrintOutput[e_] := CellPrint @ Cell[BoxData @ MakeExprBoxes @ e, "Output", GeneratedCell -> False];

