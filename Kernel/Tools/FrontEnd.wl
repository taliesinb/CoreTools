SystemExports[
  "IOFunction",
    AvailableWindowSize,
    AvailableScreenSize,
    UserNotebooks
];

PackageExports[
  "IOFunction",
    CallFrontEnd,
    CodeCellData,
    DisableFloatingSymbolPopup,
    PrintInputCell, PrintOutputCell,
    PrintNextCellBoxData, PrintPreviousCellBoxData,
    DeleteNextGeneratedCells
x];

(*************************************************************************************************)

(* this allows MakeImage etc to work from the terminal *)
CallFrontEnd[e_] := If[$Notebooks, MathLink`CallFrontEnd[e], System`ConvertersDump`Utilities`GetFromFE[e]];

(*************************************************************************************************)

UserNotebooks[] := DelCases[Notebooks[], MessagesNotebook[]];

(*************************************************************************************************)

DisableFloatingSymbolPopup[] := (
  SetOptions[$FrontEnd, CodeAssistOptions -> {"FloatingElementEnable" -> False}]
);

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

PrintInputCell[e_]       := DisableCoreBoxFormatting @ CellPrint @ ExpressionCell[e, "Input", GeneratedCell -> False];
PrintInputCell[Hold[e_]] := DisableCoreBoxFormatting @ CellPrint @ ExpressionCell[Defer @ e, "Input", GeneratedCell -> False];
PrintOutputCell[e_]      := CellPrint @ ExpressionCell[e, "Output", GeneratedCell -> False];
