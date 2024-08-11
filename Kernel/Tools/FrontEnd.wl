SystemExports[
  "IOFunction",
    AvailableWindowSize,
    AvailableScreenSize,
    UserNotebooks
];

PackageExports[
  "Symbol",
    Dark, Light,
  "IOFunction",
    CallFrontEnd,
    DisableFloatingSymbolPopup,
    PrintInputCell, PrintOutputCell,
    PrintNextCellBoxData, PrintPreviousCellBoxData
];

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

PrintInputCell[e_]       := DisableCoreBoxFormatting @ CellPrint @ ExpressionCell[e, "Input", "GeneratedCell" -> False];
PrintInputCell[Hold[e_]] := DisableCoreBoxFormatting @ CellPrint @ ExpressionCell[Defer @ e, "Input", "GeneratedCell" -> False];
PrintOutputCell[e_]      := CellPrint @ ExpressionCell[e, "Output", "GeneratedCell" -> False];
