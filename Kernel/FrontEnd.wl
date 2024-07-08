SystemExports[
  "SpecialFunction",
    AvailableWindowSize, AvailableScreenSize, SetDefaultStylesheet,
  "SpecialVariable",
    $InstallationStyleSheetsDirectory,
    $UserStyleSheetsDirectory,
    $CoreStyleSheetStyleNames,
    $DefaultStyleSheetStyleNames
];

PackageExports[
  "IOFunction",
    InstallCoreToolsStyleSheets,
    DisableFloatingSymbolPopup,
    PrintNextCellBoxData, PrintPreviousCellBoxData,
    NextCellOptionsData, PreviousCellOptionsData,
    StyleRules, GetStyleSheet, StyleSheetData, PrimaryStyleData
];

(*************************************************************************************************)

$InstallationStyleSheetsDirectory = PathJoin[$InstallationDirectory, "SystemFiles", "FrontEnd", "StyleSheets"];
$UserStyleSheetsDirectory = PathJoin[$UserBaseDirectory, "SystemFiles", "FrontEnd", "StyleSheets"];
$CoreToolsStyleSheetsDirectory = PathJoin[$CoreToolsRootPath, "StyleSheets"];
$InstalledCoreToolsStyleSheetsDirectory = PathJoin[$UserStyleSheetsDirectory, "CoreTools"];

InstallCoreToolsStyleSheets[] := Locals[
  sourcePath = $CoreToolsStyleSheetsDirectory;
  targetPath = $InstalledCoreToolsStyleSheetsDirectory;
  If[!FileExistsQ[targetPath], CreateDirectory @ targetPath];
  Map[
    CopyFile[#, PathJoin[targetPath, FileNameTake @ #], OverwriteTarget -> True]&,
    FileNames["*.nb", sourcePath]
  ]
];

SetDefaultStylesheet[Automatic] := (
  CurrentValue[DefaultStyleDefinitions] = "Default.nb";
  SetOptions[EvaluationNotebook[], StyleDefinitions -> "Default.nb"];
);

SetDefaultStylesheet[name:"DarkMode"|"LightMode"] := Locals[
  If[!FileExistsQ[$InstalledCoreToolsStyleSheetsDirectory],
    InstallCoreToolsStyleSheets[]];
  fileName = StrJoin["CoreTools/", name, ".nb"];
  nb = GetStyleSheet[fileName];
  If[Head[nb] =!= Notebook, ReturnFailed[]];
  CurrentValue[$FrontEnd, DefaultStyleDefinitions] = fileName;
  SetOptions[EvaluationNotebook[], StyleDefinitions -> fileName];
];

DisableFloatingSymbolPopup[] := (
  SetOptions[$FrontEnd, CodeAssistOptions -> {"FloatingElementEnable" -> False}]
);

(*************************************************************************************************)

GetStyleSheet[name_] := GetStyleSheet[name] = System`Convert`CommonDump`GetStyleSheet[name];

(*************************************************************************************************)

$CoreStyleSheetStyleNames = {
  "Input", "InputOnly", "Output",
  "Code", "InitializationCell", "ExternalLanguage", "ExternalLanguageDefault",
  "Print", "PrintTemporary",
  "Echo", "EchoBefore", "EchoAfter", "EchoTiming",
  "Message", "MSG"
};

$DefaultStyleSheetStyleNames = {
  "Title", "Subtitle", "Subsubtitle",
  "Chapter", "Subchapter",
  "Section", "Subsection", "Subsubsection", "Subsubsubsection", "Subsubsubsubsection",
  "Text", "SmallText", "CodeText",
  "Item", "ItemParagraph", "Subitem", "SubitemParagraph", "Subsubitem", "SubsubitemParagraph",
  "ItemNumbered", "SubitemNumbered", "SubsubitemNumbered",
  "InlineFormula", "DisplayFormula", "DisplayFormulaNumbered", "DisplayFormulaEquationNumber",
  "Program",
  "Reference", "Author", "Affiliation", "Abstract"
};

StyleSheetData[name_, stylePatt_] := Assoc @ Occurences[
  GetStyleSheet[name],
  Cell[StyleData[styleName:stylePatt], rules___] :> Rule[styleName, Assoc @ rules]
];

PrimaryStyleData[] := PrimaryStyleData[] = Join[
  StyleSheetData["Core.nb", Alternatives @@ $CoreStyleSheetStyleNames],
  StyleSheetData["Default.nb", Alternatives @@ $DefaultStyleSheetStyleNames]
];

PrimaryStyleData[name_Str] := Lookup[PrimaryStyleData[], name];

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

DeclareStrict[StyleRules]

StyleRules[styleName_Str] :=
  DeleteCases[ContextMenu -> _] @ AbsoluteCurrentValue[{StyleDefinitions, styleName}];

StyleRules[cell_CellObject] := Locals[
  cellData = NotebookRead[cell];
  If[Head[cellData] =!= Cell, ReturnFailed[]];
  StyleRules @ cellData
];

StyleRules[Cell[_, styleNames___String, styles:OptionsPattern[]]] := (
  inheritedStyles = Map[StyleRules, {styleNames}];
  Map[evalRD] @ Normal @ Merge[Flatten @ {styles, inheritedStyles}, mergeStyleValues]
);

evalRD[RuleDelayed[a_, b_]] := Construct[RuleDelayed, a, b];
evalRD[e_] := e;

mergeStyleValues[{Inherited..., a_, ___}] := a;
mergeStyleValues[_] := Automatic;

