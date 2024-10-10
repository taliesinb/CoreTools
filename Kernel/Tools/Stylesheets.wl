PackageExports[
  "Symbol",
    Dark, Light,
  "IOFunction",
    SetDefaultStylesheet,
    GetDefaultStylesheet,
    InstallCoreToolsStyleSheets,
    OpenCoreToolsStyleSheets,
    UninstallCoreToolsStyleSheets,
    GetNotebookStylesheet,
    SetNotebookStylesheet,
    StyleRules, GetStyleSheet,
    StyleSheetData, PrimaryStyleData,
  "Predicate",
    DarkModeQ
  "Variable",
    $CoreStyleSheetStyleNames,
    $InstalledCoreToolsStyleSheetsDirectory,
    $SystemStyleSheetsDirectory,
    $UserStyleSheetsDirectory,
    $CoreToolsStyleSheetsDirectory,
    $DefaultStyleSheetStyleNames
];

(*************************************************************************************************)

$SystemStyleSheetsDirectory             = PathJoin[$InstallationDirectory, "SystemFiles", "FrontEnd", "StyleSheets"];
$UserStyleSheetsDirectory               = PathJoin[$UserBaseDirectory, "SystemFiles", "FrontEnd", "StyleSheets"];
$CoreToolsStyleSheetsDirectory          = PathJoin[$CoreToolsRootPath, "StyleSheets"];
$InstalledCoreToolsStyleSheetsDirectory = PathJoin[$UserStyleSheetsDirectory, "CoreTools"];

(*************************************************************************************************)

General::coreToolsSheetsNotInstalled = "CoreTools stylesheets not installed to ``."

InstallCoreToolsStyleSheets[] := Locals @ CatchMessages[
  sourcePath = $CoreToolsStyleSheetsDirectory;
  targetPath = $InstalledCoreToolsStyleSheetsDirectory;
  If[!FileExistsQ[targetPath], CreateDirectory @ targetPath];
  If[AnyTrue[UserNotebooks[], notebookHasCoreSheetQ],
    ReturnFailed["existingSheetUsers"]];
  Map[
    CopyFile[#, PathJoin[targetPath, StringDelete[FileNameTake @ #, "CoreTools"]], OverwriteTarget -> True]&,
    FileNames["*.nb", sourcePath]
  ]
];

(*************************************************************************************************)

OpenCoreToolsStyleSheets[] := Scan[NotebookOpen, FileNames["*.nb", $CoreToolsStyleSheetsDirectory]];

(*************************************************************************************************)

UninstallCoreToolsStyleSheets[] := Locals @ CatchMessages[
  If[!DirectoryQ[$InstalledCoreToolsStyleSheetsDirectory],
    ReturnFailed["coreToolsSheetsNotInstalled", $InstalledCoreToolsStyleSheetsDirectory]];
  Quiet @ SetDefaultStylesheet["Default.nb"];
  DeleteDirectory[$InstalledCoreToolsStyleSheetsDirectory, DeleteContents -> True];
];

(*************************************************************************************************)

General::existingDefaultSheet = "The default stylesheet is set to a CoreTools stylesheet. Run SetDefaultStylesheet[None] first, then restart."
General::existingSheetUsers = "One or more notebooks already have a CoreTools stylesheet. Run SetNotebookStylesheet[All, None] first."

checkForCoreSheetUsage[] := Then[
  If[AnyTrue[UserNotebooks[], notebookHasCoreSheetQ], ThrowMsg["existingSheetUsers"]],
  If[coreSheetNameQ @ GetDefaultStylesheet[], ThrowMsg["existingDefaultSheet"]]
];

coreSheetNameQ[s_Str] := StringStartsQ[s, "CoreTools"];
coreSheetNameQ[_] := False;
notebookHasCoreSheetQ[nb_NotebookObject] := coreSheetNameQ @ GetNotebookStylesheet @ nb;

(*************************************************************************************************)

DarkModeQ[] := TrueQ[Apply[Avg, CurrentValue[Background]] < 0.5];

GetNotebookStylesheet = CaseOf[
  $[]                  := GetNotebookStylesheet @ EvaluationNotebook[];
  $[nb_NotebookObject] := fromFrontEndFile @ Lookup[Options[nb, StyleDefinitions], StyleDefinitions];
];

SetNotebookStylesheet = CaseOf[
  $[sheet_]                    := SetNotebookStylesheet[EvaluationNotebook[], sheet];
  $[All, sheet_]               := CatchMessages @ Scan[setSheet[toFrontEndSheet @ sheet], UserNotebooks[]];
  $[nb_NotebookObject, sheet_] := CatchMessages @ setSheet[toFrontEndSheet @ sheet] @ nb;
];

setSheet[sheet_][nb_] := SetOptions[nb, StyleDefinitions -> sheet];

(*************************************************************************************************)

General::unknownStyleSheet = "`` is not a recognized stylesheet spec. Using \"Default.nb\".";

toFrontEndSheet = CaseOf[
  Auto | None  := "Default.nb";
  "Default.nb" := "Default.nb";
  Dark         := $ @ "CoreTools/DarkMode.nb";
  Light        := $ @ "CoreTools/LightMode.nb";
  name_String  := toUserSheetPath @ name;
  name_        := ThrowMsg["unknownStyleSheet", name];
];

General::missingSheet = "Stylesheet named `` does not exist in ``.";
toUserSheetPath[name_] := Locals[
  path = PathJoin[$UserStyleSheetsDirectory, name];
  If[!FileExistsQ[path], ThrowMsg["missingSheet", path, $UserStyleSheetsDirectory]];
  toFrontEndFile @ name
];

fromFrontEndFile = CaseOf[
  FrontEnd`FileName[dir_List, name_String, ___Rule] :=
    StrJoin @ Riffle[ToList[dir, name], "/"];
  str_String := str;
];

toFrontEndFile = CaseOf[
  Automatic := "Default.nb";
  $[path_String /; StringContainsQ[path, "/"]] := Locals[
    pathElems = StringSplit[path, "/"];
    Make[FrontEnd`FileName,
      Most @ pathElems, Last @ pathElems,
      CharacterEncoding -> "UTF-8"
    ]
  ];
  str_Str := str;
];

(*************************************************************************************************)

SetDefaultStylesheet::sheetChangeRequiresRestart = "Restart Mathematica to observe changes to default stylesheet to ``.";
SetDefaultStylesheet::noSheetChange = "Default stylesheet already set to target value ``.";
SetDefaultStylesheet[sheet_] := Locals @ CatchMessages[
  newDefault = toFrontEndSheet @ sheet;
  currentDefault = CurrentValue[$FrontEnd, DefaultStyleDefinitions];
  If[currentDefault === newDefault,
    Message[SetDefaultStylesheet::noSheetChange, fromFrontEndFile @ newDefault]
  ,
    Message[SetDefaultStylesheet::sheetChangeRequiresRestart, fromFrontEndFile @ newDefault];
    CurrentValue[$FrontEnd, DefaultStyleDefinitions] = newDefault;
  ];
];

GetDefaultStylesheet[] := fromFrontEndFile @ CurrentValue[$FrontEnd, DefaultStyleDefinitions];

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

StyleSheetData[name_, stylePatt_] := Dict @ Occurrences[
  GetStyleSheet[name],
  Cell[StyleData[styleName:stylePatt], rules___] :> Rule[styleName, Dict @ rules]
];

PrimaryStyleData[] := PrimaryStyleData[] = Join[
  StyleSheetData["Core.nb", Alternatives @@ $CoreStyleSheetStyleNames],
  StyleSheetData["Default.nb", Alternatives @@ $DefaultStyleSheetStyleNames]
];

PrimaryStyleData[name_Str] := Lookup[PrimaryStyleData[], name];

(*************************************************************************************************)

SetStrict[StyleRules]

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

