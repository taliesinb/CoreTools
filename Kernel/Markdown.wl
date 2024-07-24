PackageExports[
  "IOFunction",
    ParseMarkdownFile,
    ParseMarkdownString,
    ExportToJupyter,
    ExportToLiterateHaskell,
    ExportToNotebook,
    ExportToMarkdown,
    RenumberNumberedBlocks,
  "SymbolicHead",
    ParagraphBlock,
    TitleBlock,
    NumberedBlock,
    ItemBlock,
    MathBlock,
    CodeBlock,
    ImageBlock,
    IndentBlock,
    InlineMathBlock,
    InlineCodeBlock,
    DecorationBlock,
  "OptionSymbol",
    ForbiddenBlocks,
    GlobalStringReplacements,
    BlockRewriteRules,
    RenumberItems,
  "Predicate",
    ValidMarkdownBlocksQ
];

(*************************************************************************************************)

toBlocksList = CaseOf[
  File[file_]   := checkBlocks @ postProcBlocks @ ParseMarkdownFile @ file;
  blocks_       := checkBlocks @ postProcBlocks @ Flatten @ ToList @ blocks;
];

postProcBlocks[blocks2_] := Locals[
  blocks = Take[blocks2, UpTo @ $maxItems];
  If[$forbiddenBlocks =!= {}, blocks //= Discard[toMatcher @ $forbiddenBlocks]];
  If[$renumberItems, blocks //= renumberBlocks];
  If[$blockRewriteRules =!= {}, blocks //= Map[ReplaceAll @ $blockRewriteRules]];
  blocks
];

toMatcher[p_] := MatchQ[p];
toMatcher[s_Symbol] := MatchQ[Blank @ s];
toMatcher[l_List] := OrOp @@ Map[toMatcher, l];

$maxItems = Infinity;
$renumberItems = True;
$blockRewriteRules = {};

renumberBlocks[blocks_] := Locals[
  $n = 1;
  blocks /. NumberedBlock[_, s_] :> NumberedBlock[$n++, s]
];

(*************************************************************************************************)

General::invalidMarkdownBlock = "Not a valid markdown block: ``.";
checkBlocks[b_] :=
  Catch[testBlocks @ b; b, testBlocks, ThrowErrorMessage["invalidMarkdownBlock", #1]&];

ValidMarkdownBlocksQ[b_] :=
  Catch[testBlocks @ b; True, testBlocks, False&];

testBlocks = CaseOf[
  l_List := Scan[testBlocks, l];
  ParagraphBlock[_Str]          := Null;
  TitleBlock[_Int, _Str]        := Null;
  NumberedBlock[_Int, s_Str]    := Null;
  ItemBlock[_Str]               := Null;
  MathBlock[_Str]               := Null;
  CodeBlock[_Str | Auto, _Str]  := Null;
  ImageBlock[_Str]              := Null;
  IndentBlock[_Str, b_]         := Scan[$$, b];
  DecorationBlock["HorizontalLine"] := Null;
  e_ := Throw[e, testBlocks];
];

(*************************************************************************************************)

$commonExportOptions = {
  MaxItems          -> Infinity,
  ForbiddenBlocks   -> {},
  TabSpacings       -> 2,
  RenumberItems     -> True,
  BlockRewriteRules -> {}
};

Options[ExportToJupyter] = Append[$commonExportOptions, GlobalStringReplacements -> None];

getJupyterTemplate[filename_] := getJupyterTemplate[filename] =
  ReadRawJSONFile @ DataPath["Jupyter", filename];

jupyterTemplate[filename_, args___] :=
  getJupyterTemplate[filename] //
    ReplaceAll[{args}] //
    ReplaceAll[{"StringJoin", s___} :> RuleCondition @ StringJoin[s]];

ExportToJupyter[file_, blockData_, OptionsPattern[]] := Locals @ CatchError[ExportToJupyter,
  UnpackOptions[
    globalStringReplacements,
    $tabSpacings, $maxItems, $forbiddenBlocks, $renumberItems, $blockRewriteRules
  ];
  blocks = toBlocksList @ blockData;
  If[renumberItems, blocks //= RenumberNumberedBlocks];
  blocks //= mergeTextBlocks;
  cells = Flatten @ Map[blocksToJupyter] @ blocks;
  json = jupyterTemplate["Notebook.ipynb", "#cells" -> cells];
  jsonStr = WriteRawJSONString[json, "Compact" -> 4];
  If[!StringQ[jsonStr], ReturnFailed[]];
  jsonStr //= katexToMathJax;
  If[globalStringReplacements =!= None,
    jsonStr //= StringReplace[globalStringReplacements]];
  Switch[file,
    _Str,
      If[!StrEndsQ[file, ".ipynb"], ReturnFailed[]];
      ExportUTF8[file, jsonStr],
    None,
      jsonStr,
    _,
      $Failed
  ]
];

$textBlockP = Alternatives @@ Map[Blank,
  {ParagraphBlock, NumberedBlock, ItemBlock, MathBlock}];

freeOfInitBold[e_] := FreeQ[e, StrStartsP["**"]];

mergeTextBlocks[blocks_] :=
  SequenceReplace[blocks, {p:Repeated[$textBlockP ? freeOfInitBold, {2, 10}]} :> GroupedBlocks[{p}]]

blocksToJupyter = CaseOf[

  CodeBlock[Auto, str_String] := $$ @ CodeBlock[guessLang @ str, str];

  CodeBlock["Text", str_String] := jupyterTemplate["markdown.json",
    "#lines" -> Join[{"```\n"},StringSplitAfter[str, "\n"], {"\n```\n"}]
  ];

  CodeBlock["Python", str_String] := jupyterTemplate["code.json",
    "#hash" -> Base36Hash[str],
    "#lines" -> StringSplitAfter[str, "\n"]
  ];

  ImageBlock[path_Str] := Locals[
    img = Import @ path;
    width = IntStr @ Round[First[ImageDimensions @ img] * 1/2];
    encoded = encodeImageToBase64 @ img;
    jupyterTemplate["image.json", "#width" -> width, "#encoded" -> encoded]
  ];

  IndentBlock[title_String, cells_] := Splice @ Map[$$] @ Flatten @ {
    TitleBlock[4, title],
    cells
  };

  GroupedBlocks[list_List] := jupyterTemplate["markdown.json",
    "#lines" -> Flatten @ Riffle[toMdLines /@ list, {"\n\n"}]
  ];

  block_ := jupyterTemplate["markdown.json",
    "#lines" -> toMdLines @ block
  ];

];

katexToMathJax[s_Str] := StringReplace[s, $katexToMathJaxRules];

$katexToMathJaxRules = {
  "\\\\gdef"  -> "\\\\def",
  "\\\\bm"    -> "\\\\boldsymbol",
  "\\\\reals" -> "\\\\mathbb{R}"
}

encodeImageToBase64[img_] := EncodeBase64 @ ExportString[img, "PNG",
  CompressionLevel -> 1, "ColorMapLength" -> 16, IncludeMetaInformation -> False
];

(*************************************************************************************************)

guessLang[s_String] := Which[
  StringContainsQ[s, StartOfLine ~~ "def "], "Python",
  True,                                      "Text"
];

(*************************************************************************************************)

trimEmptyLines[s_Str] := StringReplace[s, Regex["\n\\s*\n"] -> "\n"];

(*************************************************************************************************)

Options[ExportToMarkdown] = $commonExportOptions;

ExportToMarkdown[blockData_, OptionsPattern[]] := Locals @ CatchError[ExportToMarkdown,
  UnpackOptions[$tabSpacings, $maxItems, $forbiddenBlocks, $renumberItems, $blockRewriteRules];
  blocks = toBlocksList @ blockData;
  toMdString @ blocks
];

BlockToMarkdown[e_] := toMdElem @ e;

toMdLines[e_] := ReplaceAll[mdElem -> StringJoin] @ Map[toMdElem] @ ToList @ e;

toMdString[e_] := StringJoin @ Riffle[toMdLines @ e, "\n\n"];

toMdElem = CaseOf[
  ParagraphBlock[s_Str]       := mdElem[s];
  TitleBlock[i_Int, s_Str]    := mdElem[StringRepeat["#", i], " ", s];
  NumberedBlock[i_, s_Str]    := mdElem[IntStr @ i, ". ", s];
  ItemBlock[s_Str]            := mdElem["* ", s];
  MathBlock[s_Str]            := mdElem["$$\n", s, "\n$$"];
  CodeBlock[l_Str, s_Str]     := mdElem["```\n", s, If[StrEndsQ[s, "\n"], "", "\n"], "```"];
  ImageBlock[path_Str]          := mdElem["IMAGE"];
  IndentBlock[title_Str, blocks_]  := mdElem["- ", title, "\n", indent4 @ toMdString @ blocks];
  DecorationBlock["HorizontalLine"] := "---";
];

indent4[s_Str] := StringRiffle[StringPrepend["    "] @ StringSplit[s, EndOfLine], "\n"];

(*************************************************************************************************)

Options[ExportToNotebook] = $commonExportOptions;

ExportToNotebook[blockData_, OptionsPattern[]] := Locals @ CatchError[ExportToNotebook,
  UnpackOptions[$tabSpacings, $maxItems, $forbiddenBlocks, $renumberItems, $blockRewriteRules];
  blocks = toBlocksList @ blockData;
  cells = Flatten @ blocksToNotebookCells @ blocks;
  NotebookPut @ Notebook @ cells
];

blocksToNotebookCells = CaseOf[
  list_List                   := Map[$$, list];
  ParagraphBlock[s_Str]       := Cell[toText @ s, "Text"];
  TitleBlock[i_Int, s_Str]    := Cell[toText @ s, Switch[i, 1, "Section", 2, "Subsction", 3, "Subsubsection", 4, "Subsubsubsection"]];
  NumberedBlock[_, s_Str]     := Cell[toText @ s, "ItemNumbered"];
  ItemBlock[s_Str]            := Cell[toText @ s, "Item"];
  MathBlock[s_Str]            := $$ @ CodeBlock["Latex", s];
  CodeBlock[Auto, s_Str]      := Cell[s, "ExternalLanguage"];
  CodeBlock[l_Str, s_Str]     := Cell[s, "ExternalLanguage", CellEvaluationLanguage -> ToUpperCase1[l]];
  ImageBlock[p_Str]           := Cell[BoxData @ ToBoxes @ Import @ p, "Output"];
  IndentBlock[s_Str, t_]      := indentedGroup[s, $$ @ t];
  DecorationBlock["HorizontalLine"] := $horizontalLineCell;
];

toText[e_] := e;

indentedGroup[title_, group_] := Locals[
  cells = Flatten @ {
    Cell[BoxData @ title, "Subsubsubsection", ShowGroupOpener -> True], group
  };
  groupData = CellGroupData[cells, {1}];
  end = Cell["", "Subsubsubsection", $invisibleCell];
  {groupData, end}
];

$invisibleCell = Sequence[
  Editable -> False, Selectable -> False,
  ShowCellBracket -> False, CellMargins -> {{0, 0}, {1, 1}},
  CellElementSpacings -> {"CellMinHeight" -> 1},
  CellFrameMargins -> 0
];

$horizontalLineCell := Cell["", "Text", $invisibleCell,
  CellSize -> {Inherited, 4}, CellFrameColor -> GrayLevel[0.75]];

(*************************************************************************************************)

  (* ReplacePart[$JupyterTemplate, "cells" -> cellsData]; *)

(*************************************************************************************************)

$currentPath = None;
ParseMarkdownFile[path_Str] := Locals[
  $currentPath = FileNameDrop[path];
  content = ImportUTF8 @ path;
  If[FailureQ[content], ReturnFailed[]];
  ParseMarkdownString @ content
];

ParseMarkdownString[md_Str] :=
  parseLines @ StrRep[{"\[OpenCurlyQuote]", "\[CloseCurlyQuote]"} -> "'"] @ md;

(*************************************************************************************************)

With[{Newline = EndOfLine, LineFragment = Regex["[^\n]*"]},
  $linePatterns = {
    Shortest["$$\n" ~~ s___ ~~ "\n$$\n"] :>
      MathBlock[trimEmptyLines @ StringTrim @ s],

    Shortest["```" ~~ s___ ~~ "```"] :>
      toCodeBlock[s],

    "![" ~~ Except[{"]", "\n"}].. ~~ "](" ~~ path:Except[{")", "\n"}].. ~~ ")" :>
      ImageBlock[toRelPath @ path],

    "- " ~~ title:LineFragment ~~ Newline ~~ lines:Regex["\n(    [^\n]*\n)+"] :>
      IndentBlock[title, parseLines @ StringDelete[lines, StartOfLine ~~ "    "]],

    "- " ~~ line:LineFragment :>
      ItemBlock[line],

    h:Repeated["#", {1, 4}] ~~ " " ~~ title:LineFragment :>
      TitleBlock[StrLen[h], StrTrim @ title],

    d:(DigitCharacter..) ~~ ". " ~~ rest:LineFragment :>
      NumberedBlock[FromDigits @ d, rest],

    "---" ~~ EndOfLine :>
      DecorationBlock["HorizontalLine"]
  }
];

toRelPath[path_] := Locals[
  p = URLParse[path, "Path"];
  If[$currentPath =!= None, FileNameJoin @ ToList[$currentPath, p], p]
];

linePatternToFn[lhs_ :> rhs_] := applyStrings[StringReplace[StartOfLine ~~ lhs :> rhs]];

$lineFunction := $lineFunction = RightComposition @@ Map[linePatternToFn, $linePatterns];

f_applyStrings[s_String]  := First[f][s];
f_applyStrings[s_StrExpr] := Map[f, s];
f_applyStrings[e_]        := e;

(*************************************************************************************************)

toParagraphs[s_Str] :=
  Splice @ Map[ParagraphBlock] @ StringTrim @
  StringSplit[s, Repeated["\n", {2, Infinity}]];

Repeated["\n", {2, Infinity}]
parseLines[e_] := Locals[
  chunks = strExprToList @ $lineFunction @ e;
  chunks //= Map[applyStrings @ toParagraphs];
  chunks //= DeleteCases[ParagraphBlock[""]];
  chunks
];

strExprToList[s_StrExpr] := List @@ s;
strExprToList[s_Str] := List @ s;

(*************************************************************************************************)

toCodeBlock[str_Str] := Locals[
 If[StringStartsQ[str, LetterCharacter],
    {lang, code} = StringSegmentBefore[str, "\n"];
    lang //= ToUpperCase1;
  ,
    lang = Automatic;
    code = str;
  ];
  CodeBlock[lang, trimCode @ code]
];

trimCode[s_] := StrTrimLR[replaceTabs @ s, (WhitespaceCharacter... ~~ "\n").., Whitespace];

$tabSpacings = 2;
makeSpaces[n_] := makeSpaces[n] = StringRepeat[" ", n];
replaceTabs[s_] := StringReplace[s, "\t" -> makeSpaces[$tabSpacings]];