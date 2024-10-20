SystemExports[
  "FormOption",  MaxRows, MaxColumns, ItemLabels, ItemFunction, TooltipFunction, MaxWidth, MaxHeight,
  "FormHead",    NiceMulticolumn, NiceGrid, NiceGridAll, CodePane, CodeTooltip, CodeTooltipBoxes, PlainGrid
];

PackageExports[
  "BoxFunction", CodePaneBox,
  "Variable",    $CodePaneBaseStyle
];

(*************************************************************************************************)

PlainGrid::usage = "PlainGrid[data] is not smart, just applies nice formatting.";

MakeBox[PlainGrid[entries:{___List}, opts___Rule], _] :=
  Construct[plainGridBoxes, Map[MakeBox, Unevaluated @ entries, {2}], opts];

plainGridBoxes[entries_, opts___] := GridBox[
  entries, opts,
  (* ColumnAlignments -> Left, Dividers -> All,
  ItemSize -> {{Automatic, {$maxPaneH + 5}}},
  BaseStyle -> {FontFamily -> "Source Code Pro", FontSize -> 12},
  FrameStyle -> GrayLevel[0.5],
  RowSpacings -> 1, ColumnSpacings -> 1.1,
  RowMinHeight -> 1.1, RowAlignments -> Baseline,
  GridFrameMargins -> {{1.5, 1.5}, {.5, 1.5}}, *)
  FrameStyle       -> $plainGridFrameStyle,
  BaseStyle        -> {FontFamily -> "Source Code Pro", FontSize -> 12},
  GridBoxAlignment -> {"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
  GridBoxDividers  -> {"Columns" -> {{True}}, "Rows" -> {{True}}},
  GridBoxItemSize  -> {"Columns" -> {{All}},  "Rows" -> {{1.1}}},
  GridBoxSpacings  -> {
    "Columns" -> {Offset[1.05], {Offset[0.77]}, Offset[1.05]},
    "Rows"    -> {Offset[0.6],  {Offset[0.4]}, Offset[0.2]}
  }
];

$plainGridFrameStyle = GrayLevel[0.5];

(*************************************************************************************************)

CodePane::usage =
"CodePane[data] formats as data but limited to a ImageSize of {300, 200}.
CodePane[data, size] specifies maximum size.
* it uses \"Input\" style.
* it uses a FontSize of 12.
* it will make some attempt to truncate values that are extremely large such that they do not produce massive box structures.
* this truncation is done attempting to avoid how the result is displayed.
";

(* these are defaults for interactive testing, they are actually set by the grid functions during boxification *)
$paneSize = {UpTo @ 300, UpTo @ 200};

DeclaredHere[CodePane]

MakeBox[CodePane[args__], _] := CodePaneBox[args];

SetHoldC[CodePaneBox];

CodePaneBox[code_, opts___Rule] := CodePaneBox[code, $paneSize, opts];

CodePaneBox[code_, sz:Except[_Rule], opts___Rule] := Block[
  {$CoreFormatting = False}, With[
  {charLimit = Ceiling @ Max[(0.95 * getTotalChars[sz]) - 3, 5]},
  {exprBoxes = MakeTruncatedBoxes[NoEval @ code, charLimit, Max[charLimit - 5, 3], 8]},
  PaneBox[exprBoxes,
    ImageSize -> sz, BaseStyle -> {opts},
    DefaultBaseStyle -> $CodePaneBaseStyle,
    BaselinePosition -> Baseline,
    ContentPadding -> False,
    StripOnInput -> True
  ]
]];

$paneBoxPP = 4;

$CodePaneBaseStyle = {
  FontSize             -> 12,
  ShowAutoStyles       -> False,
  ShowStringCharacters -> True,
  LineSpacing          -> {1, 0},
  AutoIndent           -> True,
  AutoSpacing          -> True,
  LineIndent           -> 1,
  Hyphenation          -> False,
  FontWeight           -> "DemiBold",
  PrintPrecision       -> $paneBoxPP,
  NumberMarks          -> False,
  ShowInvisibleCharacters -> True,
  GraphicsBoxOptions   -> {ImageSize -> 100},
  StripStyleOnPaste    -> True,       (* effective? *)
  TextClipboardType    -> "InputText" (* effective? *)
};

getTotalChars[sz_] := Module[
  {c =     Floor[getMaxW[sz] / $fontW],
   r = Max[Floor[getMaxH[sz] / ($fontH + 1)], 1]},
  c * r
];

getMaxW[UpTo[{w_, _}]] := getMax @ w;
getMaxH[UpTo[{_, h_}]] := getMax @ h;
getMaxW[{w_, _}]  := getMax @ w;
getMaxH[{_, h_}]  := getMax @ h;
getMaxW[i_] := getMax[i];
getMaxH[i_] := 100;

getMax[i_Integer] := i;
getMax[UpTo[i_]] := i;
getMax[{___, i_}] := getMaxW @ i;
getMax[_] := 1000;

(*************************************************************************************************)

$cellSizeCache = Data`UnorderedAssociation[];
$fontW = 6.8; $fontH = 16;

estimateCellSize[expr_] /; MatchQ[$paneSize, {_Integer, _Integer}] := $paneSize + $cellExtraSize;

$itemDisplayFn = CodePane;
$itemFn = Identity;

estimateCellSize[$tlCell] := None;
estimateCellSize[$rawLabel[expr_]] := estimateCellSize[expr];

(* TODO: make this a smart multi-level hash that hashes function definitions *)
estimateCellSize[expr_] := Module[
  {hash = Hash @ expr, size, width},
  If[!MissingQ[size = $cellSizeCache[hash]], Return @ size];
  width = GuessInputStrLen[$itemDisplayFn @ $itemFn @ expr, PrintPrecision -> $paneBoxPP] * $fontW;
  size = If[width <= $maxPaneW,
    {width, $fontH},
    {$maxPaneW, Min[Ceiling[width / $maxPaneW] * $fontH, $maxPaneH]}
  ];
  $cellSizeCache[hash] = size + $cellExtraSize
];

(**************************************************************************************************)

CodeTooltip::usage = "CodeTooltip[data, value] formats value in a tooltip using CodePane.";

MakeBox[CodeTooltip[a_, b_, sz_:Automatic], form_] :=
  CodeTooltipBoxes[MakeBox[a, form], MakeBox[CodePane[b, sz], form], Automatic];

MakeBox[CodeTooltip[a_, None, ___], form_] := MakeBox[a, form];

(**************************************************************************************************)

CodeTooltipBoxes[exprBoxes_, None, ___] := exprBoxes;

CodeTooltipBoxes[exprBoxes_, tooltipBoxes_, size_:Automatic] := TagBox[
  TooltipBox[
    exprBoxes,
    PaneBox[tooltipBoxes,
      ImageMargins -> {{5, 5}, {5, 5}},
      ImageSize -> expandSizeSpec[size],
      Background -> GrayLevel[1]
    ],
    Alignment -> Center,
    TooltipStyle -> {Background -> GrayLevel[1], CellFrameColor -> None, CellFrame -> 0},
    TooltipDelay -> 0
  ],
  MouseAppearanceTag["Arrow"]
];

CodeTooltip::badSizeSpec = "`` is not a valid size spec.";

expandSizeSpec[Automatic]                    = {{20, 300}, {20, 200}};
expandSizeSpec[n_Integer | UpTo[n_Integer]] := {{20,   n}, {20,   n}};
expandSizeSpec[sz:{_, _}]                   := fromUpTo /@ sz;
expandSizeSpec[sz_] := (Message[CodeTooltip::badSizeSpec, sz]; expandSizeSpec[Automatic]);

fromUpTo[i_Integer | UpTo[i_Integer]] := {20, i};
fromUpTo[r:{_Integer, _Integer}] := r;
fromUpTo[sz_] := (Message[CodeTooltip::badSizeSpec, sz]; {20, 200});

(*************************************************************************************************)

NiceGridAll[data_, opts___Rule] := NiceGrid[data, MaxRows -> Infinity, opts];

NiceGrid::usage = "NiceGrid[data] formats data as a nice grid.
* data can be any combination of a square list/association of list/associations.
* data can also be a list of rules.
* data can be an list/association of pure values.
* data formats using PlainGrid.

* Splits -> Automatic splits the table up into chunks displayed left to right.

* MaxWidth, MaxHeight give the total image size to use, Automatic being the current window size.
* MaxColumns -> Automatic limits the columns to fit into image height.
* MaxRows -> Automatic limits the rows (after splitting) to fit into image width.
* MaxRows, MaxColumns can be None or Infinity to indicate no limit.

* ItemSize -> {UpTo[300], UpTo[200]} gives the size for all items.
* ItemFunction -> fn applies a function before display.

* ItemDisplayFunction -> CodePane applies the given function before final display.
";

Options[NiceGrid] = {
  MaxWidth            -> Automatic,
  MaxHeight           -> Automatic,
  MaxRows             -> Automatic,
  MaxColumns          -> Automatic,
  "Splits"            -> Automatic,
  ItemSize            -> {UpTo @ 300, UpTo @ 150},
  ItemFunction        -> None,
  ItemDisplayFunction -> Id
};

MakeBox[NiceGrid[data_, opts___Rule], StandardForm] := niceGridBoxes[data, opts];

$maxInf = 100000;

SetHoldC[niceGridBoxes];
niceGridBoxes[data_, opts___Rule] := Block[
  {maxWidth, maxHeight, maxRows, maxCols, splits, itemSize, itemFunction,
   $maxPaneW, $maxPaneH, $paneSize, $availableSize,
   rows, tuple, rowLabelTitle, rowLabels, colLabels, columns,
   numRows, rowsPerSplit,
   hdots = False, vdots = False,
   colWidths, rowMaxHeight},

  {maxWidth, maxHeight, maxRows,    maxCols,  splits , itemSize, $itemFn,      $itemDisplayFn} = OptionValue[NiceGrid, {opts},
  {MaxWidth, MaxHeight, MaxRows, MaxColumns, "Splits", ItemSize, ItemFunction, ItemDisplayFunction}];
  processItemSize @ itemSize;
  processMaxWidthHeight[maxWidth, maxHeight];
  {maxRows, maxCols} //= ReplaceAll[None|All -> $maxInf];
  If[$itemFn === None, $itemFn = Identity];

  If[splits === None || TrueQ[splits < 0], splits = 1];
  If[!MatchQ[splits, (_Integer ? Positive) | Automatic], Return[$Failed]];

  tuple = Catch @ toRowsColsEntries @ data;
  If[MatchQ[tuple, _$tableError], Return @ errorGridBox[tuple]];
  If[!ListQ[tuple], Return[$Failed]];
  {rowLabelTitle, rowLabels, colLabels, columns} = tuple;
  numRows = Length @ rowLabels;

  If[numRows === 0,
    Return @ subtableGridBoxes[rowLabelTitle, rowLabels, colLabels, columns, False, False]];

  rowsPerSplit = numRows;

  If[maxCols === Automatic || maxRows === Automatic || splits =!= 1,
    trimCols[maxCols];
    {colWidths, rowMaxHeight} = getColWidthsRowMaxHeightForColumns @ Prepend[columns, rowLabels];
    maxVisibleRows = chooseMaxRows[rowMaxHeight, maxHeight];
    If[maxRows === Automatic, maxRows                = maxVisibleRows];
    If[IntegerQ[splits],      {splits, rowsPerSplit} = chooseManualSplits[splits, numRows]];
    If[splits  === Automatic, {splits, rowsPerSplit} = chooseAutoSplits[colWidths, maxWidth, maxVisibleRows, numRows]];
    If[maxCols === Automatic, maxCols                = chooseMaxCols[colWidths, (maxWidth / splits) - (splits - 1) * $splitTableSpacing]];
  ];

  trimRows[maxRows * splits]; (* rowsPerSplit should be <= maxRows *)
  trimCols[maxCols];

  If[rowsPerSplit >= numRows,
    subtableGridBoxes[rowLabelTitle, rowLabels, colLabels, columns, hdot, vdot]
  ,
    GridBox[List @ MapThread[
      subtableGridBoxes[rowLabelTitle, #1, colLabels, #2, hdot, vdot]&,
      {partChunks[rowsPerSplit] @ rowLabels, Transpose[partChunks[rowsPerSplit] /@ columns]}
    ], RowAlignments -> Top, ColumnSpacings -> $splitTableSpacing / 14]
  ]
];

errorGridBox[$tableError[msg_]] := plainGridBoxes[
  List @ List @ ToBoxes @ Item[Style[msg, Bold], FrameStyle -> RGBColor[1, .5, .5]]
];

(*************************************************************************************************)

trimRows[n_Integer] := If[numRows > n && n > 0,
  columns   = Take[columns, All, n];
  rowLabels = Take[rowLabels, n];
  vdots     = True;
];

trimCols[n_Integer] := If[Length[columns] > n && n > 0,
  columns   = Take[columns, n];
  colLabels = Take[colLabels, n];
  hdots     = True;
];

$splitTableSpacing = 15;

$cellExtraSize = {14, 8};

chooseManualSplits[splits2_, numRows_] := Module[
  {splits = splits2, rowsPerSplit, neededRowsPerSplit},
  splits = Min[splits, numRows];
  neededRowsPerSplit = Ceiling[numRows / splits];
  rowsPerSplit = Min[neededRowsPerSplit, maxRows];
  neededSplits = Ceiling[numRows / rowsPerSplit];
  splits = Min[splits, neededSplits]; (* don't do more splits than necessary *)
  {splits, rowsPerSplit}
];

(* how many splits we actually use depends on how many splits it will take to show all rows *)
chooseAutoSplits[colWidths_, maxWidth_, maxRows_, numRows_] := Module[
  {tableWidth, maxSplits, splits, newMaxWidth, neededSplits, neededRowsPerSplit, rowsPerSplit},
  tableWidth = Total @ colWidths;
  maxSplits = max1 @ Floor[maxWidth / tableWidth];
  neededSplits = Ceiling[numRows / maxRows];
  splits = Min[maxSplits, neededSplits];
  neededRowsPerSplit = Ceiling[numRows / splits];
  rowsPerSplit = Min[maxRows, neededRowsPerSplit];
  {splits, rowsPerSplit}
];

chooseMaxRows[rowMaxHeight_, maxHeight_] :=
  max1 @ Floor[maxHeight / rowMaxHeight];

chooseMaxCols[colWidths_, maxWidth_] := Module[
  {w = 0, n}, (* n includes the labels *)
  n = LengthWhile[colWidths, (w += #) <= maxWidth&] - 1;
  max1 @ n
];

partChunks[n_][list_] := Partition[list, UpTo[n]];

(*************************************************************************************************)

subtableGridBoxes[rowLabelTitle_, rowLabels2_, colLabels2_, columns2_, hdot_, vdot_] := Block[
  {rowLabels = rowLabels2, colLabels = colLabels2, columns = columns2, rows, numRows, numCols},

  columns = fmtCells /@ columns;
  PrependTo[columns, fmtLabels @ rowLabels];

  rows = Transpose @ columns;

  If[colLabels =!= {None} && Apply[UnsameQ, colLabels],
    If[Length[rows] > 0, PrependTo[colLabels, rowLabelTitle]];
    PrependTo[rows, fmtLabels @ colLabels]
  ];

  numRows = Length @ rows; numCols = Length @ First @ rows;
  If[vdots, rows //=    Append[PadRight[{$vellCell}, numCols, SpanFromLeft]]];
  If[hdots, rows //= appendCol[PadRight[{$hellCell}, numRows, SpanFromAbove] // If[vdots, Append[$brCell], Identity]]];

  If[!VectorQ[rows, ListQ], Return @ BoxMsgBox["NiceGrid"]];
  rows = Map[MakeBox, rows, {2}];

  plainGridBoxes[rows, ColumnAlignments -> {Right, Left}]
];

appendCol[col_][rows_] := appendCol[rows, col];
appendCol[rows_, col_] := MapThread[Append, {rows, col}];

$hellCell = Item["\[Ellipsis]",         Frame -> {{True, False}, {False, False}}, Background -> None, Alignment -> Center];
$vellCell = Item["\[VerticalEllipsis]", Frame -> {{False, False}, {False, True}}, Background -> None, Alignment -> Center];
$brCell   = Item["", Background -> White, Frame -> False];

(*************************************************************************************************)

max1[n_] := Max[n, 1];
takeUpTo[n_][list_] := Take[list, UpTo @ Max[n, 1]];

(*************************************************************************************************)

getColWidthsRowMaxHeightForColumns[columns_] := Module[
  {sizeTable, colRowWidths, colRowHeights},
  tableSizes = Map[col |-> Map[estimateCellSize, sampleList[12] @ col], columns];
  {colRowWidths, colRowHeights} = Transpose[tableSizes, {2, 3, 1}];
  Ceiling @ {Max /@ colRowWidths, Max @ colRowHeights}
];

(*************************************************************************************************)

fmtLabels[a_List]  := Map[fmtLabel, a];
fmtCells[a_List]   := Map[fmtItem, a];

fmtItem[e_]        := $itemDisplayFn @ $itemFn @ e;
fmtItem[None]      := fmtSpecialItem[" \[LongDash] "];
fmtItem[_Missing]  := fmtSpecialItem[" ? "];

fmtSpecialItem[e_] := Item[Style[e, FontWeight -> Bold, ShowStringCharacters -> False, FontColor -> RGBColor[0.9, 0.6, 0.6]], Alignment -> {Center, Center}];

fmtLabel[$tlCell]               := Item["", Background -> White, Frame -> {{False, True}, {True, False}}];
fmtLabel[$rawLabel[s_]]         := grayItem @ CodePane[s, FontWeight -> Bold, ShowStringCharacters -> False];
fmtLabel[e_]                    := Module[
  {item = $itemFn @ e},
  If[$itemDisplayFn === Id, item,
  If[simpleStringQ @ item,
    grayItem @ CodePane[item, FontWeight -> Bold, FontColor -> GrayLevel[0.4], ShowStringCharacters -> False],
    grayItem @ CodePane[item, FontWeight -> Bold]
  ]]
];

simpleStringQ[""]       := False;
simpleStringQ[e_String] := StringFreeQ[e, WhitespaceCharacter | "\"" | "\\"];
simpleStringQ[e_]       := False;

grayItem[e_]       := Item[e, Background -> GrayLevel[0.96]];

(*************************************************************************************************)

{$rawLHS, $rawRHS, $rawKEY, $rawVAL} = Map[$rawLabel, {"LHS", "RHS", "KEY", "VAL"}];

rangeLabels[n_Integer] := Map[$rawLabel, Range @ n];
rangeLabels[list_List] := rangeLabels @ Length @ list;

toRowsColsEntries[df_MathTools`DataFrame] :=
  toRowsColsEntries[Keys[df] -> Values[df]];

toRowsColsEntries[{}] := $tableError["{}"];

toRowsColsEntries[<||>] := $tableError["\[LeftAssociation]\[RightAssociation]"];

(* assoc of assocs *)
toRowsColsEntries[data:{__Dict ? Developer`HoldAtomQ}] := Module[{data2, keys, cols},
  data2 = KeyUnion[data];
  keys = Keys @ First @ data2;
  cols = Lookup[data2, Key[#]]& /@ keys;
  {$tlCell, rangeLabels @ data2, keys, cols}
];

(* square matrix *)
toRowsColsEntries[data_List /; Length[Dimensions[data, 2]] == 2] :=
  {$tlCell, rangeLabels @ data, rangeLabels @ First @ data, Transpose @ data};

NiceGrid::ruleLists = "Rule of lists: different lengths `` and ``.";

(* transposed rules *)
toRowsColsEntries[keys_List -> vals_List] :=
  If[Length[keys] =!= Length[vals],
    Message[NiceGrid::ruleLists, Length @ keys, Length @ vals];
    Throw @ $Failed
  ,
    {$rawLHS, keys, {$rawRHS}, List @ vals}
  ];

(* named columns *)
toRowsColsEntries[keys_List -> vals:{__List} /; Apply[Equal, Length /@ vals]] :=
  {$tlCell, rangeLabels @ First @ vals, keys, vals};

(* rules *)
toRowsColsEntries[rules:{__Rule}] :=
  toRowsColsEntries[Keys[rules] -> Values[rules]];

(* rules with some delayed delayed: avoid evaluation *)
toRowsColsEntries[rules:{(_Rule | _RuleDelayed)..}] := Module[
  {keys, vals}, {keys, vals} = heldKeysVals @ rules;
  {$rawLHS, keys, {$rawRHS}, List @ vals}
];

heldKeysVals[rules_List] := Transpose @ Map[heldKeyValPair, rules];
heldKeyValPair[key_ -> val_] := {key, val};
heldKeyValPair[key_ :> val_] := {key, PrivHold @ val};

toRowsColsEntries[data_Dict ? Developer`HoldAtomQ] := Module[
  {keys, vals}, {keys, vals} = heldKeysVals @ Normal @ data;
  If[AllTrue[vals, ListQ] && Apply[Equal, Length /@ vals],
    {$rawKEY, keys, rangeLabels @ First @ vals, vals},
    {$rawKEY, keys, {$rawVAL}, List @ vals}
  ]
];

toRowsColsEntries[data_List] :=
  {None, rangeLabels @ data, {None}, List @ data};

NiceGrid::notMatrix = "Unknown input data shape with head ``.";
toRowsColsEntries[head_] := (Message[NiceGrid::notMatrix, Head @ head]; Throw @ $Failed);

(*************************************************************************************************)

NiceMulticolumn::usage = "NiceMulticolumn[data] tries to pick a nice number of columns on their estimated content sizes.";

Options[NiceMulticolumn] = {
  ItemSize        -> {UpTo[300], UpTo[200]},
  MaxWidth        -> Automatic,
  MaxHeight       -> Automatic,
  MaxColumns      -> 50,
  MaxRows         -> Infinity,
  ItemLabels      -> Automatic,
  ItemFunction    -> CodePane,
  TooltipFunction -> CodeTooltip
};

MakeBox[NiceMulticolumn[data_, opts___Rule], StandardForm] := RuleCondition @ niceMulticolumnBoxes[data, opts];

SetHoldC[niceMulticolumnBoxes];
niceMulticolumnBoxes[items2:ListDictP, opts___Rule] := Block[
  {items = items2,
   maxWidth, maxHeight, itemSize, $maxCols, $maxRows, itemFunction, tooltipFunction, itemLabels,
   $maxPaneW, $maxPaneH, $paneSize, $availableSize,
   len, keys, spans, meanSize, numC, numR,
   labels, rowsFunction, rows, columns, $short = False},

  {maxWidth, maxHeight, itemSize,   $maxCols, $maxRows, itemFunction, tooltipFunction, itemLabels} = OptionValue[NiceMulticolumn, {opts},
  {MaxWidth, MaxHeight, ItemSize, MaxColumns,  MaxRows, ItemFunction, TooltipFunction, ItemLabels}];
  processItemSize @ itemSize;
  processMaxWidthHeight[maxWidth, maxHeight];

  If[itemFunction === None, itemFunction = Identity];
  If[tooltipFunction === None, tooltipFunction = #1&];

  len = Length @ items;
  If[len === 0, Return @ errorGridBox @ $tableError @ If[AssociationQ[items], "{}", "\[LeftAssociation]\[RightAssociation]"]];
  If[AssociationQ @ items,
    keys = Keys @ items; items = Values @ items,
    keys = Range @ len
  ];
  spans = {All};
  Do[
    meanSize      = getMeanCellSizeForSpans[items, spans];
    {numC, numR}  = pickTableSize[meanSize];
    {spans, numR} = pickColumnSpans[len, numC, numR];
  ,
    3
  ];

  labels = Switch[itemLabels,
    None,      tooltipFunction = Identity; keys,
    Automatic, keys,
    _List,     PadRight[labels, len, None]
  ];

  rowsFunction = MapThread[tooltipFunction[itemFunction @ #1, #2]&];

  If[(len / numC) <= 2.5,
    (* switch to horizontal layout *)
    rows = Partition[rowsFunction[{items, labels}], UpTo @ numC];
    If[Length[rows] > 1, Part[rows, -1] //= padWithEmpty[numC]];

  ,
    columns = Map[span |-> rowsFunction[{Part[items, span], Part[labels, span]}], spans];
    $short = Total[Length /@ columns] < Length[items];
    columns = MapThread[Prepend, {columns, makeColTitle[First @ meanSize] /@ spans}];
    numR += 1 + Boole[$short];
    If[$short, columns //= Map[Append[$dots]]];
    columns //= Map[padWithEmpty[numR]];
    rows = Transpose @ columns;
  ];

  If[!VectorQ[rows, ListQ], Return @ BoxMsgBox["NiceMulticolumn"]];
  rows = Map[MakeBox, rows, {2}];

  plainGridBoxes[rows]
];

padWithEmpty[n_][list_] := PadRight[list, n, $emptyPane];

$emptyPane = Item["", Background -> GrayLevel[0.8]];

niceMulticolumnBoxes[___] := Fail;

(*************************************************************************************************)

makeColTitle[w_][i_ ;; j_] := fmtLabel @ $rawLabel @
  If[w < 50,
    IntegerString[i] <> "-\n" <> IntegerString[j],
    IntegerString[i] <> "-" <> IntegerString[j]
  ];

$dots = Item["\[VerticalEllipsis]", Alignment -> Center, Frame -> {{False, False}, {False, True}}];

(*************************************************************************************************)

unitRange[n_] := Range[0, n-1]/(n-1);

pickColumnSpans[n_, 1, nr2_] := Module[
  {nr = Min[nr2, n]},
  {List @ Span[1, Min[nr, n]], nr}
];

pickColumnSpans[n_, nc_, nr2_] := Module[
  {last, starts, nr = nr2, spans},
  If[nr * nc > n, nr = Max[Ceiling[n / nc], 1]];
  If[nc >= n, Return @ {Map[Span[#, #]&, Range[1,n]], nr}];
  last = (n - nr + 1);
  starts = Floor[unitRange[nc] * (last - 1) + 1];
  spans = Span[#, # + nr-1]& /@ starts;
  {spans //. $fixSpanOverlaps, nr}
];

$fixSpanOverlaps = {l___, Span[a_, b_], Span[b2_, c_], r___} /; b2 <= b :> {l, Span[a, b2-1], Span[b2, c], r};

(*************************************************************************************************)

getMeanCellSizeForSpans[list_, spans_] := Module[{sizeTable, itemWidth, itemHeight},
  sizeTable = Map[
    span |-> Map[estimateCellSize, sampleList[4] @ Part[list, span]],
    spans
  ];
  sizeTable = Transpose @ PadRight @ sizeTable;
  itemWidth = Part[sizeTable, All, All, 1];
  itemWidth = Mean[Max /@ Transpose[itemWidth]];
  itemHeight = Part[sizeTable, All, All, 2];
  itemHeight = Mean[Max /@ itemHeight];
  Ceiling @ {itemWidth, itemHeight}
];

sampleList[n_][list_] := Part[list, upTo[Length @ list, n]];

upTo[len_, 1] := {1};
upTo[len_, n_] /; len <= n := Range @ len;
upTo[len_, n_] := Floor[unitRange[n] * (len - 1) + 1];

(*************************************************************************************************)

pickTableSize[sz_] := Floor @ MapThread[Clip, {
  $availableSize / sz,
  {{1, $maxCols}, {1, $maxRows}}
}];

(*************************************************************************************************)

SetHoldA[processMaxWidthHeight];

processMaxWidthHeight[maxWidth_, maxHeight_] := Block[
  {windowSize := windowSize = AvailableWindowSize[]},
  {maxWidth, maxHeight} = {maxWidth, maxHeight} /. (All|Infinity|None) -> 999999;
  If[ maxWidth === Automatic,  maxWidth = Round @ Clip[First[windowSize],      {300, 1200}]];
  If[maxHeight === Automatic, maxHeight = Round @ Clip[Last[windowSize] * .75, {300, 1200}]];
  $availableSize = {maxWidth, maxHeight};
];

(*************************************************************************************************)

processItemSize[w:(_Integer | UpTo[_Integer])] := processItemSize[{w, UpTo @ 200}];

processItemSize[UpTo[{w_Integer, h_Integer}]] := processItemSize[{UpTo[w], UpTo[h]}];

processItemSize[sz_List] := (
  $maxPaneW = getMaxW @ sz;
  $maxPaneH = getMaxH @ sz;
  $paneSize = sz;
);

General::badItemSize = "ItemSize -> `` is invalid.";
processItemSize[_] := (
  Message[General::badItemSize, sz];
  processItemSize[{UpTo @ 300, UpTo @ 200}]
);

