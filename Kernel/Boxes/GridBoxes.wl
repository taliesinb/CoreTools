SystemExports[
  "FormHead",
    RawGrid, RawColumn, RowGrid, ColumnGrid
];

PackageExports[
  "FormHead",
    RawGrid, RowGrid, ColGrid, TightRowGrid, TightColGrid,
    RawCol,
  "BoxFunction",
    ColumnBox, ColBox,
    GridBoxRule, RowGridBox, ColGridBox, TightRowGridBox, TightColGridBox
];

(**************************************************************************************************)

DefineAliasRules[
  RawCol  -> RawColumn,
  ColBox  -> ColumnBox,
  ColGrid -> ColumnGrid
];

(**************************************************************************************************)

GridBoxRule = CaseOf[
  $[ItemsEqual, rows_, cols_] := Make[Seq, Rule[RowsEqual, rows],      Rule[ColsEqual, cols]];
  $[ItemDivs, rows_, cols_]   := Make[Seq, Rule[RowLines, rows],       Rule[ColLines, cols]];
  $[ItemJust, rows_, cols_]   := Make[Seq, Rule[RowJust, rows],        Rule[ColJust, cols]];
  $[ItemGaps, rows_, cols_]   := Make[Seq, Rule[RowGaps, rows],        Rule[ColGaps, cols]];
  $[ItemSize, rows_, cols_]   := Make[Seq, Rule[RowHeights, rows],     Rule[ColumnWidths, cols]];
  $[Background, rows_, cols_] := Make[Seq, Rule[RowBackgrounds, rows], Rule[ColumnBackgrounds, cols]];
  $[ItemStyle, rows_, cols_]  := $[GridItemStyle, rows, cols];
  $[key_Sym, None, None]      := Rule[key, {}];
  $[key_Sym, rows_, None]     := Rule[key, {"Rows" -> rows}];
  $[key_Sym, None, cols_]     := Rule[key, {"Columns" -> cols}];
  $[key_Sym, rows_, cols_]    := Rule[key, {"Rows" -> rows, "Columns" -> cols}];
];

GridBoxRule[key_, rows_, cols_, True]  := GridBoxRule[key, cols, rows];
GridBoxRule[key_, rows_, cols_, False] := GridBoxRule[key, rows, cols];

(**************************************************************************************************)

SetForm1[RowGrid, ColGrid, TightRowGrid, TightColGrid];
SetBoxFn[RowGridBox, ColGridBox]

SystemBoxDefs[
  RowGrid[list_List, sp:NumP:1.0, opts___Rule] := RowGridBox[MapMakeBox @ list, N @ sp, opts];
  ColGrid[list_List, sp:NumP:1.0, opts___Rule] := ColGridBox[MapMakeBox @ list, N @ sp, opts];
  TightRowGrid[list_List, opts___Rule]         := TightRowGridBox[MapMakeBox @ list, opts];
  TightColGrid[list_List, opts___Rule]         := TightColGridBox[MapMakeBox @ list, opts];
];

RowGridBox[list_List, sp:NumP:1.0, opts___Rule] := GridBox[ToRowVec @ list, opts, ColGaps -> sp, $baseGridOpts];
ColGridBox[list_List, sp:NumP:0.8, opts___Rule] := GridBox[ToColVec @ list, opts, RowGaps -> sp, $baseGridOpts];

$baseGridOpts = Seq[ColJust -> Left, BLinePos -> {1,1}];

(**************************************************************************************************)

SetBoxFn[TightRowGridBox, TightColGridBox];

TightRowGridBox[list_List, opts___Rule] := RowGridBox[list, opts, $tightGridOpts];
TightColGridBox[list_List, opts___Rule] := ColGridBox[list, opts, $tightGridOpts];

$tightGridOpts = Seq[RowGaps -> 0, ColGaps -> 0, GridFrameMargins -> {{0, 0}, {0, 0}}];

(**************************************************************************************************)

SetForm1[RawCol, RawGrid]

SystemBoxDefs[
  RawCol[list_List, opts___]      := ColBox[MapMakeBox @ list, opts];
  RawGrid[{}, ___Rule]            := "";
  RawGrid[grid_List, opts___Rule] := GridBox[MapMakeBox2 @ grid, opts];
];

(**************************************************************************************************)

SetBoxFn[ColBox]

"ColBox[list, align?, spacing?, baseline?]."

ColBox[list_List, args:Blank03, opts___Rule] :=
  iColBox[list, {opts}, args];

iColBox[list_, opts_, align_:Left, spacing_:Auto, bline_:Auto] := GridBox[
  ToList /@ list, Seq @@ opts,
  BLinePos -> toColBase[bline],
  ColJust -> align,
  RowGaps -> spacing
];

toColBase = CaseOf[
  Top    := Scaled @ 1.0;
  Cen    := Scaled @ 0.5;
  Bot    := Scaled @ 0.0;
  n_Int  := {n, 1};
  Auto   := {1, 1};
];

