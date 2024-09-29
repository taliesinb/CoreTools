SystemExports[
  "Option",
    TableHeadingStyle,
    FramePadding, RowFrames, RowFrameStyle, RowFramePadding, SpanningFrame,
    StylingFunction,
  "Head",
    StringBlockTemplate,
    StringTable, StringMatrix, StringRow, StringColumn, StringFrame, StringFrameLabeled,
  "FormHead",
    StringBlockForm
];

PackageExports[
  "Function",
    ToStr, BoxToString, ToStrBlock, ToStrInternal,
  "Head",
    StrBlock, HSpace, HStrBlock, VStrBlock,
  "Predicate",
    HasToStrFnQ
];

(**************************************************************************************************)

Attributes[ToStr] = {Flat, OneIdentity};

ToStr = CaseOf[
  s_Str     := s;
  s_Symbol  := SymbolName @ s;
  i_Int     := IntStr @ i;
  r_Real    := RealString[r, 3];
  expr_     := StrJoin @ iToStr @ expr;
];

iToStr[e_] := CatchMessages[ToStr, StrJoin @ blockToStrs @ formToBlock @ e];

(**************************************************************************************************)

SetHoldC @ ToStrInternal;
SetPred1 @ SetHoldC @ HasToStrFnQ;

ToStr /: SetDelayed[ToStr[$LHS_], $RHS_] := With[
  {head = First @ PatHead @ $LHS},
  HasToStrFnQ[head] = True;
  First @ MakeSetDelayed[ToStrInternal[$LHS], $RHS]
];

Protect[ToStr];

(**************************************************************************************************)

formToBlock = CaseOf[

  d:DatumP := makeSingleBlock @ ToStr @ d;

  s_StrBlock := s;

  Null := StrBlock[{""}, 0, 1];

  RawBoxes[b_] := boxesToBlock @ b;

  Style[e_, style___] := styleInBlock[$, e, {style}];

  Invisible[elem_] := Apply[spacerBlock[#2, #3]&, $ @ elem];

  c_Column     := $ @ colToBlock @ c;
  r_Row        := $ @ rowToBlock @ r;
  s_StringForm := $ @ sformToBlock @ s;
  g_Grid       := gridToBlock @ g;
  s_Spacer     := spacerToBlock @ s;
  f_Framed     := frameToBlock @ f;

  $rawBlock[block_] := block;
  VStrBlock[args_, align_, delim_] := vstackBlocks[riffle[$ /@ args, delim], align];
  HStrBlock[args_, align_, delim_] := hstackBlocks[riffle[$ /@ args, delim], align];

  Labeled[a_, l_] := $ @ StringColumn[{a, l}, ColumnAlignments -> Center, RowSpacings -> 1];
  ParenSeq[e_]    := delimBlock[{e}, "()", None];

  UnderlinedForm[e_] := vframeBlock[$ @ e, {None, "-"}, True];

  expr_ := iToStrFallback @ expr;
];

(**************************************************************************************************)

iToStrFallback = CaseOf[
  (head_ ? BurrowThroughHeadQ)[arg1_, ___] := formToBlock @ $ @ arg1;
  expr:(_Sym ? HasToStrFnQ)[___]           := makeSingleBlock @ checkToStr[expr, ToStrInternal @ expr];
  head_Sym ? HasCoreBoxQ                 := boxToBlock @ MakeBoxes @ head;
  expr:(_Sym ? HasCoreBoxQ)[___]         := boxToBlock @ MakeBoxes @ expr;
  expr_                                    := makeSingleBlock @ checkToStr[expr, Null];
];

SetHoldF @ checkToStr;

checkToStr[expr_, str_Str]      := str;
checkToStr[head_Symbol[___], _] := {HoldSymbolName[head], "[", CDots, "]"};
checkToStr[head_Symbol, _]      := HoldSymbolName[head];
checkToStr[_, _]                := "\[EmptySquare]";

failStrBlock[e_] := makeSingleBlock @

(**************************************************************************************************)

sformToBlock = CaseOf[
  StringForm[t_Str, args___, Alignment -> align_] := makeSFormBlock[t, {args}, align];
  StringForm[t_Str, args___]                      := makeSFormBlock[t, {args}, Top];
];

makeSFormBlock[template_, args_List, align_] := Row[
  Riffle[StrSplit[template, "``", All], args],
  Alignment -> align
];

(**************************************************************************************************)

colToBlock = CaseOf[
  Column[args_List, opts___Rule]               := $ @ Column[args, Lookup[{opts}, Alignment, Left]];
  Column[args_List, align_, opts___Rule]       := $ @ Column[args, align, Lookup[{opts}, Spacings, 0]];
  Column[args_List, align_, spacing_, ___Rule] := VStrBlock[args, align, Spacer @ spacing];
];

rowToBlock = CaseOf[
  Row[args_List, opts___Rule]                  := $ @ Row[args, None, opts];
  Row[args_List, delim_, opts___Rule]          := HStrBlock[args, Lookup[{opts}, Alignment, Top], delim];
];

spacerToBlock = CaseOf[
  Spacer[w_Int]                                := $ @ Spacer[{w, 1}];
  Spacer[{w_Int, h_Int}]                       := spacerBlock[w, h];
  Spacer[0] | Spacer[{0,0}] | None | Nothing   := None;
];

frameToBlock = CaseOf[
  Framed[e_, opts__Rule]                       := $ @ StringFrame[e, opts];
  Framed[e_]                                   := makeFrame[$ @ e, False];
  Framed[e_, RoundingRadius -> n_]             := makeFrame[$ @ e, n > 0];
];

(**************************************************************************************************)

boxToBlock = CaseOf[

  StyleBox[e_, style___]                              := styleInBlock[$, e, {style}];

  TemplateBox[space_List, "Spacer1"]                  := spacerBlock[space];

  TagBox[GridBox[rows_List, ___], "Column"]           := $ @ Column[rows];
  g_GridBox | TagBox[g_GridBox, "Grid"]               := gridBoxToBlock @ g;

  RowBox[args_List, opts___Rule]                      := $ @ HStrBlock[args, Lookup[{opts}, Alignment, Top], None];
  RowBox[args_List, delim:Except[_Rule], opts___Rule] := $ @ HStrBlock[args, Lookup[{opts}, Alignment, Top], delim];

  TagBox[GridBox[rows_List, ___], "Column"]           := $ @ colToBlock @ Column[rows];
  gb_GridBox | TagBox[gb_GridBox, "Grid"]             := gridBoxToBlock @ gb;

  VStrBlock[args_, align_, delim_]                    := vstackBlocks[riffle[$ /@ args, delim], align];
  HStrBlock[args_, align_, delim_]                    := hstackBlocks[riffle[$ /@ args, delim], align];

  f_FractionBox                                       := makeSingleBlock @ evalFracBox[f];
];

(**************************************************************************************************)

ToStrBlock[e_] := CatchMessages @ formToBlock @ e;

(**************************************************************************************************)

General::stringBlockError = "Internal error when converting to string: unexpected result ``.";

blockToStrs[res_] := ThrowMsg["stringBlockError", res];

blockToStrs[StrBlock[e_List, _, _]] := Riffle[rowToStrings[Flatten @ {#}]& /@ e, "\n"];

rowToStrings = CaseOf[
  HSpace[w_Int]     := repChar[" ", w];
  HSpace[w_]        := $ @ repChar[" ", w];
  $[{l___, $sbg[col_], Shortest[m___], $ebg[col_], r___} /; Count[{l}, _$sbg] === Count[{l}, _$ebg]] :=
    {$ @ {l}, $stylingFunction[StrJoin @ $ @ {m}, normStyle /@ {Background -> col}], $ @ {r}};
  e_Str                := e;
  None                 := "";
  Style[e_, args___]   := $stylingFunction[$ @ e, ToList[args]];
  Style[e_, ___]       := e;
  list_List            := $ /@ list;
];

repChar[s_, w_Int] := ConstList[s, w];
repChar[s_, w_] := Locals[
  w2 = w; c = 0;
  While[Abs[w2 - Round[w2]] > 0.0001, w2 -= 3/4; c++];
  If[w2 < 0, ThrowMsg["stringSpaceFailure", w]];
  Flatten[{
    repChar[s, Round[w2]],
    Style[StrJoin @ repChar[s, c], "Midscript"]
  }]
];

General::stringSpaceFailure = "Spacer `` could not be achieved with combination of normal and superscript characters."

(**************************************************************************************************)

General::stringBadGrid = "First argument to `` is not a list of lists."

gridBoxToBlock[GridBox[rows_List, opts___Rule]] := Locals[

  UnpackOptionsAs[GridBox, {opts}, gridBoxAlignment, gridBoxSpacings, gridBoxFrame, gridBoxBackground, gridBoxDividers];
  {h, w} = Dims[rows, 2];

  If[!ListVectorQ[rows], ThrowMsg["stringBadGrid", GridBox]];

  background = If[MatchQ[gridBoxBackground, {"Columns" -> {{_}}}], Part[gridBoxBackground, 1, 2, 1, 1], None];
  frame = MatchQ[gridBoxFrame, {"ColumnsIndexed" -> {{{1, -1}, {1, -1}} -> True}}];
  dividers = Switch[gridBoxDividers,
    {"Columns" -> {{True}}, "Rows" -> {{True}}}, frame = True; Center,
    {"Columns" -> {False, {True}, False}, "Rows" -> {False, {True}, False}}, Center,
    _, None
  ];

  rowAlignments = Lookup[{opts},    RowAlignments, ParseCyclicSpec[h] @ Lookup[gridBoxAlignment, "Rows", Top]];
  colAlignments = Lookup[{opts}, ColumnAlignments, ParseCyclicSpec[w] @ Lookup[gridBoxAlignment, "Columns", Left]];
  rowSpacings   = Lookup[{opts},      RowSpacings, ParseCyclicSpec[h] @ RepAll[Lookup[gridBoxSpacings, "Rows", 1.0], {Auto -> 1, None -> 0}]];
  colSpacings   = Lookup[{opts},   ColumnSpacings, ParseCyclicSpec[w] @ RepAll[Lookup[gridBoxSpacings, "Columns", 0.8], {Auto -> 0.8, None -> 0}]];

  rowSpacings = Round[rowSpacings - 1.0];
  colSpacings = Round[colSpacings];

  gstackBlocks[
    Map2[formToBlock, rows],
    ColumnAlignments -> colAlignments, RowAlignments -> rowAlignments,
    ColumnSpacings -> colSpacings, RowSpacings -> rowSpacings,
    Frame -> frame, Background -> background, Dividers -> dividers
  ]
]

(**************************************************************************************************)

$gridOptions = {
  RowAlignments -> Top,
  ColumnAlignments -> Left,
  RowSpacings -> 0,
  ColumnSpacings -> 1,
  Dividers -> None,
  Frame -> None,
  FramePadding -> None,
  FrameStyle -> None,
  RowFrames -> None,
  RowFrameStyle -> None,
  RowFramePadding -> None,
  SpanningFrame -> False,
  Background -> None
};

Options[gridToBlock2] = Join[$gridOptions, {Alignment -> {Left, Top}, Spacings -> {0, 0}}];

General::stringBadGrid = "Malformed grid: ``.";
gridToBlock[Grid[rows_List, opts___Rule]] := gridToBlock2[rows, opts];
gridToBlock[g_] := ThrowMsg["stringBadGrid", g];

gridToBlock2[rows_, opts:OptionsPattern[]] := Locals[
  If[!ListVectorQ[rows], ThrowMsg["stringBadGrid", Grid]];
  {calign, ralign} = EnsurePair @ alignment;
  {cspace, rspace} = EnsurePair @ spacings;
  UnpackOptions[alignment, spacings];
  extraOpts = {};
  If[MemberQ[{opts}, Spacings -> _],  JoinTo[extraOpts, {ColumnSpacings -> cspace, RowSpacings -> rspace}]];
  If[MemberQ[{opts}, Alignment -> _], JoinTo[extraOpts, {ColumnAlignments -> calign, RowAlignments -> ralign}]];
  gstackBlocks[Map2[formToBlock, rows], Seq @@ extraOpts, NarrowOptions @ opts]
]

(**************************************************************************************************)

$extFrameP = _Str | None | Dashed;

General::badGridFrame = "Frame -> `` is not a valid setting."
Options[gstackBlocks] = $gridOptions;

gstackBlocks[rows2_List, OptionsPattern[]] := Locals[

  UnpackOptions[
    rowAlignments, rowSpacings,
    columnAlignments, columnSpacings,
    dividers, frame, framePadding, frameStyle,
    rowFrames, rowFrameStyle, rowFramePadding,
    spanningFrame, background
  ];

  frameStyle //= normFrameStyle;
  rowFrameStyle //= normFrameStyle;
  {numRows, numCols} = Dims[rows2, 2];

  rows = riffleCols[columnSpacings] @ riffleRows[rowSpacings] @ rows2;
  widths = Part[rows, All, All, 2];
  heights = Part[rows, All, All, 3];
  maxWidths = Max /@ Transpose[widths];
  maxHeights = Max /@ heights;

  If[!ListQ[rowAlignments], rowAlignments = Table[rowAlignments, numRows]];
  If[!ListQ[columnAlignments], columnAlignments = Table[columnAlignments, numCols]];
  If[Total[rowSpacings] != 0, rowAlignments = Riffle[rowAlignments, Top]];
  If[Total[columnSpacings] != 0, columnAlignments = Riffle[columnAlignments, Left]];

  items = MapIndexed[{elem, rc} |-> (
    {r, c} = rc;
    vpadBlock[Part[maxHeights, r], Part[rowAlignments, r]] @ hpadBlock[Part[maxWidths, c], Part[columnAlignments, c]] @ elem
  ), rows, {2}];

  If[StrQ[rowFrames],
    {lext, rext} = Lookup[$hextTableNames, rowFrames];
    jump = If[Total[rowSpacings] === 0, All, 1;;-1;;2];
    If[IntQ[rowFramePadding] && rowFramePadding > 0,
      items = MapAt[padBlock[#, Left -> rowFramePadding]&, items, {All, 1}];
      items = MapAt[padBlock[#, Right -> rowFramePadding]&, items, {All, -1}];
    ];
    If[lext =!= None, items = MapAt[hframeBlock[#, {lext, None}, rowFrameStyle, spanningFrame, None]&, items, {jump, 1}]];
    If[rext =!= None, items = MapAt[hframeBlock[#, {None, rext}, rowFrameStyle, spanningFrame, None]&, items, {jump, -1}]];
    If[Total[rowSpacings] =!= 0,
      If[lext =!= None, items = MapAt[hframeBlock[#, {" ", None}, None, True, None]&, items, {2;;-1;;2, 1}]];
      If[rext =!= None, items = MapAt[hframeBlock[#, {None, " "}, None, True, None]&, items, {2;;-1;;2, -1}]];
    ];
    widths = Part[items, All, All, 2]; heights = Part[items, All, All, 3]; maxWidths = Max /@ Transpose[widths]; maxHeights = Max /@ heights;
  ];
  totalWidth = Total @ maxWidths; totalHeight = Total @ maxHeights;
  If[dividers === Center,
    items = addDivs[items, maxWidths, maxHeights];
    totalWidth += Len[maxWidths] - 1;
    totalHeight += Len[maxHeights] - 1;
  ];
  grid = Catenate @ Map[Flatten /@ Transpose[Col1[#]]&, items];
  block = StrBlock[grid, totalWidth, totalHeight];
  framePadding //= ParsePadding;
  block = blockPadding[block, framePadding];

  Switch[frame,
    Part[maxWidths,   1] += Part[framePadding, 1, 1];
    Part[maxWidths,  -1] += Part[framePadding, 1, 2];
    Part[maxHeights, -1] += Part[framePadding, 2, 1];
    Part[maxHeights, -1] += Part[framePadding, 2, 2];
    totalWidth = P2[block]; totalHeight = P3[block];
    True | "Round" | "Square",
      sfn = applyStyle @ frameStyle;
      If[dividers === Center,
        we = makeNotchedSides["│", {"├", "┤"}, totalHeight, maxHeights];
        sn = makeNotchedSides["─", {"┴", "┬"}, totalWidth, maxWidths];
      ,
        we = ConstList["│", {2, totalHeight}];
        sn = ConstList[repChar["─", totalWidth], 2];
      ];
      StrBlock[
        apply8patch[First @ block, Map2[sfn, we], sfn /@ sn, sfn /@ If[frame === "Round", $roundCompass, $squareCompass]],
        totalWidth + 2, totalHeight + 2
      ],
    {$extFrameP, $extFrameP} | _Str,
      hframeBlock[block, frame, frameStyle, spanningFrame, background],
    False | None,
      block,
    _,
      ThrowMsg["badGridFrame", frame];
  ]
];

(**************************************************************************************************)

General::stringFracWidth = "Fractional width `` occurred in unsupported context."
makeNotchedSides[char_, {notch1_, notch2_}, n_, sizes_] := Locals[
  If[!IntQ[n], ThrowMsg["stringFracWidth", n]];
  notches = Accumulate[Most[sizes] + 1];
  side = ConstList[char, n];
  List[
    ReplacePart[side, Transpose[List @ notches] -> notch1],
    ReplacePart[side, Transpose[List @ notches] -> notch2]
  ]
];

(**************************************************************************************************)

riffleCols[0] := Id;
riffleCols[n_][rows_] := riffle[#, Spacer[n]]& /@ rows;
riffleCols[ns_List][rows_] := If[Total[ns] == 0, rows,
  MapThread[
    riffle[#1, $rawBlock @ spacerBlock[#2, 1]]&,
    {rows, PadRight[ns, Len @ rows]}
  ]
];

riffleRows[0] = Id;
riffleRows[n_][rows_] := Transpose[riffle[#, Spacer[{1, n}]]& /@ Transpose[rows]];
riffleRows[ns_List][rows_] := If[Total[ns] == 0, rows,
  Transpose @ MapThread[
    riffle[#1, Spacer[{1, #2}]]&,
    {Transpose @ rows, PadRight[ns, Len @ Transpose @ rows]}
  ]
];

(**************************************************************************************************)

addDivs[items_, ws_, hs_] := Locals[
  Table[
    Switch[IntQ /@ {r, c},
      {True, True},  Part[items, r, c],
      {True, False}, makeVBar[Part[hs, r]],
      {False, True}, makeHBar[Part[ws, c]],
      {False, False}, StrBlock[{"┼"}, 1, 1]
    ],
    {r, 1, Len[hs], 1/2},
    {c, 1, Len[ws], 1/2}
  ]
];

(**************************************************************************************************)

makeVBar[h_] := StrBlock[ConstList["│", h], 1, h];
makeHBar[w_] := StrBlock[List @ repChar["─", w], w, 1];

(**************************************************************************************************)

delimBlock[e_List, frame_, style_] := hframeBlock[processHoriz @ e, frame, style, True, None];

processHoriz[{}]    := spacerBlock[1, 1];
processHoriz[{a_}]  := formToBlock @ a;
processHoriz[{a__}] := formToBlock @ Row[{a}, ", ", Alignment -> Center];

processVert[{}]     := spacerBlock[1, 1];
processVert[{a_}]   := formToBlock @ a;
processVert[{a__}]  := formToBlock @ Column[{a}];

spacerBlock[w_, h_] := StrBlock[ConstList[HSpace[w], h], w, h];

riffle[{}, _] := {};
riffle[args_, Spacer[0] | Spacer[{0,0}] | None | Nothing] := args;
riffle[args_, e_] := ScalarRiffle[args, formToBlock @ e];

(**************************************************************************************************)

makeSingleBlock[s_Str] /; StringContainsQ[s, "\n"] := makeBlock @ StrSplit[s, "\n"];
makeSingleBlock[elem_] := Locals[
  (* frags = applyStyleStack @ elem; *)
  StrBlock[List @ elem, atomWidth @ elem, 1]
];

makeBlock[rows_List, align_:Left] := Locals[
  widths = atomWidth /@ rows;
  maxWidth = Max @ widths;
  paddedRows = MapThread[hpadAtom[maxWidth, align], {rows, widths}];
  StrBlock[paddedRows, maxWidth, Len @ rows]
];

(**************************************************************************************************)

hpadAtom[tw_, halign_][atom_, w_] := Locals[
  d = tw - w;
  If[d <= 0, atom, Switch[halign,
    Left,   padAtomLeftRight[{0, d}] @ atom,
    Right,  padAtomLeftRight[{d, 0}] @ atom,
    Center, padAtomLeftRight[{Floor[d/2], Ceiling[d/2]}] @ atom,
    _,      ThrowMsg["stringBadAlignment", halign, {Left, Center, Right}]
  ]]
];

General::stringBadAlignment = "Bad alignment ``: should be one of ``."

padAtomLeftRight[{0,  0 }] := Id;
padAtomLeftRight[{0,  r_}][row_] := Flatten @ {row, HSpace @ r};
padAtomLeftRight[{l_, 0 }][row_] := Flatten @ {HSpace @ l, row};
padAtomLeftRight[{l_, r_}][row_] := Flatten @ {HSpace @ l, row, HSpace @ r};

atomWidth = CaseOf[
  Style[e_, ___, "Superscript" | "Subscript", ___] := ($ @ e) * 3 / 4;
  Style[e_, ___]    := $ @ e;
  e_List            := Total[$ /@ e];
  s_Str             := StrLen @ s;
  HSpace[n_]        := n;
];

(**************************************************************************************************)

padBlock[block:StrBlock[cols_List, w_, h_], pspec_] := Locals[
  {{l, r}, {b, t}} = Round @ ParsePadding @ pspec;
  If[l == r == b == t == 0, Return @ block];
  w2 = w + l + r; h2 = h + b + t;
  StrBlock[
    padBottomTop[{b, t}, w2] @ padLeftRight[{l, r}] @ cols,
    w2, h2
  ]
];

padLeftRight[{0, 0}] := Id;
padLeftRight[spec_][rows_] := Map[padAtomLeftRight[spec], rows];

(**************************************************************************************************)

vstackBlocks[{b_StrBlock}, _] := b;

vstackBlocks[rows_List, halign_] := Locals[
  widths = Col2[rows];
  heights = Col3[rows];
  maxWidth = Max @ widths;
  paddedRows = Map[hpadBlock[maxWidth, halign], rows];
  StrBlock[Catenate @ Col1[paddedRows], maxWidth, Total[heights]]
];

hpadBlock[tw_, halign_][StrBlock[rows_, w_, h_]] :=
  StrBlock[hpadAtom[tw, halign][#, w]& /@ rows, tw, h];

(**************************************************************************************************)

hstackBlocks[{b_StrBlock}, _] := b;

hstackBlocks[cols_List, valign_] := Locals[
  widths = Col2[cols];
  heights = Col3[cols];
  maxHeight = Max @ heights;
  paddedCols = Map[vpadBlock[maxHeight, valign], cols];
  StrBlock[MapThread[List, Col1[paddedCols]], Total @ widths, maxHeight]
];

vpadBlock[th_, valign_][StrBlock[rows_, w_, h_]] := Locals[
  d = th - h;
  extendedRows = If[d <= 0, rows, Switch[valign,
    Top,        padBottomTop[{d, 0}, w] @ rows,
    Bottom,     padBottomTop[{0, d}, w] @ rows,
    Center,     padBottomTop[{Floor[d/2], Ceiling[d/2]}, w] @ rows,
    _Int,       o = Clip[valign - 1, {0, d}]; padBottomTop[{d - o, o}, w] @ rows,
    _,          ThrowMsg["stringBadAlignment", valign, {Top, Center, Bottom}]
  ]];
  StrBlock[extendedRows, w, th]
];

hspaceList[h_, w_] := ConstList[HSpace[w], h];

padBottomTop[{0, 0}, _] := Id;
padBottomTop[{b_, t_}, w_][rows_] := Join[hspaceList[t, w], rows, hspaceList[b, w]];

(**************************************************************************************************)

blockPadding[block_, None | 0] := block;

blockPadding[block:StrBlock[elems2_, w2_, h2_], framePadding_] := Locals[
  elems = elems2; w = w2; h = h2;
  {{l, r}, {b, t}} = Round @ ParsePadding @ framePadding;
  If[l == r == b == t == 0, Return @ block];
  If[t > 0, PrependTo[elems, HSpace[w]]; h = h + t];
  If[b > 0, AppendTo[elems, HSpace[w]]; h = h + b];
  If[l > 0, space = hspaceList[h, l]; elems = MapThread[Flatten @ {#1, #2}&, {space, elems}]];
  If[r > 0, space = hspaceList[h, r]; elems = MapThread[Flatten @ {#1, #2}&, {elems, space}]];
  StrBlock[elems, w + l + r, h]
];

(**************************************************************************************************)

Options[StringBlockForm] = {
  StylingFunction -> "Linear"
}

CoreBox[sb:StringBlockForm[_, ___Rule]] :=
  MaybeEval @ Replace[stringBlockFormBoxes[sb], Except[_TemplateBox] :> Fail];

(* the first template slot is linear syntax for FE display,
the second is HTML, which is ready to be inserted straight into markdown as-is. *)
stringBlockFormBoxes[StringBlockForm[b_, opts___Rule]] := CatchMessages[StringBlockForm,
  TemplateBox[{
    ToInputString @ fixExtensibleChars @ MakeStringBlock[b, StylingFunction -> "Linear", opts],
    (* StringBlock[b, StylingFunction -> "HTML", opts] *)
    "TODO: HTML version"
    },
   "StringBlockForm"
  ]
];

fixExtensibleChars[str_Str] := StrRep[str, {"\[VerticalLine]" -> "⎥", "\[HorizontalLine]" -> "—"}];

(**************************************************************************************************)

Options[MakeStringBlock] = Options[StringBlockForm];

$stylingFunction = Seq1;
$styleStack = {};

MakeStringBlock[e_, OptionsPattern[]] := Locals[
  UnpackOptions[stylingFunction];
  $styleStack = {};
  $stylingFunction = Switch[stylingFunction,
    "Linear", linearStyling,
    "HTML",   htmlStyling,
    "Input",  inputStyling,
    "Cell",   cellStyling,
    None,     Seq1,
    _,        stylingFunction
  ];
  fragments = blockToStrs @ formToBlock @ e;
  If[stylingFunction === "Cell",
    Cell[
      TextData @ RepRep[Flatten @ fragments, {l___, ls_Str, rs_Str, r___} :> {l, ls <> rs, r}],
      "PreformattedCode"
    ],
    StrJoin @ fragments
  ]
];

(**************************************************************************************************)

(* these have special code in toCodeMarkdown to pick them up *)
cellStyling[e_, {l___, "Subscript", r___}] := Cell[BoxData @ SubscriptBox["", cellStyling[e, {l, r}]]];
cellStyling[e_, {l___, "Midscript", r___}] := StyleBox[cellStyling[e, {l, r}], FontSize -> 9.2];
cellStyling[e_, {l___, "Superscript", r___}] := Cell[BoxData @ SuperscriptBox["", cellStyling[e, {l, r}]]]
cellStyling[e_, {s__}] := StyleBox[e, s];
cellStyling[e_, {}] := e;

wrapCode[code_, str_] := If[CharQ[str], {name, ":", str}, {name, "{", str, "}"}];

inputStyling[e_, {l___, "Subscript", r___}] := {"_{", inputStyling[e, {l, r}], "}"};
inputStyling[e_, {l___, "Midscript", r___}] := {"_{", inputStyling[e, {l, r}], "}"};
inputStyling[e_, {l___, "Superscript", r___}] := {"^{", inputStyling[e, {l, r}], "}"};
(* inputStyling[e_, {currentStyleSetting[FontColor, cname_]}] := wrapCode[{"First", STake[cname, -1]}, e]; *)
(* inputStyling[e_, {FontColor -> color_}] := wrapCode[$colorShortcodeMappingInverse @ color, e]; *)
inputStyling[e_, s_] := e;

htmlStyling[e_, {l___, "Subscript", r___}] := {"<sub>", htmlStyling[e, {l, r}], "</sub>"};
htmlStyling[e_, {l___, "Midscript", r___}] := {"<sub>", htmlStyling[e, {l, r}], "</sub>"};
htmlStyling[e_, {l___, "Superscript", r___}] := {"<sup>", htmlStyling[e, {l, r}], "</sup>"};
htmlStyling[e_, style_] := htmlStyledString[e, style];

linearStyling[e_, {l___, "Subscript", r___}] := linearStyling[Subscript["", e], {l, r}];
linearStyling[e_, {l___, "Midscript", r___}] := linearStyling[Subscript["", e], {l, r}];
linearStyling[e_, {l___, "Superscript", r___}] := linearStyling[Superscript["", e], {l, r}];
linearStyling[e_Str, {}] := e;
linearStyling[e_, {}] := ToString[e, StandardForm];
linearStyling[e_, {s__}] := ToString[Style[e, s], StandardForm];

(**************************************************************************************************)

(* iToStr[s:StringTable[_List, ___Rule]] := iStringTable @@ s;

Options[StringTable] = {
  TableHeadings -> None,
  TableHeadingStyle -> $Gray
}

iStringTable[rows_List, OptionsPattern[]] := Locals @ CatchMessages[StringTable,
  If[!AnyMatrixQ[rows], ThrowMsg["notmatrix", rows]];
  items = Map2[toStrBlock, rows];
  UnpackOptions[tableHeadings, tableHeadingStyle];
  {rowStyle, colStyle} = EnsurePair @ tableHeadingStyle;
  SetAuto[tableHeadings, {Auto, Auto}];
  SetNone[tableHeadings, {None, None}];
  If[!MatchQ[tableHeadings, {_, _}], ReturnFailed[]];
  {rowHeadings, colHeadings} = tableHeadings;
  {numRows, numCols} = Dims[rows, 2];
  colAlignment = ConstList[Left, numCols];
  If[rowHeadings =!= None,
    $headingStyleFn = StyleOp @ rowStyle;
    rowHeadings = procTableHeadings[rowHeadings, numRows];
    items = MapThread[Prepend, {items, rowHeadings}];
  ];
  If[colHeadings =!= None,
    $headingStyleFn = StyleOp @ colStyle;
    colHeadings = procTableHeadings[colHeadings, numCols];
    If[rowHeadings =!= None, PrependTo[colHeadings, spacerBlock[1, 1]]];
    PrependTo[colAlignment, Right];
    PrependTo[items, colHeadings];
  ];
  gstackBlocks[items, ColumnSpacings -> 1, ColumnAlignments -> colAlignment]
];

StringTable::badtableheading = "`` is not a recognized setting for TableHeadings."
procTableHeadings[Auto, n_] := procTableHeadings[Range @ n, n];
procTableHeadings[str_Str, n_] := procTableHeadings[Chars @ str, n]
procTableHeadings[list_List, n_] := formToBlock[$headingStyleFn[#]]& /@ PadRight[list, n, ""];
procTableHeadings[other_, _] := ThrowMsg["badtableheading", other];

(**************************************************************************************************)

iToStr[s:StringMatrix[_List, ___Rule]] := iStringMatrix @@ s;

Options[StringMatrix] = {
  RowAlignments -> Top,
  ColumnAlignments -> Left,
  RowSpacings -> 0,
  ColumnSpacings -> 1,
  Dividers -> None,
  Frame -> None,
  FramePadding -> None,
  FrameStyle -> None,
  RowFrames -> None,
  RowFrameStyle -> None,
  RowFramePadding -> None,
  SpanningFrame -> False,
  Background -> None
};

StringMatrix::notmatrix = "Expected a matrix, found ``."

iStringMatrix[rows_List, opts___Rule] := Locals[
  If[!AnyMatrixQ[rows], ThrowMsg["notmatrix", rows]];
  gstackBlocks[Map2[toStrBlock, rows], opts]
];

(**************************************************************************************************)

StringBlockTemplate::badtemplate = "Template `` should be a string or list of strings."
StringBlockTemplate::badtemplatearg = "`` reqeuested, but only `` available."

toStrBlock[StringBlockTemplate[template:(_Str | {___String}), args___]] := Locals @ CatchMessages[StringBlockTemplate,
  lines = Switch[template,
    _Str,     StrSplit[template, "\n"],
    {___Str}, template,
    _,           ThrowMsg["badtemplate", template];
  ];
  items = {args};
  $sbtAlign = Center;
  If[MatchQ[L[items, None], Alignment -> _], $sbtAlign = Part[items, -1, 2]; items = Most[items]];
  items = Map[toStrBlock, items];
  lines = StrRep[lines, {
    "#" ~~ i:DigitCharacter :> $rawBlock @ SafePart[items, FromDigits @ i],
    "$" ~~ i:DigitCharacter :> $rawBlock @ blockToHSpace @ SafePart[items, FromDigits @ i]
  }] /. Missing["PartAbsent", j_] :> ThrowMsg["badtemplatearg", j, Len @ items];
  verticalBlocks = Map[blockLine, lines];
  vstackBlocks[verticalBlocks, Left]
];

blockLine = CaseOf[
  StrExpr[a___]          := $ @ {a};
  s_Str                  := $ @ {s};
  list_List              := hstackBlocks[Map[formToBlock, list], $sbtAlign];
];

blockToHSpace[StrBlock[_, w_, h_]] := spacerBlock[w, 1];

(**************************************************************************************************)

Options[StringRow] = {
  RowAlignments -> Top,
  ColumnSpacings -> 0,
  Frame -> None,
  FramePadding -> None,
  FrameStyle -> None,
  SpanningFrame -> False,
  Background -> None
}

toStrBlock[StringRow[items_List, OptionsPattern[]]] := Locals[
  items = Map[toStrBlock, items];
  UnpackOptions[rowAlignments, columnSpacings, frame, framePadding, frameStyle, spanningFrame, background];
  frameStyle //= normFrameStyle;
  If[columnSpacings =!= 0, items = riffle[items, Spacer[columnSpacings]]];
  block = hstackBlocks[items, rowAlignments];
  block = blockPadding[block, framePadding];
  hframeBlock[block, frame, frameStyle, spanningFrame, background]
];

(**************************************************************************************************)

Options[StringColumn] = {
  ColumnAlignments -> Left,
  RowSpacings -> 0,
  Frame -> None,
  FramePadding -> None,
  FrameStyle -> None,
  SpanningFrame -> False,
  Background -> None
}

toStrBlock[StringColumn[items_List, OptionsPattern[]]] := Locals[
  items = Map[toStrBlock, items];
  UnpackOptions[columnAlignments, rowSpacings, frame, framePadding, frameStyle, spanningFrame, background];
  frameStyle //= normFrameStyle;
  If[rowSpacings =!= 0, items = riffle[items, Spacer[{1, rowSpacings}]]];
  block = vstackBlocks[items, columnAlignments];
  block = blockPadding[block, framePadding];
  hframeBlock[block, frame, frameStyle, spanningFrame, background]
];

(**************************************************************************************************)

Options[StringFrame] = {
  Frame -> True,
  FrameStyle -> None,
  Background -> None
};

toStrBlock[StringFrame[item_, OptionsPattern[]]] := Locals[
  UnpackOptions[frame, frameStyle, background];
  hframeBlock[toStrBlock @ item, frame, frameStyle, True, background]
];

(**************************************************************************************************)

Options[FrameLabeled] = {
  LabelSpacing -> {1, 0},
  LabelStyle -> $Gray,
  FrameTicks -> False (* TODO: make this introduce little marks in the gap *)
};

toStrBlock[FrameLabeled[item_, specs_, OptionsPattern[]]] := Locals[
  item //= toStrBlock;
  UnpackOptions[labelSpacing, frameTicks, labelStyle];
  {hlabelSpacing, vlabelSpacing} = EnsurePair @ labelSpacing;
  $hoffset = 0; $voffset = 0; $labelStyleFn = StyleOp @ labelStyle;
  Fold[applyFrameLabel, item, ToList @ specs]
];

ToStr::badflspec = "Bad FrameLabel spec ``."

applyFrameLabel[block_, side_ -> Style[items_, style___]] := Locals[
  $labelStyleFn = StyleOp @ style;
  applyFrameLabel[block, side -> items]
];

toLabelItems[pos_Int -> label_] := pos -> label;
toLabelItems[pos_ -> labels_] := Locals[
  labels //= toLabelValues;
  pos = If[H[pos] == Span, Part[Range[100], pos], pos];
  Splice @ RuleThread[Take[pos, Len @ labels], labels]
];

toLabelValues = CaseOf[
  str_Str := Chars @ str;
  list_List := list;
]

applyFrameLabel[block_, side:(Left|Right) -> spec_] := Locals[
  items = Map[toLabelItems, ToList @ spec];
  isLeft = side === Left;
  rows = ConstList[StrBlock[{""}, 0, 1], P3[block]];
  rules = (#1 + $voffset) -> clipOneLine[formToBlock @ $labelStyleFn @ #2]& @@@ Sort[items];
  rows = ReplacePart[rows, rules];
  labelBlock = vstackBlocks[rows, If[isLeft, Right, Left]];
  labelBlock = padBlock[labelBlock, If[isLeft, Right, Left] -> hlabelSpacing];
  If[isLeft, $hoffset ^= $hoffset + P2[labelBlock]];
  hstackBlocks[If[isLeft, {labelBlock, block}, {block, labelBlock}], Top]
];

ToStr::overlapfl = "Overlapping FrameLabel spec ``."

applyFrameLabel[block_, side:(Top|Bottom) -> spec_] := Locals[
  isTop = side === Top;
  spec = Map[toLabelItems, ToList @ spec];
  {positions, items} = KeysValues @ Sort @ spec;
  positions += $hoffset;
  itemBlocks = Map[formToBlock @ $labelStyleFn @ #&, items];
  itemWidths = Col2[itemBlocks];
  gaps = Differences @ Prepend[1] @ positions;
  gaps -= Prepend[0] @ Most @ itemWidths;
  If[Min[gaps] < 0, ThrowMsg["overlapfl", side -> spec]];
  spacerBlocks = Map[spacerBlock[#, 1]&, gaps];
  blocks = Catenate @ Transpose[{spacerBlocks, itemBlocks}];
  labelBlock = hstackBlocks[blocks, If[isTop, Bottom, Top]];
  If[isTop, $voffset ^= $voffset + P3[labelBlock]];
  vstackBlocks[If[isTop, {labelBlock, block}, {block, labelBlock}], Left]
];

applyFrameLabel[_, spec_] := ThrowMsg["badflspec", spec];

clipOneLine = CaseOf[
  block:StrBlock[_, _, 1] := block;
  StrBlock[rows_, w_, _] := StrBlock[Take[rows, 1], w, 1]
];
 *)
(**************************************************************************************************)



(*
(**************************************************************************************************)

processBoxes[boxes_] :=
  formToBlock @ RepAll[EvaluateTemplateBoxFull @ boxes, $boxFixups]

$boxFixups = SubscriptBox[a_Str, b_] /; SEndsQ[a, " "] :> SubscriptBox[STrim @ a, b];

evalFracBox = CaseOf[
  FractionBox[i_Str, j_Str] := StrJoin[i, "/", j];
]

(**************************************************************************************************)

trimBlock[block:StrBlock[rows_, w_, h_]] := Locals[
  rows = rows //. {
    {l___, HSpace[s1_], HSpace[s2_]} :> {l, HSpace[s1 + s2]},
    {l___, s:" ".., r___HSpace} :> {l, HSpace[Len @ {s}], r}
  };
  spaceLens = rightSpaceLen /@ rows;
  trimCount = Min[spaceLens];
  If[trimCount == 0, Return @ block];
  StrBlock[trimRowRight, w - trimCount, h]
];

rightSpaceLen = CaseOf[
  {___, HSpace[s_]} := s;
  _                  := 0;
]

trimRowRight = CaseOf[
  {l___, HSpace[s_]} := If[s - trimCount <= 0, {l}, {l, HSpace[s - trimCount]}];
]

(**************************************************************************************************)

stringFormBlock[template_, args_List, align_] :=
  formToBlock @ Row[
    Riffle[StrSplit[template, "``", All], args],
    Alignment -> align
  ];

(**************************************************************************************************)

parseLinearSyntax[str_] /; SFreeQ[str, "\!\(\*"] := str;

linearBalancedQ[str_] := StringCount[str, "\("] == StringCount[str, "\)"];

parseLinearSyntax[str_] := Row @ SCases[str, {
  "\!\(\*" ~~ inner:Shortest[___] ~~ "\)" /; linearBalancedQ[inner] :> parseInnerLinearSyntax[inner],
  fragment__ /; SFreeQ[fragment, "\!"] :> fragment
}];

parseInnerLinearSyntax[str_] := Locals[
  str2 = StrRep[str,
    "\(" ~~ inner:Shortest[___] ~~ "\)" /; linearBalancedQ[inner] :> "\"" <> inner <> "\""
  ];
  ToExpression[str] /. {StyleBox -> Style, SubscriptBox -> Subscript, SuperscriptBox -> Superscript, RowBox -> Row}
];

(**************************************************************************************************)
*)

General::stringBagFrameStyle = "Setting of FrameStyle -> `` should be a color or None.";

normFrameStyle = CaseOf[
  color_ ? ColorQ      := FontColor -> color;
  (* i_Int             := currentStyleSetting[FontColor, "Color" <> IntStr[i]]; *)
  r:Rule[FontColor|Background, _] := r;
  other_               := (Message[General::stringBagFrameStyle, other]; None);
  None                 := None;
]

normStyle = CaseOf[
  color_ ? ColorQ                                      := FontColor -> color;
  i_Int                                                := $[FontColor -> i];
(*FontColor -> i_Int                                   := currentStyleSetting[FontColor, "Color" <> IntStr[i]];
  Background -> i_Int                                  := currentStyleSetting[Background, "Background" <> IntStr[i]];*)
  Bold                                                 := FontWeight -> Bold;
  Italic                                               := FontSlant -> Italic;
  Plain                                                := Splice[{FontWeight -> Plain, FontSlant -> Plain}];
  Rule[FontVariations, vars_List]                      := Splice @ Map[normFontVar, vars];
  Underlined                                           := Underlined;
  Struckthrough                                        := "Subscript";
  r:Rule[FontWeight|FontSlant|FontColor|Background, _] := r;
  r:RuleDelayed[FontColor|Background, _]               := r;
  s:"Subscript"|"Superscript"                          := s;
  _                                                    := Nothing;
];

normFontVar = CaseOf[
  "Underline" -> True     := Underlined;
  "StrikeThrough" -> True := "Subscript";
  _                       := Nothing
]

(**************************************************************************************************)

$styleStack = {};
styleInBlock[fn_, e_, style_] := InheritedBlock[
  {$styleStack},
  $styleStack = joinStyles[normStyle /@ {style}, $styleStack];
  fn @ item
];

(**************************************************************************************************)

joinStyles[s1_, s2_] := DelDups[Join[s1, s2], First[#1, "ZZ"] === First[#2, "YY"]&];

applyStyleStack[e_] /; $styleStack === {} := e;
applyStyleStack[e_] := applyStyle[Sequence @@ $styleStack] @ e;

applyStyle[] := Id;
applyStyle[None] := Id;
applyStyle[s_List] := Apply[applyStyle, s];

as_applyStyle[list_List] := Map[as, list];
_applyStyle[s_HSpace] := s;
_applyStyle[" "] := " ";
_applyStyle[None] := None;
applyStyle[s___][e_Str] := Style[e, s];

(**************************************************************************************************)

$roundCompass = {"╭", "╮", "╰", "╯"};
$squareCompass = {"┌", "┐", "└", "┘"};

makeFrame[StrBlock[rows_, w_, h_], r_] := Locals[
  we = ConstList["│", {2, h}];
  sn = ConstList[repChar["─", w], 2];
  rows2 = apply8patch[rows, we, sn, If[r, $roundCompass, $squareCompass]];
  StrBlock[rows2, w + 2, h + 2]
]

apply8patch[rows_, {w_, e_}, {s_, n_}, {nw_, ne_, sw_, se_}] :=
  Join[
    List @ Flatten @ {nw, n, ne},
    MapThread[Flatten @ {#1, #2, #3}&, {w, rows, e}],
    List @ Flatten @ {sw, s, se}
  ];

(**************************************************************************************************)

hextTable = CaseOf[
  None                := None;
  "|"                 := ext1["│"];
  " "                 := ext1[" "];
  Dashed              := ext1["┊"];
  "RoundLeft"         := ext3["(", "╭", "│", "╰"];
  "RoundRight"        := ext3[")", "╮", "│", "╯"];
  "MidRoundLeft"      := ext5["(", "╭", "│", "┤", "│", "╰"];
  "MidRoundRight"     := ext5[")", "╮", "│", "├", "│", "╯"];
  "SquareLeft"        := ext3["[", "┌", "│", "└"];
  "SquareRight"       := ext3["]", "┐", "│", "┘"];
  "MidSquareLeft"     := ext5["[", "┌", "│", "┤", "│", "└"];
  "MidSquareRight"    := ext5["]", "┐", "│", "├", "│", "┘"];
  "LeftFloor"         := ext3["⌊", "⌊", "│", "│"];
  "RightFloor"        := ext3["⌋", "⌋", "│", "│"];
  "LeftCeiling"       := ext3["⌈", "⌈", "│", "│"];
  "RightCeiling"      := ext3["⌉", "⌉", "│", "│"];
  "DoubleSquareLeft"  := ext3["⟦", "╓", "║", "╙"];
  "DoubleSquareRight" := ext3["⟧", "╖", "║", "╜"];
  "["                 := ext3["[", "⎡", "⎢", "⎣"];
  "]"                 := ext3["]", "⎤", "⎥", "⎦"];
  "{"                 := ext5["}", "⎧", "⎸", "⎨", "⎸", "⎩"];
  "}"                 := ext5["}", "⎫", "⎹", "⎬", "⎹", "⎭"];
  "("                 := ext3["(", "⎛", "⎜", "⎝"];
  ")"                 := ext3[")", "⎞", "⎟", "⎠"];
];

$hextTableNames = UDict[
  "Round"        -> {"RoundLeft", "RoundRight"},
  "MidRound"     -> {"MidRoundLeft", "MidRoundRight"},
  "Square"       -> {"SquareLeft", "SquareRight"},
  "MidSquare"    -> {"MidSquareLeft", "MidSquareRight"},
  "DoubleSquare" -> {"DoubleSquareLeft", "DoubleSquareRight"},
  "Floor"        -> {"LeftFloor", "RightFloor"},
  "Ceiling"      -> {"LeftCeiling", "RightCeiling"},
  "["            -> {"[", None},
  "{"            -> {"{", None},
  "("            -> {"(", None},
  "[]"           -> {"[", "]"},
  "{}"           -> {"{", "}"},
  "()"           -> {"(", ")"}
];


(**************************************************************************************************)

General::stringBadFrameSpec = "`` is not a valid spec for Frame, which should be one of ``."

hframeBlock[block_, name_Str, rest___] :=
  hframeBlock[block, Lookup[$hextTableNames, name, ThrowMsg["stringBadFrameSpec", name, Keys @ $hextTableNames]], rest];

hframeBlock[block_, None | {None, None}, ___] := block;

hframeBlock[StrBlock[grid_, w_, h_], {l_, r_}, style_, spanning_, background_] := Locals[
  $n = h; sfn = applyStyle @ style;
  {lext, rext} = MapThread[Map[sfn, extend[hextTable[#1], spanning, #2]]&, {{l, r}, {False, True}}];
  grid2 = MapThread[DeleteNone @ Flatten @ {#1, #2, #3}&, {lext, grid, rext}];
  If[background =!= None, grid2 = ToList[$sbg[background], #, $ebg[background]]& /@ grid2];
  StrBlock[grid2, w + If[l === None, 0, 1] + If[r === None, 0, 1], h]
];

(**************************************************************************************************)

vextTable = CaseOf[
  None                 := None;
  "-"                  := ext1["─"];
  Dashed               := ext1["┈"];
  "RoundTop"           := ext3["⏜", "╭", "─", "╮"];
  "RoundBottom"        := ext3["⏝", "╰", "─", "╯"];
  "MidRoundTop"        := ext3["⏜", "╭", "─", "┴", "─", "╮"];
  "MidRoundBottom"     := ext3["⏝", "╰", "─", "┬", "─", "╯"];

  "SquareTop"          := ext3["⎴", "┌", "─", "┐"];
  "SquareBottom"       := ext3["⎵", "└", "─", "┘"];
  "MidSquareTop"       := ext3["⎴", "┌", "─", "┐"];
  "MidSquareBottom"    := ext3["⎵", "└", "─", "┘"];

  "DoubleSquareTop"    := ext3["═", "╒", "═", "╕"];
  "DoubleSquareBottom" := ext3["═", "╘", "═", "╛"];
];

$vextTableNames = UDict[
  "Round"        -> {"RoundTop", "RoundBottom"},
  "MidRound"     -> {"MidRoundTop", "MidRoundBottom"},
  "Square"       -> {"SquareTop", "SquareBottom"},
  "MidSquare"    -> {"MidSquareTop", "MidSquareBottom"},
  "DoubleSquare" -> {"DoubleSquareTop", "DoubleSquareBottom"}
];

vframeBlock[arg1_, arg2_] := vframeBlock[arg1, arg2, None, True];

vframeBlock[block_, name_Str, style_, spanning_] :=
  vframeBlock[block, Lookup[$vextTableNames, name, ThrowMsg["stringBadFrameSpec", name, Keys @ $vextTableNames]], style, spanning];

vframeBlock[block_, None | {None, None}, _, _] := block;

vframeBlock[StrBlock[grid_, w_, h_], {t_, b_}, style_, spanning_] := Locals[
  $n = w; sfn = StyleOp @ style;
  {bext, text} = MapThread[Map[sfn, extend[vextTable[#1], spanning, #2]]&, {{b, t}, {False, True}}];
  grid2 = grid;
  If[text =!= None, PrependTo[grid2, text]];
  If[bext =!= None, AppendTo[grid2, bext]];
  StrBlock[grid2, w, h + If[b === None, 0, 1] + If[t === None, 0, 1]]
];

(**************************************************************************************************)

extend[None, _, _] := ConstList[None, $n];
extend[s_, False, side_] := ReplacePart[ConstList[" ", $n], If[side, -1, 1] -> First[s]];
extend[s_, True, side_] := extendSpanning[s];

extendSpanning = CaseOf[
  ext1[c_]                     := ConstList[c, $n];
  ext3[s_, l_, m_, r_]         := If[$n === 1, {s}, Flatten @ {l, ConstList[m, $n - 2], r}];
  ext5[s_, l_, a_, m_, b_, r_] := Locals[
    If[$n === 1, Return @ {s}];
    n2 = ($n - 3) / 2;
    Flatten @ {l, ConstList[a, Floor @ n2], m, ConstList[a, Ceiling @ n2], r}
  ]
];

(**************************************************************************************************)
