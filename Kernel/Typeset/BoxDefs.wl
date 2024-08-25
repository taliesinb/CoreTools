PackageExports[
  "BoxFunction",
    RBox,
    SpacerBox, RaiseBox, LowerBox, MarginBox, ColumnBox,
    SpanStyleBox, TightBox, NiceTooltipBox, SkeletonBox,
    FnRowBox, FnBracketRowBox, FnParenRowBox, FnBracketBoxOp, FnParenBoxOp,
    DelimitedRBox,   RiffledRBox,   SpaceRBox,   CommaRBox,   ColonRBox,   SColonRBox,   ArrowRBox,   BraceRBox,   AngleRBox,   ParenRBox,   BracketRBox,   DBracketRBox,   AssocRBox,
    DelimitedRowBox, RiffledRowBox, SpaceRowBox, CommaRowBox, ColonRowBox, SColonRowBox, ArrowRowBox, BraceRowBox, AngleRowBox, ParenRowBox, BracketRowBox, DBracketRowBox, AssocRowBox,
    DimmedBox, StatusAreaBox, CursorIconBox, LiteralStringBox,
    ApplyEndStyleBox, ApplyIndentBox,
    UnderlinedBox, ItalicBox, SemiBoldBox, BoldBox, PlainBox,
    FontColorBox, FontSizeBox, FontSizeDeltaBox,
    FontBox, RobotoFontBox, CodeFontBox, CodeSansFontBox, SansFontBox, SerifFontBox,
    RobotoFont, CodeFont, CodeSansFont, SansFont, SerifFont,
    VeryLargeBox, LargeBox, MediumBox, SmallBox, VerySmallBox, TinyBox,
    ClickBox, ClickBoxOp, NoClickBox, EventHandlerBox,
    FlattenStyleBox, BuryStyleBox,
    UnderBraceBox, OverBraceBox, UnderBracketBox, OverBracketBox, UnderParenBox, OverParenBox,
    TextIconBox, NamedTextIconBox,
    CodeStyleBox,
  "FormHead",
    RaiseForm, LowerForm, MarginForm, RawColumn, RawRow, RawGrid, TightForm, NiceTooltip,
    DelimitedSeq, RiffledSeq, SpaceSeq, CommaSeq, ColonSeq, SColonSeq, ArrowSeq, BraceSeq, AngleSeq, ParenSeq, BracketSeq, DBracketSeq,
    DelimitedRow, RiffledRow, SpaceRow, CommaRow, ColonRow, SColonRow, ArrowRow, BraceRow, AngleRow, ParenRow, BracketRow, DBracketRow,
    Dimmed, LiteralStringForm, LiteralCommaStringForm,
    UnderlinedForm, ItalicForm, SemiBoldForm, BoldForm, PlainForm,
    VeryLargeForm, LargeForm, MediumForm, SmallForm, VerySmallForm, TinyForm,
    ClickForm, ClickFormOp,
    FlattenStyle, BuryStyle,
    CodeStyle,
  "Operator",
    FormBurrowing, BoxBurrowing,
    StyleOp, StyleBoxOp,
  "MetaFunction",
    DefineStyleFormBox, DefineSeqRowFormBox,
  "Variable",
    $BuryThroughForms, $BuryThroughBoxes,
  "Predicate",
    BurrowThroughHeadQ, BurrowThroughBoxHeadQ
];

(**************************************************************************************************)

RBox[args___] := RowBox[{args}];

(**************************************************************************************************)

NoClickBox[box_] := CursorIconBox["Arrow"]  @ EventHandlerBox[{"MouseClicked", _} :> Null] @ box;

SetHoldR[ClickBox, ClickForm]

CoreBoxes[ClickForm[expr_, body_]] := ClickBox[MakeBoxes @ expr, body];
CoreBoxes[ClickForm[expr_, body1_, body2_]] := ClickBox[MakeBoxes @ expr, body1, body2];

ClickBox[box_, body_] := CursorIconBox["LinkHand"] @ EventHandlerBox[{"MouseClicked", 1} :> body] @ box;
ClickBox[box_, body1_, body2_] := CursorIconBox["LinkHand"] @ EventHandlerBox[{{"MouseClicked", 1} :> body1, {"MouseClicked", 2} :> body2}] @ box;

SetHoldA[ClickBoxOp, ClickFormOp]

ClickBoxOp[body_][box_]             := ClickBox[box, body];
ClickBoxOp[body1_, body2_][box_]    := ClickBox[box, body1, body2];
ClickFormOp[body_][expr_]           := ClickForm[expr, body];
ClickFormOp[body1_, body2_][expr_]  := ClickForm[expr, body1, body2];

(**************************************************************************************************)

DeclareCurry2[CursorIconBox, StatusAreaBox, EventHandlerBox]

CursorIconBox[boxes_, icon_Str] := TagBox[boxes, MouseAppearanceTag[icon]];

StatusAreaBox[Fail, _]              := Fail;
StatusAreaBox[boxes_, label_String] := TagBox[boxes, Identity, TagBoxNote -> label];
StatusAreaBox[boxes_, label_]       := TagBox[boxes, Identity, TagBoxNote -> ToString[label, InputForm]];

EventHandlerBox[boxes_, rules_] := TagBox[boxes, EventHandlerTag @ ToList[rules, $eventHandlerRules]];
$eventHandlerRules = {Method -> "Preemptive", PassEventsDown -> Automatic, PassEventsUp -> False};

(**************************************************************************************************)

(* even works in InputForm *)
LiteralStringBox[s_Str] := $PrintLiteral[s];

CoreBoxes[LiteralStringForm[s_Str]] := LiteralStringBox[s];
CoreBoxes[LiteralCommaStringForm[s:{__Str}]] := LiteralStringBox @ StringRiffle[s, ", "];

(**************************************************************************************************)

MakeBoxDefinitions[
  TightForm[e_] := TightBox @ MakeBoxes @ e;
]

TightBox[e_] := StyleBox[e, AutoSpacing -> False, AutoIndent -> False, LineBreakWithin -> False];

(**************************************************************************************************)

DeclareCurry2[SpanStyleBox]

SpanStyleBox[box_, {min_, max_}] := StyleBox[box, SpanMinSize -> min, SpanMaxSize -> max];

(**************************************************************************************************)

MakeBoxDefinitions[
  MarginForm[e_, m__]          := MarginBox[MakeBoxes @ e, m];
  RaiseForm[e_, n_ ? NumericQ] := RaiseBox[MakeBoxes @ e, n];
  LowerForm[e_, n_ ? NumericQ] := LowerBox[MakeBoxes @ e, n];
];

(**************************************************************************************************)

MakeBoxDefinitions[
  NiceTooltip[expr_, tooltip_] := NiceTooltipBox[MakeBoxes @ expr, DisableCoreBoxInteractivity @ MakeBoxes @ tooltip];
];

DeclareCurry2[NiceTooltipBox]

NiceTooltipBox[Fail, ___] := Fail;
NiceTooltipBox[boxes_, tooltipBoxes_, opts___] := TagBox[
  TooltipBox[
    boxes,
    PaneBox[tooltipBoxes,
      ImageMargins -> {{5, 5}, {5, 5}},
      ImageSize -> {{20, All}, {15, All}}
    ],
    Alignment -> Center,
    TooltipStyle -> {opts, Background -> GrayLevel[1], CellFrameColor -> None, CellFrame -> 0},
    TooltipDelay -> 0
  ],
  MouseAppearanceTag["Arrow"]
];

(**************************************************************************************************)

SkeletonBox[b_] := RBox["\[LeftGuillemet]", b, "\[RightGuillemet]"];

(**************************************************************************************************)

SpacerBox[s_] := TemplateBox[{s}, "Spacer1"];
SpacerBox[w_, h_] := TemplateBox[{w, h}, "Spacer2"];

(**************************************************************************************************)

MarginBox[boxes_, n:NumP] :=
  If[n > 0, MarginBox[boxes, {n, n}, {0, 0}], boxes];

MarginBox[boxes_, {l_, r_}] :=
  MarginBox[boxes, {l, r}, {0, 0}];

MarginBox[boxes_, {l_, r_}, {b_, t_}] :=
  AdjustmentBox[boxes, BoxMargins -> {{l, r}, {b, t}}];

MarginBox[boxes_, {l_, r_}, {b_, t_}, bl_] :=
  AdjustmentBox[boxes, BoxMargins -> {{l, r}, {b, t}}, BoxBaselineShift -> bl];

MarginBox[padding_][boxes_] := MarginBox[boxes, padding];

DeclareCurry2[RaiseBox, LowerBox];

RaiseBox[e_, n_] :=
  AdjustmentBox[e, BoxBaselineShift -> -n];

LowerBox[e_, n_] :=
  AdjustmentBox[e, BoxBaselineShift -> n];

(**************************************************************************************************)

DeclareCurry1[FnRowBox, FnBracketRowBox, FnParenRowBox]

FnRowBox[p_, RowBox[{a__}]]    := RowBox[{p, a}];
FnRowBox[p_, a_]               := RowBox[{p, a}];

FnBracketRowBox[f_, list_List] := FnRowBox[f, BracketRowBox[list]];
FnParenRowBox[f_, list_List]   := FnRowBox[f, ParenRowBox[list]];

FnBracketBoxOp[f_][args___]    := FnRowBox[f, BracketRBox[args]];
FnParenBoxOp[f_][args___]      := FnRowBox[f, ParenRBox[args]];

(**************************************************************************************************)

MakeBoxDefinitions[
  DelimitedRow[l_, m_, r_][a_List]       := RiffledRowBox[MakeBoxes @ l, MakeBoxes @ m, MakeBoxes @ r][MakeBoxes /@ a];
  DelimitedSeq[l_, m_, r_][a___]         := MakeBoxes @ DelimitedRow[l, m, r] @ list @ a;
];

DelimitedRBox[l_, m_, r_][]              := RowBox @ {l, r};
DelimitedRBox[l_, m_, r_][bs__]          := RowBox @ {l, RiffledRBox[m][bs], r};
DelimitedRBox[l_, m_, r_, n_][]          := RowBox @ {l, SpacerBox[n * 2], r};
DelimitedRBox[l_, m_, r_, n_][bs__]      := RowBox @ {l, SpacerBox[n], RiffledRBox[m][bs], SpacerBox[n], r};

DelimitedRowBox[l_, m_, r_][{}]          := RowBox @ {l, r};
DelimitedRowBox[l_, m_, r_][bs_List]     := RowBox @ {l, RiffledRowBox[m][bs], r};
DelimitedRowBox[l_, m_, r_, n_][{}]      := RowBox @ {l, SpacerBox[n * 2], r};
DelimitedRowBox[l_, m_, r_, n_][bs_List] := RowBox @ {l, SpacerBox[n], RiffledRowBox[m][bs], SpacerBox[n], r};

(**************************************************************************************************)

MakeBoxDefinitions[
  RiffledRow[r_][a_List] := RiffledRowBox[MakeBoxes @ r][MakeBoxes /@ a];
  RiffledSeq[r_][a___]   := RiffledRowBox[MakeBoxes @ r][MakeBoxes /@ {a}];
];

RiffledRBox[r_][b_]        := b;
RiffledRBox[r_][b___]      := RowBox @ Riffle[{b}, r];

RiffledRowBox[r_][{b_}]    := b;
RiffledRowBox[r_][{bs___}] := RowBox @ Riffle[{bs}, r];

(**************************************************************************************************)

(* SeqBox[b_] := b;
SeqBox[bs__] := RowBox @ List @ bs;

MakeBoxDefinitions[
  SeqForm[b_]    := MakeBoxes @ b;
  SeqForm[bs___] := RowBox @ Map[MakeBoxes, {bs}];
];
 *)

(**************************************************************************************************)

DeclareThenScan[DefineStyleFormBox]

DefineStyleFormBox[Then[boxSym_, formSym_, style_]] := (
  CoreBoxes[formSym[expr_]] := BuryStyleBox[style] @ MakeBoxes @ expr;
  boxSym[boxes_]            := BuryStyleBox[style] @ boxes;
);

DeclaredHere[UnderlinedBox, ItalicBox, SemiBoldBox, BoldBox, PlainBox, VeryLargeBox, LargeBox, MediumBox, SmallBox, VerySmallBox, TinyBox];
DeclaredHere[UnderlinedForm, ItalicForm, SemiBoldForm, BoldForm, PlainForm, VeryLargeForm, LargeForm, MediumForm, SmallForm, VerySmallForm, TinyForm];

DefineStyleFormBox[
  UnderlinedBox; UnderlinedForm; Underlined,
  ItalicBox;     ItalicForm;     FontSlant -> Italic,
  SemiBoldBox;   SemiBoldForm;   FontWeight -> "SemiBold",
  BoldBox;       BoldForm;       FontWeight -> Bold,
  PlainBox;      PlainForm;      Plain,
  VeryLargeBox;  VeryLargeForm;  Large,
  LargeBox;      LargeForm;      Larger,
  MediumBox;     MediumForm;     Medium,
  SmallBox;      SmallForm;      Smaller,
  VerySmallBox;  VerySmallForm;  Small,
  TinyBox;       TinyForm;       Tiny
];

(**************************************************************************************************)

DeclareCurry2[FontColorBox, FontSizeBox, FontSizeDeltaBox]

FontColorBox::badColor = "Not a color: ``.";
FontColorBox[boxes_, color_] := StyleBox[boxes, FontColor -> toFontColor[color]];
toFontColor = CaseOf[
  r_ ? NumQ   := GrayLevel[r];
  c_ ? ColorQ := c;
  e_          := (Message[FontColorBox::badColor, e]; Red)
];

FontSizeBox[boxes_, n_]      := StyleBox[boxes, FontSize -> n];
FontSizeDeltaBox[boxes_, n_] := StyleBox[boxes, FontSize -> (Inherited + n)];

(**************************************************************************************************)

$BuryThroughForms = Alt[EventHandler, NiceTooltip, Style, RaiseForm, LowerForm, MarginForm, ClickForm, NiceTooltip];
$BuryThroughBoxes = Alt[_TagBox, _TooltipBox, _StyleBox, _AdjustmentBox];

s_BuryStyle[expr:$BuryThroughForms] := MapFirst[s, expr];
s_BuryStyle[expr_]                  := Style[expr, Seq @@ s];

s_BuryStyleBox[boxes:$BuryThroughBoxes] := MapFirst[s, boxes];
s_BuryStyleBox[boxes_]                  := StyleBox[boxes, Seq @@ s];

SetPred1[BurrowThroughHeadQ, BurrowThroughBoxHeadQ];

BurrowThroughHeadQ[head_Sym]    := MatchQ[head, $BuryThroughForms];
BurrowThroughBoxHeadQ[head_Sym] := MatchQ[head, $BuryThroughBoxes];

(**************************************************************************************************)

DeclaredHere[FormBurrowing, BoxBurrowing];

fn_FormBurrowing[expr:$BuryThroughBoxes] := MapFirst[fn, expr];
fn_FormBurrowing[expr_]                  := First[fn] @ expr;

fn_BoxBurrowing[boxes:$BuryThroughBoxes] := MapFirst[fn, boxes];
fn_BoxBurrowing[boxes_] := First[fn] @ boxes;

(**************************************************************************************************)

StyleOp[] = Id;
StyleOp[None] = Id;
StyleOp[spec___][Nothing] := Nothing;
StyleOp[spec___][e_] := Style[e, spec];

StyleBoxOp[] = Id;
StyleBoxOp[None] = Id;
StyleBoxOp[spec___][Nothing] := Nothing;
StyleBoxOp[spec___][e_] := StyleBox[e, spec];

(**************************************************************************************************)

CoreBoxes[CodeStyle[expr_, opts___Rule]] := CodeStyleBox[DisableCoreBoxFormatting @ MakeBoxes @ expr, opts];

CodeStyleBox[box_, opts___] := StyleBox[box, opts, FontFamily -> "Source Code Pro", AutoSpacing -> False];

(**************************************************************************************************)

DeclaredHere[RobotoFontBox, CodeFontBox, CodeSansFontBox, SansFontBox, SerifFontBox]
DeclaredHere[RobotoFont, CodeFont, CodeSansFont, SansFont, SerifFont]

(* CoreBoxes[formSym[expr_]] := BuryStyleBox[style] @ MakeBoxes @ expr; *)
DeclareCurry2[FontBox]
FontBox[e_, f_] := BuryStyleBox[FontFamily -> f][e];

declareFontBoxForm[boxSym_, formSym_, family_] := (
  boxSym[boxes_] := BuryStyleBox[FontFamily -> family][boxes];
  SystemBoxes[formSym[expr_]] := boxSym @ MakeBoxes @ expr;
);

MapApply[declareFontBoxForm, {
  {RobotoFontBox,   RobotoFont,   "Roboto"},
  {CodeFontBox,     CodeFont,     "Source Code Pro"},
  {CodeSansFontBox, CodeSansFont, "Source Sans Code"},
  {SansFontBox,     SansFont,     "Source Sans Pro"},
  {SerifFontBox,    SerifFont,    "Source Serif Pro"}
}];

(**************************************************************************************************)

FlattenStyle[Style[Style[expr_, s1___], s2___]] := FlattenStyle @ Style[expr, s1, s2];
FlattenStyle[expr_] := expr;

FlattenStyleBox[StyleBox[StyleBox[boxes_, s1___], s2___]] := FlattenStyleBox @ StyleBox[boxes, s1, s2];
FlattenStyleBox[boxes_] := boxes;

(**************************************************************************************************)

defSeqRowFormBox[fnSeq_, fnRow_][seqSym_, rowSym_, rboxSym_, boxSym_, {l_, c_, r_}] := Then[
  MakeBoxDefinitions[
    a_seqSym           := fnSeq[l, c, r] @@ Map[MakeBoxes, Apply[List, Unevaluated @ a]];
    rowSym[e_List]     := fnRow[l, c, r] @ Map[MakeBoxes, e];
    rowSym[e_List, m_] := fnRow[l, m, r] @ Map[MakeBoxes, e];
  ],
  a_rboxSym            := fnSeq[l, c, r] @@ Unevaluated[a],
  boxSym[e_List]       := fnRow[l, c, r] @ e,
  boxSym[e_List, m_]   := fnRow[l, m, r] @ e
];

defSeqRowFormBox[fnSeq_, fnRow_][seqSym_, rowSym_, rboxSym_, boxSym_, {x_}] := Then[
  MakeBoxDefinitions[
    a_seqSym           := fnSeq[x] @@ Map[MakeBoxes, Apply[List, Unevaluated @ a]];
    rowSym[e_List]     := fnRow[x] @ Map[MakeBoxes, e];
    rowSym[e_List, m_] := fnRow[x] @ Map[MakeBoxes, e];
  ],
  a_rboxSym            := fnSeq @@ Unevaluated[a],
  boxSym[e_List]       := fnRow[x] @ e,
  boxSym[e_List, m_]   := fnRow[x] @ e
];

defSeqRowFormBox[_, _][args___] := Print @ List[args];

DefineSeqRowFormBox[fnSeq_, fnRow_, tuples__List] := MapApply[defSeqRowFormBox[fnSeq, fnRow], {tuples}];

DeclaredHere[BraceSeq, AngleSeq, ParenSeq, BracketSeq, DBracketSeq];
DeclaredHere[BraceRow, AngleRow, ParenRow, BracketRow, DBracketRow];
DeclaredHere[BraceRBox, AngleRBox, ParenRBox, BracketRBox, DBracketRBox];
DeclaredHere[BraceRowBox, AngleRowBox, ParenRowBox, BracketRowBox, DBracketRowBox];

DefineSeqRowFormBox[DelimitedRBox, DelimitedRowBox,
  {BraceSeq,    BraceRow,    BraceRBox,    BraceRowBox,    {"{", ",", "}"}},
  {AngleSeq,    AngleRow,    AngleRBox,    AngleRowBox,    {"\[LeftAngleBracket]", ",", "\[RightAngleBracket]"}},
  {ParenSeq,    ParenRow,    ParenRBox,    ParenRowBox,    {"(", ",", ")"}},
  {BracketSeq,  BracketRow,  BracketRBox,  BracketRowBox,  {"[", ",", "]"}},
  {DBracketSeq, DBracketRow, DBracketRBox, DBracketRowBox, {"\[LeftDoubleBracket]", ",", "\[RightDoubleBracket]"}}
];

DeclaredHere[SpaceSeq, CommaSeq, ColonSeq, SColonSeq, ArrowSeq];
DeclaredHere[SpaceRow, CommaRow, ColonRow, SColonRow, ArrowRow];
DeclaredHere[SpaceRBox, CommaRBox, ColonRBox, SColonRBox, ArrowRBox];
DeclaredHere[SpaceRowBox, CommaRowBox, ColonRowBox, SColonRowBox, ArrowRowBox];

DefineSeqRowFormBox[RiffledRBox, RiffledRowBox,
  {SpaceSeq,  SpaceRow,  SpaceRBox,  SpaceRowBox,  {" "}},
  {CommaSeq,  CommaRow,  CommaRBox,  CommaRowBox,  {","}},
  {ColonSeq,  ColonRow,  ColonRBox,  ColonRowBox,  {":"}},
  {SColonSeq, SColonRow, SColonRBox, SColonRowBox, {";"}},
  {ArrowSeq,  ArrowRow,  ArrowRBox,  ArrowRowBox,  {"\[Rule]"}}
];

AssocRowBox[a_List] := DelimitedRowBox["\[LeftAssociation]", ",", "\[RightAssociation]"][a];
AssocRBox[a___]     := DelimitedRBox["\[LeftAssociation]", ",", "\[RightAssociation]"][a];

(**************************************************************************************************)

ApplyEndStyleBox[s___][RowBox[{l_, m___, r_}]] := RowBox[{StyleBox[l, s], m, StyleBox[r, s]}];
ApplyEndStyleBox[___][boxes_] := boxes;

ApplyIndentBox[boxes_, n_:0] := doIndent[n][boxes];

doIndent[n_?Negative] := Id;
doIndent[n_][StyleBox[boxes_, s___]]      := StyleBox[doIndent[n] @ boxes];
doIndent[n_][RowBox[list_]]               := Map[doIndent[n], list];
doIndent[n_][RowBox[{l_Str, m_, r_Str}]]  := RowBox[{l <> "\n", addNewlines[n-1][m], "\n" <> r}];
doIndent[_][boxes_] := boxes;

addNewlines[n_?Negative] := Id;
addNewlines[n_][RowBox[els:{___, ",", ___}]] := RowBox[Map[procLine[n], els]];
procLine[n_][","] := Splice[{",", "\n"}];
procLine[n_][b_]  := RowBox[{"  ", doIndent[n-1][b]}];

(**************************************************************************************************)

MakeBoxDefinitions[
  Dimmed[e_] := DimmedBox @ ToBoxes @ e
];

DimmedBox[e_] := StyleBox[e, Opacity[0.3]];

(**************************************************************************************************)

MakeBoxDefinitions[

  RawRow[list_List, riffle_] :=
    RowBox[
      Riffle[Map[MakeBoxes, list],
      MakeBoxes @ riffle]
    ];

  RawRow[list_List] :=
    RowBox[Map[MakeBoxes, list]];

  RawColumn[list_List, opts___] :=
    ColumnBox[Map[MakeBoxes, list], opts];

  RawGrid[{}, ___Rule] := "";
  RawGrid[grid_List, opts___Rule] :=
    GridBox[Map[MakeBoxes, grid, {2}], opts];

];

(**************************************************************************************************)

ColumnBox[list_List, align:Except[_Rule]:Left, spacing:Except[_Rule]:Automatic, opts___Rule] :=
  GridBox[
    List /@ list,
    opts,
    ColumnAlignments -> align,
    BaselinePosition -> Scaled[0.5],
    GridBoxSpacings -> {"Rows" -> {{spacing}}}
  ];

(**************************************************************************************************)

UnderBraceBox[a_, b_] := UnderscriptBox[UnderscriptBox[a, "\[UnderBrace]"], b];
OverBraceBox[a_, b_] := OverscriptBox[OverscriptBox[a, "\[OverBrace]"], b];

UnderBracketBox[a_, b_] := UnderscriptBox[UnderscriptBox[a, "\[UnderBracket]"], b];
OverBracketBox[a_, b_] := OverscriptBox[OverscriptBox[a, "\[OverBracket]"], b];

UnderParenBox[a_, b_] := UnderscriptBox[UnderscriptBox[a, "\[UnderParenthesis]"], b];
OverParenBox[a_, b_] := OverscriptBox[OverscriptBox[a, "\[OverParenthesis]"], b];

(**************************************************************************************************)

NamedTextIconBox["Times", opts_:{}, pad_:{{1,1}, {1,1}}] := TextIconBox[
  StyleBox[LineBox[{{{-1, -1}, {1, 1}}, {{-1, 1}, {1, -1}}}], Seq @@ opts],
  {{-1, 1}, {-1, 1}}, {1, 1} * .6, None, 10, pad
];

NamedTextIconBox["SquareUnion", opts_:{}, pad_:{{1,1}, {1,1}}] := TextIconBox[
  StyleBox[LineBox[{{-1, 1}, {-1, -1}, {1, -1}, {1, 1}}], Seq @@ opts],
  {{-1, 1}, {-1, 1}}, {0.7, .9}, None, 9, pad
];

TextIconBox[boxes_, bounds_, baseImageSize_, background_, bshift_, pad:{{l_, r_}, {b_, t_}}] :=
  DynamicBox[AdjustmentBox[
    Construct[
      GraphicsBox,
      boxes,
      PlotRange -> bounds, PlotRangePadding -> 0, AspectRatio -> Full, PlotRangeClipping -> False,
      ImageSize -> Ceiling[baseImageSize * Round @ CurrentValue[FontSize] + {l + r, b + t} , .5],
      ImagePadding -> pad, BaselinePosition -> Axis, Background -> background
    ],
    BoxBaselineShift -> (-bshift / Round @ CurrentValue[FontSize])
  ], TrackedSymbols :> {}
];

