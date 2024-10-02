SystemExports[
  "FormHead",
    LabelTop,
    RaiseForm, LowerForm, MarginForm, HMarginForm, VMarginForm,
    RawColumn, RawRow, RawGrid, TightForm, NiceTooltip, ErrorTooltip,
    DelimitedSeq, RiffledSeq, SpaceSeq, CommaSeq, ColonSeq, SColonSeq, ArrowSeq, BraceSeq, AngleSeq, ParenSeq, BracketSeq, DBracketSeq,
    DelimitedRow, RiffledRow, SpaceRow, CommaRow, ColonRow, SColonRow, ArrowRow, BraceRow, AngleRow, ParenRow, BracketRow, DBracketRow,
    GrayForm, Dimmed,
    RowGrid, ColGrid, TightRowGrid, TightColGrid,
    LiteralStringRow, UnlimitedRow,
    UnderlinedForm, ItalicForm, SemiBoldForm, BoldForm, PlainForm,
    FontColored, FontScaled, FontSized,
    VeryLargeForm, LargeForm, MediumForm, SmallForm, VerySmallForm, TinyForm,
    ClickForm, ClickFormOp,
    FlattenStyle, BuryStyle,
    CodeStyle, BoldSyntaxForm,
    ExpanderForm
];

PackageExports[
  "BoxFunction",
    RBox,
    LabelTopBox,
    SpacerBox, RaiseBox, LowerBox, MarginBox, HMarginBox, VMarginBox, ColumnBox,
    SpanStyleBox, TightBox, NiceTooltipBox, ErrorTooltipBox, SkeletonBox,
    FnRowBox, FnRBox,
    RiffBox,
    DelimitedRBox,   RiffledRBox,   SpaceRBox,   CommaRBox,   ColonRBox,   SColonRBox,   ArrowRBox,   BraceRBox,   AngleRBox,   ParenRBox,   BracketRBox,   DBracketRBox,   AssocRBox,
    DelimitedRowBox, RiffledRowBox, SpaceRowBox, CommaRowBox, ColonRowBox, SColonRowBox, ArrowRowBox, BraceRowBox, AngleRowBox, ParenRowBox, BracketRowBox, DBracketRowBox, AssocRowBox,
    GrayBox, DimmedBox,
    GridBoxRule, RowGridBox, ColGridBox, TightRowGridBox, TightColGridBox,
    StatusAreaBox, CursorIconBox,
    ApplyEndStyleBox, ApplyIndentBox,
    UnderlinedBox, ItalicBox, SemiBoldBox, BoldBox, PlainBox,
    FontColorBox, FontSizeBox, FontSizeDeltaBox, FontScaleBox,
    FontBox, RobotoFontBox, CodeFontBox, CodeSansFontBox, SansFontBox, SerifFontBox,
    RobotoFont, CodeFont, CodeSansFont, SansFont, SerifFont,
    VeryLargeBox, LargeBox, MediumBox, SmallBox, VerySmallBox, TinyBox,
    ClickBox, ClickBoxOp, NoClickBox, EventHandlerBox,
    FlattenStyleBox, BuryStyleBox,
    UnderBraceBox, OverBraceBox, UnderBracketBox, OverBracketBox, UnderParenBox, OverParenBox,
    TextIconBox, NamedTextIconBox,
    CodeStyleBox, BoldSyntaxBox,
    BoxFnErrorBox,
    ExpanderBoxes, HoldExpanderBoxes,
  "BoxSymbol",   CommaB,     GCommaB,    CommaSB,     GCommaSB,      QuadB,
  "FormSymbol",  Comma,      GComma,     CommaS,      GCommaS,       Quad,
  "BoxFunction", CommaSBox,  GCommaBox,  GCommaSBox,  RepCommaBox,
  "FormHead",    CommaSForm, GCommaForm, GCommaSForm, RepCommaForm,
  "Operator",
    FormBurrowing, BoxBurrowing,
    StyleOp, StyleBoxOp,
  "MetaFunction",
    DefineStyleFormBox, DefineSeqRowFormBox, DeclareExpanderBoxes,
  "Variable",
    $BuryThroughForms, $BuryThroughBoxes,
  "Predicate",
    BurrowThroughHeadQ, BurrowThroughBoxHeadQ,
    AtomFormHeadQ, CompoundFormHeadQ
];

(**************************************************************************************************)

RBox[args___] := RowBox[{args}];

(*************************************************************************************************)

CoreBox[LabelTop[label_, expr_]] := LabelTopBox[label, expr];

LabelTopBox[label_, expr_] := ColumnBox[
  {StyleBox[MakeBoxes @ label,
    FontWeight -> Bold, FontFamily -> "Source Code Sans",
    FontSize -> Inherited],
   MakeBoxes @ expr},
  Left, 0.2
];

SetCurry1BoxFn @ LabelTopBox;

(**************************************************************************************************)

SetBoxFn @ NoClickBox;

NoClickBox[box_] := CursorIconBox["Arrow"]  @ EventHandlerBox[{"MouseClicked", _} :> Null] @ box;

(**************************************************************************************************)

SetForm1[ClickForm];
SetHoldR[ClickBox, ClickForm];
SetBoxFn[ClickBox];

CoreBox[ClickForm[expr_, body_]] := ClickBox[MakeBoxes @ expr, body];
CoreBox[ClickForm[expr_, body1_, body2_]] := ClickBox[MakeBoxes @ expr, body1, body2];

ClickBox[box_, body_] := CursorIconBox["LinkHand"] @ EventHandlerBox[{"MouseClicked", 1} :> body] @ box;
ClickBox[box_, body1_, body2_] := CursorIconBox["LinkHand"] @ EventHandlerBox[{{"MouseClicked", 1} :> body1, {"MouseClicked", 2} :> body2}] @ box;

(**************************************************************************************************)

SetHoldA[ClickFormOp, ClickBoxOp];

ClickFormOp[body_][expr_]           := ClickForm[expr, body];
ClickFormOp[body1_, body2_][expr_]  := ClickForm[expr, body1, body2];

ClickBoxOp[body_][box_]             := ClickBox[box, body];
ClickBoxOp[body1_, body2_][box_]    := ClickBox[box, body1, body2];

SetCurryBoxFn @ ClickBoxOp;

(**************************************************************************************************)

CursorIconBox[boxes_, icon_Str] := TagBox[boxes, MouseAppearanceTag[icon]];

StatusAreaBox[Fail, _]              := Fail;
StatusAreaBox[boxes_, label_String] := TagBox[boxes, Identity, TagBoxNote -> label];
StatusAreaBox[boxes_, label_]       := TagBox[boxes, Identity, TagBoxNote -> ToString[label, InputForm]];

EventHandlerBox[boxes_, rules_] := TagBox[boxes, EventHandlerTag @ ToList[rules, $eventHandlerRules]];

SetCurry2BoxFn[CursorIconBox, StatusAreaBox, EventHandlerBox]

$eventHandlerRules = {Method -> "Preemptive", PassEventsDown -> Automatic, PassEventsUp -> False};

(**************************************************************************************************)

CoreBox[UnlimitedRow[list_List]]        := RowBox @ MapMakeBox @ list;
CoreBox[UnlimitedRow[list_List, riff_]] := RiffledRowBox[MapMakeBox @ list, MakeBox @ riff];

(**************************************************************************************************)

CoreBox[LiteralStringRow[s:{StrP...}, r:StrP:","]] := LitStrRowBox[s, r];

SetBoxFn[LitStrRowBox];

LiteralStringBox[s_Str] := ToBoxes @ LitStr @ s; (* does this do anything ? *)

LitStrRowBox[{}, ___]              := LiteralStringBox[""];
LitStrRowBox[s:{__Str}, r_Str:","] := LiteralStringBox @ StrJoin @ Riffle[s, r];

(**************************************************************************************************)

SetForm1 @ TightForm;
SetBoxFn @ TightBox;

MakeBoxDefinitions[
  TightForm[e_] := TightBox @ MakeBoxes @ e;
]

TightBox[e_] := StyleBox[e, AutoSpacing -> False, AutoIndent -> False, LineBreakWithin -> False];

(**************************************************************************************************)

SetCurry2BoxFn @ SpanStyleBox;

SpanStyleBox[box_, {min_, max_}] := StyleBox[box, SpanMinSize -> min, SpanMaxSize -> max];

(**************************************************************************************************)

SetForm1[MarginForm, HMarginForm, VMarginForm, RaiseForm, LowerForm];

MakeBoxDefinitions[
  MarginForm[e_, m__]           := MarginBox[MakeBoxes @ e, m];
  HMarginForm[e_, h:NumP|Num2P] := HMarginBox[MakeBoxes, e, h];
  VMarginForm[e_, v:NumP|Num2P] := VMarginBox[MakeBoxes, e, v];
  RaiseForm[e_, n_ ? NumericQ]  := RaiseBox[MakeBoxes @ e, n];
  LowerForm[e_, n_ ? NumericQ]  := LowerBox[MakeBoxes @ e, n];
];

(**************************************************************************************************)

RaiseBox[e_, n_] := AdjustmentBox[e, BoxBaselineShift -> -n];
LowerBox[e_, n_] := AdjustmentBox[e, BoxBaselineShift -> n];

SetCurry2BoxFn[RaiseBox, LowerBox];

(**************************************************************************************************)

MarginBox = ExtendCaseOf[
  $[boxes_, n:NumP]                  := If[n > 0, $[boxes, {n, n}, {0, 0}], boxes];
  $[boxes_, {l_, r_}]                := $[boxes, {l, r}, {0, 0}];
  $[boxes_, {l_, r_}, {b_, t_}]      := AdjustmentBox[boxes, BoxMargins -> {{l, r}, {b, t}}];
  $[boxes_, {l_, r_}, {b_, t_}, bl_] := AdjustmentBox[boxes, BoxMargins -> {{l, r}, {b, t}}, BoxBaselineShift -> bl];
];

SetCurry2BoxFn @ MarginBox;

VMarginBox[boxes_, v_]       := VMarginBox[boxes, {v, v}];
VMarginBox[boxes_, {b_, t_}] := AdjustmentBox[boxes, BoxMargins -> {{0, 0}, {b, t}}];
HMarginBox[boxes_, h_]       := HMarginBox[boxes, {h, h}];
HMarginBox[boxes_, {l_, r_}] := AdjustmentBox[boxes, BoxMargins -> {{l, r}, {0, 0}}];

SetCurry2BoxFn[VMarginBox, HMarginBox];

(**************************************************************************************************)

SetForm1 @ NiceTooltip;

MakeBoxDefinitions[
  NiceTooltip[expr_, tooltip_] := NiceTooltipBox[MakeBoxes @ expr, BlockInteractive @ MakeBoxes @ tooltip];
];

(**************************************************************************************************)

SetCurry2BoxFn @ NiceTooltipBox;

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

SetForm1[ErrorTooltip];
SetBoxFn @ ErrorTooltipBox;

MakeBoxDefinitions[
  ErrorTooltip[expr_, rest___] := ErrorTooltipBox[MakeBoxes @ expr, rest]
];

ErrorTooltipBox[boxes_] := FrameBox[boxes,
  ContentPadding -> False, FrameStyle -> $LightRed,
  Background -> RGBColor[1,0.95,0.95]
];

ErrorTooltipBox[boxes_, rest__] := NiceTooltipBox[
  ErrorTooltipBox @ boxes,
  BlockInteractive @ MsgFormBox[rest]
];

(**************************************************************************************************)

SetBoxFn @ SkeletonBox;

SkeletonBox[b_] := RBox["\[LeftGuillemet]", b, "\[RightGuillemet]"];

(**************************************************************************************************)

SetBoxFn @ SpacerBox;

SpacerBox[s_] := TemplateBox[{s}, "Spacer1"];
SpacerBox[w_, h_] := TemplateBox[{w, h}, "Spacer2"];

(**************************************************************************************************)

FnRowBox[f_, {}]         := RowBox[{f, "[", "]"}];
FnRowBox[f_, {box_}]     := RowBox[{f, "[", box, "]"}];
FnRowBox[f_, row_List]   := RowBox[{f, "[", RowBox @ Riffle[row, ","], "]"}];
FnRowBox[f_, row_RowBox] := RowBox[{f, "[", row, "]"}];

FnRBox[f_Str, args___] := FnRowBox[f, {args}];

SetCurry1BoxFn[FnRowBox, FnRBox]

(**************************************************************************************************)

SetForm1[DelimitedRow];

RiffBox = RiffledRowBox;

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

SetForm1[RiffledRow];

SetCurry2 @ RiffledRow;

MakeBoxDefinitions[
  RiffledRow[a_List, r_] := RiffledRowBox[MakeBoxes /@ a, MakeBoxes @ r];
  RiffledSeq[r_][a___]   := RiffledRowBox[MakeBoxes @ r][MakeBoxes /@ {a}];
];

RiffledRBox[r_][b_]        := b;
RiffledRBox[r_][b___]      := RowBox @ Riffle[{b}, r];

RiffledRowBox[{b_}, r_]    := b;
RiffledRowBox[{bs___}, r_] := RowBox @ Riffle[{bs}, r];

SetCurry2BoxFn @ RiffledRowBox;

(**************************************************************************************************)

DeclareThenScan[DefineStyleFormBox]

DefineStyleFormBox[Then[boxSym_, formSym_, style_]] := (
  CoreBox[formSym[expr_]] := BuryStyleBox[style] @ MakeBoxes @ expr;
  boxSym[boxes_]          := BuryStyleBox[style] @ boxes;
);

DeclaredHere[UnderlinedBox, ItalicBox, SemiBoldBox, BoldBox, PlainBox, VeryLargeBox, LargeBox, MediumBox, SmallBox, VerySmallBox, TinyBox];
DeclaredHere[UnderlinedForm, ItalicForm, SemiBoldForm, BoldForm, PlainForm, VeryLargeForm, LargeForm, MediumForm, SmallForm, VerySmallForm, TinyForm];

SetBoxFn[UnderlinedBox, ItalicBox, SemiBoldBox, BoldBox, PlainBox, VeryLargeBox, LargeBox, MediumBox, SmallBox, VerySmallBox, TinyBox];

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

SetCurry2[FontColored, FontScaled, FontSized]

CoreBox[FontColored[expr_, color:(ColorP|NumP)]] := FontColorBox[MakeBox @ expr, color];
CoreBox[FontScaled[expr_, scale:NumP]] := FontScaleBox[MakeBox @ expr, scale];
CoreBox[FontSized[expr_, size:NumP]]  := FontScaleBox[MakeBox @ expr, size];

(**************************************************************************************************)

FontColorBox::badColor = "Not a color: ``.";
FontColorBox[boxes_, color_] := StyleBox[boxes, FontColor -> toFontColor[color]];

toFontColor = CaseOf[
  r_ ? NumQ   := GrayLevel[r];
  c_ ? ColorQ := c;
  e_          := (Message[FontColorBox::badColor, e]; Red)
];

FontSizeBox[boxes_, n_]         := StyleBox[boxes, FontSize -> n];
FontSizeBox[boxes_, Scaled[s_]] := FontScaleBox[boxes, s];
FontSizeDeltaBox[boxes_, n_]    := StyleBox[boxes, FontSize -> (Inherited + n)];
FontScaleBox[boxes_, n_]        := StyleBox[boxes, FontSize -> (Inherited * n)];

SetCurry2BoxFn[FontColorBox, FontSizeBox, FontSizeDeltaBox, FontScaleBox]

(**************************************************************************************************)

(* TODO: replace this with CompoundFormHeadP *)

$BuryThroughForms = Alt[_EventHandler, _NiceTooltip, _Style, _RaiseForm, _LowerForm, _MarginForm, _ClickForm, _NiceTooltip];
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

SetForm0 @ CodeStyle;
SetBoxFn @ CodeStyleBox;

CoreBox[CodeStyle[expr_, opts___Rule]] := CodeStyleBox[BlockFormatting @ MakeBoxes @ expr, opts];

CodeStyleBox[box_, opts___] := StyleBox[box, opts, FontFamily -> "Source Code Pro", ShowStrChars -> True];

(**************************************************************************************************)

SetForm1[RobotoFont, CodeFont, CodeSansFont, SansFont, SerifFont];

SetBoxFn[RobotoFont, CodeFont, CodeSansFont, SansFont, SerifFont];
DeclaredHere[RobotoFont, CodeFont, CodeSansFont, SansFont, SerifFont];
DeclaredHere[RobotoFontBox, CodeFontBox, CodeSansFontBox, SansFontBox, SerifFontBox];

(* CoreBox[formSym[expr_]] := BuryStyleBox[style] @ MakeBoxes @ expr; *)
FontBox[e_, f_Str] := BuryStyleBox[FontFamily -> f][e];
SetCurry2BoxFn[FontBox]

declareFontBoxForm[boxSym_, formSym_, family_] := (
  boxSym[boxes_] := BuryStyleBox[FontFamily -> family][boxes];
  SystemBox[formSym[expr_]] := boxSym @ MakeBoxes @ expr;
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

SetBoxFn[FlattenStyleBox];
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
  a_rboxSym            := fnSeq[x] @@ Unevaluated[a],
  boxSym[e_List]       := fnRow[x] @ e,
  boxSym[e_List, m_]   := fnRow[x] @ e
];

defSeqRowFormBox[_, _][args___] := Print @ List[args];

DefineSeqRowFormBox[fnSeq_, fnRow_, tuples__List] := MapApply[defSeqRowFormBox[fnSeq, fnRow], {tuples}];

SetForm1[BraceRow, AngleRow, ParenRow, BracketRow, DBracketRow];

DeclaredHere[BraceSeq, AngleSeq, ParenSeq, BracketSeq, DBracketSeq];
DeclaredHere[BraceRow, AngleRow, ParenRow, BracketRow, DBracketRow];
DeclaredHere[BraceRBox, AngleRBox, ParenRBox, BracketRBox, DBracketRBox];
DeclaredHere[BraceRowBox, AngleRowBox, ParenRowBox, BracketRowBox, DBracketRowBox];
SetBoxFn[BraceRBox, AngleRBox, ParenRBox, BracketRBox, DBracketRBox];
SetBoxFn[BraceRowBox, AngleRowBox, ParenRowBox, BracketRowBox, DBracketRowBox];

DefineSeqRowFormBox[DelimitedRBox, DelimitedRowBox,
  {BraceSeq,    BraceRow,    BraceRBox,    BraceRowBox,    {"{", ",", "}"}},
  {AngleSeq,    AngleRow,    AngleRBox,    AngleRowBox,    {"\[LeftAngleBracket]", ",", "\[RightAngleBracket]"}},
  {ParenSeq,    ParenRow,    ParenRBox,    ParenRowBox,    {"(", ",", ")"}},
  {BracketSeq,  BracketRow,  BracketRBox,  BracketRowBox,  {"[", ",", "]"}},
  {DBracketSeq, DBracketRow, DBracketRBox, DBracketRowBox, {"\[LeftDoubleBracket]", ",", "\[RightDoubleBracket]"}}
];

(**************************************************************************************************)

SetForm1[SpaceRow, CommaRow, ColonRow, SColonRow, ArrowRow];

DeclaredHere[SpaceSeq, CommaSeq, ColonSeq, SColonSeq, ArrowSeq];
DeclaredHere[SpaceRow, CommaRow, ColonRow, SColonRow, ArrowRow];
DeclaredHere[SpaceRBox, CommaRBox, ColonRBox, SColonRBox, ArrowRBox];
DeclaredHere[SpaceRowBox, CommaRowBox, ColonRowBox, SColonRowBox, ArrowRowBox];
SetBoxFn[SpaceRBox, CommaRBox, ColonRBox, SColonRBox, ArrowRBox];
SetBoxFn[SpaceRowBox, CommaRowBox, ColonRowBox, SColonRowBox, ArrowRowBox];

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

DeclaredHere[Comma, CommaS, GComma, GCommaS, Quad];

MakeBoxDefinitions[
  Comma   := CommaB;
  CommaS  := CommaSB;
  GComma  := GCommaB;
  GCommaS := GCommaSB;
  Quad    := QuadB;
];

CommaB = ",";
CommaSB = ",\[ThinSpace]";
GCommaB = StyleBox[",", $Gray];
GCommaSB = StyleBox[",\[ThinSpace]", $Gray];
QuadB = TemplateBox[{8}, "Spacer1"];

(**************************************************************************************************)

SetForm1[CommaSForm, GCommaForm, GCommaSForm, RepCommaForm];

DeclaredHere[CommaSForm, GCommaForm, GCommaSForm, RepCommaForm];

MakeBoxDefinitions[
  CommaSForm[e_]       := CommaSBox  @ MakeBoxes @ e;
  GCommaForm[e_]       := GCommaBox  @ MakeBoxes @ e;
  GCommaSForm[e_]      := GCommaSBox @ MakeBoxes @ e;
  RepCommaForm[e_, c_] := RepCommaBox[MakeBoxes @ c] @ MakeBoxes @ e;
  RepCommaForm[c_][e_] := RepCommaBox[MakeBoxes @ c] @ MakeBoxes @ e;
];

DeclaredHere[CommaSBox, GCommaBox, GCommaSBox, RepCommaBox];

CommaSBox[e_]  := RepCommaBox[CommaSB]  @ e;
GCommaBox[e_]  := RepCommaBox[GCommaB]  @ e;
GCommaSBox[e_] := RepCommaBox[GCommaSB] @ e;

RepCommaBox[b_, c_]                 := RepCommaBox[c] @ b;
r_RepCommaBox[b_:$BuryThroughBoxes] := MapFirst[r, b];
r_RepCommaBox[list_List]            := VectorReplace[list, "," -> First[r]];


(**************************************************************************************************)

ApplyEndStyleBox[s___][RowBox[{l_, m___, r_}]] := RowBox[{StyleBox[l, s], m, StyleBox[r, s]}];
ApplyEndStyleBox[___][boxes_] := boxes;

(**************************************************************************************************)

SetBoxFn[ApplyIndentBox]
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

SetForm1[Dimmed, GrayForm]
SetBoxFn[GrayBox, DimmedBox]

MakeBoxDefinitions[
  Dimmed[e_]   := DimmedBox @ ToBoxes @ e;
  GrayForm[e_] := GrayBox @ ToBoxes @ e;
];

GrayBox[e_]   := StyleBox[e, $Gray];
DimmedBox[e_] := StyleBox[e, Opacity[0.3]];

(**************************************************************************************************)

GridBoxRule = CaseOf[
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

MakeBoxDefinitions[
  RowGrid[list_List, sp:NumP:1.0, opts___Rule] := RowGridBox[MapMakeBox @ list, N @ sp, opts];
  ColGrid[list_List, sp:NumP:1.0, opts___Rule] := ColGridBox[MapMakeBox @ list, N @ sp, opts];
  TightRowGrid[list_List, opts___Rule]         := TightRowGridBox[MapMakeBox @ list, opts];
  TightColGrid[list_List, opts___Rule]         := TightColGridBox[MapMakeBox @ list, opts];
];

RowGridBox[list_List, sp:NumP:1.0, opts___Rule] := GridBox[ToRowVec @ list, opts, ColGaps -> sp, $baseGridOpts];
ColGridBox[list_List, sp:NumP:0.8, opts___Rule] := GridBox[ToColVec @ list, opts, RowGaps -> sp, $baseGridOpts];

$baseGridOpts = Seq[ColJust -> Left, BaselinePos -> {1,1}];

(**************************************************************************************************)

SetBoxFn[TightRowGridBox, TightColGridBox];

TightRowGridBox[list_List, opts___Rule] := RowGridBox[list, opts, $tightGridOpts];
TightColGridBox[list_List, opts___Rule] := ColGridBox[list, opts, $tightGridOpts];

$tightGridOpts = Seq[RowGaps -> 0, ColGaps -> 0, GridFrameMargins -> {{0, 0}, {0, 0}}];

(**************************************************************************************************)

SetForm1[RawRow, RawColumn, RawGrid]

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

SetBoxFn[ColumnBox]

ColumnBox::usage = "ColumnBox[list, align?, spacing?, baseline?].";

ColumnBox[list_List, args___, opts___Rule] :=
  iColumnBox[list, {opts}, args];

iColumnBox[list_, opts_, align_:Left, spacing_:Auto, baseLine_:Auto] := GridBox[
  ToList /@ list, Seq @@ opts,
  BaselinePos -> toColBase[baseLine], ColJust -> align, RowGaps -> spacing
];

toColBase = CaseOf[
  Top    := Scaled[1.0];
  Center := Scaled[0.5];
  Bottom := Scaled[0.0];
  n_Int  := {n, 1};
  Auto   := {1, 1};
];

(**************************************************************************************************)

SetBoxFn[UnderBraceBox, OverBraceBox, UnderBracketBox, OverBracketBox, UnderParenBox, OverParenBox];

UnderBraceBox[a_, b_] := UnderscriptBox[UnderscriptBox[a, "\[UnderBrace]"], b];
OverBraceBox[a_, b_] := OverscriptBox[OverscriptBox[a, "\[OverBrace]"], b];

UnderBracketBox[a_, b_] := UnderscriptBox[UnderscriptBox[a, "\[UnderBracket]"], b];
OverBracketBox[a_, b_] := OverscriptBox[OverscriptBox[a, "\[OverBracket]"], b];

UnderParenBox[a_, b_] := UnderscriptBox[UnderscriptBox[a, "\[UnderParenthesis]"], b];
OverParenBox[a_, b_] := OverscriptBox[OverscriptBox[a, "\[OverParenthesis]"], b];

(**************************************************************************************************)

SetBoxFn[NamedTextIconBox, TextIconBox];

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
      ImagePadding -> pad, BaselinePos -> Axis, Background -> background
    ],
    BoxBaselineShift -> (-bshift / Round @ CurrentValue[FontSize])
  ], TrackedSymbols :> {}
];

(**************************************************************************************************)

SetForm1[BoldSyntaxForm]
SetBoxFn[BoldSyntaxBox]

CoreBox[BoldSyntaxForm[expr_]] := BoxBurrowing[BoldSyntaxBox] @ MakeBoxes[expr];

BoldSyntaxBox[RowBox[{a_, syn_Str, b_}]] := RowBox[{a, " ", BoldBox[syn], " ", b}];
BoldSyntaxBox[boxes_] := boxes;

(**************************************************************************************************)

DeclareSeqScan[DeclareExpanderBoxes]

DeclareExpanderBoxes[sym_Sym] := CoreBox[sym[args___]] := HoldExpanderBoxes[sym, args];

(**************************************************************************************************)

$remExpansions = Inf;

CoreBox[ExpanderForm[head_Sym[args___]]] :=
  StyleBox[HoldExpanderBoxes[head, args], ShowStringCharacters -> True];

CoreBox[ExpanderForm[head_Sym[args___], level_]] := StyleBox[
  BlockSet[$remExpansions, level, HoldExpanderBoxes[head, args]],
  StyleBox[HoldExpanderBoxes[head, args], ShowStringCharacters -> True]
];

SetHoldC[HoldExpanderBoxes, makeExpanderBoxes1, makeExpanderBoxes2, openHead, closeHead];

ExpanderBoxes[args___] := HoldExpanderBoxes[args];
HoldExpanderBoxes[head_Sym, args___] := makeExpanderBoxes2[head, {args}];

makeExpanderBoxes1[expr_] := MakeBoxes[expr];
makeExpanderBoxes1[head_Sym[args___]] := makeExpanderBoxes2[head, {args}];

makeExpanderBoxes2[Rule, {d:DatumP, rhs_}] :=
  joinFirstRow[RowBox[{MakeBoxes @ d, "\[Rule]", makeExpanderBoxes1 @ rhs}]];

joinFirstRow[boxes_] := boxes;
joinFirstRow[RowBox[{a_, b_, GridBox[{{f1_, fr___}, rest___}, opts___]}]] :=
  GridBox[{{RowBox[{a, b, f1}], fr}, rest}, opts];


makeExpanderBoxes2[head_Sym, {}] := RBox[openHead @ head, closeHead @ head];
makeExpanderBoxes2[head_Sym, args_List] := ColumnBox[
  FlatList[
    openHead @ head,
    MapMostLast[
      addTabComma, RBox["\t", #]&,
      If[$remExpansions > 0,
        BlockDecrement[$remExpansions, HoldMap[makeExpanderBoxes1, args]],
        MapMakeBox @ args
      ]
    ],
    closeHead @ head
  ],
  Left,
  RowAlignments -> Baseline
];

openHead[head_] := RBox[MakeBoxes @ head, "["];
openHead[List]  := "{";
openHead[Dict]  := LAssoc;

closeHead[_]    := "]";
closeHead[List] := "}";
closeHead[Dict] := RAssoc;

addTabComma[boxes_] := RBox["\t", boxes, ","];
addTabComma[GridBox[grid_, opts___]] := RBox["\t", Make[GridBox, MapAt[addComma, grid, {-1, -1}], opts]];
addComma[box_] := RBox[box, ","];
