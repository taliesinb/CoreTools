PackageExports[
  "BoxFunction",
    RBox,
    RaiseBox, LowerBox, MarginBox, ColumnBox, TightBox, NiceTooltipBox, SkeletonBox,
    FnRowBox, FnBracketRowBox, FnParenRowBox,
    DelimitedRBox,   RiffledRBox,   SpaceRBox,   CommaRBox,   ColonRBox,   SColonRBox,   ArrowRBox,   BraceRBox,   AngleRBox,   ParenRBox,   BracketRBox,   PartRBox,   AssocRBox,
    DelimitedRowBox, RiffledRowBox, SpaceRowBox, CommaRowBox, ColonRowBox, SColonRowBox, ArrowRowBox, BraceRowBox, AngleRowBox, ParenRowBox, BracketRowBox, PartRowBox, AssocRowBox,
    DimmedBox, StatusAreaBox, CursorIconBox, LiteralStringBox,
    ApplyEndStyleBox, ApplyIndentBox,
    UnderlinedBox, ItalicBox, SemiBoldBox, BoldBox, PlainBox, FontBox,
    VeryLargeBox, LargeBox, MediumBox, SmallBox, VerySmallBox, TinyBox,
    ClickBox,
    FlattenStyleBox, BuryStyleBox,
  "FormHead",
    RaiseForm, LowerForm, MarginForm, RawColumn, RawRow, RawGrid, TightForm, NiceTooltip,
    DelimitedSeq, RiffledSeq, SpaceSeq, CommaSeq, ColonSeq, SColonSeq, ArrowSeq, BraceSeq, AngleSeq, ParenSeq, BracketSeq, PartSeq,
    DelimitedRow, RiffledRow, SpaceRow, CommaRow, ColonRow, SColonRow, ArrowRow, BraceRow, AngleRow, ParenRow, BracketRow, PartRow,
    Dimmed, LiteralStringForm,
    UnderlinedForm, ItalicForm, SemiBoldForm, BoldForm, PlainForm,
    VeryLargeForm, LargeForm, MediumForm, SmallForm, VerySmallForm, TinyForm,
    ClickForm,
    FlattenStyle, BuryStyle,
  "IOFunction",
    CoreBoxes, SystemBoxes,
  "DebuggingFunction",
    MakeCoreBoxes,
  "FormHead",
    Unformatted, Uninteractive,
  "ScopingFunction",
    DisableCoreBoxFormatting, DisableCoreBoxInteractivity,
  "MetaFunction",
    DeclareCoreBoxes, DeclareCoreSubBoxes, MakeBoxDefinitions, DefineStyleFormBox, DefineSeqRowFormBox,
  "SpecialVariable",
    $UseCoreBoxFormatting, $UseCoreBoxInteractivity,
  "Predicate",
    HasCoreBoxesQ,
  "MetaFunction"
];

(**************************************************************************************************)

DeclareHoldAllComplete[HasCoreBoxesQ]
DeclarePredicate1[HasCoreBoxesQ]

SetInitial[$coreBoxHead, UAssoc[]]

HasCoreBoxesQ[s_] = Lookup[$coreBoxHead, Hold @ s, False];

(**************************************************************************************************)

DeclareHoldAllComplete[MakeCoreBoxes, DisableCoreBoxFormatting, DisableCoreBoxInteractivity]

(* we'll add to these definitions *)
MakeCoreBoxes[e_] := FailEval;

DisableCoreBoxFormatting[body_]    := Block[{$UseCoreBoxFormatting = False}, body];
DisableCoreBoxInteractivity[body_] := Block[{$UseCoreBoxInteractivity = False}, body];

MakeBoxes[Unformatted[lhs_], form:StandardForm | TraditionalForm]   := DisableCoreBoxFormatting @ MakeBoxes[lhs, form];
MakeBoxes[Uninteractive[lhs_], form:StandardForm | TraditionalForm] := DisableCoreBoxInteractivity @ MakeBoxes[lhs, form];

(**************************************************************************************************)

DeclareHoldAll[DeclareCoreBoxes, DeclareCoreSubBoxes]
DeclareDeclare[DeclareCoreBoxes, DeclareCoreSubBoxes]

SetInitial[$UseCoreBoxFormatting, True]
SetInitial[$UseCoreBoxInteractivity, True]

DeclareCoreBoxes[sym_Symbol] := With[
  {name = SymbolName[sym]},

  $coreBoxHead[Hold[sym]] = True;

  MakeBoxes[$LHS:sym | _sym, StandardForm] /; $UseCoreBoxFormatting :=
    TryEval @ StatusAreaBox[MakeCoreBoxes @ $LHS, name];

  MakeBoxes[$LHS:sym | _sym, TraditionalForm] :=
    TryEval @ MakeCoreBoxes @ $LHS;
];

DeclareCoreSubBoxes[sym_Symbol] := With[
  {name = SymbolName[sym]},

  $coreBoxHead[Hold[sym]] = True;

  MakeBoxes[$LHS:(_sym[___]), StandardForm] /; $UseCoreBoxFormatting :=
    TryEval @ StatusAreaBox[MakeCoreBoxes @ $LHS, name];

  MakeBoxes[$LHS:(_sym[___]), TraditionalForm] :=
    TryEval @ MakeCoreBoxes @ $LHS;
];

(**************************************************************************************************)

CoreBoxes::unknownHead = "Don't know which symbol CoreBox rules are associated with."
setupCoreBoxes[_] := Message[CoreBoxes::unknownHead];
setupCoreBoxes[Hold[sym_Symbol]] := (DeclareCoreBoxes[sym]; setupCoreBoxes[sym] := Null);

CoreBoxes /: SetDelayed[CoreBoxes[lhs_], rhs_] := (
  setupCoreBoxes @ PatternHeadSymbol @ lhs;
  MakeCoreBoxes[lhs] := rhs;
);

Protect[CoreBoxes];

(**************************************************************************************************)

SystemBoxes /: SetDelayed[SystemBoxes[lhs_], rhs_] := (
  MakeBoxes[lhs, StandardForm | TraditionalForm] := rhs;
);

Protect[SystemBoxes];

(**************************************************************************************************)

DeclareThenScan[MakeBoxDefinitions]

MakeBoxDefinitions[SetDelayed[lhs_, rhs_]] := (
  MakeBoxes[lhs, StandardForm | TraditionalForm] := rhs;
);

(**************************************************************************************************)

RBox[args___] := RowBox[{args}];

(**************************************************************************************************)

DeclareHoldRest[ClickBox, ClickForm]

CoreBoxes[ClickForm[expr_, body_]] := ClickBox[MakeBoxes @ expr, body];

ClickBox[box_, body_] := TagBox[
  TagBox[box, EventHandlerTag[{
    {"MouseClicked", 1} :> body,
    Method -> "Preemptive",
    PassEventsDown -> Automatic, PassEventsUp -> True
    }]
  ],
  MouseAppearanceTag["LinkHand"]
];

(**************************************************************************************************)

DeclareCurry2[CursorIconBox, StatusAreaBox]

CursorIconBox[boxes_, icon_Str] := TagBox[boxes, MouseAppearanceTag[icon]];

StatusAreaBox[Fail, _] := Fail;
StatusAreaBox[boxes_, label_String] := TagBox[boxes, Identity, TagBoxNote -> label];
StatusAreaBox[boxes_, label_]       := TagBox[boxes, Identity, TagBoxNote -> ToString[label, InputForm]];

(**************************************************************************************************)

(* even works in InputForm *)
LiteralStringBox[s_Str] := $PrintLiteral[s];
CoreBoxes[LiteralStringForm[s_Str]] := LiteralStringBox[s];

(**************************************************************************************************)

MakeBoxDefinitions[
  TightForm[e_] := TightBox @ MakeBoxes @ e;
]

TightBox[e_] := StyleBox[e, AutoSpacing -> False, AutoIndent -> False, LineBreakWithin -> False];

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

FnRowBox[p_, RowBox[{a__}]] := RowBox[{p, a}];
FnRowBox[p_, a_] := RowBox[{p, a}];

FnBracketRowBox[f_, list_List] := FnRowBox[f, BracketRowBox[list]];
FnParenRowBox[f_, list_List] := FnRowBox[f, ParenRowBox[list]];

(**************************************************************************************************)

MakeBoxDefinitions[
  DelimitedRow[l_, m_, r_][a_List] := RiffledRowBox[MakeBoxes @ l, MakeBoxes @ m, MakeBoxes @ r][MakeBoxes /@ a];
  DelimitedSeq[l_, m_, r_][a___]   := MakeBoxes @ DelimitedRow[l, m, r] @ list @ a;
];

DelimitedRBox[l_, m_, r_][]       := RowBox @ {l, r};
DelimitedRBox[l_, m_, r_][bs__]   := RowBox @ {l, RiffledRBox[m][bs], r};

DelimitedRowBox[l_, m_, r_][{}]      := RowBox @ {l, r};
DelimitedRowBox[l_, m_, r_][bs_List] := RowBox @ {l, RiffledRowBox[m][bs], r};

(**************************************************************************************************)

MakeBoxDefinitions[
  RiffledRow[r_][a_List] := RiffledRowBox[MakeBoxes @ r][MakeBoxes /@ a];
  RiffledSeq[r_][a___]   := RiffledRowBox[MakeBoxes @ r][MakeBoxes /@ {a}];
];

RiffledRBox[r_][b_] := b;
RiffledRBox[r_][b___] := RowBox @ Riffle[{b}, r];

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

$BuryThroughForms = Alt[EventHandler, NiceTooltip, Style];
$BuryThroughBoxes = Alt[_TagBox, _TooltipBox, _StyleBox];

s_BuryStyle[expr:$BuryThroughForms] := MapFirst[s, expr];
s_BuryStyle[expr_]                  := Style[expr, Seq @@ s];

s_BuryStyleBox[boxes:$BuryThroughBoxes] := MapFirst[s, boxes];
s_BuryStyleBox[boxes_]                  := StyleBox[boxes, Seq @@ s];

(**************************************************************************************************)

(* CoreBoxes[formSym[expr_]] := BuryStyleBox[style] @ MakeBoxes @ expr; *)
DeclareCurry2[FontBox]
FontBox[e_, f_] := BuryStyleBox[FontFamily -> f][e];

(**************************************************************************************************)

FlattenStyle[Style[Style[expr_, s1___], s2___]] := FlattenStyle @ Style[expr, s1, s2];
FlattenStyle[expr_] := expr;

FlattenStyleBox[StyleBox[StyleBox[boxes_, s1___], s2___]] := FlattenStyleBox @ StyleBox[boxes, s1, s2];
FlattenStyleBox[boxes_] := boxes;

(**************************************************************************************************)

DefineSeqRowFormBox[fnSeq2_, fnRow2_, tuples__List] := MapApply[
  {seqSym, rowSym, rboxSym, boxSym, data} |-> With[
    {fnSeq = fnSeq2 @@ data, fnRow = fnRow2 @@ data},
    MakeBoxDefinitions[
      a_seqSym       := fnSeq @@ Map[MakeBoxes, Apply[List, Unevaluated @ a]];
      rowSym[l_List] := fnRow @ Map[MakeBoxes, l];
    ];
    a_rboxSym        := fnSeq @@ Unevaluated[a];
    boxSym[l_List]   := fnRow @ l;
  ],
  {tuples}
];

DefineSeqRowFormBox[DelimitedRBox, DelimitedRowBox,
  {BraceSeq,   BraceRow,   BraceRBox,   BraceRowBox,   {"{", ",", "}"}},
  {AngleSeq,   AngleRow,   AngleRBox,   AngleRowBox,   {"⟨", ",", "⟩"}},
  {ParenSeq,   ParenRow,   ParenRBox,   ParenRowBox,   {"(", ",", ")"}},
  {BracketSeq, BracketRow, BracketRBox, BracketRowBox, {"[", ",", "]"}},
  {PartSeq,    PartRow,    PartRBox,    PartRowBox,    {"⟦", ",", "⟧"}}
];

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
    GridBoxSpacings -> {"Rows" -> {{spacing}}}
  ];

