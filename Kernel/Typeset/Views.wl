PackageExports[
  "FormHead",
    ListView,
    NestedView,
    GroupedView,
    RowView, ColumnView, RowColumnView,
    LabeledFlipView,
    PickView, MapView, EitherView,
  "BoxFunction",
    ClickAnimateBoxes, DynamicProgressBarBox,
    NiceClickBox, DeployBox,
    TightRowGridBox, TightColumnGridBox,
    OpenerColumnBox, DeployBox,
  "OptionSymbol",
    ClickFunction, ViewSize,
  "Function",
    LabelBy, ViewSampling
];

PrivateExports[
  "MetaFunction",
    DefineViewForm
];

(*************************************************************************************************)

DeclareCurry2[LabelBy];

LabelBy[list_List, fn_] := Map[item |-> Labeled[item, fn[item]], list];

(*************************************************************************************************)

$genericViewOptions = {
  ClickFunction -> None,
  ViewSize      -> 16,
  MaxItems      -> Inf,
  ItemFunction  -> None
};

$viewOption = UDict[];

DeclareHoldFirst[makeViewBoxesCommon]

(* first apply the explicit options, then whatever we've inherited
from higher up, then the defaults for this view *)
makeViewBoxesCommon[body_, form_Symbol, rules___Rule] := Block[
  {$currentViewDepth = $currentViewDepth + 1,
   $viewOption = UDict @ ToList[Options @ form, $viewOption, rules]},
  body
];

(*************************************************************************************************)

DeclareStrict[DefineViewForm]

DefineViewForm[RuleD[form_Symbol[lhs___], rhs_]] := Then[
  Options[form] = $genericViewOptions,
  CoreBoxes[form[lhs, opts___Rule]] := makeViewBoxesCommon[rhs, form, opts]
];

(*************************************************************************************************)

DefineViewForm[GroupedView[list_List, fns_] :> groupedView[list, fns]];

Options[GroupedView] = JoinOptions[{ViewSize -> 8, LabelFunction -> CodePane, ViewSampling -> False}, $genericViewOptions];

groupedView[list_List, fns2_] := Locals[
  fns = fns2;
  If[RuleQ[fns],
    {fns, $itemFn} = FirstLast @ fns,
    $itemFn = $viewOption[ItemFunction];
  ];
  SetNone[$itemFn, Id];
  fns = ToList @ fns;
  depth = Len @ fns;
  groups = GroupBy[list, fns];
  $spanCol = ConstList["\[SpanFromLeft]", depth];
  $lastPath = ConstList[$dummy, depth];
  $labelPath = {};
  $viewSize = $viewOption[ViewSize];
  Collecting[$cols, PathScan[$labelPath, visitGroup[depth], KeySort @ groups]];
  grid = GridBox[
    Flip @ Prepend[Prepend[groupFnBox /@ Rev[fns], ""]] @ $cols,
    FrameStyle -> GrayLevel[0.75],
    Dividers -> All,
    ColumnSpacings -> 2,
    RowMinHeight -> {1.2, {1.3}},
    GridBoxDividers -> {"Columns" -> {{True}}, "Rows" -> {{True}}},
    GridFrameMargins -> {{1, 1}, {1, 1}}
  ];
  FrameBox[grid, Background -> GrayLevel[1], FrameStyle -> None]
];

visitGroup[1][items_] := Module[{labels},
  noSplit = True;
  labels = ZipMap[
    {this, last} |-> If[noSplit && SameQ[this, last], "\[SpanFromLeft]", noSplit = False; groupLabelBox @ this],
    $labelPath, $lastPath
  ];
  $lastPath = $labelPath;
  boxes = MapFirstRest[
    item |-> $cols @ Prepend[Rev @ labels, groupItemBox @ item],
    item |-> $cols @ Prepend[$spanCol, dimLeftBox @ groupItemBox @ item],
    limitItems[items, $viewSize]
  ];
  If[Len[items] > $viewSize,
    $cols @ Prepend[$spanCol, groupEllipsisBox[Len[items] - $viewSize]]
  ];
];

limitItems[items_] := limitItems[items, $viewOption[ViewSize]];
limitItems[items_, size_] := Which[
  Len[items] <= size,        items,
  $viewOption[ViewSampling], RandomChoice[items, size],
  True,                      Take[items, $viewSize]
];

fnBoxes[fn_] := CodePaneBoxes[fn, {UpTo[200], UpTo[30]}];
groupEllipsisBox[n_] := dimLeftBox @ OverscriptBox["\[Ellipsis]", NatStr[n]];
groupFnBox[list_List]   := ColumnBox[fnBoxes /@ list, Left, 0];
groupFnBox[f_RightComposition] := groupFnBox[List @@ f];
groupFnBox[other_]     := fnBoxes @ other;
groupItemBox[item_]    := ToBoxes @ $itemFn @ item;
groupLabelBox[label_]  := ToBoxes @ $viewOption[LabelFunction] @ label;

dimLeftBox[box_] := ItemBox[box, Frame -> {{GrayLevel[0.95], Auto}, {Auto, Auto}}];

visitGroup[d_][dict_] := PathScan[$labelPath, visitGroup[d-1], KeySort @ dict];


(*************************************************************************************************)

DefineViewForm[ListView[list_List] :> listBrowserBoxes[list]];

listBrowserBoxes[list_List] := With[
  {n$$ = Len @ list},
  {blue = $LightBlue, gray = GrayLevel[0.95], purple = $LightPurple},
  {clickFn = $viewOption @ ClickFunction},
  {itemBox = FrameBox[
    DynamicBox[
      ClickBox[
        ToBoxes @ Set[r$$, Part[list, i$$]],
        clickFn @ r$$
      ],
      TrackedSymbols :> {i$$}],
    FrameMargins -> 10, FrameStyle -> {Thick, $LightGray}
   ],
  buttonRowBox = makeStandardBrowseArrowBoxes[
    i$$, n$$,
    NiceClickBox["\[DownArrow]", PrintInputCell @ Part[list, i$$], "Pink"]
  ],
  progressBox = DynamicProgressBarBox[{i$$, n$$}, {200, 10}]},
  DynamicModuleBox[
    {i$$ = 1, r$$ = None},
    GridBox[
      {{buttonRowBox}, {progressBox}, {itemBox}},
      GridBoxSpacings -> {"Rows" -> {0.1, 0.1, {.5}}},
      GridBoxAlignment -> {"Columns" -> {{Left}}}
    ],
    DynamicModuleValues -> {i$$}
  ]
];

(*************************************************************************************************)

Options[NestedView] = $genericViewOptions;

NestedView::notNestedExpression = "Cannot use nesting for non-compound data with head ``.";
NestedView[expr_, opts___Rule] := Locals[
  $nestedViewOpts = opts;
  If[ListDictQ[expr],
    iNestedView @ expr
  ,
    Message[NestedView::notNestedExpression, Head @ expr];
    expr
  ]
];

iNestedView = CaseOf[
  array_ ? PackedQ := DataForm @ array;
  list:ListDictP   := RowColumnView[$ /@ list, $nestedViewOpts];
  other_           := $Failed
];

$rowOrColumnDepth = 0;

(*************************************************************************************************)

DefineViewForm[RowColumnView[items:ListDictP] :> rowColumnViewBoxes @ items];

DeclareHoldAllComplete[rowColumnViewBoxes, rowableQ];

rowableQ[items_] := HoldLen[items] < 32 && AllTrue[items, DatumQ];

rowColumnViewBoxes[items_ ? rowableQ] := rowViewBoxes @ list;
rowColumnViewBoxes[items_]            := columnViewBoxes @ list;

(*************************************************************************************************)

DefineViewForm[RowView[items:ListDictP] :> rowViewBoxes @ items];

DeclareHoldAllComplete[rowViewBoxes];

rowViewBoxes[items_List] := GridBox[ToRowVec   @ MapMakeBoxes @ items, $rowViewGridOpts];
rowViewBoxes[items_Dict] := GridBox[KeysValues @ MapMakeBoxes @ items, $rowViewGridOpts];

$rowViewGridOpts = Seq[
  GridBoxDividers -> {"Columns" -> {False, {True}, False}, "Rows" -> {{None}}},
  FrameStyle -> GrayLevel[0.5],
  BaselinePosition -> {{1, 1}, Baseline},
  RowMinHeight -> 1.2, ColumnSpacings -> 1.2
];

(*************************************************************************************************)

DefineViewForm[ColumnView[items:ListDictP] :> columnViewBoxes @ items];

DeclareHoldAllComplete[columnViewBoxes];

columnViewBoxes[items_List] := GridBox[ToColVec    @ MapMakeBoxes @ items, $rowViewGridOpts];
columnViewBoxes[items_Dict] := GridBox[DictToPairs @ MapMakeBoxes @ items, $rowViewGridOpts];

$rowViewGridOpts = Seq[
  GridBoxDividers -> {"Columns" -> {False, {True}, False}, "Rows" -> {{None}}},
  FrameStyle -> GrayLevel[0.5],
  BaselinePosition -> {{1, 1}, Baseline},
  RowMinHeight -> 1.2, ColumnSpacings -> 1.2
];

(**************************************************************************************************)

CoreBoxes[LabeledFlipView[items:ListDictP, opts___Rule]] :=
  blockView @ labeledFlipViewBoxes[items, opts];

labeledFlipViewBoxes[items_Dict, opts___] :=
  labeledFlipViewBoxes[Normal @ items, opts];

labeledFlipViewBoxes[items_List, opts___] :=
  labeledFlipViewBoxes[RangeRules @ items, opts];

labeledFlipViewBoxes[items:{___Rule}, opts___] := With[
  {range = Range @ Len @ items, keys = ToBoxes /@ Keys @ items, vals = ToBoxes /@ Values @ items},
  {keys$$ = RuleThread[range, keys], vals$$ = RuleThread[range, vals]},
  {isize = Lookup[{opts}, ImageSize, Auto], labelPos = Lookup[{opts}, LabelPosition, Top]},
  {togglerBox = CursorIconBox["LinkHand"] @ StyleBox[
    TogglerBox[Dynamic[i$$], keys$$, ImageSize -> All],
    Bold, FontFamily -> "Fira", FontSize -> 10]
  },
  {paneSelectorBox = PaneSelectorBox[vals$$, Dynamic[i$$], ImageSize -> isize, Alignment -> {Left, Top}]},
  Switch[labelPos,
    Top,
    DynamicModuleBox[{i$$ = 1},
      GridBox[{{togglerBox}, {paneSelectorBox}}, ColumnAlignments -> Left, RowAlignments -> Center]
    ],
    Left,
    DynamicModuleBox[{i$$ = 1},
      GridBox[{{RotationBox[togglerBox, BoxRotation -> 1.5708], paneSelectorBox}}, ColumnAlignments -> Left, RowAlignments -> Left]
    ],
    _,
    Message[LabeledFlipView::badLabelPos, labelPos]; $Failed
  ]
];
LabeledFlipView::badLabelPos = "LabelPosition -> `` should be either Top or Left."

(**************************************************************************************************)

Options[PickView] = Options[ListView];

CoreBoxes[PickView[list_List]] := blockView @ pickBrowserBoxes[list];

pickBrowserBoxes[list_List, opts:OptionsPattern[]] := With[
  {n$$ = Len @ list},
  {blue = $LightBlue, gray = GrayLevel[0.95], purple = $LightPurple},
  {clickFn = OptionValue[PickView, {opts}, ClickFunction]},
  {itemBox = FrameBox[
    DynamicBox[
      ClickBox[
        ToBoxes @ Set[r$$, Part[list, i$$]],
        selected$$ //= If[MemberQ[selected$$, i$$], DelCases[i$$], Append[i$$] /* Sort]
      ],
      TrackedSymbols :> {i$$}
    ],
    FrameMargins -> 10,
    FrameStyle -> Dynamic[
      If[MemberQ[selected$$, i$$], {Thick, $LightGreen}, {Thick, $LightGray}],
      TrackedSymbols :> {selected$$, i$$}
    ]
  ],
  buttonRowBox = makeStandardBrowseArrowBoxes[
    i$$, n$$,
    NiceClickBox["\[DownArrow]", PrintInputCell @ Part[list, i$$], "Pink"]
  ],
  progressBox =   DynamicProgressBarBox[{i$$, n$$}, {200, 10}],
  pickedButtonRowBox = makeBrowseArrowBoxes[
    {"Green", "Green"},
    i$$ = F[selected$$, i$$], i$$ = Replace[Max @ Select[selected$$, LessThan @ i$$], -Inf -> i$$],
    fractionBox[IndexOf[selected$$, i$$, "?"], Len @ selected$$], None,
    i$$ = Replace[Min @ Select[selected$$, GreaterThan @ i$$], Inf -> i$$], i$$ = L[selected$$, i$$],
    NiceClickBox["\[DownArrow]", PrintInputCell @ selected$$, "Pink"]
  ]},
  DynamicModuleBox[
    {i$$ = 1, r$$ = None, selected$$ = {}},
    GridBox[
      {{buttonRowBox}, {pickedButtonRowBox}, {progressBox}, {itemBox}},
      GridBoxSpacings -> {"Rows" -> {0.1, 0.1, {.5}}},
      GridBoxAlignment -> {"Columns" -> {{Left}}}
    ],
    DynamicModuleValues -> {i$$}
  ]
];

(**************************************************************************************************)

MapView::usage = "MapView[f$, list$] maps f$ over list$, showing the results in an interactive browser."

CoreBoxes[MapView[f_, list_List]] := blockView @ mappedBrowserBoxes[f, list];

mappedBrowserBoxes[f_, list_List] := With[
  {n$$ = Len @ list},
  {blue = $LightBlue, gray = GrayLevel[0.95], purple = $LightPurple},
  {itemBox = FrameBox[
    DynamicBox[ToBoxes @ Set[r$$, f @ Part[list, i$$]], TrackedSymbols :> {i$$}],
    FrameMargins -> 10, FrameStyle -> $LightGray],
  buttonRowBox = makeStandardBrowseArrowBoxes[
    i$$, n$$,
    NiceClickBox["\[DownArrow]", PrintInputCell @ deferSub[f, Part[list, i$$]], "Pink"],
    NiceClickBox["\[DownArrowBar]", PrintInputCell @ {Part[list, i$$], r$$}, "Pink"]
  ],
  progressBox = DynamicProgressBarBox[{i$$, n$$}, {200, 10}]},
  DynamicModuleBox[
    {i$$ = 1, r$$ = None},
    GridBox[
      {{buttonRowBox}, {progressBox}, {itemBox}},
      GridBoxSpacings -> {"Rows" -> {0.1, 0.1, {.5}}},
      GridBoxAlignment -> {"Columns" -> {{Left}}}
    ],
    DynamicModuleValues -> {i$$}
  ]
];

deferSub[f_, i_] := Apply[Defer, ConstructHoldComplete[f, i]];

(**************************************************************************************************)

DeclareHoldAll[makeBrowseArrowBoxes, makeStandardBrowseArrowBoxes]

makeStandardBrowseArrowBoxes[i_, n_, rest___] := makeBrowseArrowBoxes[
  {Auto, "Gray"},
  i = 1, modDec[i, n],
  fractionBox[i, n], If[CurrentValue["ShiftKey"], modDec[i, n], modInc[i, n]],
  modInc[i, n], i = n,
  rest
]

fractionBox[i_, n_] := StyleBox[RowBox[{i, "/", n}], Plain, 14];

_makeStandardBrowseArrowBoxes := viewError[];

makeBrowseArrowBoxes[{col1_, col2_}, first_, prev_, main_, mainBody_, next_, last_, rest___] := RowBox[{
  NiceClickBox["\[LeftArrowBar]", first, col1],
  NiceClickBox["\[LeftArrow]",    prev, col1],
  NiceClickBox[
    DynamicBox @ PaneBox[main, ImageSize -> {82, 10}, Alignment -> Center, BaselinePosition -> Baseline],
    mainBody,
    col2
  ],
  NiceClickBox["\[RightArrow]",    next, col1],
  NiceClickBox["\[RightArrowBar]", last, col1],
  rest
}];

_makeBrowseArrowBoxes := viewError[];

General::internalViewError = "Internal view error.";
viewError[] := ThrowMsg["internalViewError"];

(**************************************************************************************************)

DeclareHoldFirst[modInc, modDec];
modInc[var_, n_] := Set[var, Mod[var + 1, n, 1]];
modDec[var_, n_] := Set[var, Mod[var - 1, n, 1]];

(**************************************************************************************************)

CoreBoxes[EitherView[a_, b_]] := blockView @ eitherViewBoxes[MakeBoxes @ a, MakeBoxes @ b];

eitherViewBoxes[a_, b_] :=
  DynamicModuleBox[
    {flippervar$$ = 1},
    TagBox[TagBox[
      FrameBox[
        PaneSelectorBox[{1 -> a,  2 -> b}, Dynamic @ flippervar$$, ImageSize -> Auto, ImageMargins -> 5],
        FrameStyle -> GrayLevel[0.5]
      ],
      EventHandlerTag[{
        "MouseClicked" :> Set[flippervar$$, Mod[flippervar$$ + 1, 2, 1]],
        "MouseDragged" :> Set[flippervar$$, Mod[flippervar$$ + 1, 2, 1]]
      }]
    ], MouseAppearanceTag["LinkHand"]],
    DynamicModuleValues -> {flippervar$$}
  ];

(**************************************************************************************************)

TightRowGridBox[list_] :=
  GridBox[{list},
    GridBoxAlignment -> {"Columns" -> {{Left}}, "Rows" -> {{Top}}},
    GridBoxSpacings  -> {"Rows" -> {0, {0.5}, 0}, "Columns" -> {0, {0.5}, 0}},
    GridFrameMargins -> {{0, 0}, {0, 0}}
  ]

TightColumnGridBox[list_] :=
  GridBox[List /@ list,
    GridBoxAlignment -> {"Columns" -> {{Left}}, "Rows" -> {{Top}}},
    GridBoxSpacings  -> {"Rows" -> {0, {0.5}, 0}, "Columns" -> {0, {0.5}, 0}},
    GridFrameMargins -> {{0, 0}, {0, 0}}
  ];

(**************************************************************************************************)

OpenerColumnBox[a_] := a;

OpenerColumnBox[a_, b__] := With[
  {a1 = ClickBox[a, open$$ = False],
   a2 = ClickBox[a, open$$ = True]},
  {a1b = TightColumnGridBox[Prepend[{b}, a1]]},
  DynamicModuleBox[
    {open$$ = 1},
    DynamicBox[
      If[TrueQ @ open$$, a1b, a2],
      TrackedSymbols :> {open$$}
    ],
    DynamicModuleValues -> {open$$}
  ]
];

OpenerColumnBox[a_, b_] := With[
  {a1 = ClickBox[a, open$$ = False],
   a2 = ClickBox[a, open$$ = True]},
  {a1b = TightColumnGridBox[Prepend[{b}, a1]]},
  DynamicModuleBox[
    {open$$ = 1},
    DynamicBox[
      If[TrueQ @ open$$, a1b, a2],
      TrackedSymbols :> {open$$}
    ],
    DynamicModuleValues -> {open$$}
  ]
];

(**************************************************************************************************)


DeclareHoldFirst[DynamicProgressBarBox];

DynamicProgressBarBox[{i_, n_}, {w_, h_}, color_:$LightPurple] := mouseMoveBox[
  DeployBox @ GraphicsBox[
    {color, RectangleBox[{0, 0}, {Dynamic @ i, 1}]},
    ImageSize -> {w, h}, PlotRange -> {{0, n}, {0, 1}}, PlotRangePadding -> 0,
    ImagePadding -> 0, AspectRatio -> Full
  ],
  Replace[
    MousePosition["Graphics"],
    {x_, y_} :> With[{z = Clip[Round @ x, {1, n}]}, If[z =!= i, Set[i, z]]]
  ]
];

DeclareHoldRest[mouseMoveBox]

mouseMoveBox[box_, body_] := CursorIconBox[
  EventHandlerBox[box, {"MouseClicked" :> body, "MouseDragged" :> body}],
  "FrameLRResize"
];

(**************************************************************************************************)

DeclareHoldRest[NiceClickBox];

NiceClickBox[text_, action_] := NiceClickBox[text, action, Auto];
NiceClickBox[text_, action_, color_] := ClickBox[buttonBox[text, color], action];

buttonBox[e_, c_Str] := buttonBox[e, $bboxColors @ c];
buttonBox[e_, Auto] := buttonBox[e, {$LightBlue, $Blue}];
buttonBox[e_, c_] := buttonBox[e, {Lighter @ c, c}];
buttonBox[e_, {c1_, c2_}] := FrameBox[
  StyleBox[DeployBox @ e, Bold, 15],
  Background -> c1, FrameStyle -> c2,
  Alignment -> Baseline, FrameMargins -> {{5, 5}, {2, 0}}
];

$bboxColors = <|
  "Gray" -> {GrayLevel[0.9], $Gray},
  "Blue" -> {$LightBlue, $Blue},
  "Green" -> {$LightGreen, $Green},
  "Red" -> {$LightRed, $Red},
  "Pink" -> {$LightPink, $Pink},
  "LightPurple" -> {$LightPurple, $Purple},
  "Orange" -> {$LightOrange, $Orange}
|>;

(**************************************************************************************************)

DeployBox[b_] := TagBox[b, Deploy, DefaultBaseStyle -> "Deploy"];

