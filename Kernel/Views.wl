PackageExports[
  "FormHead",
    ListView, LabeledFlipView, PickView, MapView, EitherView,
  "BoxFunction",
    ClickAnimateBoxes, DynamicProgressBarBox,
    NiceClickBox, DeployBox,
    TightRowGridBox, TightColumnGridBox,
    OpenerColumnBox, DeployBox,
  "OptionSymbol",
    ClickFunction
];

(*************************************************************************************************)

Options[ListView] = {
  ClickFunction -> None
};

MakeBoxes[ListView[list_List, opts___Rule], form_] := listBrowserBoxes[list, opts];

listBrowserBoxes[list_List, opts:OptionsPattern[]] := With[
  {n$$ = Len @ list},
  {blue = $LightBlue, gray = GrayLevel[0.95], purple = $LightPurple},
  {clickFn = OptionValue[ListView, {opts}, ClickFunction]},
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

(**************************************************************************************************)

CoreBoxes[LabeledFlipView[items:ListDictP, opts___Rule]] :=
  labeledFlipViewBoxes[items, opts];

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

CoreBoxes[PickView[list_List]] :=
  pickBrowserBoxes[list];

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

CoreBoxes[MapView[f_, list_List]] := mappedBrowserBoxes[f, list];

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

CoreBoxes[EitherView[a_, b_]] := eitherViewBoxes[MakeBoxes @ a, MakeBoxes @ b];

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

