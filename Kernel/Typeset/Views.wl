PackageExports[
  "FormHead",
    ListView,
    NestedView, ExpanderView, JSONView,
    GroupedView, RowColumnView,
    RowView, ColumnView, ColView,
    MultiRowView, MultiColumnView, MultiColView,
    HeadingView,
    LabeledFlipView,
    PickView, MapView, EitherView,
  "BoxFunction",
    DynamicProgressBarBox,
    NiceClickBox, DeployBox,
    RowViewGridBox, ColViewGridBox,
    OpenerColumnBox, DeployBox,
  "Option",
    MultiGaps, MultiDivs, ViewSampling,
  "Function",
    LabelBy,
  "BoxFunction",
    ListDictMakeBoxes1D, ListDictMakeBoxes2D
];

PrivateExports[
  "MetaFunction",
    DefineViewForm
];

(*************************************************************************************************)

SetCurry2[LabelBy];

LabelBy[list_List, fn_] := Map[item |-> Labeled[item, fn[item]], list];

(*************************************************************************************************)

$genericViewOptions = {
  ClickFn        -> None,
  ViewSize       -> 16,
  MaxItems       -> Inf,
  ItemFn         -> None,
  LabelGaps       -> 1,
  ItemGaps        -> 1,
  LabelDividers  -> True,
  ItemDividers   -> False
};

$currentViewDepth = 0;

$viewOption = Dict[];

SetHoldF[makeViewBoxesCommon]

(* first apply the explicit options, then whatever we've inherited
from higher up, then the defaults for this view *)
makeViewBoxesCommon[body_, form_Symbol, rules___Rule] := Block[
  {$currentViewDepth = $currentViewDepth + 1,
   $viewOption = Dict @ ToList[Options @ form, $viewOption, rules]},
  If[$currentViewDepth > 8, "$TOODEEP", body]
];

makeViewBoxesCommon[___] := "$FAILED";

(*************************************************************************************************)

SetStrict[DefineViewForm]

DefineViewForm[RuleD[form_Symbol[lhs___], rhs_]] := Then[
  Options[form] = $genericViewOptions,
  CoreBox[form[lhs, opts___Rule]] := makeViewBoxesCommon[rhs, form, opts]
];

(*************************************************************************************************)

DeclaredHere[HeadingView];
SetForm1[HeadingView];

DefineViewForm[HeadingView[items:ListDictP] :> headingViewBoxes @ items];

SetHoldC[headingViewBoxes];

headingViewBoxes = CaseOf[
  {}         := RBox[LBrace, " ", RBrace];
  EmptyDict  := RBox[LAssoc, " ", RAssoc];
  EmptyUDict := RBox["UDict", "[", " ", "]"];
  items_List := At[$, RangeDict @ items];
  items_Dict := ColGridBox[KeyValueMap[LabelTopBox, items], 1];
  e_         := RedMsgFormBox["bad HeadingView data: ``", e];
];

(*************************************************************************************************)

DefineAliasRules[
  ColumnView      -> ColView,
  MultiColumnView -> MultiColView
];

(*************************************************************************************************)

BlockUnprotect[{Row, Column, Multicolumn},
  Row[d_Dict, opts___Rule]         := RowView[d, NarrowOpts @ opts];
  Column[d_Dict, opts___Rule]      := ColumnView[d, NarrowOpts @ opts];
  Multicolumn[d_Dict, opts___Rule] := MultiColumnView[d, NarrowOpts @ opts];
];

(*************************************************************************************************)

SetHoldF[ListDictMakeBoxes1D, ListDictMakeBoxes2D];

ListDictMakeBoxes1D[expr_, isHor_, itemBoxFn_, keyBefore_, keyBoxFn_, maxSize2_, maxCount_] := Locals[

  sizeFn = If[isHor, P1, P2];
  maxSize = sizeFn @ EnsurePair @ maxSize2;
  {isDict, heldKeys, heldItems} = toHeldKeysItems @ expr;

  len = Len @ heldItems;
  n = i = 0; total = 0; list = {}; tooBig = False;
  slop = Max[maxSize * 0.1, 5];
  While[!tooBig && ++i <= len,
    itemBox = itemBoxFn @ Part[heldItems, i];
    itemSize = ToImageSize @ RawBoxes @ itemBox;
    size = sizeFn @ itemSize;
    If[size > maxSize,
      size = 20;
      itemBox = HoldElidedBox @@ Part[heldItems, i];
    ];
    tooBig = total + size > maxSize;
    total += size;
    If[tooBig && size > slop, Break[]];
    If[isDict,
      keyBox = keyBoxFn @ Part[heldKeys, i];
      itemBox = If[keyBefore, {keyBox, itemBox}, {itemBox, keyBox}]];
    AppendTo[list, itemBox];
    n += 1;
  ];
  If[n < len, AppendTo[list, makeDots1D[len - n, isDict, isHor]]];
  list
];

makeDots1D[m_, True, True]    := {CDotsBox[m, True], ""};
makeDots1D[m_, True, False]   := {ItemBox[CDotsBox[m, False], Alignment -> Center], ""};
makeDots1D[m_, False, isHor_] := CDotsBox[m, isHor];

ListDictMakeBoxes2D[expr_, isHor_, itemBoxFn_, keyBefore_, keyBoxFn_, maxSize_, maxItems_] := Locals[

  sizeFn = If[isHor, Id, Rev];
  {maxSize1, maxSize2} = sizeFn @ EnsurePair @ maxSize;
  {maxItems1, maxItems2} = sizeFn @ EnsurePair @ maxItems;
  {isDict, heldKeys, heldItems} = toHeldKeysItems @ expr;
  slop = Max[maxSize1 * 0.1, 5];
  len = Len @ heldItems;
  n = i = 0; total1 = total2 = other = 0; list1 = list2 = {};

  While[++i <= len,
    itemBox = itemBoxFn @ Part[heldItems, i];
    itemSize = ToImageSize @ RawBoxes @ itemBox;
    {size1, size2} = sizeFn @ itemSize;
    If[(size1 > maxSize1 || size2 > maxSize2) && i > 1,
      size1 = size2 = 20;
      itemBox = HoldElidedBox @@ Part[heldItems, i];
    ];
    MaxTo[other, size2];
    If[isDict,
      keyBox = keyBoxFn @ Part[heldKeys, i];
      itemBox = If[keyBefore, {keyBox, itemBox}, {itemBox, keyBox}]];
    If[(total1 + size1 <= maxSize1) || i == 0,
      AppendTo[list1, itemBox];
      total1 += size1;
      MaxTo[other, size2];
    ,
      useSlop = total1 + size1 <= maxSize1 + slop;
      If[useSlop,
        AppendTo[list1, itemBox];
        MaxTo[other, size2];
      ];
      n += Len @ list1;
      AppendTo[list2, list1];
      total2 += other;
      list1 = {};
      If[total2 > maxSize2, Break[]];
      If[useSlop,
        total1 = other = 0,
        total1 = size1; other = size2;
        list1 = {itemBox}
      ];
    ];
  ];
  If[list1 =!= {},
    n += Len[list1];
    AppendTo[list2, list1];
  ];
  If[n < len,
    dots = makeDots2D[len - n, isDict, isHor];
    If[total1 < size1 - 20,
      AppendTo[PN @ list2, dots],
      Part[list2, -1, -1] = dots
    ]
  ];
  list2
];

makeDots2D[m_, True, True]    := {CDotsBox[m, True], ""};
makeDots2D[m_, True, False]   := {ItemBox[CDotsBox[m, False], Alignment -> Center], ""};
makeDots2D[m_, False, isHor_] := CDotsBox[m, isHor];

toHeldKeysItems = CaseOf[
  dict_Dict  := List[True, Keys[NoEval @ dict, HoldC], Values[NoEval @ dict, HoldC]];
  list_List  := List[False, None, Thread @ HoldC @ list];
  held_HoldC := List[False, None, List @@ Map[HoldC, held]];
];

(*************************************************************************************************)

$rowColViewOptions = {
   ItemFn -> None,  ItemDivs -> False,  ItemGaps -> 1,  ItemStyle -> None,
  LabelFn -> None, LabelDivs -> False, LabelGaps -> 1, LabelStyle -> Bold,
  MaxWidth -> Auto, MaxHeight -> Auto, MaxItems -> Inf, MaxSize -> 1500
};

$multiRowColViewOptions = FlatList[
  $rowColViewOptions,
  MultiGaps -> 2.5,
  MultiDivs -> True
];

rcOptVal = UDict[$rowColViewOptions];

Options[MultiColView] = $multiRowColViewOptions;
Options[MultiRowView] = $multiRowColViewOptions;

SetForm0[MultiRowView, MultiColView];

CoreBox[MultiRowView[Unlimited[items_], opts___Rule]] := MakeBoxes[MultiRowView[items, MaxHeight -> Inf, opts]];
CoreBox[MultiColView[Unlimited[items_], opts___Rule]] := MakeBoxes[MultiColView[items, MaxWidth -> Inf, opts]];
CoreBox[RowView[Unlimited[items_], opts___Rule]] := MakeBoxes[RowView[items, MaxSize -> Inf, opts]];
CoreBox[ColView[Unlimited[items_], opts___Rule]] := MakeBoxes[ColView[items, MaxSize -> Inf, opts]];

CoreBox[MultiRowView[items:ListDictP, opts___Rule]] := multiRowColViewBoxes[True,  MultiRowView, items, List @ opts];
CoreBox[MultiColView[items:ListDictP, opts___Rule]] := multiRowColViewBoxes[False, MultiColView, items, List @ opts];

SetHoldC @ multiRowColViewBoxes;

multiRowColViewBoxes[_, e:EmptyP, _] := trivialViewBoxes @ e;

multiRowColViewBoxes[isHor_, head_, items_, opts_] := Locals[
  MessageOnUnknownOptions[head, opts];
  UnpackSymbolsAs[head, opts, itemFn, labelFn, maxWidth, maxHeight, maxSize, maxItems, multiGaps, multiDivs];
  InheritVar[rcOptVal]; BindTo[rcOptVal, opts]; $isHor = isHor;
  {maxSize1, maxSize2} = If[PairQ[maxSize], maxSize, {maxSize, 1000}];
  SetAuto[maxWidth, If[isHor, maxSize1, maxSize2]];
  SetAuto[maxHeight, If[isHor, maxSize2, maxSize1]];
  maxSize = {maxWidth, maxHeight};
  itemFn = itemFnBoxes @ IfNone[itemFn, Id];
  labelFn = itemFnBoxes @ IfNone[labelFn, Id];
  entries = ListDictMakeBoxes2D[items, isHor, itemFn, !isHor, labelFn, maxSize, maxItems];
  subGrids = If[isHor, Map[RowViewGridBox], Map[ColViewGridBox]] @ entries;
  If[SingleQ[subGrids], Return @ P1 @ subGrids];
  If[isHor,
    ColGridBox[subGrids, RowGaps -> multiGaps, RowLines -> toDivCol[multiDivs]],
    RowGridBox[subGrids, ColGaps -> multiGaps, ColLines -> toDivCol[multiDivs]]
  ]
];

itemFnBoxes[Id][HoldC[item_]] := MakeBoxes @ item;
itemFnBoxes[f_][HoldC[item_]] := ToBoxes @ f @ item;

(*************************************************************************************************)

(* OldMultiColumnView[items_, nrows_, ncols_:Inf, maxWidth_:1000] := Locals[
  If[DictQ[items], toFn = Dict; fromFn = Normal, toFn = fromFn = Id];
  colFn = ColumnView[toFn @ #, MaxPlotSize -> Inf, MaxItems -> Inf, ItemGap -> 1]&;
  RowView[
    colFn /@ Take[Partition[fromFn @ items, UpTo[nrows]], UpTo[nrows * ncols]],
    MaxPlotSize -> maxWidth, MaxItems -> ncols, ItemGap -> 3
  ]
];
 *)
(*************************************************************************************************)

SetForm0[RowView, ColView];

Options[RowView] = $rowColViewOptions;
Options[ColView] = $rowColViewOptions;

CoreBox[RowView[items:ListDictP, opts___Rule]] := rowColViewBoxes[True,  RowView, items, List @ opts];
CoreBox[ColView[items:ListDictP, opts___Rule]] := rowColViewBoxes[False, ColView, items, List @ opts];

SetHoldC[rowColViewBoxes];

rowColViewBoxes[_, e:EmptyP, _, _] := trivialViewBoxes @ e;

rowColViewBoxes[isHor_, head_, items_, opts_] := Locals[
  MessageOnUnknownOptions[head, opts];
  UnpackSymbolsAs[head, opts, itemFn, labelFn, maxWidth, maxHeight, maxSize, maxItems];
  InheritVar[rcOptVal]; BindTo[rcOptVal, opts]; $isHor = isHor;
  SetAuto[maxWidth, maxSize];
  SetAuto[maxHeight, maxSize];
  maxSize = {maxWidth, maxHeight};
  itemFn  = itemFnBoxes @ IfNone[itemFn, Id];
  labelFn = itemFnBoxes @ IfNone[labelFn, Id];
  entries = ListDictMakeBoxes1D[items, isHor, itemFn, !isHor, labelFn, maxSize, maxItems];
  If[isHor,
    RowViewGridBox @ entries,
    ColViewGridBox @ entries
  ]
];

trivialViewBoxes = CaseOf[
  {}             := RBox[LBrace, " ", RBrace];
  EmptyDict      := RBox[LAssoc, " ", RAssoc];
  EmptyUDict     := RBox["UDict", "[", " ", "]"];
  _              := RedMsgFormBox["bad data"];
];

RowViewGridBox = CaseOf[
  {}                     := "";
  {e1:Except[_List]}     := e1;
  boxes_List             := GridBox[ToRowVec @ boxes, $rcViewOpts];
  boxes_List ? dictRowsQ := GridBox[Flip @ boxes, $rViewLabelStyle, $rcViewOpts];
  e_                     := RedMsgFormBox["bad data: ``", Head @ e]
];

ColViewGridBox = CaseOf[
  {}                     := "";
  {e1:Except[_List]}     := e1;
  boxes_List             := GridBox[ToColVec @ boxes, $rcViewOpts];
  boxes_List ? dictRowsQ := GridBox[boxes, $cViewLabelStyle, $rcViewOpts];
  e_                     := RedMsgFormBox["bad data: ``", Head @ e]
];

dictRowsQ[boxes_] := ListQ @ P1 @ boxes;

$cViewLabelStyle := GridBoxRule[ItemStyle, None, {Directive @ rcOptVal @ LabelStyle, None}]
$rViewLabelStyle := Seq[ColumnAlignments -> Center, GridBoxRule[ItemStyle, {None, Directive @ rcOptVal @ LabelStyle}, None]];

$rcViewOpts := Seq[
  ColumnAlignments -> Left, RowMinHeight -> 1.2, BaselinePosition -> {1, 1},
  GridBoxRule[ItemDivs, toDivCol @ rcOptVal @ ItemDivs, toDivCol @ rcOptVal @ LabelDivs, $isHor],
  GridBoxRule[ItemGaps, If[$isHor, 1.2, 0.8] * rcOptVal[ItemGaps], If[$isHor, 0.5, 1.2] * rcOptVal[LabelGaps], $isHor]
];

toDivCol = CaseOf[
  True := GrayLevel[0.8];
  e_   := e;
];

(*************************************************************************************************)

DeclaredHere[GroupedView];

SetForm0[GroupedView];

DefineViewForm[GroupedView[list_List, fns_] :> groupedView[list, fns]];

Options[GroupedView] = JoinOptions[{ViewSize -> 8, LabelFunction -> CodePane, ViewSampling -> False}, $genericViewOptions];

groupedView[list_List, fns2_] := Locals[
  fns = fns2;
  If[RuleQ[fns],
    {fns, $itemFn} = FirstLast @ fns,
    $itemFn = $viewOption[ItemFn];
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

fnBoxes[fn_] := CodePaneBox[fn, {UpTo[200], UpTo[30]}];
groupEllipsisBox[n_] := dimLeftBox @ OverscriptBox["\[Ellipsis]", NatStr[n]];
groupFnBox[list_List]   := ColumnBox[fnBoxes /@ list, Left, 0];
groupFnBox[f_RightComposition] := groupFnBox[List @@ f];
groupFnBox[other_]     := fnBoxes @ other;
groupItemBox[item_]    := ToBoxes @ $itemFn @ item;
groupLabelBox[label_]  := ToBoxes @ $viewOption[LabelFunction] @ label;

dimLeftBox[box_] := ItemBox[box, Frame -> {{GrayLevel[0.95], Auto}, {Auto, Auto}}];

visitGroup[d_][dict_] := PathScan[$labelPath, visitGroup[d-1], KeySort @ dict];

(*************************************************************************************************)

DeclaredHere[ListView];

SetForm0[ListView];

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

makeNestingView[boxFn_, gid_, expr_] := Module[
  {$expPath = {}, $expCache = UDict[]},
  makeNestingViewInner[boxFn, $expPath, $expCache, $viewOption, gid]
];

makeNestingView[boxFn_, gid_, expr_] /; TrueQ[$CoreInteractive] := With[
  {viewOptions = $viewOption},
  DynamicModuleBox[{$expPath = {}, $expCache = Dict[], $expOptions = viewOptions, $gid = gid},
    DynamicBox[
      If[HasDownDefsQ[makeNestingViewInner],
        makeNestingViewInner[boxFn, $expPath, $expCache, $expOptions, $gid],
        "---"
      ],
      TrackedSymbols :> {$expPath}
    ]
  ]
];

(*************************************************************************************************)

SetHoldC @ makeNestingViewInner;

makeNestingViewInner[boxFn_, expPath_, expCache_, expOptions_, gid_] := Module[
  {subBoxes, rowBoxes, pathBoxes},
  subBoxes = Table[
    makeSubviewBoxes[Take[expPath, i], boxFn, expPath, expCache, expOptions, gid],
    {i, 0, Len @ expPath}
  ];
  rowBoxes = RowGridBox[subBoxes, ColumnSpacings -> 3
    (* GridBoxDividers -> {"Columns" -> {False, {GrayLevel[0.5]}, False}} *)];
  pathBoxes = ClickBox[
    StyleBox[RowBox @ Riffle[MakeBoxes /@ Flatten[expPath], ","], Gray],
    PrintInputCell @ expPath
  ];
  ColumnBox[{rowBoxes, pathBoxes}, Left, 1, RowMinHeight -> 1.2]
];

(*************************************************************************************************)

SetHoldR @ makeSubviewBoxes;

makeSubviewBoxes[ourPath_, boxFn_, pathVar_, cacheVar_, optionsVar_, idVar_] := CachedTo[
  cacheVar, ourPath,
  Block[{$pathInfo = Hold[pathVar, ourPath, idVar], $viewOption = optionsVar},
      applySubExprBoxes[boxFn, idVar, ourPath]
  ]
];

applySubExprBoxes[boxFn_, id_Int, {}]        := GlobalWeakTableGet[id, boxFn];
applySubExprBoxes[boxFn_, id_Int, path_List] := Extract[GlobalWeakTableGet @ id, Flatten @ path, boxFn];

nextViewBoxes[item:DatumP, parts_] := expItemBoxes @ item;
nextViewBoxes[item_, parts_]       := nextViewButton[expItemBoxes @ item, parts, $pathInfo]

nextViewButton[boxes_, parts_, _] := StyleBox[boxes, FontColor -> $DarkBlue];
nextViewButton[boxes_, parts_List, Hold[pathVar_, ourPath_, id_]] :=
  ClickBox[
    StyleBox[boxes, FontColor -> $DarkBlue],
    If[CurrentValue["ShiftKey"],
      printSubExpr @ Extract[GlobalWeakTableGet @ id, FlatList[ourPath, parts], Hold],
      Set[pathVar, Append[ourPath, parts]]
    ]
  ];

printSubExpr[Hold[expr_]] := If[
  ByteCount[NoEval @ expr] > 512,
  PrintInputCell @ Iconize @ Unevaluated @ expr,
  PrintInputCell @ Hold @ expr
];

(*************************************************************************************************)

DeclaredHere[JSONView];

SetForm0[JSONView];

DefineViewForm[JSONView[expr_] :> makeNestingView[jsonViewBoxes, GlobalWeakTablePut @ expr, expr]];

SetOptions[JSONView, MaxItems -> 8];

SetHoldC[jsonViewBoxes, jsonExprBoxes, jsonCompoundBox];

jsonViewBoxes[expr_] := Block[{$subPath = {}}, jsonExprBoxes @ expr];

jsonExprBoxes = CaseOf[
  {}          := "[]";
  EmptyDict   := "{}";
  {d_DatumP}  := RBox["[", jsonDataBox @ d, "]"];
  list_List   := jsonCompoundBox["[", "]", list];
  dict_Dict   := jsonCompoundBox["{", "}", dict];
  e_          := jsonDataBox @ e;
];

jsonDataBox = CaseOf[
  {}          := "[]";
  EmptyDict   := "{}";
  list_List   := RBox["[", HoldLenBox @ list, "]"];
  dict_Dict   := RBox["{", HoldLenBox @ dict, "}"];
  s_Str       := StyleBox[If[StrLen[s] < 16, MakeBoxes @ s, MakeBoxes[StringDrop[s, 14] <> Dots]], ShowStringCharacters -> True];
  d:DatumP    := StyleBox[MakeBoxes @ d, ShowStringCharacters -> True];
  e_          := HoldElidedBox @ e;
];

jsonCompoundBox[top_, bot_, expr_] := Locals[
  If[HoldLen[expr] > 16 && $subPath =!= {}, Return @ jsonNextView[expr]];
  entries = ListDictMakeBoxes1D[Evaluate @ HoldArgsP @ expr, False, jsonEntryBox, False, None, 800, 16];
  If[VFreeQ[entries, {EventHandlerBox, GridBox}] && Total[Occurences[entries, s_Str ? HAtomQ :> StrLen[s]]] < 24,
    RBox[top, RowBox @ Riffle[entries, ","], bot]
  ,
    entries = Map[RBox["\t", #]&, entries];
    ColumnBox[FlatList[top, entries, bot], Left, BaselinePosition -> {1,1}]
  ]
];

SetHoldC[jsonSubExprBoxes, jsonNextViewBoxes, jsonSubBoxes, jsonNextView];

jsonEntryBox[HoldC[{val_, ind_Int}]] := BlockAppend[$subPath, ind, jsonSubBoxes @ val];
jsonEntryBox[HoldC[{val_, key_Str}]] := BlockAppend[$subPath, Key @ key, joinFirstRow @ RBox[jsonKeyBox @ key, jsonSubBoxes @ val]];

jsonKeyBox[key_] := StyleBox[key <> ": ", $DarkGray, AutoSpacing -> False];

jsonSubBoxes[d:DatumP] := jsonDataBox @ d;
jsonSubBoxes[list:{Repeated[DatumP, {0, 5}]}] := jsonExprBoxes @ list;
jsonSubBoxes[expr_]  := If[Len[$subPath] < 4, jsonExprBoxes @ expr, jsonNextView @ expr];

jsonNextView[expr_] := nextViewButton[jsonDataBox @ expr, $subPath, $pathInfo];

(*************************************************************************************************)

addTabComma[boxes_] := RBox["\t", boxes, ","];
addTabComma[GridBox[grid_, opts___]] := RBox["\t", Make[GridBox, MapAt[addComma, grid, {-1, -1}], opts]];
addComma[box_] := RBox[box, ","];

joinFirstRow[boxes_] := boxes;
joinFirstRow[RowBox[{a_, GridBox[{{f1_, fr___}, rest___}, opts___]}]] :=
  GridBox[{{joinFirstRow @ RowBox[{a, f1}], fr}, rest}, opts];

(*************************************************************************************************)

DeclaredHere[ExpanderView];

SetForm0[ExpanderView];

DefineViewForm[ExpanderView[expr_] :> makeNestingView[expViewBoxes, GlobalWeakTablePut @ expr, expr]];

SetHoldA[expViewBoxes, expItemBoxes, expColumnBoxes];

expViewBoxes = CaseOf[
  e:EmptyP    := expItemBoxes @ e;
  {d:DatumP}  := ParenRBox @ expItemBoxes @ d;
  l_List      := expColumnBoxes[expListFieldBoxes, l];
  d_Dict      := expColumnBoxes[expDictFieldBoxes, d];
  e_          := expItemBoxes @ e;
];

expItemBoxes = CaseOf[
  s_Str       := If[StrLen[s] < 16, MakeBoxes @ s, MakeBoxes[StringDrop[s, 14] <> Dots]];
  p:DatumP    := MakeBoxes @ p;
  e_          := HoldElidedBox @ e;
];

expColumnBoxes[fn_, expr_] := GridBox[
  spanLast[2] @ viewMapMakeBox[expr, 2, fn],
  $colViewLabelOpts, $colViewGridOpts
];

SetHoldF[expListFieldBoxes, expDictFieldBoxes, nextViewBoxes];

expListFieldBoxes[item_, ind_]         := {expItemBoxes @ ind, nextViewBoxes[item, {ind}]};
expDictFieldBoxes[item_, key_]         := {expItemBoxes @ key, nextViewBoxes[item, {Key @ key}]};

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
  array_ ? HPackedQ := MatrixForm @ array;
  list:ListDictP    := RowColumnView[$ /@ list, $nestedViewOpts];
  other_            := $Failed
];

(*************************************************************************************************)

$rowOrColumnDepth = 0;

(*************************************************************************************************)

DefineViewForm[RowColumnView[items:ListDictP] :> rowColumnViewBoxes @ items];

SetHoldC[rowColumnViewBoxes, rowableQ];

rowableQ[items_] := HoldLen[items] < 32 && AllTrue[NoEval @ items, HoldDatumQ];

rowColumnViewBoxes[items_ ? rowableQ] := rowViewBoxes @ list;
rowColumnViewBoxes[items_]            := colViewBoxes @ list;
rowColumnViewBoxes[_]                 := "$FAILED";

(**************************************************************************************************)

DeclaredHere[LabeledFlipView];

SetForm0[LabeledFlipView];

CoreBox[LabeledFlipView[items:ListDictP, opts___Rule]] :=
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

SetForm0[PickView];

Options[PickView] = Options[ListView];

CoreBox[PickView[list_List]] := blockView @ pickBrowserBoxes[list];

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
    i$$ = First[selected$$, i$$], i$$ = Replace[Max @ Select[selected$$, LessThan @ i$$], -Inf -> i$$],
    fractionBox[IndexOf[selected$$, i$$, "?"], Len @ selected$$], None,
    i$$ = Replace[Min @ Select[selected$$, GreaterThan @ i$$], Inf -> i$$], i$$ = Last[selected$$, i$$],
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

SetForm0[MapView];

MapView::usage = "MapView[f$, list$] maps f$ over list$, showing the results in an interactive browser."

CoreBox[MapView[f_, list_List]] := blockView @ mappedBrowserBoxes[f, list];

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

deferSub[f_, i_] := Apply[Defer, MakeHoldComplete[f, i]];

(**************************************************************************************************)

SetHoldA[makeBrowseArrowBoxes, makeStandardBrowseArrowBoxes]

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

SetHoldF[modInc, modDec];
modInc[var_, n_] := Set[var, Mod[var + 1, n, 1]];
modDec[var_, n_] := Set[var, Mod[var - 1, n, 1]];

(**************************************************************************************************)

SetForm0[EitherView];

CoreBox[EitherView[a_, b_]] := blockView @ eitherViewBoxes[MakeBoxes @ a, MakeBoxes @ b];

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

OpenerColumnBox[a_] := a;

OpenerColumnBox[a_, b__] := With[
  {a1 = ClickBox[a, open$$ = False],
   a2 = ClickBox[a, open$$ = True]},
  {a1b = TightColGridBox[Prepend[{b}, a1]]},
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
  {a1b = TightColGridBox[Prepend[{b}, a1]]},
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

SetHoldF[DynamicProgressBarBox];

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

SetHoldR[mouseMoveBox]

mouseMoveBox[box_, body_] := CursorIconBox[
  EventHandlerBox[box, {"MouseClicked" :> body, "MouseDragged" :> body}],
  "FrameLRResize"
];

(**************************************************************************************************)

SetHoldR[NiceClickBox];

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

