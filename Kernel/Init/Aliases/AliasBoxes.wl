SystemExports[
  "OptionSymbol",
    ViewSize,
    MaxItems,
    MaxSize, MaxWidth, MaxHeight,
     ItemPosition,  ItemSpacings,  ItemDividers,  ItemAlignments,  ItemSizes,
    LabelPosition, LabelSpacings, LabelDividers, LabelAlignments, LabelSizes,
    ItemFunction, LabelFunction, ClickFunction, TooltipFunction, SizeFunction
];

PackageExports[
  "GraphicsDirective",
    APointSize, AThickness, ADashing,

  "BoxFunction",
    SubBox, SuperBox, SubsuperBox, DotsBox,

  "FormHead",
    DotsForm, StrForm,

  "BoxOptionSymbol",
    GridItemSize, GridItemStyle, GridMargins, GridDivs,
    RowSizes, RowGaps, RowJust, ColSizes, ColGaps, ColJust, ColLines, ColsEqual,
    ItemsEqual,

  "OptionSymbol",
    BaselinePos,
     ItemPos,  ItemGaps,  ItemDivs,  ItemJust,
    LabelPos, LabelGaps, LabelDivs, LabelJust,

  "OptionSymbol",
    ItemFn, LabelFn, ClickFn, TooltipFn, SizeFn,
    ColorFn, VertexColorFn
];

(*************************************************************************************************)

DefineAliasRules[
  SubBox         -> SubscriptBox,
  SuperBox       -> SuperscriptBox,
  SubsuperBox    -> SubsuperscriptBox
];

(*************************************************************************************************)

DefineAliasRules[
  APointSize     -> AbsolutePointSize,
  AThickness     -> AbsoluteThickness,
  ADashing       -> AbsoluteDashing
];

(*************************************************************************************************)

DefineAliasRules[
  GridItemSize   -> GridBoxItemSize,
  GridItemStyle  -> GridBoxItemStyle,
  GridMargins    -> GridFrameMargins,
  GridDivs       -> GridBoxDividers
];

DefineAliasRules[
  RowSizes       -> RowHeights,
  RowGaps        -> RowSpacings,
  RowJust        -> RowAlignments,
  ColSizes       -> ColumnWidths,
  ColGaps        -> ColumnSpacings,
  ColJust        -> ColumnAlignments,
  ColLines       -> ColumnLines,
  ColsEqual      -> ColumnsEqual
];

DefineAliasRules[
  BaselinePos    -> BaselinePosition,
  ItemPos        -> ItemPosition,
  ItemGaps       -> ItemSpacings,
  ItemDivs       -> ItemDividers,
  ItemJust       -> ItemAlignments,
  LabelPos       -> LabelPosition,
  LabelGaps      -> LabelSpacings,
  LabelDivs      -> LabelDividers,
  LabelJust      -> LabelAlignments
];

(*************************************************************************************************)

DefineAliasRules[
  ItemFn         -> ItemFunction,
  LabelFn        -> LabelFunction,
  ClickFn        -> ClickFunction,
  TooltipFn      -> TooltipFunction,
  SizeFn         -> SizeFunction
];

DefineAliasRules[
  ColorFn        -> ColorFunction,
  VertexColorFn  -> VertexColorFunction
];

