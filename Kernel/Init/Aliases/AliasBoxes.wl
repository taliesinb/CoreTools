SystemExports[
  "Option",
    ViewSize,
    MaxItems,
    MaxSize, MaxWidth, MaxHeight,
     ItemPosition,  ItemSpacings,  ItemDividers,  ItemAlignments,  ItemSizes,
    LabelPosition, LabelSpacings, LabelDividers, LabelAlignments, LabelSizes,
    ItemFunction, LabelFunction, ClickFunction, TooltipFunction, SizeFunction,

  "FormHead",
    MessageArgumentForm, OutputExpressionForm, LiteralString
];

PackageExports[
  "GraphicsDirective",
    APointSize, AThickness, ADashing,

  "IOFunction",
    MakeBox, ToBox,

  "BoxFunction",
    SubBox, SuperBox, OverBox, UnderBox, SubsuperBox,
    AdjBox, InterpBox, G2DBox, G3DBox, SBox, TBox,
    LitStrBox, LitStrRowBox,
    DotsBox, RawBox,

  "FormHead",
    MsgArgForm, OutExprForm,
    LitStr, LitStrRow, SrcLoc,

  "BoxOption",
    GridItemSize, GridItemStyle, GridMargins, GridDivs,
    RowSizes, RowGaps, RowJust, ColSizes, ColGaps, ColJust, ColLines, ColsEqual,
    ItemsEqual, ShowStrChars,

  "Option",
    BaselinePos,
     ItemPos,  ItemGaps,  ItemDivs,  ItemJust,
    LabelPos, LabelGaps, LabelDivs, LabelJust,

  "Option",
    ItemFn, LabelFn, ClickFn, TooltipFn, SizeFn,
    ColorFn, VertexColorFn
];

(*************************************************************************************************)

(* TODO: alias RBox for RowBox, SBox for SeqB[---] for RBox *)

DefineAliasRules[
  SubBox         -> SubscriptBox,
  SuperBox       -> SuperscriptBox,
  OverBox        -> OverscriptBox,
  UnderBox       -> UnderscriptBox,
  SubsuperBox    -> SubsuperscriptBox,
  AdjBox         -> AdjustmentBox,
  InterpBox        -> InterpretationBox,
  G2DBox         -> GraphicsBox,
  G3DBox         -> Graphics3DBox,
  SBox           -> StyleBox,
  TBox           -> TemplateBox,
  RawBox         -> RawBoxes,
  LitStrBox      -> LiteralStringBox,
  LitStrRowBox   -> LiteralStringRowBox
];

(*************************************************************************************************)

DefineAliasRules[
  APointSize     -> AbsolutePointSize,
  AThickness     -> AbsoluteThickness,
  ADashing       -> AbsoluteDashing
];

(*************************************************************************************************)

DefineAliasRules[
  MakeBox       -> MakeBoxes,
  ToBox         -> ToBoxes
];

(*************************************************************************************************)

DefineAliasRules[
  SrcLoc         -> SourceLocation,
  MsgArgForm     -> MessageArgumentForm,
  OutExprForm    -> OutputExpressionForm,
  LitStrRow      -> LiteralStringRow,
  LitStr         -> $PrintLiteral,
  LiteralString  -> $PrintLiteral
];

(*************************************************************************************************)

DefineAliasRules[
  GridItemSize   -> GridBoxItemSize,
  GridItemStyle  -> GridBoxItemStyle,
  GridMargins    -> GridFrameMargins,
  GridDivs       -> GridBoxDividers
];

DefineAliasRules[
  ShowStrChars   -> ShowStringCharacters,
  RowSizes       -> RowHeights, (* THIS IS A NO-OP *)
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

