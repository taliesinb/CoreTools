SystemExports[

  "FormOption",
    ViewSize, MaxItems, MaxSize, MaxWidth, MaxHeight,
     ItemPosition,  ItemSpacings,  ItemDividers,  ItemAlignments,  ItemSizes,
    LabelPosition, LabelSpacings, LabelDividers, LabelAlignments, LabelSizes,
    ItemFunction, LabelFunction, ClickFunction, TooltipFunction, SizeFunction, ClickData,

  "GraphicsPrimitive",
    Graphics2D,

  "PlotOption",
    JoinStyle, JoinPosition, Shortcut, Setback,
    FrameColor, FrameThickness, FrameOpacity, FrameDashing,
    NodeThickness, EdgeThickness, PlotScale,

  "Symbol",
    TopLeft, TopCenter, TopRight,
    CenterLeft, CenterCenter, CenterRight,
    BottomLeft, BottomCenter, BottomRight
];

PackageExports[

  "GraphicsDirective",
    APointSize, AThickness, ADashing,
    Wht, Blk, RGB, GL, Dir,

  "SpecialFunction",
    MakeBox, ToBox,

  "BoxFunction",
    SubBox, SuperBox, OverBox, UnderBox, SubsuperBox,
    AdjBox, InterpBox, G2DBox, G3DBox, SBox, TBox,
    RawBox,

  "FormHead",
    MsgArgForm, OutExprForm,
    LitStr, LitStrRow, SrcLoc,
    StdForm, SysForm, Col,

  "BoxOption",
    GridItemSize, GridItemStyle, GridMargins, GridDivs,
    RowSizes, RowGaps, RowJust, ColSizes, ColGaps, ColJust, ColLines, ColsEqual,
    ItemsEqual,

  "FormOption",
    BLinePos, ShowStrChars,
    Just, Gaps, BCol, Rounding, AutoPad,
    TSize, TFamily, TSlant, TSubstitutions, TWeight, TVariations, TOptions,

  "GraphicsOption",
    ISize, IMargin, FMargin, PMargin,
    PScale, PRange, ARatio, PClip, PLabel,
    FStyle, FTicks,

  "PlotOption",
    ColorFn, VertexColorFn,
    NodeBCol, NodeClickFn, NodeClickData, EdgeClickFn, EdgeClickData, FanDist,
    JoinPos, SplitPos,
    FColor, FThick, FOpacity, FDashing,
    NodeThick, EdgeThick,

  "FormOption",
     ItemPos,  ItemGaps,  ItemDivs,  ItemJust,
    LabelPos, LabelGaps, LabelDivs, LabelJust,
    ItemFn, LabelFn, ClickFn, TooltipFn, SizeFn,

  "Symbol",
    Bef, Aft,
    Hor, Ver, Cen,
    Lef, Rig,
    Bot,
    TopL, TopC, TopR,
    CenL, CenC, CenR,
    BotL, BotC, BotR,
    BLine
];

(*************************************************************************************************)

DefineAliasRules[
  Bef        -> Before,
  Aft        -> After,
  Hor        -> Horizontal,
  Ver        -> Vertical,
  Cen        -> Center,
  Lef        -> Left,
  Rig        -> Right,
  Bot        -> Bottom,
  TopL       -> TopLeft,
  TopC       -> TopCenter,
  TopR       -> TopRight,
  CenL       -> CenterLeft,
  CenC       -> CenterCenter,
  CenR       -> CenterRight,
  BotL       -> BottomLeft,
  BotC       -> BottomCenter,
  BotR       -> BottomRight,
  BLine      -> Baseline
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
  InterpBox      -> InterpretationBox,
  G2DBox         -> GraphicsBox,
  G3DBox         -> Graphics3DBox,
  SBox           -> StyleBox,
  TBox           -> TemplateBox,
  RawBox         -> RawBoxes
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
  StdForm        -> StandardForm,
  SysForm        -> SystemForm,
  LitStr         -> $PrintLiteral,
  LitStrRow      -> LiteralStringRow,
  Col            -> Column
];

(*************************************************************************************************)

DefineAliasRules[
  GridItemSize   -> GridBoxItemSize,
  GridItemStyle  -> GridBoxItemStyle,
  GridMargins    -> GridFrameMargins,
  GridDivs       -> GridBoxDividers
];

DefineAliasRules[
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
  BCol           -> Background,
  Just           -> Alignment,
  Gaps           -> Spacings,
  BLinePos       -> BaselinePosition,
  Rounding       -> RoundingRadius,
  AutoPad        -> ContentPadding
];

DefineAliasRules[
  TSize          -> FontSize,
  TFamily        -> FontFamily,
  TWeight        -> FontWeight,
  TSlant         -> FontSlant,
  TVariations    -> FontVariations,
  TOptions       -> PrivateFontOptions,
  ShowStrChars   -> ShowStringCharacters,
  TSubstitutions -> FontSubstitutions
];

DefineAliasRules[
  ColorFn        -> ColorFunction,
  ISize          -> ImageSize,
  IMargin        -> ImagePadding,
  FMargin        -> FrameMargins,
  PLabel         -> PlotLabel,
  PScale         -> PlotScale,
  PMargin        -> PlotRangePadding,
  PRange         -> PlotRange,
  PClip          -> PlotRangeClipping,
  ARatio         -> AspectRatio
];

DefineAliasRules[
  FColor         -> FrameColor,
  FThick         -> FrameThickness,
  FOpacity       -> FrameOpacity,
  FDashing       -> FrameDashing,
  FStyle         -> FrameStyle,
  FTicks         -> FrameTicks
];

DefineLiteralRules[
  Wht            -> GrayLevel[1],
  Blk            -> GrayLevel[0]
];

DefineAliasRules[
  RGB            -> RGBColor,
  GL             -> GrayLevel,
  Dir            -> Directive
];

DefineAliasRules[
  NodeBCol       -> NodeBackground,
  VertexColorFn  -> VertexColorFunction,
  NodeClickFn    -> NodeClickFunction,
  EdgeClickFn    -> EdgeClickFunction,
  JoinPos        -> JoinPosition,
  SplitPos       -> SplitPosition,
  FanDist        -> FanDistance,
  NodeThick      -> NodeThickness,
  EdgeThick      -> EdgeThickness
];

