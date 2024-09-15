PrivateExports[
  "Function",
  procDPlotOptions,
  makeDCoordsHandler,
  GraphicsXYApply,
  "MessageFunction",
  badDPlotSetting, checkDPlotSetting,
  "Variable",
  $dPlotRectangleStyle, $dPlotFrameStyle, $dPlotSelectionRectangleStyle, $dPlotLabelStyle,
  $dPlotSelectionColor, $dPlotNoneColor, $dPlotNoneSymbol,
  $baseDPlotOptions,
  $dFrameTop, $dFrameBottom, $dFrameLeft, $dFrameRight
];

(*************************************************************************************************)

$dPlotRectangleStyle          = Sequence[EdgeForm @ {GrayLevel[0.5], AbsoluteThickness[1]}, FaceForm @ GrayLevel[0.8]];
$dPlotFrameStyle              = Sequence[FaceForm @ None, EdgeForm @ GrayLevel[0.5]];
$dPlotSelectionRectangleStyle = Sequence[FaceForm @ $dPlotSelectionColor, EdgeForm @ None];
$dPlotLabelStyle              = Sequence[];

(*************************************************************************************************)

$dPlotSelectionColor          = RGBColor[0.42, 0.69, 0.96, 0.2];
$dPlotNoneColor               = $DarkRed;

(*************************************************************************************************)

$dPlotNoneSymbol = "\[LongDash]";

(*************************************************************************************************)

$baseDPlotOptions = {
  ImageSize              -> Automatic,
  ImagePadding           -> 10,
  AdditionalImagePadding -> 0,
  ColorRules             -> {},
  DynamicFilter          -> All,
  DynamicSelection       -> All,
  PlotLabel              -> None
};

(*************************************************************************************************)

procDPlotOptions[head_Symbol, opts_List, opts2___Rule] := Locals[

  UnpackOptionsAs[head, opts,
    imageSize, imagePadding, additionalImagePadding,
    colorRules, plotLabel,
    dynamicFilter, dynamicSelection
  ];

  UnpackStringOptions[{opts2}, 1, scaleFactor, heightFactor];
  scaleFactor *= 300;

  SetAuto[imageSize, Scaled[1]];
  SetScaledFactor[imageSize, scaleFactor];

  imageSize = Floor @ Switch[imageSize,
    NumP,          {1, 1 / heightFactor} * imageSize,
    {NumP, NumP},  imageSize,
    True,          checkDPlotSetting[False, opts, ImageSize];
  ];

  imagePadding //= ParsePadding;
  additionalImagePadding //= ParsePadding;
  checkDPlotSetting[NumMatQ[imagePadding], opts, ImagePadding];
  checkDPlotSetting[NumMatQ[additionalImagePadding], opts, AdditionalImagePadding];

  scale = {xScale, yScale} = 1.0 / imageSize;

  padding = imagePadding + additionalImagePadding;
  paddedSize = ImageSizePad[imageSize, padding];
  dSelection = DValue[dynamicSelection, "Filter"];
  dFilter = dynamicFilter;

  If[!MatchQ[colorRules, {Rule[_Integer, ColorP]...}], badDPlotSetting[ColorRules, colorRules]];
  otherData = {scale, paddedSize, padding, plotLabel, colorRules};

  PackAssociation[xScale, yScale, dSelection, dFilter, otherData, imageSize]
];

_procDOptions := BadArguments[];

(*************************************************************************************************)

PrivateFunction[badDPlotSetting, checkDPlotSetting]

General::badDynamicPlotOptionSetting = "Bad setting `` -> ``.";

badDPlotSetting[sym_Symbol, value_] := ThrowMessage[sym -> "badDynamicPlotOptionSetting", value];

checkDPlotSetting[True, _, _] := Null;
checkDPlotSetting[other_, opts_, sym_] := badDPlotSetting[sym, Lookup[opts, sym]];

(*************************************************************************************************)

PrivateFunction[makeDGraphicsBox]

Options[makeDGraphicsBox] = {
  "PlotBoxes"      -> $Failed,
  "LabelBoxes"     -> None,
  "TickBoxes"      -> None,
  "SelectionBoxes" -> $Failed,
  "PlotRange"      -> $Failed,
  "FramePadding"   -> $Failed,
  "Frame"          -> True,
  "Handler"        -> None,
  "OtherData"      -> $Failed
};

makeDGraphicsBox[OptionsPattern[]] := Scope[
  UnpackOptions[
    plotBoxes, selectionBoxes, labelBoxes, tickBoxes,
    plotRange, framePadding, frame,
    handler,
    otherData
  ];
  {scale, paddedSize, padding, plotLabel, colorRules} = otherData;

  (* apply color rules dynamically *)
  plotBoxes //= insertColorRules[colorRules];

  primitiveBoxes = {
    attachStyle[plotBoxes,      $dPlotRectangleStyle],
    attachStyle[selectionBoxes, $dPlotSelectionRectangleStyle],
    attachStyle[labelBoxes,     $dPlotLabelStyle],
    attachStyle[tickBoxes,      $dPlotLabelStyle]
  };

  (* calculate the frame *)
  If[frame =!= False,
    plotSize = PlotRangeSize @ plotRange;
    frameBounds = EnlargeBounds[plotRange, framePadding * (plotSize * scale)];
    {{frameL, frameB}, {frameR, frameT}} = frameCorners = Transpose @ frameBounds;
    frameBoxes = Switch[frame,
      True,      Apply[RectangleBox, frameCorners],
      LeftRight, Cons[LineBox, {{{frameL, frameB}, {frameL, frameT}}, {{frameR, frameB}, {frameR, frameT}}}],
      TopBottom, Cons[LineBox, {{{frameL, frameT}, {frameR, frameT}}, {{frameL, frameB}, {frameR, frameB}}}]
    ];
    primitiveBoxes //= ReplaceAll[{$dFrameTop -> frameT, $dFrameBottom -> frameB, $dFrameLeft -> frameL, $dFrameRight -> frameR}];
    AppendTo[primitiveBoxes, StyleBox[frameBoxes, $dPlotFrameStyle]];
  ];

  graphicsBoxes = Cons[GraphicsBox,
    primitiveBoxes,
    PlotRange -> plotRange,
    ImageSize -> paddedSize,
    ImagePadding -> padding,
    AspectRatio -> Full,
    PlotLabel -> If[plotLabel === None, None, ToBoxes @ plotLabel],
    PlotRangeClipping -> False,
    BaseStyle -> {FontFamily -> "Source Code Pro", FontSize -> 10}
  ];

  (* attach the handler if any *)
  If[handler =!= None, graphicsBoxes = TagBox[graphicsBoxes, handler]];

  graphicsBoxes
];

attachStyle[None, ___] := Nothing;
attachStyle[boxes_, style___] := If[FreeQ[boxes, StyleBox], StyleBox[boxes, style], boxes];

(* TODO: handle PointBox etc *)
insertColorRules[{}] = Id;
insertColorRules[rules_][DBoxes[expr_]] := DBoxes[applyDColorRules[expr, rules]];
insertColorRules[rules_][_] := $Failed;

(**************************************************************************************************)

PrivateFunction[applyDColorRules]

applyDColorRules[boxes_, {}] := boxes;

applyDColorRules[boxes_, rules_List] := Scope[
  MapApply[
    {i, c} |-> ApplyTo[boxes, MapAt[StyleBoxOperator[FaceEdgeForm @ c], i]],
    rules
  ];
  boxes
];

(**************************************************************************************************)

SetHoldR[makeDCoordsHandler];

makeDCoordsHandler[coordFn_, down_, up_] :=
  makeDCoordsHandler[coordFn, down, None, up];

makeDCoordsHandler[coordFn_, down_, drag_, up_, dragUp_] :=
  makeDCoordsHandler[coordFn, down, drag; dragUp, up; dragUp];

makeDCoordsHandler[coordFn_, down_, drag_, up_] := EventHandlerTag[{
  makeHandlerTagRule1["MouseDown",    coordFn, down],
  makeHandlerTagRule1["MouseDragged", coordFn, drag],
  makeHandlerTagRule1["MouseUp",      coordFn, up]
}];

SetHoldA[makeHandlerTagRule1];

makeHandlerTagRule1[_, _, None]              := Nothing;
makeHandlerTagRule1[name_, coordFn_, body_]  := makeHandlerTagRule2[name, coordFn, Function[body] /. $ -> #];
makeHandlerTagRule2[name_, coordFn_, fn_]    := name :> GraphicsXYApply[coordFn, fn];

_makeDCoordsHandler := BadArguments[];

(**************************************************************************************************)

GraphicsXYApply[coordFn_, fn_] := Replace[
  MousePosition["Graphics"],
  xy:{_, _} :> At[fn, coordFn @ xy]
];
