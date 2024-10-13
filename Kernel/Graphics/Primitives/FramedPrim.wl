SystemExports[
  "GraphicsPrimitive", FramedPrimitive,
  "GraphicsBoxFunction", FramedPrimitiveBox,
  "Option", FrameColor, FrameThickness, FrameOpacity, FrameDashing
];

(**************************************************************************************************)

DeclaredHere[FramedPrimitive]

Options[FramedPrimitive] = {
  FrameColor     -> Black,
  FrameOpacity   -> None,
  Background     -> None,
  FrameThickness -> 1,
  FrameDashing   -> None,
  RoundingRadius -> 0,
  FrameMargins   -> 0
};

DefineGPrim[FramedPrimitive, "Primitives", FramedPrimitiveBox]

Options[FramedPrimitiveBox] = Options[FramedPrimitive];

FramedPrimitiveBox[prims_, opts___Rule] := Locals[
  UnpackSymbolsAs[
    FramedPrimitiveBox, List @ opts,
    frameColor, frameThickness, frameDashing, background, roundingRadius, frameMargins
  ];
  boxes = ToGraphicsBoxes @ prims;
  range = GBoxRange @ boxes;
  range += {{-1, 1}, {-1, 1}} * ParsePadding[frameMargins];
  {bl, tr} = Transpose @ range;
  thick = AThickness @ frameThickness;
  If[frameDashing === None,
    frameBoxes = Construct[RectangleBox, bl, tr, RoundingRadius -> roundingRadius];
    frameBoxes //= StyleBoxOp[FaceForm @ background, EdgeForm[{frameColor, thick}]];
  ,
    (* Note: dashing precludes rounding radius without extra work to discretize the curve !*)
    frameBoxes = StyleBox[EmptyRectangleBox[bl, tr], frameColor, thick, Dashing @ frameDashing];
    If[ColorQ[background],
      frameBoxes = {frameBoxes, StyleBox[Construct[RectangleBox, bl, tr], FaceForm @ background, EdgeForm @ None]}];
  ];
  List[frameBoxes, boxes]
]
