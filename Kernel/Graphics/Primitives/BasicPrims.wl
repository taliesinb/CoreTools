SystemExports[
  "GraphicsPrimitive", HalfCircle, HalfDisk, EmptyRectangle, EmptyPolygon, ShowBounds,
  "GraphicsBoxFn",     HalfCircleBox, HalfDiskBox, EmptyRectangleBox, EmptyPolygonBox, ShowBoundsBox
];

(**************************************************************************************************)

DeclaredHere[ShowBounds]

DefineGPrim[ShowBounds, "Primitives", ShowBoundsBox]

ShowBoundsBox[prims_] := Block[{$DebugBounds = True}, ToGraphicsBoxes @ prims];

(**************************************************************************************************)

DefineGPrim[HalfCircle, "Pos,PosDelta", HalfCircleBox]
DefineGPrim[HalfDisk,   "Pos,PosDelta", HalfDiskBox]

HalfCircleBox[p:Pos2P, d:Pos2P] := Locals[
  ang = PairAngle[d];
  p1 = p + VecRot90[d];
  p2 = p + VecRot90CW[d];
  List[
    Construct[CircleBox, p, Norm @ d, {ang - Pi/2, ang + Pi/2}],
    Construct[LineBox, {p1, p2}]
  ]
];

HalfDiskBox[p:Pos2P, d:Pos2P] := Locals[
  ang = PairAngle[d];
  Construct[DiskBox, p, Norm @ d, {ang - Pi/2, ang + Pi/2}]
];

(**************************************************************************************************)

DefineGPrim[EmptyRectangle, "Pos,Pos", EmptyRectangleBox]
DefineGPrim[EmptyPolygon, "PosList | PosLists", EmptyPolygonBox, {2, 3}]

EmptyRectangleBox[{x1_, y1_}, {x2_, y2_}] := Make[JoinedCurveBox,
  List @ Line @ ToPacked @ {{x1, y1}, {x2, y1}, {x2, y2}, {x1, y2}},
  CurveClosed -> True
];

EmptyPolygonBox[p:PosAListsP] := Map[emptyPolyBox, p];
EmptyPolygonBox[p:PosAListP] := emptyPolyBox @ p;

emptyPolyBox[p_] := Make[JoinedCurveBox,
  List @ Line @ ToPacked @ p,
  CurveClosed -> True
];

