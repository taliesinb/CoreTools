SystemExports[
  "GraphicsPrimitive", HalfCircle, HalfDisk
];

(**************************************************************************************************)

DefineGPrim[HalfCircle, "Pos,PosDelta", halfCircleBoxes]
DefineGPrim[HalfDisk,   "Pos,PosDelta", halfDiskBoxes]

halfCircleBoxes[HalfCircle[p:Pos2P, d:Pos2P]] := Locals[
  ang = PairAngle[d];
  p1 = p + VecRot90[d];
  p2 = p + VecRot90CW[d];
  List[
    Construct[CircleBox, p, Norm @ d, {ang - Pi/2, ang + Pi/2}],
    Construct[LineBox, {p1, p2}]
  ]
];

halfDiskBoxes[HalfDisk[p:Pos2P, d:Pos2P]] := Locals[
  ang = PairAngle[d];
  Construct[DiskBox, p, Norm @ d, {ang - Pi/2, ang + Pi/2}]
];
