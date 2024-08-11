SystemExports[
  "GraphicsPrimitive", EmptyRectangle, EmptyPolygon
];

(**************************************************************************************************)

DefineGPrim[EmptyRectangle, "Pos,Pos", makeEmptyRectangleBoxes]

makeEmptyRectangleBoxes = ExtendCaseOf[
  EmptyRectangle[a:PosAP, b:PosAP] := EmptyRectangleBox[a, b]
];

(**************************************************************************************************)

DefineGPrim[EmptyPolygon, "PosList | PosLists", makeEmptyPolygonBoxes, {2, 3}]

makeEmptyPolygonBoxes = ExtendCaseOf[
  EmptyPolygon[p:PosAListP]  := EmptyPolygonBox[p];
  EmptyPolygon[p:PosAListsP] := Map[EmptyPolygonBox, p]
];
