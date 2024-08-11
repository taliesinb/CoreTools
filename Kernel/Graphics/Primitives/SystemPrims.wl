PackageExports[];

(**************************************************************************************************)

DefineGPrimSig["Pos,Pos", Cuboid | Rectangle]; (* EmptyRectangle *)

DefineGPrimSig["Pos | PosList", Point];

DefineGPrimSig["PosList | PosLists", Polygon | Polyhedron | Line | Arrow | Triangle];

DefineGPrimSig["Pos?Radius | PosList?Radius", Circle | Disk | Sphere | Ball];

DefineGPrimSig["Pos?Radius", Annulus | Cube];

DefineGPrimSig["PosList", GraphicsComplex | BSplineCurve | BezierCurve | Simplex];

DefineGPrimSig["PosList?Radius | PosLists?Radius | Curve?Radius", Tube];

DefineGPrimSig["PosPair", InfiniteLine | HalfLine];

DefineGPrimSig["PosPair?Radius", Cylinder | Cone | CapsuleShape | StadiumShape];

DefineGPrimSig["Pos,PosDelta", InfiniteLine | HalfLine];

DefineGPrimSig["Opaque,Pos", Text | Inset];

DefineGPrimSig["Opaque", Arrowheads];

DefineGPrimSig["Primitives,Pos", Translate];

DefineGPrimSig["Primitives", Rotate | GeometricTransformation | Scale | Interpretation | Style |
  Annotation | Tooltip | StatusArea | PopupWindow | Mouseover | Hyperlink | EventHandler | Button];


