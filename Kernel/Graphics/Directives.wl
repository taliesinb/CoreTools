SystemExports[
  "GraphicsDirective", SolidStyled, EmptyStyled
];

PackageExports[
  "GraphicsDirective", FaceEdged, Faced, Edged,
  "GraphicsDirective", ResolveDirective
];

PrivateExports[
  "MetaFunction", DefineDirective
];

(**************************************************************************************************)

SetHoldA @ DefineDirective;

DefineDirective[sym_Sym, defs__SetD] := Then[
  SystemBoxes[s_sym[e_]] := StyleBox[MakeBoxes @ e, ResolveDirective @ s],
  ReleaseHold @ MapCol1[ResolveDirective, Hold[defs]]
];

ResolveDirective[_] := Directive[Pink, EdgeForm @ Pink, FaceForm @ Pink];

(**************************************************************************************************)

DefineDirective[SolidStyled,
  SolidStyled[col_ ? ColorQ]                := Directive[col, tintedFaceForm @ col],
  SolidStyled[col_ ? ColorQ, th_ ? NumberQ] := Directive[col, tintedFaceForm @ col, EdgeForm @ AThickness @ th]
];

DefineDirective[EmptyStyled,
  EmptyStyled[col_ ? ColorQ]                := Directive[col, FaceForm @ None, EdgeForm @ col],
  EmptyStyled[col_ ? ColorQ, th_ ? NumberQ] := Directive[col, FaceForm @ None, EdgeForm @ Directive[col, AThickness @ th]]
];

