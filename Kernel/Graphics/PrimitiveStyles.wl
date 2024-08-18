SystemExports[
  "GraphicsPrimitive", Styled
];

PrivateExports[
  "SpecialVariable", $CurrentGStyles
];

(**************************************************************************************************)

$StyleData = UDict[];

DefineGPrim[Styled, "Primitives", withStyleBoxes]

withStyleBoxes[_] := $Failed;
withStyleBoxes[Styled[boxes_]] := boxes;
withStyleBoxes[Styled[boxes_]] := boxes;

withStyleBoxes[Styled[boxes_, opts___]] := Locals[


];

