SystemExports[
  "FormHead", LabelTop
];

PackageExports[
  "BoxFn",    LabelTopBox
];

(*************************************************************************************************)

CoreBox[LabelTop[label_, expr_]] := LabelTopBox[label, expr];

LabelTopBox[label_, expr_] := ColumnBox[
  {StyleBox[MakeBoxes @ label,
    FontWeight -> Bold, FontFamily -> "Source Code Sans",
    FontSize -> Inherited],
   MakeBoxes @ expr},
  Left, 0.2
];

SetCurry1BoxFn @ LabelTopBox;
