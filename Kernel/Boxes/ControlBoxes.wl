SystemExports[
  "FormHead", Unformatted, Uninteractive, FormBlock
];

PackageExports[
  "BoxFn",    FormBlockBox
];

(**************************************************************************************************)

"Unformatted[expr$] displays expr$ with all core formatting turned off."
"Uninteractive[expr$] displays expr$ with all core interactivity turned off."

MakeBox[  Unformatted[lhs_], form_] /; $CoreFormatting :=  BlockFormatting @ MakeBox[lhs, form];
MakeBox[Uninteractive[lhs_], form_] /; $CoreFormatting := BlockInteractive @ MakeBox[lhs, form];

(*************************************************************************************************)

"FormBlock[var$ = val$, expr$] typesets expr$ with the given override, applied at boxification.
FormBlock[{var$1 = val$1, $$}, expr$] applies multiple settings."

SetHoldF @ FormBlock;

CoreBox[FormBlock[settings_, body_]] := FormBlockBox[settings, body];

SetHoldA @ SetBoxFn @ FormBlockBox;

FormBlockBox = CaseOf[
  $[set_Set,    expr_]   := $[{set}, expr];
  $[sets:{__Set}, expr_] := Block[sets, MakeBox @ expr];
];

(**************************************************************************************************)
