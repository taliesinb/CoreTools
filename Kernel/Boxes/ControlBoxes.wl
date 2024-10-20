SystemExports[
  "FormHead", Unformatted, Uninteractive, FormBlock, Themed
];

PackageExports[
  "BoxFn",     FormBlockBox,
  "SpecFn",    MakeThemedBox
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

SetBoxFn @ SetHoldC @ MakeThemedBox;

SetCoreSubBox[Themed]

CoreBox[Themed[spec___][expr_]] := MakeThemedBox[expr, spec];

MakeThemedBox[expr_, spec_]   := BlockTheme[spec, MakeBox @ expr];
MakeThemedBox[expr_, spec__]  := BlockTheme[{spec}, MakeBox @ expr];
