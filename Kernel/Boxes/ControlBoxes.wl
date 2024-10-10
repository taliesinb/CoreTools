SystemExports[
  "FormHead", Unformatted, Uninteractive, FormBlock, Themed
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

(* approach:
have an option called Theme, which is our version of PlotTheme.
RegisterTheme[head, name, opts] will set $ThemeData[head, name] = UDict[opts].
look through theme stack:
  if we see a single string, look up $ThemeData[head, name], and recurse through IT as a stack if present.
  if we see a rule head -> spec, look through it.
  if we see a rule prop -> value, our answer is value.
  if we see Theme -> 'name', look through $ThemeDa
any recursion can return DefaultValue, which will cause us to keep looking.

*)

(* ThemeLookup[head_Symbol, prop_Symbol] :=  *)

CoreBox[themed_Themed] := themedBoxes[themed];

SetHoldC @ themedBoxes;

themedBoxes = CaseOf[
  Themed[expr_, theme_Str] := MakeBox @ expr;
  Themed[expr_]            := MakeBox @ expr;
  _                        := $Failed;
];

