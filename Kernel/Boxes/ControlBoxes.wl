SystemExports[
  "FormHead", Unformatted, Uninteractive, FormBlock, Themed
];

(**************************************************************************************************)

"Unformatted[expr$] displays expr$ with all core formatting turned off."
"Uninteractive[expr$] displays expr$ with all core interactivity turned off."

MakeBoxes[  Unformatted[lhs_], form_] /; $CoreFormatting :=  BlockFormatting @ MakeBoxes[lhs, form];
MakeBoxes[Uninteractive[lhs_], form_] /; $CoreFormatting := BlockInteractive @ MakeBoxes[lhs, form];

(*************************************************************************************************)

"FormBlock[var$ = val$, expr$] typesets expr$ with the given override, applied at boxification.
FormBlock[{var$1 = val$1, $$}, expr$] applies multiple settings."

SetHoldF @ FormBlock;

CoreBox[FormBlock[settings_, body_]] := formBlockBoxes[settings, body];

SetHoldA @ formBlockBoxes;

formBlockBoxes = CaseOf[
  $[set_Set,    expr_] := formBlockBoxes[{set}, expr];
  $[{set__Set}, expr_] := Block[{sym}, MakeBoxes @ expr];
  $[bad_,       expr_] := ErrorTooltipBox[MakeBoxes @ expr, FormBlock::badSettingArg, HoldForm @ bad];
  _                    := $Failed
];

FormBlock::badSettingArg = "Not a Set or list of Sets: ``"

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
  Themed[expr_, theme_Str] := MakeBoxes @ expr;
  Themed[expr_]            := MakeBoxes @ expr;
  _                        := $Failed;
];

