SystemExports[
  "FormHead",
    FontColored, FontGlow, FontScaled, FontSized,
    CodeFont, CodeSansFont, SansFont, SerifFont, RobotoFont,
    CodeStyle, BoldSyntaxForm
];

PackageExports[
  "BoxFn",
    FontFamilyBox, FontColorBox, FontGlowBox, FontScaleBox, FontSizeBox, FontSizeDeltaBox,
    UnderlinedBox, ItalicBox, SemiBoldBox, BoldBox, PlainBox,
    CodeFontBox, CodeSansFontBox, SansFontBox, SerifFontBox, RobotoFontBox,
    CodeStyleBox, BoldSyntaxBox
];

(**************************************************************************************************)

SetForm1 @ SetCurry2[FontColored, FontGlow, FontScaled, FontSized];

SystemBox[FontColored[expr_, color__]] := FontColorBox[MakeBox @ expr, color];
SystemBox[FontGlow[expr_, color_]]     := FontGlowBox[MakeBox @ expr, size];
SystemBox[FontScaled[expr_, scale_]]   := FontScaleBox[MakeBox @ expr, scale];
SystemBox[FontSized[expr_, size_]]     := FontSizeBox[MakeBox @ expr, size];

(**************************************************************************************************)

SetCurry2BoxFn @ FontFamilyBox;

(* CoreBox[formSym[expr_]] := ApplyStyleBox[style] @ MakeBoxes @ expr; *)
FontFamilyBox[e_, f_Str] := ApplyStyleBox[FontFamily -> f][e];

(**************************************************************************************************)

SetCurry2BoxFn[FontColorBox, FontGlowBox, FontScaleBox, FontSizeBox, FontSizeDeltaBox]

FontColorBox[boxes_, color_]    := StyleBox[boxes, FontColor -> NiceColor[color]];
FontGlowBox[boxes_, color_]     := StyleBox[boxes, FontVariations -> {"Underlight" -> NiceColor[color]}];

FontSizeBox[boxes_, n_]         := StyleBox[boxes, FontSize -> n];
FontSizeBox[boxes_, Scaled[s_]] := FontScaleBox[boxes, s];
FontScaleBox[boxes_, n_]        := StyleBox[boxes, FontSize -> (Inherited * n)];
FontSizeDeltaBox[boxes_, n_]    := StyleBox[boxes, FontSize -> (Inherited + n)];

(**************************************************************************************************)

SetForm1[UnderlinedForm, ItalicForm, SemiBoldForm, BoldForm, PlainForm];
SetBoxFn[UnderlinedBox, ItalicBox, SemiBoldBox, BoldBox, PlainBox];

DefineStyleFormBox[
  UnderlinedBox ; UnderlinedForm ; Underlined,
  ItalicBox     ; ItalicForm     ; FontSlant -> Italic,
  SemiBoldBox   ; SemiBoldForm   ; FontWeight -> "SemiBold",
  BoldBox       ; BoldForm       ; FontWeight -> Bold,
  PlainBox      ; PlainForm      ; Plain
];

(**************************************************************************************************)

SetForm1[CodeFont, CodeSansFont, SansFont, SerifFont, RobotoFont];
SetBoxFn[CodeFontBox, CodeSansFontBox, SansFontBox, SerifFontBox, RobotoFontBox];

DefineStyleFormBox[
  RobotoFontBox   ; RobotoFont   ; FontFamily -> "Roboto",
  CodeFontBox     ; CodeFont     ; FontFamily -> "Source Code Pro",
  CodeSansFontBox ; CodeSansFont ; FontFamily -> "Source Sans Code",
  SansFontBox     ; SansFont     ; FontFamily -> "Source Sans Pro",
  SerifFontBox    ; SerifFont    ; FontFamily -> "Source Serif Pro"
];

(**************************************************************************************************)

SetForm1 @ BoldSyntaxForm;
SetBoxFn @ BoldSyntaxBox;

SystemBox[BoldSyntaxForm[expr_]] := BoxBurrowing[BoldSyntaxBox] @ MakeBoxes[expr];

BoldSyntaxBox[RowBox[{a_, syn_Str, b_}]] := RowBox[{a, " ", BoldBox[syn], " ", b}];
BoldSyntaxBox[boxes_] := boxes;

(**************************************************************************************************)

SetForm0 @ CodeStyle;
SetBoxFn @ CodeStyleBox;

SystemBox[CodeStyle[expr_, opts___Rule]] := CodeStyleBox[BlockFormatting @ MakeBoxes @ expr, opts];

CodeStyleBox[box_, opts___] := StyleBox[box, opts, FontFamily -> "Source Code Pro", ShowStrChars -> True];
