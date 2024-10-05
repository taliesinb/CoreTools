PackageExports[
  "BoxFn", UnderBraceBox, OverBraceBox, UnderBracketBox, OverBracketBox, UnderParenBox, OverParenBox
];

(**************************************************************************************************)

SetBoxFn[UnderBraceBox, OverBraceBox, UnderBracketBox, OverBracketBox, UnderParenBox, OverParenBox];

UnderBraceBox[a_, b_] := UnderscriptBox[UnderscriptBox[a, "\[UnderBrace]"], b];
OverBraceBox[a_, b_] := OverscriptBox[OverscriptBox[a, "\[OverBrace]"], b];

UnderBracketBox[a_, b_] := UnderscriptBox[UnderscriptBox[a, "\[UnderBracket]"], b];
OverBracketBox[a_, b_] := OverscriptBox[OverscriptBox[a, "\[OverBracket]"], b];

UnderParenBox[a_, b_] := UnderscriptBox[UnderscriptBox[a, "\[UnderParenthesis]"], b];
OverParenBox[a_, b_] := OverscriptBox[OverscriptBox[a, "\[OverParenthesis]"], b];

