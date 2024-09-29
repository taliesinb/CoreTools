SystemExports[
  "FormHead",    OutputExpressionForm, MessageArgumentForm
];

PackageExports[
  "FormHead",    OutExprForm, MsgArgForm,
  "IOFunction",  SetPrePrintFns, UnsetPrePrintFns,
  "BoxFunction", OutExprBox, MsgArgBox, MsgCodePaneBox, MsgExprFormBox, MsgElidedBox,
  "Variable",    $OutExprFormOptions, $MsgArgPasting, $MsgArgFormat, $MsgArgExprFormOptions
];

(*************************************************************************************************)

SetPrePrintFns[] := List[
  If[ToBoxes[MsgArgForm[LiteralString["x"]]] === "\"x\"", $MessagePrePrint = MsgArgForm; True, False],
  If[ToBoxes[OutExprForm[{1,2,3}]] === RowBox[{"{", "1", ",", "2", ",", "3", "}"}], $PrePrint = OutputExpressionForm; True, False]
];

UnsetPrePrintFns[] := Then[
  If[$MessagePrePrint === MsgArgForm, Unset[$MessagePrePrint], True, False],
  If[$PrePrint === OutExprForm, Unset[$PrePrint], True, False]
];

(*************************************************************************************************)

ClearAll[OutExprForm];
ClearAll[MsgArgForm];

MakeBoxes[OutExprForm[e_], _] := OutExprBox[e];
MakeBoxes[OutExprForm[e___], _] := OutExprBox[e];

OutExprBox[]      := FnRBox["Seq"];
OutExprBox[e___]  := With[{opts = $OutExprFormOptions}, MakeExprBoxes[e, opts]];
OutExprBox[s_Str] := s;
OutExprBox[SystemForm[e_]] := MakeBoxes @ e;
OutExprBox[f:FullForm[_]]  := MakeBoxes @ f;

$OutExprFormOptions = {
  MaxLength -> {24, 8, 4, 4, 4, 2, 2},
  MaxDepth -> 8,
  MaxStrLen -> 64
};

(**************************************************************************************************)

SystemBox[MsgArgForm[e_]] := MsgArgBox @ e;

(**************************************************************************************************)

$MsgArgPasting::usage =
"$MsgArgPasting controls whether MsgArgForm boxes should allow click-to-paste.";

SetInitial[$MsgArgPasting, True];

SetHoldC @ MsgArgBox;

MsgArgBox[expr_] := Block[{$MessagePrePrint = Automatic, $CoreInteractive = False}, msgArg1 @ expr];

(**************************************************************************************************)

SetHoldC @ litStrQ;
seqRBox[box_] := RowBox[{"Seq", "[", box, "]"}];
litStrQ[str_Str] := StrLen[str] < 128 && StrHasQ[str, " " | "/"] && ASCIIQ[str];

(**************************************************************************************************)

SetHoldC @ msgArg1;

msgArg1 = CaseOf[
  {}                      := RBox["{", " ", "}"];
  EmptyUDict              := RBox["UDict", "[", " ", "]"];
  EmptyDict               := RBox[LAssoc, " ", RAssoc];
  ""                      := RBox[DQuote, DQuote];
  b_RawBoxes              := b;
  l:LitStr[StrP]          := MakeBox @ l;
  f_Failure               := LitStrBox @ FailureString @ f;
  pSeq[]                  := seqRBox @ " ";
  pSeq[e_]                := seqRBox @ $ @ e;
  PrivHold[]              := seqRBox @ " ";
  PrivHold[e_]            := $ @ e;
  PrivHoldSeq[]           := "()";
  PrivHoldSeq[e_]         := $ @ e;
  HoldForm[e_]            := $ @ e;
  s:StrP ? litStrQ        := LitStrBox @ s;
  l_SrcLoc ? SrcLocQ      := LitStrBox @ SrcLoxBox @ l;
  l:{__SrcLoc ? SrcLocQ}  := LitStrRowBox @ Map[SrcLoxBox] @ DelDups @ l;
  e_ /; $MsgArgPasting    := BlockFalse[$MsgArgPasting, PasterBox[msgArg2 @ e, e]];
  e_                      := msgArg2 @ e,

  {pSeq -> Alt[Seq, PrivSeq]}
];

SetHoldC @ msgArg2;

msgArg2 = CaseOf[
  d:DatumP                 := MakeBoxes @ d;
  f_Failure                := FailureString @ f;
  pHold[a_, b_]            := seqRBox @ commaBox[msgArg1 @ a, msgArg1 @ b];
  pHold[a_, b_, c_]        := seqRBox @ commaBox[msgArg1 @ a, msgArg1 @ b, msgArg1 @ c];
  pHold[a_, b_, c_, __]    := seqRBox @ commaBox[msgArg1 @ a, msgArg1 @ b, msgArg1 @ c, CDots];
  pHoldSeq[a_, b_]         := commaBox[msgArg1 @ a, msgArg1 @ b];
  pHoldSeq[a_, b_, c_]     := commaBox[msgArg1 @ a, msgArg1 @ b, msgArg1 @ c];
  pHoldSeq[a_, b_, c_, __] := commaBox[msgArg1 @ a, msgArg1 @ b, msgArg1 @ c, CDots];
  LitStrRow[s:{__Str}, r_Str:", "] := LitStrRowBox[s, r];
  expr_                    := msgArg3 @ expr,

  {pHold -> Alt[Seq, PrivSeq, PrivHold],
   pHoldSeq -> Alt[HoldForm, PrivHoldSeq],
   commaBox -> CommaRBox}
];

SetHoldC @ msgArg3;

$MsgArgFormat::usage =
"$MsgArgFormat controls MsgArgForm formatting, and can be one of 'CodePane', 'CodeForm', 'ExprForm', 'ElidedForm', or None.";

SetInitial[$MsgArgFormat, "ExprForm"];

msgArg3[expr_] := Switch[$MsgArgFormat,
  "CodePane",   MsgCodePaneBox @ expr,
  "CodeForm",   MakeCodeBoxes[expr, 2, 3],
  "ExprForm",   MsgExprFormBox @ expr,
  "ElidedForm", HoldElidedBox @ expr,
  _,            ErrorPrint["Unknown $MsgArgFormat: ", $MsgArgFormat]; MakeBoxes @ expr,
  None,         MakeBoxes @ expr
];

(**************************************************************************************************)

SetHoldC[MsgCodePaneBox, MsgExprFormBox, MsgElidedBox];

MsgCodePaneBox[expr_] := PaneBox[
  MakeTruncatedBoxes[NoEval @ expr, 120, 30, 6],
  BaseStyle -> {$msgStyleOptions}, ImageSize -> {UpTo[650], UpTo[60]},
  BaselinePosition -> Baseline,
  StripOnInput -> True
];

MsgExprFormBox[expr_] := With[{opts = $MsgArgExprFormOptions}, MakeExprBoxes[expr, opts]];

SetInitial[$MsgArgExprFormOptions, {
  MaxLength   -> {4,2,1},
  MaxDepth    -> 2,
  MaxStrLen   -> 16,
  ElidePacked -> True,
  PrintPrecision -> 3,
  Multiline   -> False
}];

MsgElidedBox[expr_] := HoldElidedBox @ expr;

(**************************************************************************************************)

$msgStyleOptions = Sequence[
  ShowAutoStyles       -> False,
  ShowStringCharacters -> True,
  LineSpacing          -> {1, 0},
  AutoIndent           -> True,
  AutoSpacing          -> True,
  LineIndent           -> .5,
  Hyphenation          -> False,
  FontFamily           -> "Source Code Pro",
  FontWeight           -> "DemiBold",
  PrintPrecision       -> 3,
  NumberMarks          -> False,
  ShowInvisibleCharacters -> True,
  GraphicsBoxOptions   -> {ImageSize -> 100}
];

