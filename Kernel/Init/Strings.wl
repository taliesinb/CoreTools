SystemExports[
  "Function",
    StringReplaceRepeated,
    Base36Hash, FromHexString, HexString, FromBase36String, Base36String, FromBitString, BitString,
    StringFirst, StringRest, StringMost, StringLast, StringFirstRest, StringMostLast,
    ToLowerCase1, ToUpperCase1,
    ToTitleString, ToCamelCase, CamelCaseSplit,
    StringPositionLeft, StringPositionRight,
    StringCaseFirst, StringCaseLast,
    StringSplitPositions, StringSplitBefore, StringSplitAfter,
    StringSegmentBefore, StringSegmentAfter, StringSegment,
    StringFunction,
    RealString,
    StringTrimLeft, StringTrimRight, StringTrimLeftRight,
    StringPrepend, StringAppend,
    StringLines,
    DelimitedString, DelimitedStringRow,
    BraceString,     BraceStringRow,
    AngleString,     AngleStringRow,
    ParenString,     ParenStringRow,
    BracketString,   BracketStringRow,
    DQuotedString,   SQuotedString,
    EscapeCharacters, UnescapeCharacters,
    EscapeDQuotes, EscapeSQuotes, EscapeNewlines,


  "IOFunction",
    HoldToInputString, ToInputString, FromInputString,
  "Predicate",
    CharQ, StringStartsEndsQ, UpperCase1Q, LowerCase1Q
];

PackageExports[
  "MetaFunction",
    DefineSeqRowStringForms,
  "Function",
    StringSelect, StringDiscard, StringSelectDiscard,
    FnBracketStringRow, FnParenStringRow, FnBracketString, FnParenString,
    StrTake1, StrTakeN,
  "Variable",
    $DQuote, $SQuote, $Newline, $DNewline, $Backslash
];

(**************************************************************************************************)

HoldToInputString[e_]   := ToString[NoEval @ e, InputForm];
ToInputString[e_]       := ToString[e, InputForm];

FromInputString[str_]        := ToExpression[str, InputForm];
FromInputString[str_, head_] := ToExpression[str, InputForm, head];

(**************************************************************************************************)

StringReplaceRepeated::usage = "StringReplaceRepeated[str$, rules$] keeps applying rules$ to a string until it stops changing.";

StringReplaceRepeated[str_String, rules_] := FixedPoint[StringReplace[rules], str];
StringReplaceRepeated[rules_][str_] := StringReplaceRepeated[str, rules];

(**************************************************************************************************)

Base36Hash[e_] := Hash[e, Automatic, "Base36String"];

(**************************************************************************************************)

FromHexString::usage = "FromHexString[str$] interprets str$ as a base 16 number.";

FromHexString[s_] := FromDigits[s, 16];

HexString[n_]      := IntegerString[n, 16];
HexString[n_, l_]  := IntegerString[n, 16, l];

(**************************************************************************************************)

FromBitString[s_String] := FromDigits[s, 2];

BitString[n_]     := IntegerString[n, 2, b];
BitString[n_, b_] := IntegerString[n, 2, b];

(**************************************************************************************************)

FromBase36String::usage = "FromBase36String[str$] interprets str$ as a base 36 number.";

FromBase36String[s_String] := FromDigits[s, 36];

Base36String[n_]     := IntegerString[n, 36, b];
Base36String[n_, b_] := IntegerString[n, 36, b];

(**************************************************************************************************)

CharQ::usage = "CharQ[e$] gives True if e$ is a single-character string.";

CharQ[s_String] := StringLength[s] == 1;
CharQ[_] := False;

(**************************************************************************************************)

StringStartsEndsQ::usage = "StringStartsEndsQ[str$, prefix$, suffix$] gives True if str$ starts with prefix$ and ends with suffix$.";

StringStartsEndsQ[str_String, a_, b_] := StringStartsQ[str, a] && StringEndsQ[str, b];
StringStartsEndsQ[str_List, a_, b_] := Map[StringStartsEndsQ[#, a, b]&, str];
StringStartsEndsQ[a_, b_][str_] := StringStartsEndsQ[str, a, b];

(**************************************************************************************************)

General::emptyString1 = "First argument is an empty string.";
StringListableFunctionDefs[
  StringFirst[s_] := FastQuietCheck[StringTake[s,  1], ErrorMsg[StringFirst::emptyString1]],
  StringLast[s_]  := FastQuietCheck[StringTake[s, -1], ErrorMsg[StringLast::emptyString1]],
  StringRest[s_]  := FastQuietCheck[StringDrop[s,  1], ErrorMsg[StringRest::emptyString1]],
  StringMost[s_]  := FastQuietCheck[StringDrop[s, -1], ErrorMsg[StringMost::emptyString1]],
  StringFirstRest[s_]  := FastQuietCheck[StringTake[s, {1, 2;;}],    ErrorMsg[StringFirstRest::emptyString1]],
  StringMostLast[s_]   := FastQuietCheck[StringTake[s, {1;;-2, -1}], ErrorMsg[StringMostLast::emptyString1]]
];

(**************************************************************************************************)

DeclareListable[ToLowerCase1, ToUpperCase1]

ToLowerCase1[str_String] := StringJoin[ToLowerCase @ StringTake[str, 1], StringDrop[str, 1]];
ToUpperCase1[str_String] := StringJoin[ToUpperCase @ StringTake[str, 1], StringDrop[str, 1]];

(**************************************************************************************************)

DeclareListable[UpperCase1Q, LowerCase1Q]

UpperCase1Q[""] = False;
UpperCase1Q[str_String] := UpperCaseQ @ StringTake[str, 1];

LowerCase1Q[""] = False;
LowerCase1Q[str_String] := LowerCaseQ @ StringTake[str, 1];

(**************************************************************************************************)

DeclareListable[ToTitleString];

ToTitleString[s_String] :=
  ToLowerCase @ StringReplace[s, RegularExpression["([a-z])([A-Z])"] :> "$1 $2"];

ToCamelCase[s_String] :=
  StringJoin @ Map[ToUpperCase1, ToLowerCase @ StringSplit[s, " "]];

(**************************************************************************************************)

DeclareListable[CamelCaseSplit];

CamelCaseSplit[s_String] := StringSplit[s, RegularExpression["(?<=[a-z])(?=[A-Z])"]];

(**************************************************************************************************)

DeclareCurry2[StringTrimLeft, StringTrimRight];

StringTrimLeft[str_, lpatt_]  := StringDelete[str, StartOfString ~~ lpatt];
StringTrimRight[str_, rpatt_] := StringDelete[str, rpatt ~~ EndOfString];

DeclareCurry23[StringTrimLeftRight];

StringTrimLeftRight[str_, lpatt_, rpatt_] := StringDelete[str, {StartOfString ~~ lpatt, rpatt ~~ EndOfString}];

(**************************************************************************************************)

DeclareCurry2[StringPrepend, StringAppend];
DeclareListable1[StringPrepend, StringAppend];

StringPrepend[string_String, prefix_] := StringJoin[prefix, string];
StringAppend[string_String, suffix_] := StringJoin[string, suffix];

(**************************************************************************************************)

DeclareStrict[StringLines]
DeclareListable[StringLines]

StringLines[str_Str] := StringSplit[str, "\n"];

(**************************************************************************************************)

StringFunction[template_Str] :=
  Construct[
    Function,
    StringReplace[template, $stringFunctionSlotRules]
  ] /. {StringExpression -> StringJoin, s_Slot :> TextString[s]};


$stringFunctionSlotRules = {
  "##" -> "#",
  "#" ~~ i:DigitCharacter    :> Slot[FromDigits[i]],
  "#" ~~ w:LetterCharacter.. :> Slot[w]
};

StringFunction[File[path_Str]] := Locals[
  res = ImportUTF8 @ path;
  If[!StrQ[res], $Failed,
    $stringFunctionSlotRules = $quotedStringFunctionSlotRules;
    StringFunction @ res
  ]
];

$quotedStringFunctionSlotRules = {
  "\"#" ~~ i:DigitCharacter    ~~ "\"" :> Slot[FromDigits[i]],
  "\"#" ~~ w:Regex["[a-zA-Z][a-zA-Z0-9_$]*"] ~~ "\"" :> Slot[w]
};

(**************************************************************************************************)

DeclareHoldRest[StringPositionLeft, StringPositionRight];
DeclareCurry2[StringPositionLeft, StringPositionRight];
DeclareListable1[StringPositionLeft, StringPositionRight];

StringPositionLeft[str_String, patt_, else_:None] := First[First[StringPosition[str, patt, 1], Null], else];
StringPositionRight[str_String, patt_, else_:None] := First[Last[StringPosition[str, patt], Null], else];

(**************************************************************************************************)

General::notStringVector1 = "First argument should be a list of strings.";
DeclareCurry2[StringSelect, StringDiscard, StringSelectDiscard]

StringSelect[list_ ? StrVecQ, patt_]        := Pick[list, StringMatchQ[list, patt], True];
StringDiscard[list_ ? StrVecQ, patt_]       := Pick[list, StringMatchQ[list, patt], False];
StringSelectDiscard[list_ ? StrVecQ, patt_] := PickTrueFalse[list, StringMatchQ[list, patt]];

StringSelect[_, _] := ErrorMsg[General::notStringVector1];
StringDiscard[_, _] := ErrorMsg[General::notStringVector1];
StringSelectDiscard[_, _] := ErrorMsg[General::notStringVector1];

(**************************************************************************************************)

DeclareHoldRest[StringCaseFirst, StringCaseLast];
DeclareCurry2[StringCaseFirst, StringCaseLast];
DeclareListable1[StringCaseFirst, StringCaseLast];

StringCaseFirst[str_String, patt_, else_:None] := First[StringCases[str, patt, 1], else];
StringCaseLast[str_String, patt_, else_:None] := Last[StringCases[str, patt], else];

(**************************************************************************************************)

DeclareListable1[StringSplitPositions];

StringSplitPositions[str_String, pos_] := strSplitPos[str, pos];

strSplitPos[str_, {}] := {str};
strSplitPos[str_, i_Integer | {i_Integer}] := StringTakeDrop[str, i-1];

strSplitPos[str_, pos_List] := Module[{i = 1},
  StringTake[str, Append[
    {i, i=#; #-1}& /@ Sort[pos],
    {i, StringLength[str]}
  ]]
];

strSplitPos[_, spec_] := (Message[StringSplitAt::badSplitSpec, spec]; $Failed);
StringSplitPositions::badSplitSpec = "`` is not a valid StringSplitPositions specification.";

(**************************************************************************************************)

DeclareListable1[StringSplitBefore, StringSplitAfter];

StringSplitBefore[str_String, patt_] := StringSplitPositions[str, Part[StringPosition[str, patt], All, 1]];
StringSplitAfter[str_String, patt_] := StringSplitPositions[str, Part[StringPosition[str, patt] + 1, All, 2]];

(**************************************************************************************************)

DeclareListable1[StringSegment, StringSegmentBefore, StringSegmentAfter];

StringSegment[str_String, spec_]       := strSegment[str, spec, Null, Null];
StringSegmentBefore[str_String, spec_] := strSegment[str, spec, First, -1];
StringSegmentAfter[str_String, spec_]  := strSegment[str, spec, Last, 0];

Clear[strSegment];
strSegment[None, _, _, _] := {None, None};

strSegment[str_, EndOfString|All, _, _] := {str, ""};
strSegment[str_, Before[patt_], _, _] := strSegment[str, patt, First, -1];
strSegment[str_, After[patt_], _, _] := strSegment[str, patt, Last, 0];

strSegment[str_, patt_, fn_, off_] := Module[{pos},
  pos = fn @ First[off + StringPosition[str, patt, 1], Return[{None, None}, Module]];
  StringTakeDrop[str, pos]
];

strSegment[str2_, patts_List, fn_, off_] := Module[{str = str2, res},
  Map[patt |-> ({res, str} = strSegment[str, patt, fn, off]; res), patts]
];

strSegment[str_, patt_, Null, _] := Replace[StringSplit[str, patt, 2], {_} :> {None, None}];

(**************************************************************************************************)

DeclareListable[RealString];

$numDigits = 5;

RealString[value_, digits_Integer] := Block[{$numDigits = Max[digits, 1]}, RealString @ value];

RealString[0|0.] := "0";
RealString[real_Real ? Developer`MachineRealQ] := dropDot @ Internal`MRealToString[real, False, $numDigits];
RealString[real_Real] := GeneralUtilities`RealDigitsString[TextString; real, $numDigits];
RealString[Infinity] := "\[Infinity]";
RealString[-Infinity] := "-\[Infinity]";
RealString[number_ ? NumericQ] := RealString[N @ number];
RealString[_] := "?";

dropDot[s_] := If[StringTake[s, -1] === ".", StringDrop[s, -1], s];

(**************************************************************************************************)

DQuotedString[e_Str] := StrJoin["\"", EscapeDQuotes[e], "\""];
SQuotedString[e_Str] := StrJoin["'", EscapeSQuotes[e], "'"];

(**************************************************************************************************)

$DQuote    = "\"";
$SQuote    = "'";
$Newline   = "\n";
$DNewline  = "\n\n";
$Backslash = "\\";

$escapedDQuote = "\\\""; $escapedSQuote = "\\'"; $escapedNewline = "\\\n"; $escapedEscape = "\\\\";

$escapeDQuoteRules = {$escapedDQuote -> $escapedDQuote,   $DQuote -> $escapedDQuote};
$escapeSQuoteRules = {$escapedSQuote -> $escapedSQuote,   $SQuote -> $escapedSQuote};
$escapeNewlines    = {$escapedNewline -> $escapedNewline, $Newline -> $escapedNewline};

$escapeRules   = z:{$Newline, $DQuote, $Backslash} :> "\\" <> z;
$unescapeRules = ("\\" ~~ z:{$Newline, $DQuote, $Backslash}) :> z;

EscapeCharacters[e_]   := StringReplace[e, $escapeRules];
UnescapeCharacters[e_] := StringReplace[e, $unescapeRules];
EscapeDQuotes[e_]      := StringReplace[e, $escapeDQuoteRules];
EscapeSQuotes[e_]      := StringReplace[e, $escapeSQuoteRules];
EscapeNewlines[e_]     := StringReplace[e, $escapeSQuoteRules];

(**************************************************************************************************)

toStr1[s_Str] := s;
toStr1[e_]    := StrJoin @ toStr2 @ e

toStr2[s_Str]     := s;
toStr2[i_Int]     := IntStr @ i;
toStr2[Null]      := {};
toStr2[s_StrExpr] := Map[toStr2, s];
toStr2[l_List]    := Map[toStr2, l]
toStr2[e_]        := (Echo[e]; "\[FilledSquare]");

stringyQ[_Str | _Int | Null] := True;
stringyQ[l_List]             := VectorQ[l, stringyQ];
stringyQ[_]                  := False;

(**************************************************************************************************)

DeclareCurry1[FnBracketStringRow, FnParenStringRow]

FnBracketStringRow[f_ ? stringyQ, list_List ? stringyQ] := fnStr[f, "[", list, "]"];
FnParenStringRow[f_ ? stringyQ, list_List ? stringyQ]   := fnStr[f, "(", list, ")"];

FnBracketString[f_ ? stringyQ][args___ ? stringyQ]      := fnStr[f, "[", {args}, "]"]
FnParenString[f_ ? stringyQ][args___ ? stringyQ]        := fnStr[f, "(", {args}, ")"];

fnStr[f_, l_, args_, r_] := toStr1 @ {f, l, Riffle[args, ", "], r};

(**************************************************************************************************)

DelimitedString[l_Str, m_Str, r_Str][args___ ? stringyQ]      := delimStr[l, m, r, {args}];
DelimitedStringRow[l_Str, m_Str, r_Str][args_List ? stringyQ] := delimStr[l, m, r, args];

delimStr[l_, m_, r_, args_] := toStr1 @ {l, Riffle[args, m], r};

(**************************************************************************************************)

DefineSeqRowStringForms[fnSeq_, fnRow_, tuples__List] := MapApply[
  {seqSym, rowSym, l, m, r} |-> (
    rowSym[arg_ ? stringyQ]    := fnRow[l, m, r][arg];
    seqSym[args___ ? stringyQ] := fnSeq[l, m, r][args];
  ),
  {tuples}
];

DefineSeqRowStringForms[
  DelimitedString, DelimitedStringRow,
  {BraceString,    BraceStringRow,   "{", ",", "}"},
  {AngleString,    AngleStringRow,   "⟨", ",", "⟩"},
  {ParenString,    ParenStringRow,   "(", ",", ")"},
  {BracketString,  BracketStringRow, "[", ",", "]"}
];

