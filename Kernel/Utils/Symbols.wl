SystemExports[
  "Function",   FormalSymbol, FromFormalSymbol,
  "FormHead",   SymbolNameForm, SymbolForm,
  "Function",   SymbolNameUsage, SymbolNameType, SymbolType, SymbolTypeFast, SymbolBaseContext
];

PackageExports[
  "BoxFn",      SymbolNameBox, SymbolTypeStyle,
  "TypeSymbol", FunctionSymbol, OperatorSymbol, ImmediateSymbol, DelayedSymbol, FormattingSymbol, InertSymbol, PatternSymbol, DataSymbol, UnknownSymbol
  "Predicate",  HasFormatDefsQ, HasBoxDefsQ, HasFormatRulesQ,
  "TypeHead",   KernelSymbol
];

SessionExports[
  "CacheVariable", $SymbolTypeCache, $SymbolTypeFastCache, $HasBoxDefsCache, $BoxFormattingSymbols, $BoxFormattingRules
];

(**************************************************************************************************)

SetInitial[$SymbolTypeCache, UDict[
    Thread @ Rule[Hold /@ {Pattern, PatternTest, Condition, Blank, BlankSequence, BlankNullSequence, Repeated, Verbatim}, PatternSymbol],
    Thread @ Rule[Hold /@ {Dict, UDict, USet, OSet, MSet, Bag}, DataSymbol]
]];

SetInitial[$HasBoxDefsCache, UDict[]];

SetInitial[$SymbolTypeFastCache, $SymbolTypeCache];

(**************************************************************************************************)

SymbolBaseContext[sym_Sym | Hold[sym_Sym]] := baseContext @ Context @ NoEval @ sym;

baseContext[context_Str] := baseContext[context] = NameFirst @ context;

(**************************************************************************************************)

SymbolNameUsage[name_Str ? NameQ] := Replace[
  FromInputStr[name, HoldCompFn[sym, MessageName[sym, "usage"]]],
  _MessageName :> None
];

SymbolNameUsage[_] := None;

(**************************************************************************************************)

SetHoldC @ SymbolForm;

SetForm0[SymbolForm, SymbolNameForm];

SystemBox[SymbolForm[sym_Sym]]      := SymbolNameBox[SymPath @ sym, SymbolType @ sym];
SystemBox[SymbolNameForm[name_Str]] := SymbolNameBox[name, SymbolNameType @ name];

(**************************************************************************************************)

SetBoxFn @ SymbolNameBox;

SymbolNameBox[path_Str, type_] := Locals[
  style = Seq @@ FlatList @ SymbolTypeStyle @ type;
  {context, name} = NameMostLast @ path;
  If[context === None, context = Quiet @ Check[At[Context, path], "???"]];
  If[NameQ @ path,
    nameBox = If[MemberQ[$ContextPath, context], name, "`" <> name];
    boxes = CodeStyleBox[nameBox, style];
    boxes //= If[type === InertSymbol,
      ClickBoxOp[Null,                    CopyTextToClipboard @ path],
      ClickBoxOp[PrintDefinitions @ path, CopyTextToClipboard @ path]
    ];
    boxes //= NiceTooltipBox @ symbolTooltipBoxes[name, context, path];
  ,
    boxes = CodeStyleBox[path, $Red]
  ];
  boxes
];

(**************************************************************************************************)

SymbolTypeStyle = CaseOf[
  KernelSymbol[t_] := {$ @ t, Underlined};
  FunctionSymbol   := FontWeight -> "Bold";
  OperatorSymbol   := {FontColor -> $DarkBlue, FontWeight -> "SemiBold"};
  ImmediateSymbol  := FontColor -> $LightBlue;
  DelayedSymbol    := {FontWeight -> "Bold", FontColor -> $LightBlue};
  FormattingSymbol := $Orange;
  InertSymbol      := {$DarkGray, FontSlant -> Italic};
  PatternSymbol    := FontColor -> $LightTeal;
  DataSymbol       := {FontColor -> $LightPurple, FontWeight -> "SemiBold"};
  UnknownSymbol    := $DarkGray;
  $Failed          := Red;
];

(**************************************************************************************************)

symbolTooltipBoxes[name_Str, context_Str, path_Str] := Locals[
  usage = SymbolNameUsage @ path;
  attrs = Attributes @ path;
  ColumnBox[{
    CodeStyleBox @ ToBoxes @ name,
    CodeStyleBox @ ToBoxes @ context,
    If[NoneQ @ usage, Nothing, ToBoxes @ usage]
  }, Left, .1]
];

(**************************************************************************************************)

SetHoldC[SymbolType, SymbolTypeFast];

SymbolNameType[name_Str] := If[!NameQ[name], $Failed, FromInputString[name, SymbolType]];

SymbolType[s_Sym ? HAtomQ] := CachedTo[$SymbolTypeCache, Hold @ s, symbolType0 @ s];
SymbolType[e_]             := $Failed;

SymbolTypeFast[s_Sym ? HAtomQ] := Lookup[$SymbolTypeCache, Hold @ s, CachedTo[$SymbolTypeFastCache, Hold @ s, symbolTypeFast @ s]];
SymbolTypeFast[_]              := $Failed;

(**************************************************************************************************)

SetHoldC[symbolTypeFast, symbolType0, symbolType1, varType, aliasType]

symbolTypeFast[s_] := Block[{HasBoxDefsQ = HoldFalseFn}, symbolType0 @ s];

symbolType0 = CaseOf[
  $[s_ ? HasAnyCodesQ]   := KernelSymbol @ symbolType1 @ s;
  $[s_ ? HasAnyDefsQ]    := symbolType1 @ s;
  $[s_ ? HasFormatDefsQ] := FormattingSymbol;
  _                      := InertSymbol;
];

symbolType1[s_] := Which[
  HasSubDefsQ[s],         OperatorSymbol,
  HasIValueQ[s],          varType @ s,
  HasDValueQ[s],          DelayedSymbol,
  HasDownDefsQ[s],        FunctionSymbol,
  HasFormatDefsQ[s],      FormattingSymbol,
  True,                   UnknownSymbol
];

varType[s_] /; KeyExistsQ[$SymbolAliases, NoEval @ s] := ReleaseHold @ ReplaceAll[HoldComplete[aliasType @ s], $SymbolAliases];
varType[s_] := ImmediateSymbol;

aliasType[s_Sym ? HAtomQ] := SymbolType[s];
aliasType[_]      := ImmediateSymbol

(**************************************************************************************************)

SetHoldC @ SetPred1[HasFormatDefsQ, HasFormatRulesQ, HasBoxDefsQ]

HasFormatRulesQ[s_Sym ? HAtomQ] := Or[HasPrintCodeQ[s], And[HasNoCodesQ[s], NonEmptyQ @ FormatValues @ s]];

HasFormatDefsQ[s_Sym ? HAtomQ] := Or[HasPrintCodeQ[s], And[HasNoCodesQ[s], Or[NonEmptyQ @ FormatValues @ s, HasBoxDefsQ @ s]]];

HasBoxDefsQ[s_Sym ? HAtomQ]    := CachedTo[$HasBoxDefsCache, HoldC @ s, iHasBoxDefsQ @ s];

(**************************************************************************************************)

SetHoldC @ iHasBoxDefsQ;

iHasBoxDefsQ[s_] := VContainsQ[$cachedBoxFormattingSymbols, NoEval @ s];

$cachedBoxFormattingSymbols := $cachedBoxFormattingSymbols = $BoxFormattingSymbols;

$BoxFormattingSymbols := Union @ DelCases[$Failed] @ Map[getFmtTarget] @ Keys @ $BoxFormattingRules;

$BoxFormattingRules   := Join[FormatValues @ MakeBoxes, DownValues @ Typeset`MakeBoxes, DownValues @ MakeCBox];

(**************************************************************************************************)

$lowerFormal = Take[$FormalSymbols, 26];
$upperFormal = Drop[$FormalSymbols, 26];

SetListable[FormalSymbol];

FormalSymbol::notFormalSpec = "`` should be a integer or string containing a roman letter.";
FormalSymbol[e_] := ErrorMsg["notFormalSpec", e];
FormalSymbol[s_Str ? CharQ] := codeToFormal @ First @ ToCharCode @ s;
FormalSymbol[n_Int /; 1 <= n <= 52] := Part[$FormalSymbols, n];

codeToFormal[n_] := Which[
  65 <= n <= 90,  Part[$upperFormal, n - 64],
  97 <= n <= 122, Part[$lowerFormal, n - 96],
  True, ErrorMsg["notFormalSpec", FromCharCode @ n]
];

(*************************************************************************************************)

SetListable[FromFormalSymbol];

$fromFormalSymbols := $fromFormalSymbols = UDictThread[
  $FormalSymbols, Join[CharRange["a", "z"], CharRange["A", "Z"]]
];

FromFormalSymbol[sym_Sym] := Lookup[$fromFormalSymbols, sym, sym];
