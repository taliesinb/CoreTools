PackageExports[
  "Function",
    BrowseSymbols,
    FindFunctions, FindInertSymbols, FindDownSymbols, FindSubSymbols, FindUpSymbols, FindOwnSymbols, FindFormattingSymbols,
    FindDefinitionsContaining, FindSymbolsContaining, FindUnresolvedSymbols,
    FindFormatDefinitions,
    SymbolNameUsage,
  "Predicate",         HasFormatDefsQ, HasBoxDefsQ, UnresolvedSymbolQ,
  "FormHead",          SymbolNameForm, SymbolForm,
  "DebuggingFunction", PrintDefinitions, PrintDefinitionsLocal, PrintDefinitionsContaining, PrintFormatDefinitions, PrintStack,
  "Function",          SymbolNameType, SymbolType, SymbolTypeToPredicate,
  "TypeHead",          KernelSymbol,
  "TypeSymbol",        FunctionSymbol, OperatorSymbol, ImmediateSymbol, DelayedSymbol, FormattingSymbol, InertSymbol, PatternSymbol, DataSymbol, UnknownSymbol
];

PrivateExports[
  "CacheVariable",
    $HasBoxDefsCache, $SymbolTypeCache, $BoxFormattingRules, $BoxFormattingSymbols, $PrintDefNotebooks
];

(**************************************************************************************************)

Initially[
  $HasBoxDefsCache = UDict[];
  $SymbolTypeCache = UDict[
    Thread @ Rule[Hold /@ {Pattern, PatternTest, Condition, Blank, BlankSequence, BlankNullSequence, Repeated, Verbatim}, PatternSymbol],
    Thread @ Rule[Hold /@ {Dict, UDict}, DataSymbol]
  ];
  $PrintDefNotebooks = UDict[];
];

BrowseSymbols[glob_Str] := Column[SymbolNameForm /@ NamePaths[glob]];

(**************************************************************************************************)

SetHoldC @ SymbolForm;

SystemBoxes[SymbolForm[sym_Sym]]      := symbolNameBoxes[SymPath @ sym, SymbolType @ sym];
SystemBoxes[SymbolNameForm[name_Str]] := symbolNameBoxes[name, SymbolNameType @ name];

(**************************************************************************************************)

symbolNameBoxes[path_Str, type_] := Locals[
  style = Seq @@ FlatList @ symbolTypeToStyle @ type;
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

symbolTypeToStyle = CaseOf[
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

SymbolNameUsage[name_Str ? NameQ] := Replace[
  FromInputString[name, HoldCompFn[sym, MessageName[sym, "usage"]]],
  _MessageName :> None
];

SymbolNameUsage[_] := None;

(**************************************************************************************************)

SetPred1[HasFormatDefsQ, HasBoxDefsQ]

SetHoldC[HasFormatDefsQ, HasBoxDefsQ, iHasBoxDefsQ];

HasFormatDefsQ[s_Sym ? HoldAtomQ] := Or[HasPrintCodeQ[s], And[HasNoCodesQ[s], Or[NonEmptyQ @ FormatValues @ s, HasBoxDefsQ @ s]]];

HasBoxDefsQ[s_Sym ? HoldAtomQ] := CachedTo[$HasBoxDefsCache, HoldComp @ s, iHasBoxDefsQ @ s];

iHasBoxDefsQ[s_] := VContainsQ[$cachedBoxFormattingSymbols, NoEval @ s];

$cachedBoxFormattingSymbols := $cachedBoxFormattingSymbols = $BoxFormattingSymbols;

$BoxFormattingSymbols := Union @ DelCases[$Failed] @ Map[getFmtTarget] @ Keys @ $BoxFormattingRules;
$BoxFormattingRules   := Join[FormatValues @ MakeBoxes, DownValues @ Typeset`MakeBoxes];

(**************************************************************************************************)

SetHoldC @ getFmtTarget;

getFmtTarget[VPatternTest[a_, _]] := getFmtTarget @ a;
getFmtTarget[VCondition[a_, _]]   := getFmtTarget @ a;
getFmtTarget[VHoldP[p_]]          := getFmtTarget @ p;
getFmtTarget[(Typeset`MakeBoxes | MakeBoxes)[p_, ___]] := SymbolForm @@ PatternHeadSymbol[p];
getFmtTarget[_] := $Failed;

(**************************************************************************************************)

SetHoldC[SymbolType, symbolType0, symbolType1, varType, aliasType]

SymbolType[s_Sym ? HoldAtomQ] := CachedTo[$SymbolTypeCache, Hold @ s, symbolType0 @ s];
SymbolType[e_]                := $Failed;

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

aliasType[s_Sym ? HoldAtomQ] := SymbolType[s];
aliasType[_]      := ImmediateSymbol

(**************************************************************************************************)

SetHoldC @ UnresolvedSymbolQ;

$unresolvedSymRegex = Regex["[$i]*[A-Z]"];
nameNeedsDefQ[name_Str] := StringStartsQ[NameLast @ name, $unresolvedSymRegex] || StringEndsQ[name, {"Q", "P"}];

UnresolvedSymbolQ[sym_Sym ? HasNoDefsQ] := And[nameNeedsDefQ @ SymName @ sym, Not @ HasFormatDefsQ @ sym];
UnresolvedSymbolQ[_Sym]                 := False;

(**************************************************************************************************)

SetHoldC @ fromUnresName;

fromUnresName[sym_Sym ? HasAnyDefsQ] := Nothing;
fromUnresName[sym_Sym ? HasFormatDefsQ] := Nothing;
fromUnresName[sym_Sym] := SymbolForm @ sym;

internalNameQ[name_Str] := StringContainsQ[name, "`"];

FindUnresolvedSymbols[context_Str] := Locals[
  If[!StringEndsQ[context, "`"], ReturnFailed[]];
  glob1 = context <> "*";
  glob2 = context <> "*`*";
  names = Join[Names @ glob1, Names @ glob2];
  candidates = Select[names, internalNameQ[#] && nameNeedsDefQ[#]&];
  FromInputString[candidates, fromUnresName]
];

(**************************************************************************************************)

SymbolNameType[name_Str] := If[!NameQ[name], $Failed, FromInputString[name, SymbolType]];

(**************************************************************************************************)

(* SymbolTypeToPredicate = CaseOf[
  OperatorSymbol   := HasSubDefsQ;
  ImmediateSymbol  := HasIValueQ;
  DelayedSymbol    := HasDValueQ;
  FunctionSymbol   := HasOwnDefsQ;
  FormattingSymbol := HasPrintDefs;
  OperatorSymbol   := HasSubDefsQ;
  InertSymbol      := HasNoDefsQ;
];
 *)
(**************************************************************************************************)

toFinderFn[pred_] := toFinderFn[pred] = Fn[sym, If[pred[sym], SymbolForm @ sym, Nothing], HoldAllComplete];

(* FindSymbols[glob_String, pred_Symbol] := ToExpression[Names @ glob, InputForm, HoldCompFn[name, ]; *)

(**************************************************************************************************)

SetStrict[FindFunctions, FindInertSymbols, FindDownSymbols, FindSubSymbols, FindUpSymbols, FindOwnSymbols, FindFormattingSymbols]

defineFindFn[fn_, pred_] := fn[glob_String] := FromInputString[Names @ glob, toFinderFn @ pred];

defineFindFn @@@ {
  FindFunctions         -> HasOwnDefsQ,
  FindInertSymbols      -> HasNoDefsQ,
  FindDownSymbols       -> HasDownDefsQ,
  FindSubSymbols        -> HasSubDefsQ,
  FindUpSymbols         -> HasUpDefsQ,
  FindOwnSymbols        -> HasOwnDefsQ,
  FindFormattingSymbols -> HasFormatDefsQ
};

(* toOperatorFormSymbol[s_Symbol]         := Which[
  HasOwnDefsQ[s] || HasNoDefsQ[s], Nothing,
  HasSubDefsQ[s] && HasDownDefsQ[s] && hasSubUsageQ[s], s,
  True, Nothing
];

hasSubUsageQ[s_] := !StringQ[MessageName[s, "usage"]] || StringContainsQ[MessageName[s, "usage"], " operator "];
 *)
(**************************************************************************************************)

FindFormatDefinitions[sym_Symbol] :=
  DeleteDuplicates @ Select[$BoxFormattingRules, ContainsQ[sym]];

PrintFormatDefinitions[sym_Symbol] := Module[
  {defs = FindFormatDefinitions[sym]},
  If[defs === {}, None, PrintDefinitions @ defs]
];

(**************************************************************************************************)

DeclareHoldAllComplete[filterDefContainingPattQ];

FindSymbolsContaining[context_, pattern_] := Locals[
  symbols = Names[If[context === "System`", "System`*", {context <> "*", context <> "**`*"}]];
  symbols = DeleteCases[symbols, "In" | "Out"];
  active = FromInputString[symbols, HasAnyDefsQ];
  symbols = Pick[symbols, active, True];
  $patt = pattern;
  Quiet @ FromInputString[symbols, filterDefContainingPattQ]
]

filterDefContainingPattQ[s_] := If[FreeQ[{DownValues[s], UpValues[s], OwnValues[s], SubValues[s]}, $patt], Nothing, s]

(**************************************************************************************************)

DeclareHoldAllComplete[toActiveSymbol];

FindDefinitionsContaining[context_, pattern_] := Locals[
  definitions = GetPackageSymbol["GeneralUtilities`Definitions"];
  res = Null;
  symbols = Names[If[context === "System`", "System`*", {context <> "*", context <> "**`*"}]];
  active = FromInputString[symbols, HasAnyDefsQ];
  symbols = Pick[symbols, active, True];
  Flatten @ Map[definitionsContaining[pattern], symbols]
]

definitionsContaining[pattern_][symbol_] := (
  If[FreeQ[res = definitions[symbol], pattern], Nothing, Select[res, ContainsQ[pattern]]]
);

(**************************************************************************************************)

PrintDefinitionsContaining[context_, pattern_] := Module[
  {defs = FindDefinitionsContaining[context, pattern]},
  If[defs === {}, None, PrintDefinitions[defs];]
];

SetHoldC @ System`Private`PrintDefinitionsHook;

PrintDefinitions[sym_ ? System`Private`PrintDefinitionsHook] := Null;
PrintDefinitions[args___] := Module[{currentNb = EvaluationNotebook[], hold = Hold[args], result},
  Quiet @ NotebookClose @ Lookup[$PrintDefNotebooks, hold, None];
  result = GetPackageSymbol["GeneralUtilities`PrintDefinitions"][args];
  If[Head[result] === NotebookObject,
    (* SetSelectedNotebook[currentNb]; *)
    $PrintDefNotebooks[hold] = result;
  ,
    $Failed
  ]
];

PrintDefinitionsLocal[sym_ ? System`Private`PrintDefinitionsHook] := Null;
PrintDefinitionsLocal[args___] := Block[{
  GeneralUtilities`Debugging`PackagePrivate`$PrintDefinitionsBackground = If[DarkModeQ[], $darkDefColor, $lightDefColor]},
  DeleteNextGeneratedCells[];
  GetPackageSymbol["GeneralUtilities`PrintDefinitionsLocal"][args]
];

$lightDefColor = RGBColor[0.98, 0.945, 0.97];
$darkDefColor := $darkModeDefColor = Blend[{RGBColor[0.196, 0.220, 0.251], $DarkPurple},.1];

(**************************************************************************************************)

PrintStack[] := GetPackageSymbol["GeneralUtilities`PrintStack"][];
