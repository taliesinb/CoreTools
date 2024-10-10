PackageExports[
  "Function",          BrowseSymbols, FindFunctions, FindInertSymbols, FindDownSymbols, FindSubSymbols, FindUpSymbols, FindOwnSymbols, FindFormattingSymbols,
  "Function",          FindDefinitionsContaining, FindSymbolsContaining, FindUnresolvedSymbols, FindFormatDefinitions,
  "Predicate",         UnresolvedSymbolQ, UnresolvedNameQ,
  "DebuggingFunction", PrintDefinitions, PrintDefinitionsLocal, PrintDefinitionsContaining, PrintFormatDefinitions, PrintStack
];

PrivateExports["CacheVariable", $PrintDefNotebooks];

(**************************************************************************************************)

(* TODO: Investigate Language`$InternalContexts *)

Initially[
  $PrintDefNotebooks = UDict[];
];

BrowseSymbols[glob_Str] := Column[SymbolNameForm /@ NamePaths[glob]];

(**************************************************************************************************)

SetHoldC @ getFmtTarget;

getFmtTarget[VPatternTest[a_, _]] := getFmtTarget @ a;
getFmtTarget[VCondition[a_, _]]   := getFmtTarget @ a;
getFmtTarget[VHoldP[p_]]          := getFmtTarget @ p;
getFmtTarget[(Typeset`MakeBoxes | MakeBoxes)[p_, ___]] := SymbolForm @@ PatternHead[p];
getFmtTarget[_] := $Failed;

(**************************************************************************************************)

SetHoldC[UnresolvedSymbolQ, UnresolvedNameQ];

$unresolvedSymRegex = Regex["[$i]*[A-Z]"];

(* this is cheap, purely string-based *)
UnresolvedNameQ[name_Str] := StringStartsQ[NameLast @ name, $unresolvedSymRegex] || StringEndsQ[name, {"Q", "P"}];

UnresolvedSymbolQ[sym_Sym ? HasNoDefsQ] := And[UnresolvedNameQ @ SymName @ sym, Not @ HasFormatDefsQ @ sym];
UnresolvedSymbolQ[_Sym]                 := False;

(**************************************************************************************************)

SetHoldC @ fromUnresName;

fromUnresName[sym_Sym ? HasAnyDefsQ] := Nothing;
fromUnresName[sym_Sym ? HasFormatDefsQ] := Nothing;
fromUnresName[sym_Sym] /; $blacklistDict[SymName[sym]] := Nothing;
fromUnresName[sym_Sym] := SymbolForm @ sym;

internalNameQ[name_Str] := StringContainsQ[name, "`"];

$noDefKinds = StrSplit[
  "Option TagSymbol TagHead TagVariable SlotVariable TransientVariable SymbolicHead DataHead HoldHead TypeSymbol Symbol BoxOption TypeHead PackageFunction PackageDeclaration"
];

FindUnresolvedSymbols[context_Str:None] := Locals[
  If[StringQ[context],
    If[!StringEndsQ[context, "`"], ReturnFailed[]];
    glob1 = context <> "*";
    glob2 = context <> "*`*";
    names = Join[Names @ glob1, Names @ glob2];
    kinds = IfFailed[PackageSymbolKinds[context], {}];
  ,
    names = Names[{"CoreTools`*", "CoreTools`*`*", "Prelude`*", "Prelude`*`*"}];
    kinds = CoreToolsSymbolKinds[];
  ];
  blacklist = Catenate @ Lookup[kinds, $noDefKinds, {}];
  $blacklistDict = TrueDict @ blacklist;
  candidates = Select[names, UnresolvedNameQ];
  FromInputStr[candidates, fromUnresName]
];

(* internalUnresolvedQ[name_] := And[internalNameQ[name], UnresolvedNameQ[name]] || StringStartsQ[name, $unresolvedSymRegex]; *)

(**************************************************************************************************)

SetStrict[FindFunctions, FindInertSymbols, FindDownSymbols, FindSubSymbols, FindUpSymbols, FindOwnSymbols, FindFormattingSymbols]

defineFindFn[fn_, pred_] := fn[glob_String] := FromInputStr[Names @ glob, toFinderFn @ pred];

toFinderFn[pred_] := toFinderFn[pred] = Fn[sym, If[pred[sym], SymbolForm @ sym, Nothing], HoldAllComplete];

defineFindFn @@@ {
  FindFunctions         -> HasOwnDefsQ,
  FindInertSymbols      -> HasNoDefsQ,
  FindDownSymbols       -> HasDownDefsQ,
  FindSubSymbols        -> HasSubDefsQ,
  FindUpSymbols         -> HasUpDefsQ,
  FindOwnSymbols        -> HasOwnDefsQ,
  FindFormattingSymbols -> HasFormatDefsQ
};

(**************************************************************************************************)

FindFormatDefinitions[sym_Symbol] :=
  DeleteDuplicates @ Select[$BoxFormattingRules, ContainsQ[sym]];

PrintFormatDefinitions[sym_Symbol] := Module[
  {defs = FindFormatDefinitions[sym]},
  If[defs === {}, None, PrintDefinitions @ defs]
];

(**************************************************************************************************)

FindSymbolsContaining[context_, pattern_] := Locals[
  symbols = Names[If[context === "System`", "System`*", {context <> "*", context <> "**`*"}]];
  symbols = DeleteCases[symbols, "In" | "Out"];
  active = FromInputStr[symbols, HasAnyDefsQ];
  symbols = Pick[symbols, active, True];
  $patt = pattern;
  Quiet @ FromInputStr[symbols, filterDefContainingPattQ]
]

SetHoldC[filterDefContainingPattQ];

filterDefContainingPattQ[s_] := If[FreeQ[{DownValues[s], UpValues[s], OwnValues[s], SubValues[s]}, $patt], Nothing, s]

(**************************************************************************************************)

FindDefinitionsContaining[context_, pattern_] := Locals[
  definitions = GetPackageSymbol["GeneralUtilities`Definitions"];
  res = Null;
  symbols = Names[If[context === "System`", "System`*", {context <> "*", context <> "**`*"}]];
  active = FromInputStr[symbols, HasAnyDefsQ];
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

$pdBG = "GeneralUtilities`Debugging`PackagePrivate`$PrintDefinitionsBackground";

PrintDefinitionsLocal[sym_ ? System`Private`PrintDefinitionsHook] := Null;
PrintDefinitionsLocal[args___] := Block[
  {pdfl = GetPackageSymbol["GeneralUtilities`PrintDefinitionsLocal"], color = Symbol[$pdBG]},
  DeleteNextGeneratedCells[];
  SymbolNameSet[$pdBG, If[DarkModeQ[], $darkDefColor, $lightDefColor]];
  pdfl[args];
  SymbolNameSet[$pdBG, color];
];

$lightDefColor = RGBColor[0.98, 0.945, 0.97];
$darkDefColor := $darkModeDefColor = Blend[{RGBColor[0.196, 0.220, 0.251], $DarkPurple},.1];

(**************************************************************************************************)

PrintStack[] := GetPackageSymbol["GeneralUtilities`PrintStack"][];
