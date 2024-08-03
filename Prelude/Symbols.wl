BeginPackage["Prelude`Symbols`"]

System`PackageExports[
"Function",
System`FindLikelySymbolName, System`FindSymbolNames,
System`RegisterDynamicAliasFunction,
System`CreateDynamicAlias,
System`FullSymbolName,
System`SymbolNameFirst,
System`SymbolNameLast,
System`SymbolNameMost,
System`SymbolNameMostLast,
System`SymbolNamesGrouped,

"Predicate",
System`SymbolNameHasDefinitionsQ,

System`UserSymbolQ,
System`InertSymbolQ,
System`SystemSymbolQ,
System`InertUserSymbolQ,
System`InertSystemSymbolQ,
System`FullSymbolName,

"Head",
System`AttachImmediateValue,
System`AttachDelayedValue,

"MutatingFunction",
System`UnprotectClearAll,
System`SymbolNameSet,
System`SymbolNameSetDelayed,

"SpecialFunction",
NewSymbolHandler
];

(*************************************************************************************************)

Begin["`Private`"]

(*************************************************************************************************)

SetAttributes[UnprotectClearAll, {HoldAllComplete}];
UnprotectClearAll[e___] := (Unprotect[e]; ClearAll[e]);

(*************************************************************************************************)

declareHeldPred[syms___Symbol] := (
  SetAttributes[{syms}, HoldAllComplete];
  Scan[sym |-> Set[sym[_], False], {syms}]
);

declareHeldPred[
  System`UserSymbolQ,
  System`InertSymbolQ,
  System`SystemSymbolQ,
  System`InertUserSymbolQ,
  System`InertSystemSymbolQ
];

UserSymbolQ[s_Symbol ? Developer`HoldAtomQ]        := Context[s] =!= "System`";
InertSymbolQ[s_Symbol ? Developer`HoldAtomQ]       := System`Private`HasNoEvaluationsQ[s];
SystemSymbolQ[s_Symbol ? Developer`HoldAtomQ]      := Context[s] === "System`";
InertUserSymbolQ[s_Symbol ? Developer`HoldAtomQ]   := System`Private`HasNoEvaluationsQ[s] && Context[s] =!= "System`";
InertSystemSymbolQ[s_Symbol ? Developer`HoldAtomQ] := System`Private`HasNoEvaluationsQ[s] && Context[s] === "System`";

(*************************************************************************************************)

SetAttributes[FullSymbolName, HoldAllComplete];

FullSymbolName[sym_Symbol] := StringJoin[Internal`SymbolContext[Unevaluated @ sym], SymbolName @ Unevaluated @ sym];
FullSymbolName[list_List] := Map[FullSymbolName, Unevaluated @ list];
FullSymbolName[_] := $Failed;

SymbolNameHasDefinitionsQ[sym_String] := ToExpression[sym, InputForm, System`Private`HasAnyEvaluationsQ];

(*************************************************************************************************)

SetAttributes[{SymbolNameFirst, SymbolNameLast, SymbolNameMost, SymbolNameMostLast}, Listable];

SymbolNameFirst[str_String]    := If[StringFreeQ[str, "`"], None,        StringTake[str, First @ First @ StringPosition[str, "`"]]];
SymbolNameLast[str_String]     := If[StringFreeQ[str, "`"], str,         StringDrop[str, Max @ StringPosition[str, "`"]]];
SymbolNameMost[str_String]     := If[StringFreeQ[str, "`"], None,        StringTake[str, Max @ StringPosition[str, "`"]]];
SymbolNameMostLast[str_String] := If[StringFreeQ[str, "`"], {None, str}, StringTakeDrop[str, Max @ StringPosition[str, "`"]]];

(*************************************************************************************************)

SymbolNamesGrouped[list_List]  := Merge[Rule @@@ SymbolNameMostLast @ list, Identity];
SymbolNamesGrouped[str_String] := SymbolNamesGrouped @ Names @ str;

(*************************************************************************************************)

FindLikelySymbolName::noname = "No symbol matching name \"``\" in \"``\".";

FindLikelySymbolName[str_String] := If[
  StringFreeQ[str, "`"], FindLikelySymbolName["", str],
  FindLikelySymbolName @@ StringTakeDrop[str, Max @ StringPosition[str, "`"]]
];

FindLikelySymbolName[context_String, name_String] := Block[
  {names, words, pattern, NewSymbolHandler, $NewSymbol, foundName},
  names = FindSymbolNames[context, name, SymbolNameHasDefinitionsQ];
  names = SortBy[names, {StringCount[#, "`"], StringLength[#]}&];
  If[names =!= {},
    foundName = First @ names;
    If[$lastFoundCache[name] =!= foundName,
      $lastFoundCache[name] = foundName;
      Print["resolving \"", context, name, "\" to \"", foundName, "\"."];
    ];
    First @ names
  ,
    $Failed
  ]
];

(*************************************************************************************************)

FindSymbolNames::err = "Could not find contexts due to internal error.";

FindSymbolNames[str_String] := If[
  StringFreeQ[str, "`"], FindSymbolNames["", str],
  FindSymbolNames @@ StringTakeDrop[str, Max @ StringPosition[str, "`"]]
];

FindSymbolNames[context_String, name_String, filter_:None] := Block[
  {names, tryFind, nameGlob, glob, contextRE, $nameFilter = filter, $ic = False},
  contextRE = contextGlobToRegex[context];
  nameGlob = convertNameGlob[name];
  names = {};
  Which[
    !UpperCaseQ[name] && StringFreeQ[name, "*"] &&
      Length[names = iFindSymbolNames[contextRE, True,  name                    ]] == 1, Null,
    StringEndsQ[nameGlob, "*"] &&
      Length[names = iFindSymbolNames[contextRE, False, StringDrop[nameGlob, -1]]] >= 1, Null,
      Length[names = iFindSymbolNames[contextRE, False, nameGlob                ]] >= 1, Null,
      Length[names = iFindSymbolNames[contextRE, False, "*" <> nameGlob         ]] >= 1, Null,
    $ic = True;
      Length[names = iFindSymbolNames[contextRE, False, name                    ]] >= 1, Null,
      Length[names = iFindSymbolNames[contextRE, False, "*" <> name             ]] >= 1, Null,
      True, Null
  ];
  names
];

FindSymbolNames[context_String, name_String, "Maybe"[filter_]] := Block[{res},
  res = FindSymbolNames[context, name, filter];
  If[res =!= {}, res, FindSymbolNames[context, name, None]]
];

contextGlobToRegex[""] := RegularExpression["\\w"];

contextGlobToRegex[glob_String] := Block[
  {res, split},
  split = StringSplit[glob, "`"];
  If[split === {}, Return @ ".*"];
  split = Prepend[firstGlobElemToRegex @ First @ split] @ Map[globElemToRegex, Rest @ split];
  res = StringRiffle[split, "`(?:\\w+`)*"];
  RegularExpression @ If[StringEndsQ[res, "`"], res, res <> "`"]
];

$sysRegex = StringJoin["(?:",
"System|Developer|Language|Internal|PatternConvert|StartUp|StringPattern|Association|Compile|Compiler|",
"CompiledLibrary|ImportExport|LibraryLink|ExternalEvaluate|Data|Documentation|FE|JSONTools|PacletTools|",
"PacletManager|NumericArray|NumericArrayUtilities|NotebookTools|XML|XMLLink|SystemTools|SparseArray|",
"GeometricFunctions|FEPrivate|LocalObjects|GraphComputation|CCompilerDriver|ArchiveTools|DatabaseLink|",
"EntityFramework|Experimental|LinearAlgebra|Java|JLink|Image|GeneralUtilities|Format|Method|",
"ListableFunctionsLibrary|SearchResult|RuntimeTools|StringUtilitiesDump|Documentation|DocumentationSearch|",
"DocumentationSearcher|DateAndTime|Package|Information)"
];

firstGlobElemToRegex["qg" | "mt"] := "(?:MathTools|MTLoader)";
firstGlobElemToRegex["sys"]       := $sysRegex;
firstGlobElemToRegex[s_]          := globElemToRegex[s];

globElemToRegex[s_String] /; LowerCaseQ[s] := globElemToRegex @ ToUpperCase[s];
globElemToRegex[s_String] := StringReplace[s, {"Z" -> "\\w*", chunk:$camelChunk :> chunk <> "\\w*"}];

$camelChunk = RegularExpression["[[:upper:]][[:lower:]]*"];

convertNameGlob[glob_] := Which[
  StringContainsQ[glob, ("z" | "Z")],
    StringReplace[glob, ("z" | "Z") -> "*"],
  LowerCaseQ[glob],
    glob,
  UpperCaseQ[glob],
    StringReplace[glob, l:LetterCharacter :> l <> "@"],
  True,
    StringReplace[glob, chunk:$camelChunk :> chunk <> "*"]
];

iFindSymbolNames[contextPatt_, isExact_, symbolGlob_String] := Block[
  {candidates, symbolNames, pureNames},
  candidates = Block[{$ContextPath = {}}, Names["**`" <> symbolGlob, IgnoreCase -> $ic]];
  candidates = Select[candidates, !StringStartsQ[#, $DynamicAliasContexts]&];
  filter = StringStartsQ[candidates, contextPatt];
  candidates = Pick[candidates, filter];
  pureNames = Part[StringSplit[candidates, "`"], All, -1];
  If[isExact, Return @ Pick[candidates, pureNames, symbolGlob]];
  cands = Pick[candidates, StringMatchQ[pureNames, symbolGlob, IgnoreCase -> $ic]];
  If[$nameFilter =!= None, cands = Select[cands, $nameFilter]];
  SortBy[cands, StringLength]
];

(*************************************************************************************************)

SymbolNameSet[name_String, value_] :=
  ToExpression[name, InputForm, Function[{sym}, Set[sym, value], HoldAllComplete]];

SetAttributes[SymbolNameSetDelayed, {HoldRest}];
SymbolNameSetDelayed[name_String, value_] :=
  ToExpression[name, InputForm, Function[{sym}, SetDelayed[sym, value], HoldAllComplete]];

(*************************************************************************************************)

$DynamicAliasTable = Data`UnorderedAssociation[
  "d`"  -> Function[CreateDynamicAlias[#1, findBestSymbolHandler[#2, #3, On]]],
  "doff`" -> Function[CreateDynamicAlias[#1, findBestSymbolHandler[#2, #3, Off]]],
  "f`"  -> Function[CreateDynamicAlias[#1, findBestSymbolHandler[#2, #3]]],
  "l`"  -> Function[SymbolNameSetDelayed[#1, FindSymbolNames[#2, #3]]]
];

$DynamicAliasContexts = Keys @ $DynamicAliasTable;

findBestSymbolHandler[context_, name_] := attachName @ FindLikelySymbolName[context, name];
findBestSymbolHandler[context_, name_, fn_] := attachName[FindLikelySymbolName[context, name], fn];

attachName[name_String, fn_] := Replace[
  attachName[name],
  AttachDelayedValue[sym_] :> AttachDelayedValue[fn[sym]; sym]
];

attachName[name_String] := ToExpression[name, InputForm, AttachDelayedValue];
_attachName := (Beep[]; Fail);


(*************************************************************************************************)

refreshRequiresHandlingQ[] := (
  $requiresHandlingQ = StringStartsQ[Keys @ $DynamicAliasTable];
);

refreshRequiresHandlingQ[];

NewSymbolHandler[aliasName_String, aliasContext_String ? $requiresHandlingQ] /; ($Context === "Global`") := Block[
  {$NewSymbol}, With[{pair = StringSplit[aliasContext, "`", 2]}, {base = First @ pair, context = Last @ pair},
  $DynamicAliasTable[base <> "`"][aliasContext <> aliasName, context, aliasName]
]];

(*************************************************************************************************)

RegisterDynamicAliasFunction::badUsage = "`` is not a valid usage.";

r_RegisterDynamicAliasFunction := (Message[RegisterDynamicAliasFunction::badUsage, HoldForm[r]]; $Failed);

RegisterDynamicAliasFunction[prefix_String, fn_] /; StringEndsQ[prefix, "`"] := (
  $DynamicAliasTable[prefix] = fn;
  refreshRequiresHandlingQ[];
);

(*************************************************************************************************)

SetAttributes[CreateDynamicAlias, HoldRest];

CreateDynamicAlias::badUsage = "First argument should be the alias, second should be an expression.";
_CreateDynamicAlias := (Message[CreateDynamicAlias::badUsage]; $Failed);

CreateDynamicAlias[alias_String, expr_] := Block[{$NewSymbol},
  evaluateDynamicAlias[
    CreateDynamicAlias[alias, expr],
    Clear[alias]; ToExpression[alias, InputForm, HoldComplete],
    expr
  ]
];

SetAttributes[AttachDelayedValue, HoldAllComplete];
SetAttributes[evaluateDynamicAlias, HoldFirst];

evaluateDynamicAlias[_, HoldComplete[alias_], AttachImmediateValue[value_]] :=
  Set[alias, value];

evaluateDynamicAlias[_, HoldComplete[alias_], AttachDelayedValue[value_]] := (
  SetDelayed[alias, value];
  value
);

CreateDynamicAlias::badResult = "`` did not produce an AttachImmediateValue, AttachDelayedValue, or Fail: ``.";
evaluateDynamicAlias[recurse_, alias_, result_] := (
  Message[CreateDynamicAlias::badResult, HoldForm @ recurse, HoldForm @ result];
  $Failed
);

evaluateDynamicAlias[recurse_, HoldComplete[alias_], Fail] := (
  alias := RuleCondition @ recurse;
  Fail
);

(*************************************************************************************************)

End[]

EndPackage[]
