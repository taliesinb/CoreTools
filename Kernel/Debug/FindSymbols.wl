SystemExports[
  "Function",
    FindFunctionSymbols, FindInertSymbols, FindDownEvaluationSymbols, FindSubEvaluationSymbols, FindUpEvaluationSymbols, FindOwnEvaluationSymbols, FindOperatorSymbols,
    FindDefinitionsContaining, FindSymbolsContaining,
    FindUnresolvedSymbols
];

PackageExports[
  "DebuggingFunction", PrintDefinitionsContaining, PrintDefinitions, PrintStack
];

(**************************************************************************************************)

DeclareHoldAllComplete[toFunctionSymbol, toInertSymbol, toDownEvaluationSymbol, toSubEvaluationSymbol, toUpEvaluationSymbol, toOwnEvaluationSymbol, toOperatorFormSymbol, hasSubUsageQ];

FindFunctionSymbols[glob_String]       := ToExpression[Names @ glob, InputForm, toFunctionSymbol];
FindInertSymbols[glob_String]          := ToExpression[Names @ glob, InputForm, toInertSymbol];
FindDownEvaluationSymbols[glob_String] := ToExpression[Names @ glob, InputForm, toDownEvaluationSymbol];
FindSubEvaluationSymbols[glob_String]  := ToExpression[Names @ glob, InputForm, toSubEvaluationSymbol];
FindUpEvaluationSymbols[glob_String]   := ToExpression[Names @ glob, InputForm, toUpEvaluationSymbol];
FindOwnEvaluationSymbols[glob_String]  := ToExpression[Names @ glob, InputForm, toOwnEvaluationSymbol];
FindOperatorSymbols[glob_String]       := ToExpression[Names @ glob, InputForm, toOperatorFormSymbol];

toFunctionSymbol[s_Symbol]             := If[System`Private`HasOwnEvaluationsQ[s] || !System`Private`HasDownEvaluationsQ[s], Nothing, s];
toInertSymbol[s_Symbol]                := If[System`Private`HasAnyEvaluationsQ[s],  Nothing, s];
toDownEvaluationSymbol[s_Symbol]       := If[System`Private`HasDownEvaluationsQ[s], Hold @ s, Nothing];
toSubEvaluationSymbol[s_Symbol]        := If[System`Private`HasSubEvaluationsQ[s],  Hold @ s, Nothing];
toUpEvaluationSymbol[s_Symbol]         := If[System`Private`HasUpEvaluationsQ[s],   Hold @ s, Nothing];
toOwnEvaluationSymbol[s_Symbol]        := If[System`Private`HasOwnEvaluationsQ[s],  Hold @ s, Nothing];
toOperatorFormSymbol[s_Symbol]         := Which[
  System`Private`HasOwnEvaluationsQ[s] || System`Private`HasNoEvaluationsQ[s], Nothing,
  System`Private`HasSubEvaluationsQ[s] && System`Private`HasDownEvaluationsQ[s] && hasSubUsageQ[s], s,
  True, Nothing
];

hasSubUsageQ[s_] := !StringQ[MessageName[s, "usage"]] || StringContainsQ[MessageName[s, "usage"], " operator "];

(**************************************************************************************************)

FindUnresolvedSymbols[context_String] := Locals[
  capNames = Names @ StrJoin[context, "Private`*`*"];
  capNames = Pick[capNames, UpperCase1Q @ SymbolNameLast @ capNames];
  ToExpression[capNames, InputForm, toInertSymbol]
];

(**************************************************************************************************)

DeclareHoldAllComplete[filterDefContainingPattQ];

FindSymbolsContaining[context_, pattern_] := Locals[
  symbols = Names[If[context === "System`", "System`*", {context <> "*", context <> "**`*"}]];
  symbols = DeleteCases[symbols, "In" | "Out"];
  active = ToExpression[symbols, InputForm, System`Private`HasAnyEvaluationsQ];
  symbols = Pick[symbols, active, True];
  $patt = pattern;
  Quiet @ ToExpression[symbols, InputForm, filterDefContainingPattQ]
]

filterDefContainingPattQ[s_] := If[FreeQ[{DownValues[s], UpValues[s], OwnValues[s], SubValues[s]}, $patt], Nothing, s]

(**************************************************************************************************)

DeclareHoldAllComplete[toActiveSymbol];

FindDefinitionsContaining[context_, pattern_] := Locals[
  definitions = GetPackageSymbol["GeneralUtilities`Definitions"];
  res = Null;
  symbols = Names[If[context === "System`", "System`*", {context <> "*", context <> "**`*"}]];
  active = ToExpression[symbols, InputForm, System`Private`HasAnyEvaluationsQ];
  symbols = Pick[symbols, active, True];
  Flatten @ Map[definitionsContaining[pattern], symbols]
]

definitionsContaining[pattern_][symbol_] := (
  If[FreeQ[res = definitions[symbol], pattern], Nothing, Select[res, ContainsQ[pattern]]]
);

(**************************************************************************************************)

PrintDefinitionsContaining[context_, pattern_] := Module[
  {defs = FindDefinitionsContaining[context, pattern]},
  If[defs === {}, None, PrintDefinitions @ defs]
];

PrintDefinitions[args___] := GetPackageSymbol["GeneralUtilities`PrintDefinitions"][args];

(**************************************************************************************************)

PrintStack[] := GetPackageSymbol["GeneralUtilities`PrintStack"][];
