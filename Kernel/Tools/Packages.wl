SystemExports[
  "IOFunction",
    GetPackageDirectory,
  "Option",
    Caching, Logging, LogLevel, Verbose, PriorityRules, MacroRules
];

PackageExports[
  "SpecialFunction", PackageTopLevelEvaluate
  "SpecialVariable", $InputFileHash
];

(*************************************************************************************************)

SetHoldA[PackageTopLevelEvaluate]

With[{ignoredSyms = Apply[Alt, Blank /@ $SymbolExportFunctions]},
PackageTopLevelEvaluate[HoldC[ignoredSyms]] := Null;
PackageTopLevelEvaluate[HoldC[ignoredSyms;]] := Null;
PackageTopLevelEvaluate[hc_] := ReleaseHold @ ExpandMacros @ hc;
];

(**************************************************************************************************)

Options[GetPackageDirectory] = {
  Verbose -> False,
  PriorityRules -> {},
  MacroRules -> None
};

GetPackageDirectory[context_, dir_, OptionsPattern[]] := Locals[
  path = NormalizePath @ dir;
  $aliasBag = Bag[];
  $publicContext = context;
  $privateContext = context <> "Private`";
  codePreprocFn = ApplyEchoSugar /* insertInputFileHash;
  If[NonEmptyQ[macroRules = OptionValue[MacroRules]],
    codePreprocFn = codePreprocFn /* ReplaceRepeated[macroRules]];
  DisableHandleExceptions @ PreludeLoadPackage[
    context, path,
    "CodePreprocessor"    -> codePreprocFn,
    "SymbolTableFunction" -> SymbolTableFromDirectives,
    "PreLoadFunction"     -> applyAliases,
    "EvaluationFunction"  -> None,
    "ContextPath"         -> {$publicContext, $privateContext, "CoreTools`", "System`"},
    "Verbose"             -> OptionValue[Verbose],
    "PriorityRules"       -> OptionValue[PriorityRules]
  ]
];

insertInputFileHash[e_] /; VFreeQ[e, $InputFileHash] := e;
insertInputFileHash[e_] := ReplaceAll[expr, $InputFileHash :> RuleEval[$CurrentPackageFileHash]];

General::loadFailure = "Could not load file ``.";
General::loadMessage = "Message issued during package evaluation: ``.";

