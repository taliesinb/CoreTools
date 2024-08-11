SystemExports[
  "IOFunction",
    GetPackageDirectory,
  "OptionSymbol",
    Verbose, PriorityRules, MacroRules
];

PackageExports[
  "SpecialFunction",
    PackageTopLevelEvaluate
];

(*************************************************************************************************)

DeclareHoldAll[PackageTopLevelEvaluate]

With[{ignoredSyms = Apply[Alt, Blank /@ Prelude`Packages`$PackageDeclarationSymbols]},
PackageTopLevelEvaluate[HoldComplete[ignoredSyms]] := Null;
PackageTopLevelEvaluate[HoldComplete[ignoredSyms;]] := Null;
PackageTopLevelEvaluate[hc_] := ReleaseHold @ ExpandMacros @ hc;
];

(**************************************************************************************************)

Options[GetPackageDirectory] = {
  Verbose -> False,
  PriorityRules -> {},
  MacroRules -> None
};

PrivateFunction[MakeTContext, TypeHash, ContextHash]

GetPackageDirectory[context_, dir_, OptionsPattern[]] := Locals[
  path = NormalizePath @ dir;
  $aliasBag = Bag[];
  $publicContext = context;
  $privateContext = context <> "Private`";
  codePreprocFn = ApplyEchoSugar;
  If[NonEmptyQ[macroRules = OptionValue[MacroRules]],
    codePreprocFn = codePreprocFn /* ReplaceRepeated[macroRules]];
  $SessionCurrentEvaluationPrintCount = 0;
  DisableErrorHandler @ Prelude`Packages`LoadPackage[
    context, path,
    "CodePreprocessor"    -> codePreprocFn,
    "SymbolTableFunction" -> extractLineExportsFromFileList,
    "PreLoadFunction"     -> applyAliases,
    "EvaluationFunction"  -> PackageTopLevelEvaluate,
    "ContextPath"         -> {$publicContext, $privateContext, "CoreTools`", "System`"},
    "Verbose"             -> OptionValue[Verbose],
    "PriorityRules"       -> OptionValue[PriorityRules]
  ]
];

General::loadFailure = "Could not load file ``.";
General::loadMessage = "Message issued during package evaluation: ``.";

(**************************************************************************************************)

extractLineExportsFromFileList[_, files_] :=
  Catenate @ Map[extractLineExportsFromFile, files];

$directivePrefixes = {"Export", "Private"};
extractLineExportsFromFile[file_] := Locals[
  $path = file;
  lines = FindList[file, $directivePrefixes, AnchoredSearch -> True];
  lines //= Select[StringStartsQ[$directivePrefixes ~~ Regex["[A-Za-z]+\\["]]];
  collectAliases[Select[lines, StringContainsQ["->"]]];
  Map[extractLineExportsFromLine, lines]
];

General::nonLocalExport = "Non-local export encountered in ``: ``.";

extractLineExportsFromLine[str_] := Module[{tokens},
  {privacy, kind, decl} = StringSegment[str, {After["Export" | "Private"], "[", "]"}];
  If[!StringQ[decl], Return @ Nothing];
  decl = StrJoin["{", StringReplace[decl, "->" -> ","], "}"];
  If[StrContainsQ[decl, "`"], AbortPackageLoading["nonLocalExport", $path, decl]];
  context = If[privacy === "Export", $publicContext, $privateContext];
  {kind, $path, context, decl}
];

(**************************************************************************************************)

$aliasRegex = Regex["([$a-zA-Z0-9]+) -> ([$a-zA-Z0-9]+)"];
collectAliases[{}] := Null
collectAliases[lines_List] := Module[{ruleChunks},
  ruleChunks = Flatten @ StringCases[lines, $aliasRegex];
  StuffBag[$aliasBag, StringJoin["DefineAliasRules[", Riffle[ruleChunks, ", "], "]"]];
];

applyAliases[] := Locals[
  commands = BagPart[$aliasBag, All];
  Block[{$Context = $privateContext, $ContextPath = {"System`", "CoreTools`", $publicContext}},
    ToExpression[commands, InputForm];
  ];
];
