BeginPackage["Prelude`Packages`"]

Prelude`Packages`Private`$systemExports = System`PackageExports[

"PackageDeclaration",
System`ExportFunction,
System`ExportControlFlowFunction,
System`ExportBoxFunction,
System`ExportDebuggingFunction,
System`ExportIOFunction,
System`ExportMessageFunction,
System`ExportMetaFunction,
System`ExportMutatingFunction,
System`ExportScopingFunction,
System`ExportSpecialFunction,
System`ExportGraphicsDirective,
System`ExportGraphicsFunction,
System`ExportGraphicsBoxFunction,
System`ExportHead,
System`ExportDataHead,
System`ExportFormHead,
System`ExportPatternHead,
System`ExportStrPatternHead,
System`ExportSpecialHead,
System`ExportOperator,
System`ExportPredicateOperator,
System`ExportOptionSymbol,
System`ExportPredicate,
System`ExportSpecialVariable,
System`ExportSymbol,
System`ExportPatternSymbol,
System`ExportStrPatternSymbol,
System`ExportFormSymbol,
System`ExportVariable,

System`ExportTypeHead,
System`ExportSymbolicHead,
System`ExportFieldSymbol,
System`ExportSortSymbol,
System`ExportTypeSymbol,

System`PrivateFunction,
System`PrivateControlFlowFunction,
System`PrivateBoxFunction,
System`PrivateDebuggingFunction,
System`PrivateIOFunction,
System`PrivateMessageFunction,
System`PrivateMetaFunction,
System`PrivateMutatingFunction,
System`PrivateScopingFunction,
System`PrivateSpecialFunction,
System`PrivateGraphicsDirective,
System`PrivateGraphicsFunction,
System`PrivateGraphicsBoxFunction,
System`PrivateHead,
System`PrivateFormHead,
System`PrivatePatternHead,
System`PrivateSpecialHead,
System`PrivateOperator,
System`PrivatePredicateOperator,
System`PrivateOptionSymbol,
System`PrivatePredicate,
System`PrivateSpecialVariable,
System`PrivateCacheVariable,
System`PrivateSymbol,
System`PrivatePatternSymbol,
System`PrivateFormSymbol,
System`PrivateVariable,

"SpecialFunction",
System`SystemExports,
System`PackageExports,
System`PrivateExports,

"MutatingFunction",
System`UnprotectClearAll,

"IOFunction",
System`GetHoldComplete,
System`GetHidden,

"SpecialVariable",
System`$SymbolAliases,
System`$KnownSymbolKinds,
System`$LastFailedExpression,
System`$CurrentPackageFile,

"SpecialFunction",
System`AbortPackageLoading,

"MessageFunction",
System`NonLethalPackageMessages

];

(* TODO: Move more things in here ! *)

PackageExports[

"SpecialVariable",
$CurrentPackageExpr,
$CurrentPackageExprCount,
$CurrentPackageErrorMessageCount,

$PackageSymbolTable,
$PackageLoadCompleteQ,
$PackageSymbolAliases,
$PackageKindDeclarations,
$PackagePreLoadActions,
$PackagePostLoadActions,
$PackageLoadFileTimings,
$SymbolAliasesDirty,
$LethalPackageMessages,

$PackageDeclarataionSymbols,

"MessageFunction",
LoadPrint,
PackageLoadMessageHandler,
PackageLoadShadowMessageHandler,
PackageLoadUncaughtThrowHandler,

"Function",
PackageSymbolKinds,
LoadedPackages,

"IOFunction",
LoadPackage,

"Predicate",
PackageLoadCompletedQ

];

Begin["`Private`"]

If[!ListQ[$PackageDeclarataionSymbols],
  $PackageDeclarataionSymbols = Cases[$systemExports,
    sym_Symbol /; StringStartsQ[SymbolName @ Unevaluated @ sym, {"Export", "Private"}]
  ];
];

(*************************************************************************************************)

General::badGetArgs = "Bad arguments: ``.";

GetHidden[path_String] := Internal`InheritedBlock[{$ContextPath}, Get[path]]
g_GetHidden := (Message[GetHidden::badGetArgs, HoldForm @ g]; $Failed);

(*************************************************************************************************)

GetHoldComplete[path_String] := Language`FullGet[path, Null, HoldComplete];
g_GetHoldComplete := (Message[GetHoldComplete::badGetArgs, HoldForm @ g]; $Failed);

(* fix bug in FullGet, which I guess is never used? *)

Unprotect[Language`FullGet];
Options[Language`FullGet] = {CharacterEncoding -> "UTF8", Path :> $Path, Method -> Automatic};
Protect[Language`FullGet];

(*************************************************************************************************)

PackageLoadCompletedQ[str_String] := Lookup[$PackageLoadCompleteQ, str, False];

If[!AssociationQ[$PackageLoadCompleteQ],
$SymbolAliasesDirty = False;
$SymbolAliases = Data`UnorderedAssociation[];
$PackageSymbolAliases = Data`UnorderedAssociation[];
$PackageLoadCompleteQ = Data`UnorderedAssociation[];
$PackageLoadFileTimings = Data`UnorderedAssociation[];
$PackageSymbolTable = Data`UnorderedAssociation[];
$PackagePreLoadActions = Association[];
$PackagePostLoadActions = Association[];
$CurrentPackageErrorMessageCount = 0;
];

(*************************************************************************************************)

SetAttributes[UnprotectClearAll, {HoldAllComplete}];
UnprotectClearAll[e___] := (Unprotect[e]; ClearAll[e]);

SetAttributes[{System`SystemExports, System`PackageExports, System`PrivateExports}, HoldAllComplete];

(*************************************************************************************************)

(* allow stand-alone running *)

System`ErrorPrint;
System`LogPrint;
System`$CellPrintLabel;
System`SublimeOpen;

If[DownValues[ErrorPrint] === {},
  ErrorPrint = Print;
  LogPrint = Print;
];

If[DownValues[SublimeOpen] === {},
  SublimeOpen[path_String | (path_String -> _)] := SystemOpen[path];
];

(*************************************************************************************************)

$ptVerbose = False;
SetAttributes[LoadPrint, HoldAll];
LoadPrint[printArgs___] := If[TrueQ[$ptVerbose], LogPrint[printArgs], Null];

(*************************************************************************************************)

Options[LoadPackage] = {
  "CodePreprocessor"    -> None,
  "ContextPath"         -> Automatic,
  "Verbose"             -> False,
  "SymbolTableFunction" -> Automatic,
  "PreLoadFunction"     -> None,
  "EvaluationFunction"  -> None,
  "BaseDirectory"       -> Automatic,
  "PriorityRules"       -> {}
};

LoadPackage::invalid = "Invalid usage: ``.";
LoadPackage::invalidBaseContext = "Invalid base context: ``.";
LoadPackage::invalidSourceSpec = "Invalid source spec: ``.";
LoadPackage::invalidSymbolTable = "SymbolTableFunction returned invalid result: ``.";
LoadPackage::unknownSymbolKinds = "One more unknown kinds encountered while loading ``: ``. See $KnownSymbolKinds.";

g_LoadPackage := (Message[LoadPackage::invalid, HoldForm @ g]; False);

LoadPackage[baseContext_String, sourceFileSpec_, opts:OptionsPattern[]] :=

  Block[{
   $NewSymbol,
   $baseContext, $baseDirectory, $basePrivateContext, $dontCleanContext, $contextPath,
   $codePreprocessor, $priorityRules,
   $fnBag, $varBag, $kindBag, $fileBag,
   $lazySymbolClearers, $lazyQueueEvaluators, $baseLen, $exprEvalFn,
   $CurrentPackageErrorMessageCount = 0,
   $SessionCurrentEvaluationPrintCount = 0,
   $SessionMaxEvaluationPrintCount = 32,
   (* Print = LogPrint, *)
   previousAliases, externalAliases,
   actions, sourceFiles, fileContextFn, fileContexts, fileContextGlob, savedVariables,
   symbolTableFn, symbolTable, invalidKinds,
   kindToContextToDecls, metaSymbolTable, pathToContextToDecls,
   preloadFn},

  Catch[

    $ptVerbose = TrueQ @ OptionValue["Verbose"];

    $priorityRules = procPriorityRules @ OptionValue["PriorityRules"];

    $codePreprocessor = OptionValue["CodePreprocessor"];
    If[$codePreprocessor === None, $codePreprocessor = Identity];

    $exprEvalFn = OptionValue["EvaluationFunction"];
    If[$exprEvalFn === None, $exprEvalFn = ReleaseHold];

    $contextPath = OptionValue["ContextPath"];
    If[$contextPath === Automatic,
      $contextPath = {baseContext, baseContext <> "Private`", "System`"}];

    externalAliases = $SymbolAliases;
    previousAliases = Lookup[$PackageSymbolAliases, baseContext, Data`UnorderedAssociation[]];
    If[Length[previousAliases] > 0,
      LoadPrint["Clearing previous aliases from this package: ", Keys[previousAliases, HoldForm]];
      (* this avoids previous aliases (even those in system which wouldn't be cleared) from applying too early *)
      $SymbolAliases = externalAliases = KeyComplement[{$SymbolAliases, previousAliases}];
      Clear[previousAliases];
    ];
    $PackageSymbolAliases[baseContext] = UAssoc[];

    LoadPrint["Loading ", baseContext];
    $PackageLoadCompleteQ[baseContext] = False;

    If[!StringEndsQ[baseContext, "`"], AbortPackageLoading["invalidBaseContext", baseContext]];
    $baseContext = baseContext;
    $basePrivateContext = baseContext <> "Private`";
    $baseDirectory = OptionValue["BaseDirectory"];

    actions = $PackagePreLoadActions[baseContext];
    If[AssociationQ[actions],
      LoadPrint["Running pre-load actions: ", Keys @ actions];
      Scan[Construct, actions]];

    sourceFiles = toSourceFiles @ sourceFileSpec;
    If[!Developer`StringVectorQ[sourceFiles] || (sourceFiles == {}),
      ErrorPrint[sourceFiles];
      AbortPackageLoading["invalidSourceSpec", sourceFileSpec]];
    fileContextFn = If[!StringQ[$baseDirectory], toFileContext,
      toRelativeFileContext @ Length @ FileNameSplit @ $baseDirectory
    ];
    fileContexts = Map[fileContextFn, sourceFiles];
(*  LoadPrint["Clearing package private context: ", $basePrivateContext];
    Construct[UnprotectClearAll, $basePrivateContext];*)
    fileContextGlob = baseContext <> "*`*";
    LoadPrint["Clearing file private contexts: ", fileContextGlob];
    savedVariables = saveVariableValues @ Names[baseContext <> "Private`$*"];
    Construct[UnprotectClearAll, fileContextGlob];
    LoadPrint["Restoring values for ", Keys @ savedVariables];
    loadVariableValues @ savedVariables;

    symbolTableFn = OptionValue["SymbolTableFunction"];
    LoadPrint["Scanning ", Length @ sourceFiles, " files for symbol exports."];
    If[symbolTableFn === Automatic, symbolTableFn = extractExportsFromFileList];
    symbolTable = symbolTableFn[baseContext, sourceFiles];
    (* ^ this is a list of {kind, path, context, decl} *)
    If[!MatchQ[symbolTable, {{_,_,_,_}...}], AbortPackageLoading["invalidSymbolTable", symbolTable]];
    $PackageSymbolTable[baseContext] = symbolTable;
    LoadPrint["Symbol table counts: ", Normal @ Merge[Total] @ Cases[symbolTable, {k_, _, _, s_} :> k -> StringCount[s, ","]+1]];

    invalidKinds = Complement[Part[symbolTable, All, 1], $KnownSymbolKinds];
    If[invalidKinds =!= {},
      AbortPackageLoading["unknownSymbolKinds", baseContext,
        FirstCase[symbolTable, {Alternatives @@ invalidKinds, ___}]]];

    LoadPrint["Creating symbols."];
    $dontCleanContext = If[baseContext === "CoreTools`", "CoreTools`Private`", None];
    kindToContextToDecls = GroupBy[symbolTable, {Extract[1], Extract[3] -> Extract[4]}];
    Quiet @ Internal`HandlerBlock[
      {"Message", PackageLoadShadowMessageHandler},
      KeyValueMap[createPackageSymbolsIn, kindToContextToDecls]
    ];

    metaSymbolTable = Cases[symbolTable, {"MetaFunction", _, _, _}];
    If[metaSymbolTable =!= {},
      LoadPrint["Attaching enqueing definitions to metafunctions."];
      $lazySymbolClearers = $lazyQueueEvaluators = Data`UnorderedAssociation[];
      pathToContextToDecls = GroupBy[metaSymbolTable, {Extract[2], Extract[3] -> Extract[4]}];
      KeyValueMap[attachLazyEnqueingDefs, pathToContextToDecls];
    ];

    preloadFn = OptionValue["PreLoadFunction"];
    If[preloadFn =!= None,
      LoadPrint["Running PreLoadFunction."];
      preloadFn[]
    ];

    LoadPrint["Loading files."];
    Internal`WithLocalSettings[
      Off[General::shdw];
    ,
      Internal`HandlerBlock[
        {"Message", PackageLoadMessageHandler},
        MapThread[loadPackageFile, {sourceFiles, fileContexts}]
      ];
    ,
      On[General::shdw];
      $PackageSymbolAliases[baseContext] = KeyComplement[{$SymbolAliases, externalAliases}];
    ];

    LoadPrint["Load complete."];
    $PackageLoadCompleteQ[baseContext] = True;

    actions = $PackagePostLoadActions[baseContext];
    If[AssociationQ[actions],
      LoadPrint["Running post-load actions: ", Keys @ actions];
      Scan[Construct, actions]
    ];

    True
  ,
    LoadPackage
  ]
];

(*************************************************************************************************)

saveVariableValues[names2_] := Module[
  {names = names2},
  isValueQ = ToExpression[names, InputForm, System`Private`HasImmediateValueQ];
  names = Pick[names, isValueQ];
  AssociationThread[names, saveVarVal /@ names]
];
saveVarVal[name_] := With[{h = Symbol[name]}, HoldComplete[h]];

loadVariableValues[assoc_] := KeyValueMap[loadVarVal, assoc];
loadVarVal[name_, HoldComplete[value_]] := SymbolNameSet[name, value];

(*************************************************************************************************)

SetAttributes[blockVariables, HoldRest];

blockVariables[{}, body_] := body;
blockVariables[syms_, body_] := blockVarBody[
  Thread[ToExpression[syms, InputForm, HoldComplete], HoldComplete],
  HoldComplete @ body
];

blockVarBody[HoldComplete[vars_], HoldComplete[body_]] :=
  Block[vars, body];

(*************************************************************************************************)

procPriorityRules[{}] := {_ -> 0};
procPriorityRules[rules_] := Append[_ -> 0] @ ReplaceAll[rules,
  glob_String /; StringContainsQ[glob, "*"] :> (_String ? (StringMatchQ[glob]))
];

(*************************************************************************************************)

SetAttributes[NonLethalPackageMessages, HoldFirst];

(* allows messages to print without aborting the whole package loading process *)
NonLethalPackageMessages[body_] :=
  Catch[body, LoadPackage];

(*************************************************************************************************)

SetAttributes[AbortPackageLoading, HoldFirst];

AbortPackageLoading[msgName_String, args___] := (
  Message[MessageName[LoadPackage, msgName], args];
  AbortPackageLoading[]
);

AbortPackageLoading[___] := AbortPackageLoading[];
AbortPackageLoading[] := Throw[False, LoadPackage];

(*************************************************************************************************)

toSourceFiles[list_List] := list;

toSourceFiles[dir_String] := Block[
  {fileList},
  If[$baseDirectory === Automatic, $baseDirectory = dir];
  $baseLen = StringLength[$baseDirectory] + 1;
  fileList = sortInitFirst @ FileNames["*.wl", dir, Infinity];
  LoadPrint[Style[Column[StringDrop[fileList, $baseLen], Left, 0], FontSize -> 10]];
  fileList
];

trimPath[path_String] := If[IntegerQ[$baseLen], StringDrop[path, $baseLen], path];

fileSortOrder[path_String] := Map[{-Replace[#, $priorityRules], #}&, FileNameSplit @ trimPath @ path];
sortInitFirst[files_List] := SortBy[files, fileSortOrder, LexicographicOrder];

toSourceFiles[File[path_String]] := Module[{lines, base},
  base = FileNameDrop @ path;
  lines = ReadList[path, Record, RecordSeparators -> "\n", NullRecords -> False];
  If[!Developer`StringVectorQ[lines], Return[$Failed]];
  FileNameJoin[{base, #}]& /@ lines
]

toFileContext[file_] := StringJoin[$basePrivateContext, FileBaseName @ file, "`"];

toRelativeFileContext[n_][file_] :=
  toFileContext @ StringReplace[FileNameDrop[file, n], $PathnameSeparator -> "`"];

(*************************************************************************************************)

createPackageSymbolsIn["Variable" | "SpecialVariable" | "CacheVariable", assoc_] :=
  KeyValueMap[createSymbolsIn, assoc];

createPackageSymbolsIn[_, assoc_] :=
  KeyValueMap[createCleanSymbolsIn, assoc];

General::invalidContextName = "Invalid context name: ``.";
createSymbolsIn[context_String, symbolsStrings_, fn_:Hold] := Block[
  {$Context = context, $ContextPath = {context, "System`"}},
  ToExpression[symbolsStrings, InputForm, fn]
];

createCleanSymbolsIn[context_String, symbolsStrings_] :=
  createSymbolsIn[context, symbolsStrings, If[context =!= $dontCleanContext, UnprotectClearAll, Identity]];

(*************************************************************************************************)

extractExportsFromFileList[context_, sourceFiles_] := Block[
  {$bag = Internal`Bag[], $baseContext = context, $basePrivateContext = context <> "Private`"},
  Scan[extractExportsFromFile, sourceFiles];
  Internal`BagPart[$bag, All]
];

General::badSymbolExportContext = "Bad symbol export context \"``\" in file \"``\".";
General::badSymbolExportKind = "Bad symbol export kind \"``\" in file \"``\".";
General::noHeader = "No header in file \"``\".";
General::badHeaderChunk = "Bad header line \"``\" in file \"``\".";

extractExportsFromFile[path_] := Block[
  {header, chunks, pair, fn, args, context, sections, $path = path},
  header = First[ReadList[path, Record, 1, RecordSeparators -> "(*****"], AbortPackageLoading["noHeader", path]];
  chunks = DeleteCases[""] @ StringTrim @ StringSplit[header, ";"];
  Scan[extractExportsFromChunk, chunks]
];

$chunkRegex = RegularExpression["\"([a-zA-Z]+)\",\\s*([$a-zA-Z0-9,\\s]+)"] :>
  {"$1", StringTrim[StringReplace["$2", Whitespace -> " "], {",", " ", "\n"}..]};

extractExportsFromChunk[chunk_] := (
  pair = StringTrim @ StringSplit[chunk, "[", 2];
  If[Length[pair] =!= 2, AbortPackageLoading["badHeaderChunk", chunk, $path]];
  {fn, args} = pair;
  context = Switch[fn,
    "SystemExports", "System`",
    "PackageExports", $baseContext,
    "PrivateExports", $basePrivateContext,
    _, AbortPackageLoading["badHeaderChunk", chunk, $path]];
  sections = StringCases[args, $chunkRegex];
  Internal`StuffBag[$bag, {#1, $path, context, "{" <> #2 <> "}"}& @@@ sections, 1]
);

(* Note: keep these in sync with the list at the top of the file *)
$KnownSymbolKinds = {
  "ControlFlowFunction",
  "Function",
  "BoxFunction",
  "DebuggingFunction",
  "IOFunction",
  "MessageFunction",
  "MetaFunction",
  "MutatingFunction",
  "ScopingFunction",
  "SpecialFunction",
  "GraphicsDirective",
  "GraphicsFunction",
  "GraphicsBoxFunction",
  "Head",
  "ObjectHead",
  "SpecialHead",
  "DataHead",
  "FormHead",
  "PatternHead",
  "StrPatternHead",
  "TypeHead",
  "SymbolicHead",
  "Operator",
  "PredicateOperator",
  "BoxOptionSymbol",
  "OptionSymbol",
  "PackageDeclaration",
  "PackageFunction",
  "Predicate",
  "SpecialVariable",
  "CacheVariable",
  "Symbol",
  "FormSymbol",
  "PatternSymbol",
  "StrPatternSymbol",
  "FieldSymbol",
  "SortSymbol",
  "TypeSymbol",
  "Variable"
};

(*************************************************************************************************)

lazyQueueEval[path_, bag_] := If[Internal`BagLength[bag] > 0,
  LoadPrint["Running unqueued evaluations for: ", Internal`BagPart[bag, All, HoldComplete]];
  Internal`BagPart[bag, All]
];

m_attachLazyEnqueingDefs := (Print["BAD: ", HoldForm[m]]);

attachLazyEnqueingDefs[path_String, symbolData_Association] := Module[{bag = Internal`Bag[]}, With[
  {symbols = Flatten @ KeyValueMap[createSymbolsIn[#1, #2, Identity]&, symbolData]},
  LoadPrint["Attaching unqueuing to: ", symbols];
  $lazySymbolClearers[path] := ClearAll[symbols];
  $lazyQueueEvaluators[path] := lazyQueueEval[path, bag];
  Map[attachEnqueingTo[#, bag]&, symbols];
]];

attachEnqueingTo[fn_, bag_] := (
  SetAttributes[fn, HoldAllComplete];
  SetDelayed[lhs_fn, Internal`StuffBag[LoadPrint["Capturing: ", HoldForm[lhs]]; bag, Unevaluated @ lhs]]
);

(*************************************************************************************************)

SetAttributes[runPackageFileExpr, HoldAllComplete];

runPackageFileExpr[expr_] /; $SymbolAliasesDirty := (
  $CurrentPackageExprCount++;
  $CurrentPackageExpr = HoldComplete[expr];
  $exprEvalFn @ ReplaceAll[HoldComplete @ expr, $SymbolAliases]
);

runPackageFileExpr[expr_] := (
  $CurrentPackageExprCount++;
  $CurrentPackageExpr = HoldComplete[expr];
  $exprEvalFn @ HoldComplete @ expr
);

General::coreToolsError = "Error loading file \"``\".";
loadPackageFile[path_, context_] := Block[
  {$CurrentPackageFile = path, $CurrentPackageExprCount = 0, $priorAliases = $SymbolAliases,
   $ContextPath = $contextPath, $Context = context, $CurrentPackageExpr = None,
   $CellPrintLabel = trimPath @ path},
  $lazySymbolClearers @ path;
  LoadPrint["Getting ", path];
  catchPackageThrows[
    $SymbolAliasesDirty = False;
    $PackageLoadFileTimings[path] = First @ AbsoluteTiming[
      Scan[runPackageFileExpr, $codePreprocessor @ ReplaceAll[
        GetHoldComplete[path],
        $SymbolAliases
      ]];
    ];
    $lazyQueueEvaluators @ path;
  ];
];

(*************************************************************************************************)

SetAttributes[catchPackageThrows, HoldAllComplete];

catchPackageThrows[e_] := Catch[e, Except[LoadPackage], PackageLoadUncaughtThrowHandler];

LoadPackage::uncaughtThrow = "Uncaught ``.";
PackageLoadUncaughtThrowHandler[value_, tag_] :=
  Message[LoadPackage::uncaughtThrow, HoldForm[Throw[value, tag]]];

(*************************************************************************************************)

PackageLoadShadowMessageHandler[Hold[Message[MessageName[sym_, "shdw"], ___], _]] := With[
  {name = "Global`" <> SymbolName[Unevaluated @ sym]},
  LogPrint["Removing ", name];
  Remove[name]
];

PackageLoadShadowMessageHandler[h_] := Null;

(*************************************************************************************************)

$LethalPackageMessages = True;
PackageLoadMessageHandler[Hold[msg_, True]] := (
  handleMessage[msg]; (* this will shortly be defined in init2.m *)
  If[$LethalPackageMessages, AbortPackageLoading[]];
);



SetAttributes[handleMessage, HoldAllComplete];

handleMessage[Message[msgName_, ___]] /; !TrueQ[$handlerRunning] := Quiet @ Module[
  {ifile, iexprs, stream, pos1, pos2, chars1, chars2, iline, $handlerRunning = True, $CellPrintLabel},
  If[$CurrentPackageErrorMessageCount++ > 4,
    ErrorPrint["*** Runaway evaluation occurred in ", FileLocation @ $CurrentPackageFile, ", aborting!"];
    Return[];
  ];
  $LastFailedExpression = $CurrentPackageExpr;
  ifile = $CurrentPackageFile;
  iexprs = $CurrentPackageExprCount;
  If[!IntegerQ[iexprs], ErrorPrint["Message in ", FileLocation @ ifile]; Return[]];
  stream = OpenRead[$CurrentPackageFile];
  While[
    pos1 = StreamPosition[stream];
    expr = Internal`UnsafeQuietCheck[
      Read[stream, HoldComplete @ Expression] /. $priorAliases // $codePreprocessor,
      $Failed
    ];
    If[expr === $Failed, Break[]];
    If[expr === EndOfFile,
      ErrorPrint["Message ", HoldForm[msgName], " occurred *somewhere* in ", FileLocation @ ifile];
      ErrorPrint["$LastFailedExpression = ", Shallow[HoldForm @@ $CurrentPackageExpr, 6, 10]];
      Return[];
    ];
    expr =!= $Failed && expr =!= $CurrentPackageExpr, Null];
  pos2 = StreamPosition[stream];
  SetStreamPosition[stream, pos1];
  While[
    pos1 = StreamPosition[stream];
    MemberQ[{32, 9, 10}, Read[stream, Byte]], Null];
  SetStreamPosition[stream, 0];
  chars2 = ReadList[stream, Byte, pos2 - 1];
  chars1 = Take[chars2, pos1];
  iline = Ceiling[(Count[chars1, 10] + Count[chars2, 10])/2] + 1;
  iline = Count[chars1, 10] + 1;
  Close[stream];
  ErrorPrint["Message ", HoldForm[msgName], " occurred at ", FileLocation[ifile, iline]];
  ErrorPrint["$LastFailedExpression = ", Shallow[HoldForm @@ $CurrentPackageExpr, 6, 10]];
];

(*************************************************************************************************)

PackageSymbolKinds[context_] := Module[
  {table},
  table = Lookup[$PackageSymbolTable, Key @ context, None];
  (* ^ this is a list of {kind, path, context, str} *)
  If[table === None, Return[$Failed]];
  Merge[Catenate] @ MapApply[
    #1 -> StringSplit[StringJoin @ StringReplace[#4, {"{","}",","} -> " "]]&,
    table
  ]
];

LoadedPackages[] := Keys @ $PackageSymbolTable;

(*************************************************************************************************)

End[]

EndPackage[]