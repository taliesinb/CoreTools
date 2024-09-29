BeginPackage["Prelude`"]

SystemExports[

  "Option",
    Caching,
    Logging,
    LogLevel,
    Verbose,

  "FormHead",
    SourceLocation,

  "IOFunction",
    GetHoldComplete,
    GetHidden,

  "SpecialVariable",
    $CurrentPackageFile,
    $CurrentPackageDirectory,

  "MessageFunction",
    NonLethalPackageMessages,

  "DebuggingFunction",
    AbortPackageLoading
];

PackageExports[

  "Function",
    PackageSymbolKinds,
    PackageSymbolNames,
    PackageSymbolTable,

  "Variable",
    $SymbolAliases,
    $SymbolAliasesDirty,
    $PackageLoadVerbose,
    $PackageCurrentlyLoading,
    $LastFailedExpression,
    $PackageLoadCompleteQ,

  "SpecialVariable",

    $CurrentPackageLineSentinel,
    $CurrentPackageFileHash,
    $CurrentPackageExpr,
    $CurrentPackageExprCount,
    $CurrentPackageMessageCount,
    $CurrentPackageQueParent,
    $CurrentPackageQueValue,

  "MessageFunction",
    LoadPrint,

  "BoxFunction",
    SourceLocationBox,

  "Function",
    PreludeLoadedPackages,

  "SpecialFunction",
    EnqueEvaluation,
    EnquedValue,

  "IOFunction",
    PreludeLoadPackage,

  "Predicate",
    PackageLoadCompletedQ

];

Begin["`Packages`Private`"]

PrivateExports[

  "MessageFunction",

    PackageLoadMessageHandler,
    PackageLoadUncaughtThrowHandler,

  "SpecialVariable",

    $PackageFileCache,
    $PackageFileModTime,
    $PackageSymbolTable,
    $PackageSymbolAliases,
    $PackagePreLoadActions,
    $PackagePostLoadActions,
    $PackageLoadFileTimings,
    $PackageModTime,
    $LethalPackageMessages
];

(*************************************************************************************************)

General::badGetArgs = "Bad arguments: ``.";

GetHidden[path_String] := Internal`InheritedBlock[{$ContextPath}, Get[path]]
g_GetHidden := (Message[GetHidden::badGetArgs, HoldForm @ g]; $Failed);

(*************************************************************************************************)

GetHoldComplete[path_String] := Language`FullGet[path, Null, HoldComplete];
g_GetHoldComplete := (Message[GetHoldComplete::badGetArgs, HoldForm @ g]; $Failed);

(*************************************************************************************************)

(* fix bug in FullGet, which I guess is never used? *)

Unprotect[Language`FullGet];
Options[Language`FullGet] = {CharacterEncoding -> "UTF8", Path :> $Path, Method -> Automatic};
Protect[Language`FullGet];

(*************************************************************************************************)

PackageLoadCompletedQ[str_String] := Lookup[$PackageLoadCompleteQ, str, False];

If[!AssociationQ[$PackageLoadCompleteQ],
$PackageLoadVerbose         = False;
$PackageCurrentlyLoading    = False;
$SymbolAliasesDirty         = False;
$SymbolAliases              = Data`UnorderedAssociation[];
$PackageSymbolAliases       = Data`UnorderedAssociation[];
$PackageLoadCompleteQ       = Data`UnorderedAssociation[];
$PackageLoadFileTimings     = Data`UnorderedAssociation[];
$PackageModTime             = Data`UnorderedAssociation[];
$PackageSymbolTable         = Data`UnorderedAssociation[];
$PackagePreLoadActions      = Association[];
$PackagePostLoadActions     = Association[];
$CurrentPackageMessageCount = 0;
$CurrentPackageFileHash     = None;
$PackageFileCache           = Data`UnorderedAssociation[];
$PackageFileModTime         = Data`UnorderedAssociation[];
];

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

SetAttributes[NonLethalPackageMessages, HoldFirst];

(* allows messages to print without aborting the whole package loading process *)
NonLethalPackageMessages[body_] :=
  Catch[body, PreludeLoadPackage];

(*************************************************************************************************)

SetAttributes[AbortPackageLoading, HoldFirst];

AbortPackageLoading[msgName_String, args___] := (
  Message[MessageName[PreludeLoadPackage, msgName], args];
  AbortPackageLoading[]
);

AbortPackageLoading[___] := AbortPackageLoading[];
AbortPackageLoading[] := If[$PackageCurrentlyLoading, Throw[False, PreludeLoadPackage], Abort[]];

(*************************************************************************************************)

Options[PreludeLoadPackage] = {
  "CodePreprocessor"    -> None,
  "ContextPath"         -> {},
  "Verbose"             -> Automatic,
  "SymbolTableFunction" -> SymbolTableFromHeaders,
  "PreLoadFunction"     -> None,
  "EvaluationFunction"  -> None,
  "BaseDirectory"       -> Automatic,
  "PriorityRules"       -> {},
  "SymbolAliasFiles"    -> {}
};

PreludeLoadPackage::invalid = "Invalid usage: ``.";
PreludeLoadPackage::invalidBaseContext = "Invalid base context: ``.";
PreludeLoadPackage::invalidSourceSpec = "Invalid source spec: ``.";
PreludeLoadPackage::invalidSymbolTable = "SymbolTableFunction returned invalid result: ``.";
PreludeLoadPackage::ignoredFile = "Ignoring file whose extension does not end with '.wl' or '.txt': ``.";
PreludeLoadPackage::symbolInitFailed = "SymbolTableInit returned an invalid result.";

g_PreludeLoadPackage := (Message[PreludeLoadPackage::invalid, HoldForm @ g]; False);

PreludeLoadPackage[baseContext_String, sourceFileSpec_, opts:OptionsPattern[]] :=

  Block[{
   $NewSymbol,
   $baseContext, $baseDirectory, $basePrivateContext, $dontCleanContext, $contextPath,
   $codePreprocessor, $priorityRules,
   $lazySymbolClearers, $lazyQueueEvaluators, $baseLen, $exprEvalFn,
   $PackageCurrentlyLoading = True,
   $CurrentPackageMessageCount = 0,
   $MaxPrintRate = 200,
   externalAliases, sourceFiles, fileContexts, symbolTable},

  Catch[

    If[!StringEndsQ[baseContext, "`"], AbortPackageLoading["invalidBaseContext", baseContext]];
    $ptVerbose = TrueQ @ Replace[OptionValue["Verbose"], Automatic -> $PackageLoadVerbose];

    LoadPrint["PreludeLoadPackage[\"", baseContext, "\"]"];
    $PackageLoadCompleteQ[baseContext] = False;

    $baseContext = baseContext;
    $basePrivateContext = baseContext <> "Private`";
    $baseDirectory = OptionValue["BaseDirectory"];
    $contextPath = OptionValue["ContextPath"];
    $contextPath = Join[{$baseContext, $basePrivateContext}, $contextPath, {"System`"}];
    LoadPrint["* will use $ContextPath: ", $contextPath];

    $priorityRules = procPriorityRules @ OptionValue["PriorityRules"];
    $symbolAliasFiles = OptionValue["SymbolAliasFiles"];
    $codePreprocessor = OptionValue["CodePreprocessor"];
    If[$codePreprocessor === None, $codePreprocessor = Identity];
    $exprEvalFn = OptionValue["EvaluationFunction"];
    If[$exprEvalFn === None, $exprEvalFn = ReleaseHold];

    externalAliases = $SymbolAliases;
    Module[{previousPackageAliases},
      previousPackageAliases = Lookup[$PackageSymbolAliases, baseContext, Data`UnorderedAssociation[]];
      If[Length[previousPackageAliases] > 0,
        LoadPrint["* clearing previous aliases from this package: ", Length @ previousPackageAliases];
        (* this avoids previous aliases (even those in system which wouldn't be cleared) from applying too early *)
        $SymbolAliases = externalAliases = KeyComplement[{$SymbolAliases, previousPackageAliases}];
      ]
    ];
    $PackageSymbolAliases[baseContext] = Data`UnorderedAssociation[];


    Module[{actions},
      actions = $PackagePreLoadActions[baseContext];
      If[AssociationQ[actions],
        LoadPrint["* running pre-load actions: ", Keys @ actions];
        Scan[Construct, actions]]];

    sourceFiles = toSourceFiles @ sourceFileSpec;
    If[!Developer`StringVectorQ[sourceFiles] || (sourceFiles == {}),
      ErrorPrint[sourceFiles];
      AbortPackageLoading["invalidSourceSpec", sourceFileSpec]];

    Module[{fileContextFn},
      If[!StringQ[$baseDirectory],
        fileContextFn = toFileContext,
        fileContextFn = toRelativeFileContext @ Length @ FileNameSplit @ $baseDirectory
      ];
      fileContexts = Map[fileContextFn, sourceFiles];
    ];
(*  LoadPrint["Clearing package private context: ", $basePrivateContext];
    Construct[UnprotectClearAll, $basePrivateContext];*)

    Module[{fileContextGlob, savedVariables},
      fileContextGlob = baseContext <> "*`*";
      LoadPrint["* clearing file private contexts: ", fileContextGlob];
      (* it is much slower to do Private`$ than ` when there are lots of symbols starting with $ in other contexts! *)
      savedVariables = saveVariableValues @ Select[Names[baseContext <> "Private`*"], StringContainsQ["`$"]];
      LoadPrint["  * saved values for ", Length @ savedVariables, " vars"];
      Construct[UnprotectClearAll, fileContextGlob];
      LoadPrint["  * restoring values."];
      loadVariableValues @ savedVariables;
    ];

    LoadPrint["* scanning ", Length @ sourceFiles, " files for symbol exports."];
    Global`$ST = symbolTable = OptionValue["SymbolTableFunction"][sourceFiles, baseContext];
    (* ^ this is a list of SymbolTableRow[kind, path, context, decl]} *)
    If[!MatchQ[symbolTable, {__SymbolTableRow}], AbortPackageLoading["invalidSymbolTable", symbolTable]];
    $PackageSymbolTable[baseContext] = symbolTable;
    LoadPrint["* symbol table count: ", SymbolTableSymbolCount @ symbolTable];

    LoadPrint["* creating symbols."];
    Module[{initResult},
      initResult = SymbolTableInit[symbolTable];
      If[!MatchQ[initResult, HoldComplete[_List]], AbortPackageLoading["symbolInitFailed"]];
    ];

    Module[{metaSymbolTable},
      metaSymbolTable = Cases[symbolTable, SymbolTableRow["MetaFunction", _, _, _]];
      If[metaSymbolTable =!= {},
        LoadPrint["* attaching enqueing definitions to metafunctions."];
        $lazySymbolClearers = $lazyQueueEvaluators = Data`UnorderedAssociation[];
        KeyValueMap[attachLazyEnqueingDefs, SymbolTableGroups[metaSymbolTable, "Path", None]];
      ,
        $lazySymbolClearers = $lazyQueueEvaluators = Null&;
      ];
    ];

    Module[{preloadFn},
      preloadFn = OptionValue["PreLoadFunction"];
      If[preloadFn =!= None,
        LoadPrint["* running PreLoadFunction."];
        preloadFn[]
      ];
    ];

    LoadPrint["* now loading ", Length @ sourceFiles, " files."];
    Internal`WithLocalSettings[
      Off[General::shdw];
    ,
      Internal`HandlerBlock[
        {"Message", PackageLoadMessageHandler},
        $PackageModTime[baseContext] = updateModTimes @ sourceFiles;
        MapThread[loadPackageFile, {sourceFiles, fileContexts}]
      ];
    ,
      On[General::shdw];
      $PackageSymbolAliases[baseContext] = KeyComplement[{$SymbolAliases, externalAliases}];
    ];

    LoadPrint["* load complete."];
    $PackageLoadCompleteQ[baseContext] = True;

    Module[{actions},
      actions = $PackagePostLoadActions[baseContext];
      If[AssociationQ[actions],
        LoadPrint["* running post-load actions: ", Keys @ actions];
        Scan[Construct, actions]
      ];
    ];

    True
  ,
    PreludeLoadPackage
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

toSourceFiles[list_List] := Map[expandFileSpec, list];

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
  lines = Select[lines, !StringStartsQ[#, "#"]&];
  expandFileSpec[FileNameJoin[{base, #}]& /@ lines]
];

expandFileSpec[list_List] := Map[expandFileSpec, list];
expandFileSpec[other_] := (Message[PreludeLoadPackage::ignoredFile, other]; Nothing);
expandFileSpec[path_String] /; StringEndsQ[path, ".wl"] := path;
expandFileSpec[path_String] /; StringEndsQ[path, ".txt"] := Splice @ toSourceFiles @ File @ path;

(*************************************************************************************************)

toFileContext[file_] := StringJoin[$basePrivateContext, FileBaseName @ file, "`"];

toRelativeFileContext[n_][file_] :=
  toFileContext @ StringReplace[FileNameDrop[file, n], $PathnameSeparator -> "`"];

(*************************************************************************************************)

(* createPackageSymbolsIn["Variable" | "SpecialVariable" | "CacheVariable", assoc_] :=
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
 *)
(*************************************************************************************************)

lazyQueueEval[path_, bag_] := If[Internal`BagLength[bag] > 0,
  LoadPrint["* running unqueued evaluations for: ", Internal`BagPart[bag, All, HoldForm]];
  Internal`BagPart[bag, All];
];

attachLazyEnqueingDefs[path_String, table_List] := Module[{bag = Internal`Bag[]}, With[
  {symbols = Flatten @ SymbolTableInit[table, Identity]},
  LoadPrint["* attaching unqueuing to ", Length @ symbols, " symbols e.g. ", SymbolName /@ Take[symbols, UpTo @ 5]];
  $lazySymbolClearers[path] := ClearAll[symbols];
  $lazyQueueEvaluators[path] := lazyQueueEval[path, bag];
  Map[attachEnqueingTo[#, bag]&, symbols];
]];

attachEnqueingTo[fn_Symbol, bag_] := (
  SetAttributes[fn, HoldAllComplete];
  SetDelayed[lhs_fn, EnqueEvaluation[bag, lhs]];
);

(*************************************************************************************************)

SetAttributes[{EnqueEvaluation}, HoldAllComplete];

EnqueEvaluation[bag_, expr_] := Module[
  {src = SourceLocation[]},
  LoadPrint["  * enquing evaluation of: ", HoldForm[expr], " to ", bag];
  Internal`StuffBag[bag, Unevaluated @ evalEnqued[src, HoldComplete @ expr]];
  EnquedValue[src]
];

evalEnqued[src_, expr_HoldComplete] := (
  $CurrentPackageFile = Part[src, 1];
  $CurrentPackageExprCount = Part[src, 2];
  $CurrentPackageQueValue = expr;
  $wrap$ @ ReleaseHold @ expr;
  $CurrentPackageQueValue = None;
);

EnquedValue[s_][a___] := Message[EnquedValue::notReady, HoldForm[s[a]]];

EnquedValue::notReady = "Attempt was made to apply enqued value: ``.";

(*************************************************************************************************)

(* TODO: $SourceLocation which expands to this, or issues a message and evaluates to SourceLocation["Unknown"] *)
SourceLocation[] /; TrueQ[$PackageCurrentlyLoading] := SourceLocation[$CurrentPackageFile, $CurrentPackageExprCount];

MakeBoxes[SourceLocation[path_String, exprNum_Integer], StandardForm] := SourceLocationBox[path, exprNum];
MakeBoxes[_SourceLocation, StandardForm] := srcLocPathBox @ "[???]";

(*************************************************************************************************)

SourceLocationBox[___] := srcLocPathBox @ "[???]";
SourceLocationBox[path_String, exprNum_Integer] := With[
  {pathStr = StringJoin["[../", FileNameTake @ path, ":#", IntegerString @ exprNum, "]"]},
  {handler = TagBox[srcLocPathBox @ pathStr,
    EventHandlerTag[{
      {"MouseClicked", 1} :> SublimeSeek[path, exprNum],
      Method -> "Preemptive", PassEventsDown -> Automatic, PassEventsUp -> False
    }]
  ]},
  TagBox[handler, MouseAppearanceTag["LinkHand"]]
];

srcLocPathBox[pathStr_String] := StyleBox[
  ToBoxes @ pathStr,
  FontFamily -> "Source Code Pro", FontWeight -> Plain, FontSlant -> Italic,
  FontColor -> RGBColor[0.08627, 0.3686, 0.6157]
];

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
  {$CurrentPackageFile = path,
   $CurrentPackageDirectory = FileNameDrop @ path,
   $CurrentPackageExprCount = 0, $priorAliases = $SymbolAliases,
   $Context = context,
   $ContextPath = $contextPath,
   $CurrentPackageExpr = None,
   $CurrentPackageFileHash = None,
   $CellPrintLabel = trimPath @ path},
  $lazySymbolClearers @ path;
  LoadPrint["  * getting ", FileNameJoin @ FileNameTake[path, -3]];
  catchPackageThrows[
    $SymbolAliasesDirty = False;
    $PackageLoadFileTimings[path] = First @ AbsoluteTiming[
      Block[{packageExpr},
        packageExpr = $codePreprocessor @ ReplaceAll[
          getHoldCompleteCached[path],
          $SymbolAliases
        ];
        $CurrentPackageFileHash := $CurrentPackageFileHash = Hash @ packageExpr;
        Scan[runPackageFileExpr, packageExpr];
      ];
    ];
    $CurrentPackageQueParent = path;
    $lazyQueueEvaluators @ path;
    $CurrentPackageQueParent = None;
  ];
];

(*************************************************************************************************)

updateModTimes[sourceFiles_List] := Max @ Map[
  Function[path, Set[$PackageFileModTime[path], UnixTime @ FileDate @ path]],
  sourceFiles
];

getHoldCompleteCached[path_] := Module[
  {lastModTime, content, thisModTime},
  {lastModTime, content} = Lookup[$PackageFileCache, path, {None, None}];
  thisModTime = Lookup[$PackageFileModTime, path, 0];
  If[content === None || thisModTime =!= lastModTime,
    content = GetHoldComplete @ path;
    If[!FailureQ[content],
      $PackageFileCache[path] = {thisModTime, content};
      $PackageFileModTime[path] = thisModTime;
    ];
  ];
  content
];

(*************************************************************************************************)

SetAttributes[catchPackageThrows, HoldAllComplete];

catchPackageThrows[e_] := Catch[e, Except[PreludeLoadPackage], PackageLoadUncaughtThrowHandler];

PreludeLoadPackage::uncaughtThrow = "Uncaught ``.";
PackageLoadUncaughtThrowHandler[value_, tag_] :=
  Message[PreludeLoadPackage::uncaughtThrow, HoldForm[Throw[value, tag]]];

(*************************************************************************************************)

$LethalPackageMessages = True;
PackageLoadMessageHandler[Hold[msg_, True]] := (
  handleMessage[msg]; (* this will shortly be defined in init2.m *)
  If[$LethalPackageMessages, AbortPackageLoading[]];
);


SetAttributes[handleMessage, HoldAllComplete];

handleMessage[Message[msgName_, ___]] /; !TrueQ[$handlerRunning] := Quiet @ Module[
  {ifile, iexprs, stream, pos1, pos2, chars1, chars2, iline, $handlerRunning = True, $CellPrintLabel},
  If[StringQ @ $CurrentPackageQueParent,
    ErrorPrint["*** Message while evaluating queue for ", $CurrentPackageQueParent];
    ErrorPrint["*** Currently evaluating ", $CurrentPackageQueValue];
  ];
  If[$CurrentPackageMessageCount++ > 4,
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

(* TODO: add System here! *)

PackageSymbolTable["Prelude`"] := PreludeSymbolTable[];

PackageSymbolTable[context_String] :=
  Lookup[$PackageSymbolTable, Key @ context, None];

(*************************************************************************************************)

PackageSymbolNames[context_String] := Module[
  {table},
  table = PackageSymbolTable @ context;
  (* ^ this is a list of {kind, path, context, str} *)
  If[table === None, Return[$Failed]];
  Flatten @ MapApply[
    {kind, path, subcontext, str} |-> Map[StringJoin[subcontext, #]&, defToNames @ str],
    table
  ]
];

defToNames[def_] := StringSplit @ StringJoin @ StringReplace[def, "," -> " "];

(*************************************************************************************************)

PackageSymbolKinds[context_String] := Module[
  {table},
  table = PackageSymbolTable @ context;
  If[table === None, Return[$Failed]];
  Merge[Catenate] @ MapApply[#1 -> defToNames[#4]&, table]
];

PreludeLoadedPackages[] := Keys @ $PackageSymbolTable;

(*************************************************************************************************)

End[]

EndPackage[]