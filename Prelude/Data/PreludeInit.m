Begin["Prelude`Private`"];

SetAttributes[safeClearAll, HoldAllComplete];
safeClearAll[e___] := (Unprotect[e]; ClearAll[e]);

create[clear_, context_, symbols_] := Block[
  {$ContextPath = {"System`", context}, $Context = context},
  ToExpression[symbols, InputForm, If[clear, safeClearAll, HoldComplete]]
];

create[True, "Prelude`", "{SystemExports, PackageExports, PrivateExports, SessionExports, CustomExports, DeclareFilePrivates, DeclareFileLocals, DeclarePackagePrivates, DeclarePackageLocals, DeclarePackageGlobals, DeclareSystemPrivates, DeclareSystemGlobals,DeclareArity, DeclareStrict, DeclareHoldFirst, DeclareHoldRest, DeclareHoldAll, DeclareHoldAllComplete, DeclareListable, DeclareUsage,PackageSymbolKinds, PackageSymbolNames, PackageSymbolTable, PackageModTime,LoadPrint,SourceLocationBox,PreludeLoadedPackages,EnqueEvaluation, EnquedValue,PreludeLoadPackage,PackageLoadCompletedQ,CoreToolsContextQ, CoreToolsSymbolQ,FindLikelySymbolName, LikelySymbolNames, RegisterDynamicAliasFunction, CreateDynamicAlias,AttachImmediateValue, AttachDelayedValue,NewSymbolHandler,SymbolTableRow,SymbolTableFromHeaders, SymbolTableFromDirectives, PreludeSymbolTable, DumpPreludeSymbolTable,SymbolTableInit, SymbolTableInitString,SymbolTableGroups, SymbolTableFind, SymbolTableSymbolList, SymbolTableSymbolLines, SymbolTableSymbolString, SymbolTableSymbolCount,WithShadowingFixup,SymbolTableValidQ,ExportFunction, ExportSpecialFunction, ExportControlFlowFunction, ExportDeclareFunction, ExportDefineFunction, ExportHoldFunction, ExportSequenceFunction, ExportDebuggingFunction, ExportIOFunction, ExportGraphicsFunction, ExportGraphicsBoxFunction, ExportBoxFunction, ExportMessageFunction, ExportMetaFunction, ExportMutatingFunction, ExportScopingFunction, ExportPackageFunction, ExportSymbol, ExportHead, ExportSpecialSymbol, ExportSpecialHead, ExportBoxSymbol, ExportBoxHead, ExportFormSymbol, ExportFormHead, ExportDataSymbol, ExportDataHead, ExportObjectSymbol, ExportObjectHead, ExportPatternSymbol, ExportPatternHead, ExportStringPatternSymbol, ExportStringPatternHead, ExportTypeSymbol, ExportTypeHead, ExportSymbolicHead, ExportFieldSymbol, ExportFieldHead, ExportSortSymbol, ExportSortHead, ExportSlotSymbol, ExportSlotHead, ExportTagSymbol, ExportTagHead, ExportVariable, ExportSpecialVariable, ExportCacheVariable, ExportRegistryVariable, ExportSlotVariable, ExportTagVariable, ExportTransientVariable, ExportOption, ExportSpecialOption, ExportBoxOption, ExportFormOption, ExportGraphicsOption, ExportGraphicsDirective, ExportGraphicsPrimitive, ExportPredicate, ExportPredicateOperator, ExportOperator, ExportDeprecated, ExportHoldHead, PrivateFunction, PrivateSpecialFunction, PrivateControlFlowFunction, PrivateDeclareFunction, PrivateDefineFunction, PrivateHoldFunction, PrivateSequenceFunction, PrivateDebuggingFunction, PrivateIOFunction, PrivateGraphicsFunction, PrivateGraphicsBoxFunction, PrivateBoxFunction, PrivateMessageFunction, PrivateMetaFunction, PrivateMutatingFunction, PrivateScopingFunction, PrivatePackageFunction, PrivateSymbol, PrivateHead, PrivateSpecialSymbol, PrivateSpecialHead, PrivateBoxSymbol, PrivateBoxHead, PrivateFormSymbol, PrivateFormHead, PrivateDataSymbol, PrivateDataHead, PrivateObjectSymbol, PrivateObjectHead, PrivatePatternSymbol, PrivatePatternHead, PrivateStringPatternSymbol, PrivateStringPatternHead, PrivateTypeSymbol, PrivateTypeHead, PrivateSymbolicHead, PrivateFieldSymbol, PrivateFieldHead, PrivateSortSymbol, PrivateSortHead, PrivateSlotSymbol, PrivateSlotHead, PrivateTagSymbol, PrivateTagHead, PrivateVariable, PrivateSpecialVariable, PrivateCacheVariable, PrivateRegistryVariable, PrivateSlotVariable, PrivateTagVariable, PrivateTransientVariable, PrivateOption, PrivateSpecialOption, PrivateBoxOption, PrivateFormOption, PrivateGraphicsOption, PrivateGraphicsDirective, PrivateGraphicsPrimitive, PrivatePredicate, PrivatePredicateOperator, PrivateOperator, PrivateDeprecated, PrivateHoldHead}"]

create[True, "System`", "{Caching, Logging, LogLevel, Verbose,Rectangular, Circular,FileUnixTime,UnprotectClearAll, UnprotectClear,ToInputString, HoldToInputString, FromInputString,UAssociation,EvaluateMap, HoldMap, HoldScan, HoldApply, HoldConstruct, HoldMapSequence, HoldHead, HoldLength, HoldSequenceLength, HoldByteCount, HoldHash, HoldSymbolName, HoldSymbolNameAlias, HoldSymbolContext, HoldSymbolPath,MapSequence, SequenceLength,NaturalNumberString, FullIntegerString,QuietCheck, TrapMessages,CheckedRHS,HoldListQ, HoldAssociationQ, HoldPackedArrayQ, PackedListQ, HoldStringQ, HoldIntegerQ, HoldNaturalQ, HoldNumberQ, HoldBooleanQ, HoldColorQ, InvalidQ, CorruptQ, ValidFileQ, KeyAbsentQ,$Invalid, $Corrupt,PrivateHoldComplete, PrivateSequence, PrivateHoldCompleteSequence,DeclaredHere,OutputExpressionForm, MessageArgumentForm,SourceLocation,GetHoldComplete, GetHidden,NonLethalPackageMessages,AbortPackageLoading,SublimeOpen, SublimeSeek, SublimeRun,FileLocation,SublimeProject, OpenInNewWindow,NamePaths, NamePathsGrouped, NameFirst, NameLast, NameMost, NameMostLast,EnsureContext,SystemContextQ, ActiveNameQ, FormalSymbolQ, UserSymbolQ, SystemSymbolQ, InertSymbolQ, ActiveSymbolQ, InertUserSymbolQ, InertSystemSymbolQ, CapitalizedSymbolQ, DocumentedSymbolQ,DefinitionRules, DefinitionCounts, KernelCodes,SymbolNameSet, SymbolNameSetDelayed,TraceSymbolChanges, TraceFunctionCalls, TraceAutoloads, BlockPrint, Capture, RawPrintBoxes, HandleUnpacking, UnpacksDuringQ, CountUnpackings, BenchmarkUnpacking, BenchmarkUnpackingData, TraceUnpackings, EnableDebugPrinting, DisableEchoPrinting, DisableMutationPrinting, FailureString, UnpackingTable, MicroTiming, MicroTimingTable,HandleSymbolChanges,CustomizedPrint, ErrorPrint, LogPrint, RawPrint, CachePrint, LabeledPrint, ValueChangePrint, DPrint, DebugEcho, DebugPrintCondition, DebugGroup, EchoPrint, MutationPrint,PrintIndented, WhenDebugging,$Disabled}"]

create[True, "Prelude`Packages`Private`", "{PackageLoadMessageHandler, PackageLoadUncaughtThrowHandler}"]

create[False, "System`", "{$FormalSymbols,$CurrentPackageFile, $CurrentPackageDirectory,$LastCapture, $LastTraceback, $MaxPrintRate, $DebugPrinting, $EchoPrinting, $CachePrinting, $MutationPrinting}"]

create[False, "Session`", "{$PreludeLoaded, $PreludeDir, $PreludeFiles, $CoreToolsLoaded, $CoreToolsPath, $CoreToolsRootPath,$LastEvaluationTime, $EvaluationsCount, $CurrentEvaluationStartTime, $CurrentEvaluationCellState, $EvaluationsSinceDict,$PreEvaluationHook, $PostEvaluationHook, $ShiftReturnHookInstalled, $SessionIDs,CreateCachedBox, CachedBox,DefaultPreEvaluationHook, DefaultPostEvaluationHook, SaveEvaluationCellState, RestoreEvaluationCellState, InstallSessionHooks, UninstallSessionHooks, UniqueSessionID, UniqueSessionString,SessionEvaluatedSinceQ,$SublimeApplicationPath, $SublimeProjectsPath, $SublimeRunCount,DebugPrint,$PrintIndent, $ShouldPrint, $CellPrintLabel, $CurrentlyTracingAutoloads, $CurrentlyTracingSymbols}"]

create[False, "Prelude`", "{$SymbolAliases, $NameAliases, $SymbolAliasesDirty, $PackageLoadVerbose,$CurrentPackage, $CurrentPackageLineSentinel, $CurrentPackageFileHash, $CurrentPackageExpr, $CurrentPackageExprCount, $CurrentPackageMessageCount, $CurrentPackageQueParent, $CurrentPackageQueValue, $PackageCurrentlyLoading, $PackageFailedExpr, $PackageErrorStack,$SymbolTableKinds, $SymbolExportFunctions}"]

create[False, "Prelude`Packages`Private`", "{$PackageLoadCompleteQ, $PackageFileCache, $PackageFileModTime, $PackageSymbolTable, $PackageSymbolAliases, $PackagePreLoadActions, $PackagePostLoadActions, $PackageLoadFileTimings, $PackageModTime, $LethalPackageMessages}"]

End[];