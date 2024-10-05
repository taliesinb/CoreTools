(*PackageFunction*)

SystemExports
PackageExports
PrivateExports
SessionExports
CustomExports
DeclareFilePrivates
DeclareFileLocals
DeclarePackagePrivates
DeclarePackageLocals
DeclarePackageGlobals
DeclareSystemPrivates
DeclareSystemGlobals


(*MetaFunction*)

DeclareArity
DeclareStrict
DeclareHoldFirst
DeclareHoldRest
DeclareHoldAll
DeclareHoldAllComplete
DeclareUsage
DeclaredHere


(*Option*)

Caching
Logging
LogLevel
Verbose
SublimeProject
OpenInNewWindow


(*SymbolicHead*)

Rectangular
Circular
AttachImmediateValue
AttachDelayedValue
SymbolTableRow


(*MutatingFunction*)

UnprotectClearAll
UnprotectClear
SymbolNameSet
SymbolNameSetDelayed


(*SpecialFunction*)

ToInputString
HoldToInputString
FromInputString
EnqueEvaluation
EnquedValue
CreateCachedBox
CachedBox
EnsureContext
DefinitionRules
DefinitionCounts
KernelCodes
NewSymbolHandler
SymbolTableInit
SymbolTableInitString


(*DataHead*)

UAssociation


(*HoldFunction*)

EvaluateMap
HoldMap
HoldScan
HoldApply
HoldConstruct
HoldMapSequence
HoldHead
HoldLength
HoldSequenceLength
HoldByteCount
HoldHash
HoldSymbolName
HoldSymbolNameAlias
HoldSymbolContext
HoldSymbolPath


(*SequenceFunction*)

MapSequence
SequenceLength


(*Function*)

NaturalNumberString
FullIntegerString
PackageSymbolKinds
PackageSymbolNames
PackageSymbolTable
PreludeLoadedPackages
DefaultPreEvaluationHook
DefaultPostEvaluationHook
SaveEvaluationCellState
RestoreEvaluationCellState
InstallSessionHooks
UninstallSessionHooks
UniqueSessionID
NamePaths
NamePathsGrouped
NameFirst
NameLast
NameMost
NameMostLast
FindLikelySymbolName
LikelySymbolNames
RegisterDynamicAliasFunction
CreateDynamicAlias
SymbolTableGroups
SymbolTableFind
SymbolTableSymbolList
SymbolTableSymbolLines
SymbolTableSymbolString
SymbolTableSymbolCount


(*MessageFunction*)

CheckedRHS
NonLethalPackageMessages
LoadPrint
PackageLoadMessageHandler
PackageLoadUncaughtThrowHandler
WithShadowingFixup


(*Predicate*)

HoldListQ
HoldAssociationQ
HoldPackedArrayQ
PackedListQ
HoldStringQ
HoldIntegerQ
HoldNaturalQ
HoldNumberQ
HoldBooleanQ
PackageLoadCompletedQ
SessionEvaluatedSinceQ
SystemContextQ
ActiveNameQ
FormalSymbolQ
UserSymbolQ
SystemSymbolQ
InertSymbolQ
ActiveSymbolQ
InertUserSymbolQ
InertSystemSymbolQ
CapitalizedSymbolQ
DocumentedSymbolQ
CoreToolsContextQ
CoreToolsSymbolQ
SymbolTableValidQ


(*Variable*)

$FormalSymbols
$SymbolAliases
$SymbolAliasesDirty
$PackageLoadVerbose
$LastEvaluationTime
$EvaluationsCount
$CurrentEvaluationStartTime
$CurrentEvaluationCellState
$EvaluationsSinceDict
$SublimeApplicationPath
$SublimeProjectsPath
$SublimeRunCount
$SymbolTableKinds
$SymbolExportFunctions


(*HoldHead*)

PrivateHoldComplete
PrivateSequence
PrivateHoldCompleteSequence


(*FormHead*)

OutputExpressionForm
MessageArgumentForm
SourceLocation


(*SpecialVariable*)

$PreludeLoaded
$PreludeDir
$PreludeFiles
$CoreToolsLoaded
$CoreToolsDir
$CoreToolsRootDir
$CurrentPackageFile
$CurrentPackageDirectory
$PackageLoadCompleteQ
$PackageFileCache
$PackageFileModTime
$PackageSymbolTable
$PackageSymbolAliases
$PackagePreLoadActions
$PackagePostLoadActions
$PackageLoadFileTimings
$PackageModTime
$LethalPackageMessages
$PreEvaluationHook
$PostEvaluationHook
$ShiftReturnHookInstalled
$SessionIDs
$LastCapture
$LastTraceback
$MaxPrintRate
$DebugPrinting
$EchoPrinting
$CachePrinting
$MutationPrinting
$PrintIndent
$ShouldPrint
$CellPrintLabel
$CurrentlyTracingAutoloads
$CurrentlyTracingSymbols


(*IOFunction*)

GetHoldComplete
GetHidden
PreludeLoadPackage
SublimeOpen
SublimeSeek
SublimeRun
SymbolTableFromHeaders
SymbolTableFromDirectives
PreludeSymbolTable
DumpPreludeSymbolTable
CustomizedPrint
ErrorPrint
LogPrint
RawPrint
CachePrint
LabeledPrint
ValueChangePrint
DPrint
EchoPrint
MutationPrint


(*DebuggingFunction*)

AbortPackageLoading
TraceSymbolChanges
TraceAutoloads
BlockPrint
Capture
RawPrintBoxes
HandleUnpacking
UnpacksDuringQ
CountUnpackings
BenchmarkUnpacking
BenchmarkUnpackingData
TraceUnpackings
EnableDebugPrinting
DisableEchoPrinting
DisableMutationPrinting
FailureString
UnpackingTable
MicroTiming
MicroTimingTable


(*TransientVariable*)

$CurrentPackageLineSentinel
$CurrentPackageFileHash
$CurrentPackageExpr
$CurrentPackageExprCount
$CurrentPackageMessageCount
$CurrentPackageQueParent
$CurrentPackageQueValue
$PackageCurrentlyLoading
$PackageFailedExpr
$PackageErrorStack


(*BoxFunction*)

SourceLocationBox


(*Head*)

FileLocation


(*ControlFlowFunction*)

HandleSymbolChanges


(*ScopingFunction*)

PrintIntended


(*TagVariable*)

$Disabled


(*PackageDeclaration*)

ExportFunction
ExportSpecialFunction
ExportControlFlowFunction
ExportDeclareFunction
ExportDefineFunction
ExportHoldFunction
ExportSequenceFunction
ExportDebuggingFunction
ExportIOFunction
ExportGraphicsFunction
ExportGraphicsBoxFunction
ExportBoxFunction
ExportMessageFunction
ExportMetaFunction
ExportMutatingFunction
ExportScopingFunction
ExportPackageFunction
ExportSymbol
ExportHead
ExportSpecialSymbol
ExportSpecialHead
ExportBoxSymbol
ExportBoxHead
ExportFormSymbol
ExportFormHead
ExportDataSymbol
ExportDataHead
ExportObjectSymbol
ExportObjectHead
ExportPatternSymbol
ExportPatternHead
ExportStringPatternSymbol
ExportStringPatternHead
ExportTypeSymbol
ExportTypeHead
ExportSymbolicHead
ExportFieldSymbol
ExportFieldHead
ExportSortSymbol
ExportSortHead
ExportSlotSymbol
ExportSlotHead
ExportTagSymbol
ExportTagHead
ExportVariable
ExportVar
ExportSpecialVariable
ExportSpecialVar
ExportCacheVariable
ExportCacheVar
ExportSlotVariable
ExportSlotVar
ExportTagVariable
ExportTagVar
ExportTransientVariable
ExportTransientVar
ExportOption
ExportSpecialOption
ExportBoxOption
ExportFormOption
ExportGraphicsOption
ExportGraphicsDirective
ExportGraphicsPrimitive
ExportPredicate
ExportPredicateOperator
ExportOperator
ExportDeprecated
ExportHoldHead
PrivateFunction
PrivateSpecialFunction
PrivateControlFlowFunction
PrivateDeclareFunction
PrivateDefineFunction
PrivateHoldFunction
PrivateSequenceFunction
PrivateDebuggingFunction
PrivateIOFunction
PrivateGraphicsFunction
PrivateGraphicsBoxFunction
PrivateBoxFunction
PrivateMessageFunction
PrivateMetaFunction
PrivateMutatingFunction
PrivateScopingFunction
PrivatePackageFunction
PrivateSymbol
PrivateHead
PrivateSpecialSymbol
PrivateSpecialHead
PrivateBoxSymbol
PrivateBoxHead
PrivateFormSymbol
PrivateFormHead
PrivateDataSymbol
PrivateDataHead
PrivateObjectSymbol
PrivateObjectHead
PrivatePatternSymbol
PrivatePatternHead
PrivateStringPatternSymbol
PrivateStringPatternHead
PrivateTypeSymbol
PrivateTypeHead
PrivateSymbolicHead
PrivateFieldSymbol
PrivateFieldHead
PrivateSortSymbol
PrivateSortHead
PrivateSlotSymbol
PrivateSlotHead
PrivateTagSymbol
PrivateTagHead
PrivateVariable
PrivateVar
PrivateSpecialVariable
PrivateSpecialVar
PrivateCacheVariable
PrivateCacheVar
PrivateSlotVariable
PrivateSlotVar
PrivateTagVariable
PrivateTagVar
PrivateTransientVariable
PrivateTransientVar
PrivateOption
PrivateSpecialOption
PrivateBoxOption
PrivateFormOption
PrivateGraphicsOption
PrivateGraphicsDirective
PrivateGraphicsPrimitive
PrivatePredicate
PrivatePredicateOperator
PrivateOperator
PrivateDeprecated
PrivateHoldHead


