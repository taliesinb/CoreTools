SystemExports[
  "Predicate",
    SystemPackageLoadedQ
];

PackageExports[
  "Variable",
    $DefinitionNotebookWidth,

  "SpecialFunction",
    RegisterPackagePatchFunctions, GetPackageSymbol, HiddenLoadPackage, ApplyPackagePatches,

  "SpecialVariable",
    $PackageNeedsPatchesQ,
    $PackageLoadedCache, $PackagePatchFunctions, $PackageAppliedPatches,
    $IsPacletManagerHooked, $PacletManagerHook, $PatchDebugging
];

PrivateExports[
  "MessageFunction",
    PatchPrint,
  "Variable",
    $DefinitionNotebookOffset
];

(*************************************************************************************************)

GetPackageSymbol[symbol_String] := GetPackageSymbol[symbol] = (
  HiddenLoadPackage[NameFirst @ symbol];
  ToExpression[symbol, InputForm]
);

GetPackageSymbol[context_String, symbols:{__String}] := GetPackageSymbol[context, symbols] = (
  HiddenLoadPackage[NameFirst @ context];
  ToExpression[StrJoin[context, #]& /@ symbols, InputForm]
);

HiddenLoadPackage::loadFailed = "Failed Get[\"``\"].";
HiddenLoadPackage[package_String] := If[!SystemPackageLoadedQ[package],
  $PackageLoadedCache[package] = True;
  If[FailureQ @ GetHidden[package],
    Message[HiddenLoadPackage::loadFailed, package];
    $PackageLoadedCache[package] =.;
    $Failed
  ,
    HiddenLoadPackage[package] := Null;
  ];
];

(*************************************************************************************************)

SetInitial[$PatchDebugging, False];

DeclareHoldAll[PatchPrint]
PatchPrint[args___] /; $PatchDebugging := LogPrint[args];

(*************************************************************************************************)

SetInitial[$PackageLoadedCache, UDict[]];

SystemPackageLoadedQ[context_] := $PackageLoadedCache[package] = Or[
  Lookup[$PackageLoadedCache, context, False],
  AnyAreTrueQ @ StringContainsQ[$LoadedFiles, StringDelete[context, "`"]]
];

(*************************************************************************************************)

Initially[
  $PackagePatchFunctions = Dict[];
  $PackageAppliedPatches = Dict[];
  $PackageNeedsPatchesQ = UDict[];
];

DeclareStrict[RegisterPackagePatchFunctions];

RegisterPackagePatchFunctions[context_String, rules__Rule] := Locals[
  If[$CurrentlyTracingAutoloads, Return[]];
  $PackageNeedsPatchesQ[context] := True;
  KeyBindTo[$PackagePatchFunctions, context, {rules}];
  PatchPrint["Registered new patch functions for ", context, ": ", Keys @ {rules}];
  If[SystemPackageLoadedQ[context],
    PatchPrint["Package ", context, " already loaded, applying patch functions."];
    ApplyPackagePatches[context]
  ];
];

(*************************************************************************************************)

$alOuter = True;
If[!TrueQ[$IsPacletManagerHooked] && !TrueQ[$CurrentlyTracingAutoloads],
  PatchPrint["Installing PacletManager and Autoload hook."];
  With[{lwlc := PacletManager`Package`loadWolframLanguageCode,
      context = PacletManager`Manager`Private`pacletContext,
      autoload := System`Dump`AutoLoad,
      context2 = System`Dump`file},
    Unprotect[autoload, Package`ActivateLoad];
    DownValues[lwlc]     = Insert[DownValues[lwlc],     Unevaluated[$PacletManagerHook[context1]], {1,-1,-1,-2}];
    DownValues[autoload] = Insert[DownValues[autoload], Unevaluated[$PacletManagerHook[context2]], {-1,2,1,2,-2}];
    loader:Package`ActivateLoad[_, _, context_String ? $PackageNeedsPatchesQ, _] /; TrueQ[$alOuter] :=
      Block[{$alOuter}, Then1[loader, $PacletManagerHook[context]]];
    Protect[autoload, Package`ActivateLoad];
  ];
  $IsPacletManagerHooked = True
];

$PacletManagerHook = ApplyPackagePatches;

(*************************************************************************************************)

ApplyPackagePatches[context_String] := Module[{patches},
  $PackageLoadedCache[context] = True;
  ApplyPackagePatches[context] = Null;
  $PackageNeedsPatchesQ[context] = False;
  patches = Lookup[$PackagePatchFunctions, context, {}];
  If[patches =!= {},
    patches = KeyDrop[patches, Lookup[$PackageAppliedPatches, context, {}]];
    PatchPrint["Applying patches for ", context, ": ", Keys @ patches];
    Scan[Construct, patches];
    KeyUnionTo[$PackageAppliedPatches, context, Keys @ patches];
  ];
];

(*************************************************************************************************)

patchMutVars[] := With[
  {findMutVars := GeneralUtilities`Control`PackagePrivate`findMutatedVariables,
   lhs          = GeneralUtilities`Control`PackagePrivate`lhs},
  If[FreeQ[DownValues[findMutVars], ApplyTo],
    DownValues[findMutVars] = Insert[
      DownValues[findMutVars],
      Unevaluated @ ApplyTo[lhs_Symbol, _],
      {1, 2, 1, 1, 2, 1, 1, 2}
    ]
  ]
];

$inPrintDefsLocal = False;
patchDefNotebook[] := With[
  {makeDefNb := GeneralUtilities`Debugging`PackagePrivate`makeDefinitionNotebook,
   defCells := GeneralUtilities`Debugging`PackagePrivate`cells,
   pdLocal := GeneralUtilities`PrintDefinitionsLocal},
  DownValues[makeDefNb] = DownValues[makeDefNb] /. {
    m_makeDefNb            :> m,
    HoldP[defCells]        :> If[TrueQ[$inPrintDefsLocal], defCells, ToList[defCells, Cell[BoxData[""], "Input"]]],
    Rule[WindowSize, _]    :> Rule[WindowSize, All],
    Rule[WindowMargins, _] :> Rule[WindowMargins, definitionNotebookMargins[]]
  };
  Unprotect[pdLocal];
  DownValues[pdLocal] = DownValues[pdLocal] /. HoldP[{a_Set, b_Set}] :> {a, b, $inPrintDefsLocal = True};
  Protect[pdLocal];
];

SetInitial[$DefinitionNotebookOffset, 0];
SetDelayedInitial[$DefinitionNotebookWidth, Clip[First[AvailableScreenSize[]] / 2, {400, 1000}]];

definitionNotebookMargins[] := Then[
  $DefinitionNotebookOffset = Mod[$DefinitionNotebookOffset + 5, 50] + 5,
  {{Automatic, $DefinitionNotebookOffset}, {5, 5}}
];

RegisterPackagePatchFunctions[
  "GeneralUtilities`",
  "MutationVariables" -> patchMutVars,
  "DefinitionNotebook" -> patchDefNotebook
];

