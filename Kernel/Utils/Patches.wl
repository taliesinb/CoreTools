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
    $IsPacletManagerHooked, $PacletManagerHook, $PatchDebugging,
    $CurrentDefinitionNotebooks
];

(*************************************************************************************************)

GetPackageSymbol[symbol_String] := GetPackageSymbol[symbol] = (
  HiddenLoadPackage[SymbolNameFirst @ symbol];
  ToExpression[symbol, InputForm]
);

GetPackageSymbol[context_String, symbols:{__String}] := GetPackageSymbol[context, symbols] = (
  HiddenLoadPackage[SymbolNameFirst @ context];
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

SetInitial[$PackageLoadedCache, UAssoc[]];

SystemPackageLoadedQ[context_] := $PackageLoadedCache[package] = Or[
  Lookup[$PackageLoadedCache, context, False],
  AnyAreTrueQ @ StringContainsQ[$LoadedFiles, StringDelete[context, "`"]]
];

(*************************************************************************************************)

SetInitial[$PackagePatchFunctions, Assoc[]];
SetInitial[$PackageAppliedPatches, Assoc[]];
SetInitial[$PackageNeedsPatchesQ, UAssoc[]];

DeclareStrict[RegisterPackagePatchFunctions];

RegisterPackagePatchFunctions[context_String, rules__Rule] := Locals[
  If[$CurrentlyTracingAutoloads, Return[]];
  $PackageNeedsPatchesQ[context] := True;
  KeyAssociateTo[$PackagePatchFunctions, context, {rules}];
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

patchDefNotebook[] := With[
  {makeDefNb := GeneralUtilities`Debugging`PackagePrivate`makeDefinitionNotebook},
  DownValues[makeDefNb] = Prepend[
    DownValues[makeDefNb] /. 600 :> $DefinitionNotebookWidth,
    HoldPattern[makeDefNb[cells_] /; !TrueQ[$insideMDNQ]] :> Block[{$insideMDNQ = True},
      wrappedMakeDefNotebook[makeDefNb, cells]
    ]
  ]
];

SetDelayedInitial[
  $DefinitionNotebookWidth,
  Clip[First[AvailableScreenSize[]] / 2, {400, 800}]
];

RegisterPackagePatchFunctions[
  "GeneralUtilities`",
  "MutationVariables" -> patchMutVars,
  "DefinitionNotebook" -> patchDefNotebook
];

(*************************************************************************************************)

$CurrentDefinitionNotebooks = Internal`Bag[];

wrappedMakeDefNotebook[makeDefNb_, cells_] := Block[{newNb},
  (* If[EvaluatedSinceQ["PrintDefinitions"],
    Quiet @ Scan[NotebookClose, Internal`BagPart[nbBag, All]];
    nbBag = Internal`Bag[];
  ]; *)
  Internal`StuffBag[
    $CurrentDefinitionNotebooks,
    makeDefNb[cells]
  ];
];


