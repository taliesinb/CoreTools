BeginPackage["Session`"];
EndPackage[];

Session`$PreludeInitFile = $InputFileName;
Session`$PreludeDir = FileNameDrop @ $InputFileName;

If[!TrueQ[Session`$PreludeLoaded] || True, Check[
  Block[{$NewSymbol, $PrePrint, $MessagePrePrint},
    Get @ FileNameJoin[{Session`$PreludeDir, "Data", "PreludeInit.m"}];
    Session`$PreludeFiles = Thread @ Map[FileNameJoin] @ Thread @ {
      Session`$PreludeDir,
      If[TrueQ[Prelude`$CurrentlyTracingAutoloads], DeleteCases["PreTracing.wl"], Identity] @ StringSplit @
        "PreBase.wl PreOverrides.wl PreSymbolTable.wl PreSublime.wl PreTracing.wl PrePackages.wl PreSession.wl PreSymbols.wl"
    };
    Scan[Get] @ Session`$PreludeFiles;
  ];
  $NewSymbol = Prelude`NewSymbolHandler;
  Session`$PreludeLoaded = True;
,
  $NewSymbol =.;
  General::preludeLoadFailure = "Failed to load prelude.";
  Message[General::preludeLoadFailure];
  Session`$PreludeLoaded = False
]];

