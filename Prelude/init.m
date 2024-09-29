If[!TrueQ[Prelude`$PreludeLoaded] || True, Check[
  Block[{$NewSymbol, $PrePrint, $MessagePrePrint},
    Prelude`$PreludeDir = FileNameDrop @ $InputFileName;
    Get @ FileNameJoin[{Prelude`$PreludeDir, "Data", "PreludeInit.m"}];
    Prelude`$PreludeFiles = Thread @ Map[FileNameJoin] @ Thread @ {
      Prelude`$PreludeDir,
      StringSplit @ "PreBase.wl PreOverrides.wl PreSymbolTable.wl PreSublime.wl PreTracing.wl PrePackages.wl PreSession.wl PreSymbols.wl"
    };
    Scan[Get] @ Prelude`$PreludeFiles;
  ];
  $NewSymbol = Prelude`NewSymbolHandler;
  Prelude`$PreludeLoaded = True;
,
  $NewSymbol =.;
  General::preludeLoadFailure = "Failed to load prelude.";
  Message[General::preludeLoadFailure];
  Prelude`$PreludeLoaded = False
]];

