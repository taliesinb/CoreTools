If[!TrueQ[Prelude`$PreludeLoaded] || True, Check[
  Block[{$ContextPath = {"System`"}, $NewSymbol},
    Prelude`$PreludeFiles = Thread @ Map[FileNameJoin] @ Thread @ {
      FileNameDrop @ $InputFileName,
      StringSplit @ "Overrides.wl Sublime.wl Tracing.wl Packages.wl Session.wl Symbols.wl"
    };
    Scan[Get] @ Prelude`$PreludeFiles;
  ];
  $NewSymbol = Prelude`Symbols`NewSymbolHandler;
  Prelude`$PreludeLoaded = True;
  Prelude`$PreludeContexts = StringJoin /@ Thread[{"Prelude`", {"", "Overrides`", "Sublime`", "Tracing`", "Packages`", "Session`", "Symbols`"}}];
,
  $NewSymbol =.;
  General::preludeLoadFailure = "Failed to load prelude.";
  Message[General::preludeLoadFailure];
  Prelude`$PreludeLoaded = False
]];