#!/usr/bin/env wolframscript

Print["Loading"];

Get @ FileNameJoin[{FileNameDrop[$InputFileName, -2], "Prelude", "init.m"}];

If[Session`$PreludeLoaded =!= True,
  Print["Prelude` not loaded, aborting."];
  Exit[1];
];

Print["Dumping Prelude` symbol table."];
If[!ListQ[Prelude`DumpPreludeSymbolTable[]],
  Print["Failed to dump."];
  Exit[2];
];

Exit[0];