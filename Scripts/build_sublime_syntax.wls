#!/usr/bin/env wolframscript

Print["Loading"];

Get @ FileNameJoin[{FileNameDrop[$InputFileName, -2], "Kernel", "init.m"}];

If[System`Private`$CoreToolsLoaded =!= True,
  Print["CoreTools not loaded, aborting."];
  Exit[1];
];

Print["Updating syntax."];
If[!ListQ[CoreTools`SublimeUpdateSyntax[]],
  Print["Failed to update."];
  Exit[2];
];

Exit[0];