PackageExports[
  "IOFunction",      SublimeViewText, LogToSublime
];

PrivateExports[
  "SpecialVariable", $SublimePackagesPath, $SublimeKindColors
];

(*************************************************************************************************)

SublimeViewText[expr_] := Locals[
  $path = NewTemporaryFilename @ StrJoin[Base36Hash[expr], ".txt"];
  sublimeExportText @ expr;
  SublimeOpen @ $path;
  expr
];

sublimeExportText = CaseOf[
  strs_ ? StrVecQ         := ExportLines[$path, strs];
  str_String              := ExportUTF8[$path, str];
  list_List ? RuleVectorQ := ExportUTF8[$path, TextString @ TableForm @ (List @@@ list)];
  assoc_Dict             := sublimeExportText @ Normal @ assoc;
];

(*************************************************************************************************)

SetHoldC[LogToSublime]

LogToSublime[expr_] := Locals[
  path = NewTemporaryFilename["sublime_log#.txt"];
  $currentPath = path;
  $currentStream = None;
  WithLocalSettings[
    Null,
    Block[{CellPrint = streamCellPrint, Print = streamPrint},
      expr
    ],
    Close @ $currentStream
  ]
];

streamPrint[args___] :=
  streamCellPrint @ Cell[BoxData @ ToBoxes @ SequenceForm[args], "Print"];

streamCellPrint[cell_Cell] := Module[{str, first = False},
  If[$currentStream === None,
    first = True;
    $currentStream = OpenWrite[$currentPath, BinaryFormat -> True]];
  str = First @ FrontEndExecute @ FrontEnd`ExportPacket[cell, "PlainText"];
  BinaryWrite[$currentStream, str];
  BinaryWrite[$currentStream, "\n"];
  If[first, SublimeRun[$currentPath, "-n"]];
];