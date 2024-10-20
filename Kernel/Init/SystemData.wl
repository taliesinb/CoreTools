PrivateExports[
  "IOFunction", LoadSystemData, SaveSystemData, GenerateSystemData
];

(**************************************************************************************************)

SetStrict @ LoadSystemData;

LoadSystemData[name_Str] := Locals[
  path = DataPath["System", name];
  If[FileExistsQ[path],
    toIOFn[path, True] @ path,
    GenerateSystemData @ name
  ]
];

toIOFn[path_ ? (StrEndsQ["wlsyms"]), in_] := If[in, ImportStringTable, ExportStringTable];
toIOFn[path_ ? (StrEndsQ["mx"]), in_]     := If[in, ImportMX, ExportMX];
toIOFn[path_, in_]                        := (Message[General::badDataExtension, LitStr @ If[in, "import", "export"], File @ path]; $Failed&);

General::badDataExtension = "Don't know how to `` file ``.";

(**************************************************************************************************)

SetStrict @ SaveSystemData;

SaveSystemData[name_Str, data_] := Locals[
  path = DataPath["System", name];
  toIOFn[path, False][path, data]
];

(**************************************************************************************************)

SetStrict @ GenerateSystemData;

GenerateSystemData::genFileNotExists = "Generator for `` does not exist: ``";
GenerateSystemData::genFileBadOutput = "Generator file `` produced a bad result: ``.";
GenerateSystemData::genFileWriteOutput = "Output of generator file `` couldn't be written to ``.";

GenerateSystemData[name_Str] := Module[{path, result, iresult},
  outPath = DataPath["System", name];
  outFn = toIOFn[outPath, False];
  If[outFn === $Failed&, Return @ $Failed];

  genPath = DataPath["System", FileBaseName[name] <> ".m"];
  If[!FileExistsQ[genPath],
    Message[GenerateSystemData::genFileNotExists, name, File @ genPath];
    Return @ $Failed
  ];
  context = "GenSysData`" <> FileNameDrop[name, -2];
  result = BlockContext[context, {"CoreTools`", "System`"}, iresult = Check[Get @ genPath, $Failed]];
  If[FailureQ[result] || result === Null,
    Message[GenerateSystemData::genFileBadOutput, File @ genPath, iresult];
    Return @ $Failed];
  If[FailureQ[outFn[outPath, result]],
    Message[GenerateSystemData::genFileWriteOutput, File @ genPath, File @ outPath]];
  result
];


