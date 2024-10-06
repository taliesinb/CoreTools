PrivateExports[
  "IOFunction",  LoadSystemData, GenerateSystemData, BinaryWriteHelper, BinaryReadHelper
];

(**************************************************************************************************)

SetStrict @ LoadSystemData;

LoadSystemData[name_Str] /; StrEndsQ[name, ".mx"] := Block[
  {path = DataPath["System", name]},
  If[FileExistsQ[path],
    ImportMX @ path,
    GenerateSystemData @ name
  ]
];

SetStrict @ GenerateSystemData;

GenerateSystemData::genFileNotExists = "Generator for `` does not exist: ``";
GenerateSystemData::genFileBadOutput = "Generator file `` produced a bad result: ``.";

GenerateSystemData[name_Str] /; StrEndsQ[name, ".mx"] := Module[{path, result, iresult},
  path = DataPath["System", StringDrop[name, -1]];
  If[!FileExistsQ[path],
    Message[GenerateSystemData::genFileNotExists, name, File @ path];
    Return @ $Failed];
  context = "GenSysData`" <> FileNameDrop[name, -2];
  result = BlockContext[context, {"CoreTools`", "System`"}, iresult = Check[Get @ path, $Failed]];
  If[FailureQ[result] || result === Null,
    Message[GenerateSystemData::genFileBadOutput, path, iresult];
    Return @ $Failed];
  ExportMX[path <> "x", result];
  result
];


