SystemExports[
  "Function",
    PathJoin, NormalizePath, EnsureDirectory, ArchiveInnerFile, NewTemporaryFilename, ShellEscape, ToJSON,
  "Head",
    FileList, RawString,
  "IOFunction",
    ImportJSONString,
    ImportLines, ImportJSON, ImportMX, ImportUTF8, ImportStringTable,
    ExportLines, ExportJSON, ExportMX, ExportUTF8, ExportStringTable, StringTableString,
    RunAppleScript, RunCommand, FindBinary,
    CopyImageToClipboard, CopyTextToClipboard,
  "Variable",
    $BinaryPaths
];

PackageExports[
  "Predicate",  UnsafePathQ
];

PrivateExports[
  "Function",   DataPath,
  "IOFunction", LoadSystemData, GenerateSystemData
];

(**************************************************************************************************)

SetStrict[PathJoin, DataPath]

PathJoin[s__Str] := FileNameJoin @ {s};
PathJoin[s__Str, FileList[list_List]] := Map[FileNameJoin, Thread @ {PathJoin @ s, list}];
PathJoin[s__Str, FileList[patt_Str]]  := FileNames[patt, PathJoin @ s];

DataPath[s_Str] := PathJoin[$CoreToolsRootPath, "Data", s]; (* TODO: replace / with $PathnameSeparator on Windows *)
DataPath[spec___] := PathJoin[$CoreToolsRootPath, "Data", spec];

(**************************************************************************************************)

UnsafePathQ[path_Str] := !StrStartsQ[path, {$HomeDirectory, $TemporaryDirectory}];
UnsafePathQ[_]        := True;

(**************************************************************************************************)

SetStrict @ EnsureDirectory;

EnsureDirectory[path_Str ? UnsafePathQ] := ThrowMsg["unsafeDir", path];
EnsureDirectory[path_Str ? FileExistsQ] := path;
EnsureDirectory[path_Str] := If[FailureQ @ CreateDirectory[path], ThrowMsg["ensureDirFailed", path], path];

General::unsafeDir       = "Unsafe directory: ``.";
General::ensureDirFailed = "Could not create directory: ``.";

(**************************************************************************************************)

SetStrict[NormalizePath]

NormalizePath[""]       := "";
NormalizePath[None]     := None;
NormalizePath[path_Str] := accentsToMarks @ StringReplaceRepeated[path, $pathNormalizationRules];

$pathNormalizationRules = {
  StartOfString ~~ "~" :> $HomeDirectory,
  $PathnameSeparator ~~ Except[$PathnameSeparator].. ~~ $PathnameSeparator ~~ ".." ~~ $PathnameSeparator :> $PathnameSeparator,
  $PathnameSeparator ~~ EndOfString :> "",
  $PathnameSeparator ~~ "." ~~ $PathnameSeparator :> ""
};

(* this is important for macOS, which does this implicitly when you use Finder *)
accentsToMarks[str_ ? PrintableASCIIQ] := str;
accentsToMarks[str_] := CharacterNormalize[str, "NFC"];

(**************************************************************************************************)

ArchiveInnerFile::missing = "Archive file `` does not exist."
ArchiveInnerFile::corrupt = "Archive file `` is corrupt."
ArchiveInnerFile::ambiguous = "Archive file `` expanded to multiple files: ``.";
ArchiveInnerFile::empty = "Archive file `` was empty."

ArchiveInnerFile[path2_Str] := Module[
  {path, hashStr, dirName, dirPath, files},
  path = NormalizePath @ path2;
  If[!FileExistsQ[path],
    Message[ArchiveInnerFile::missing, path];
    Return @ $Failed];
  If[!StringEndsQ[path, {".zip", ".tar.gz", ".gz"}], Return @ path];
  hashStr = Base36String @ FileHash @ path;
  dirName = FileBaseName[path] <> "." <> hashStr;
  dirPath = NewTemporaryFilename[dirName];
  files = If[DirectoryQ[dirPath],
    FileNames[All, dirPath, Infinity],
    CreateDirectory[dirPath]; {}
  ];
  If[files === {},
    files = ExtractArchive[path, dirPath, CreateIntermediateDirectories -> False];
    If[!ListQ[files],
      Message[ArchiveInnerFile::corrupt, path];
      Return @ $Failed]
  ];
  Switch[files,
    {},  Message[ArchiveInnerFile::empty, path]; $Failed,
    {_}, First @ files,
    _,   Message[ArchiveInnerFile::ambiguous, path]; $Failed
  ]
];

(**************************************************************************************************)

SetStrict[ImportLines]

ImportLines[file_Str]        := importLines[file];
ImportLines[file_Str, n_Int] := importLines[file, n];

importLines[file_, args___] := ReadList[
  NormalizePath @ file, Record, args,
  RecordSeparators -> "\n", NullRecords -> True
];

(**************************************************************************************************)

SetStrict[ExportLines]

ExportLines[file_Str, lines_] := If[!StrVecQ[lines], $Failed,
  ExportUTF8[file, StrJoin @ Riffle[lines, "\n"]]];

(**************************************************************************************************)

SetStrict[ImportJSONString]

ImportJSONString::badJSONString = "String `` does not appear to be valid JSON.";

ImportJSONString[jsonStr_String] := Locals[
  expr = Quiet @ Check[ReadRawJSONString @ jsonStr, $Failed];
  If[FailureQ[expr], ReturnFailed["badJSONString", jsonStr]];
  expr /. Null -> None
];

(**************************************************************************************************)

General::importFileNotExists = "File to import `` does not exist.";
General::importFileCorrupt = "File to import `` is corrupt.";
importErrorMessage[head_, path_] := If[!FileExistsQ[path],
  Message[MessageName[head, "importFileNotExists"], path]; $Failed,
  Message[MessageName[head, "importFileCorrupt"], path];   $Failed
];

(**************************************************************************************************)

SetStrict[ImportJSON]

ImportJSON[path2_String] := Block[{path, expr},
  path = ArchiveInnerFile @ NormalizePath @ path2;
  expr = Quiet @ Check[ReadRawJSONFile @ path, $Failed];
  If[FailureQ[expr],
    importErrorMessage[ImportJSON, path],
    expr /. Null -> None
  ]
];

(**************************************************************************************************)

SetStrict[ExportJSON];

ExportJSON::badExpr = "Cannot convert `` to JSON for export to ``.";
ExportJSON[path_String, expr_] := Block[{jsonStr},
  jsonStr = WriteRawJSONString[expr, "Compact" -> 4];
  If[!StringQ[jsonStr], ReturnFailed["badExpr", expr, path]];
  ExportUTF8[NormalizePath @ path, StringReplace[jsonStr, "\\/" -> "/"]]
];

(**************************************************************************************************)

SetStrict[ImportMX];

ImportMX::nefile = "File `` does not exist.";
ImportMX::fail = "File `` is corrupt.";

ImportMX[path2_String] := Block[
  {System`Private`ConvertersPrivateDumpSymbol, path = NormalizePath @ path2},
  If[Or[
    FailureQ[Quiet @ Check[Get @ path, $Failed]],
    !MatchQ[System`Private`ConvertersPrivateDumpSymbol, _HoldC]],
    importErrorMessage[ImportMX, path],
    First @ System`Private`ConvertersPrivateDumpSymbol
  ]
];

(**************************************************************************************************)

SetStrict[ExportMX];

ExportMX::fail = "Could not write expression to ``.";
ExportMX[path2_String, expr_] := Block[
  {System`Private`ConvertersPrivateDumpSymbol = HoldC[expr],
   path = NormalizePath @ path2},
  If[FailureQ @ Quiet @ Check[
    DumpSave[path, System`Private`ConvertersPrivateDumpSymbol], $Failed],
    Message[ExportMX::fail, path]; $Failed,
    path2
  ]
];

(**************************************************************************************************)

SetStrict[ImportUTF8];

ImportUTF8[path2_String] := Block[{path, bytes},
  path = NormalizePath @ path2;
  bytes = Quiet @ Check[ReadByteArray @ path, $Failed];
  Switch[bytes,
    EndOfFile,  "",
    _ByteArray, ByteArrayToString @ bytes,
    _,          importErrorMessage[ImportUTF8, path]
  ]
];

(**************************************************************************************************)

SetStrict[ExportUTF8];

ExportUTF8::openFailure = "Cannot open `` for writing.";
ExportUTF8::writeFailure = "Cannot write to ``.";

ExportUTF8[path2_String ? PrintableASCIIQ, string_String] := Module[
  {path = NormalizePath @ path2, stream},
  stream = OpenWrite[path, BinaryFormat -> True];
  If[FailureQ[stream],
    Message[ExportUTF8::openFailure, path];
    Return @ $Failed];
  WithLocalSettings[
    Null,
    Check[
      BinaryWrite[path, StringToByteArray[string]],
      Message[ExportUTF8::writeFailure, path];
      $Failed
    ],
    Close[stream];
  ]
];

$tempExportFile := $tempExportFile = NewTemporaryFilename["utf_export"];

(* TODO: gate on macOS *)
(* works around failure on e.g. ExportUTF8["~/𝖢𝖺𝗍.txt", "..."] on macOS *)
ExportUTF8[path_String, string_String] := (
  If[FailureQ @ ExportUTF8[$tempExportFile, string], $Failed,
  If[FailureQ @ RunCommand["mv", $tempExportFile, NormalizePath @ path], $Failed, path]]
);

(*************************************************************************************************)

SetStrict[ImportStringTable]

ImportStringTable::invalidSymbolTable = "`` does not contain a valid symbol table.";

ImportStringTable[path_Str] := Module[{text},
  text = ImportUTF8 @ path;
  If[!StrQ[text],
    Message[ImportStringTable::invalidSymbolTable, path];
    Return @ $Failed];
  PairsToDict @ Map[
    StringSplit /* FirstRest,
    StringLines @ StringReplace[text, "\n\t" -> " "]
  ]
];

(*************************************************************************************************)

SetStrict[StringTableString]

Options[StringTableString] = {"Sort" -> True, "Split" -> 100};

StringTableString[dict_Dict, OptionsPattern[]] := Module[
  {kvs, split, rowFn, lines},
  kvs = {Map[ToStr, Keys @ dict], Map[ToStr, Vals @ dict, {2}]};
  If[!StrVecQ[P1 @ kvs] || !VecQ[P2 @ kvs, StrVecQ], Return @ $Failed];
  If[OptionValue["Sort"], kvs //= sortKeysVals];
  split = OptionValue["Split"];
  rowFn = If[IntQ @ split,
    mulLineRow[split - 4],
    oneLineRow[Max @ StrLen @ First @ kvs]
  ];
  lines = MapThread[rowFn, kvs];
  str = StrJoin @ Riffle[lines, NL];
  If[StrQ @ str, str, $Failed]
];

sortKeysVals[{keys_, vals_}] := Module[
  {ord = Ordering @ keys},
  List[
    Part[keys, ord],
    Sort /@ Part[vals, ord]
  ]
];

oneLineRow[m_][k_, vs_] := List[
  StrPadR[k, m + 1],
  Riffle[vs, " "]
];

mulLineRow[m_][k_, {}] := {k};
mulLineRow[m_][k_, vs_] := Module[
  {c, v, line1, lineR, nlt = "\n\t"},
  v = First @ vs;
  line1 = {k, nlt, v}; c = StrLen[v];
  lineR = Table[
    v = Part[vs, i];
    n = StrLen @ v;
    c += n + 1;
    If[c >= m, c = 1; {nlt, v}, {Spc, v}]
  ,
    {i, 2, Len @ vs}
  ];
  {line1, lineR}
];

(*************************************************************************************************)

SetStrict[ExportStringTable]

Options[ExportStringTable] = Options[StringTableString];

ExportStringTable::invalidData = "`` is not an association from strings to lists of strings.";

ExportStringTable[path_Str, dict_Dict, opts:OptionsPattern[]] := Module[
  {str = StringTableString[dict, opts]},
  If[!StrQ[str],
    Message[ExportStringTable::invalidData, dict]; $Failed,
    ExportUTF8[path, str]
  ]
];

(**************************************************************************************************)

SetStrict[NewTemporaryFilename];

$tempDir := $tempDir = Module[{dir = FileNameJoin[{$TemporaryDirectory, "WL"}]},
  If[!FileExistsQ[dir], CreateDirectory[dir]]; dir];

NewTemporaryFilename[file_String] := PathJoin[$tempDir, file];

NewTemporaryFilename[file_String] /; StringContainsQ[file, "#"] :=
  NewTemporaryFilename @ StrRep[file, "#" -> uniqueSessionStr[]]

(**************************************************************************************************)

uniqueSessionStr[] := StrJoin[IntegerString[$ProcessID], "_", IntegerString[UniqueSessionID[], 10, 5]];

(**************************************************************************************************)

RunCommand::noOutput = "Command `` terminated without output, see ``.";

SetStrict[RunCommand];

RunCommand[binary:(_String | File[_String]), rawArgs___] := Module[
  {binPath, args, cmdStr, cmdFile, outFile, errFile, argStr, exitCode},

  binPath = If[StringQ[binary], FindBinary @ binary, NormalizePath @ First @ binary];
  If[!StringQ[binPath], Return @ $Failed];
  args = procCommandArg /@ {rawArgs};
  cmdStr = StringRiffle[Flatten[{binPath, args}], " "];

  cmdFile = NewTemporaryFilename[RandomBase36String[6] <> ".sh"];
  outFile = cmdFile <> ".out.txt";
  errFile = cmdFile <> ".err.txt";
  argStr = StringJoin[cmdStr, " 1>", outFile, " 2>", errFile];
  Export[cmdFile, argStr, "Text", CharacterEncoding -> "UTF-8"];

  exitCode = Run["/bin/bash -e " <> cmdFile];
  If[exitCode != 0, Return @ $Failed];

  If[!FileExistsQ[outFile],
    Message[RunCommand::noOutput, cmd, cmdFile];
    $Failed
  ,
    ImportUTF8[outFile]
  ]
];

(**************************************************************************************************)

SetStrict[ShellEscape];

ShellEscape[s_String] := If[
  StringMatchQ[s, RegularExpression["[a-zA-Z_-]+"]], s,
  StringJoin["'", StringReplace[s, {"'" -> "\\'", "\\" -> "\\\\"}], "'"]
];

(**************************************************************************************************)

(* TODO: scrape zshell etc *)
SetInitial[$BinaryPaths, {
    "/opt/homebrew/bin", "/usr/local/bin", "/usr/bin", "/usr/sbin", "/sbin", "/bin",
   $HomeDirectory, "/Applications",
   PathJoin[$HomeDirectory, "/Applications"]
}];

(**************************************************************************************************)

General::binaryNotFound = "Binary \"``\" is not present in any of the normal binary paths. Please install it.";

SetStrict[FindBinary];

FindBinary[name_String] := FindBinary[name] = SelectFirst[
  PathJoin[#, name]& /@ $BinaryPaths,
  FileExistsQ,
  Message[General::binaryNotFound, name]; $Failed
];

(**************************************************************************************************)

SetStrict[RunAppleScript]

RunAppleScript[cmd_String] := Module[{file},
  file = NewTemporaryFilename["applescript.#.scpt"];
  If[FailureQ @ ExportUTF8[file, cmd], Return @ $Failed];
  If[Run["osascript " <> file] === 0, Null, $Failed]
];

(*************************************************************************************************)

CopyImageToClipboard[expr_] := (
  CopyToClipboard @ ImagePad[Rasterize[expr, ImageFormattingWidth -> Inf, ImageResolution -> 144], 20, White];
  expr
);

(**************************************************************************************************)

SetStrict[CopyTextToClipboard]

(* TODO: undertake replacements of $ScriptLetters codepoints which mathematica substitutes for private use ones *)
CopyTextToClipboard[text_Str] := Module[{path, cmd},
  path = NewTemporaryFilename["clipboard.#.txt"];
  If[FailureQ @ ExportUTF8[path, text], Return @ $Failed];
  cmd = StrJoin["set the clipboard to ( do shell script \"cat ", path, "\" )"];
  If[RunAppleScript[cmd] === Null, text, $Failed]
];

(**************************************************************************************************)

ToJSON[e_] := ToJSON[e, True];
ToJSON[e_, compact_] := WriteRawJSONString[e, "Compact" -> compact];

$rawPlaceholder = "\[FormalZ]\[FormalZ]";
$rawPlaceholder2 = "\"" <> $rawPlaceholder <> "\"";

ToJSON[e_, compact_] /; VContainsQ[e, RawString] := Block[
  {$rawStringDict = Bag[], $i = 1},
  StringReplace[
    WriteRawJSONString[e, "Compact" -> True, "ConversionFunction" -> handleJSONRawString],
    $rawPlaceholder2 :> BagPart[$rawStringDict, $i++]
  ]
];

handleJSONRawString[RawString[raw_Str]] := (StuffBag[$rawStringDict, raw]; $rawPlaceholder);

(**************************************************************************************************)

SetStrict @ LoadSystemData;

LoadSystemData[name_Str] /; StrEndsQ[name, ".mx"] := Block[
  {path = DataPath["System", name]},
  If[FileExistsQ[path],
    ImportMX @ path,
    GenerateSystemData @ StrDrop[name, -1]
  ]
];

SetStrict @ GenerateSystemData;

GenerateSystemData::genFileNotExists = "Generator file `` does not exist.";
GenerateSystemData::genFileBadOutput = "Generator file `` produced a bad result: ``.";

GenerateSystemData[name_Str] /; StrEndsQ[name, ".m"] := Module[{path, result, iresult},
  path = DataPath["System", name];
  If[!FileExistsQ[path],
    Message[GenerateSystemData::genFileNotExists, path];
    Return @ $Failed];
  result = BlockContext["DummyContext`", iresult = Check[Get @ path, $Failed]];
  If[FailureQ[result] || result === Null,
    Message[GenerateSystemData::genFileBadOutput, path, iresult];
    Return @ $Failed];
  ExportMX[path <> "x", result];
  result
];

