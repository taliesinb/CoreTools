SystemExports[
  "Function",
    PathJoin, NormalizePath, NewTemporaryFilename, ShellEscape, ToJSON,
  "Head",
    FileList, RawString,
  "IOFunction",
    ImportJSONString,
    ImportLines, ImportJSON, ImportMX, ImportUTF8, ImportStringTable,
    ExportLines, ExportJSON, ExportMX, ExportUTF8, ExportStringTable,
    RunAppleScript, RunCommand, FindBinary,
    CopyImageToClipboard, CopyTextToClipboard,
  "Variable",
    $BinaryPaths
];

PrivateExports[
  "Function",
    DataPath
];

(**************************************************************************************************)

DeclareStrict[PathJoin, DataPath]

PathJoin[s__Str] := FileNameJoin @ {s};
PathJoin[s__Str, FileList[list_List]] := Map[FileNameJoin, Thread @ {PathJoin @ s, list}];
PathJoin[s__Str, FileList[patt_Str]]  := FileNames[patt, PathJoin @ s];

DataPath[s_Str] := PathJoin[$CoreToolsRootPath, "Data", s]; (* TODO: replace / with $PathnameSeparator on Windows *)
DataPath[spec___] := PathJoin[$CoreToolsRootPath, "Data", spec];

(**************************************************************************************************)

DeclareStrict[NormalizePath]

NormalizePath[""]       := "";
NormalizePath[None]     := None;
NormalizePath[path_Str] := accentsToMarks @ StringReplaceRepeated[path, $pathNormalizationRules];

$pathNormalizationRules = {
  StartOfString ~~ "~" :> $HomeDirectory,
  $PathnameSeparator ~~ Except[$PathnameSeparator].. ~~ $PathnameSeparator ~~ ".." ~~ $PathnameSeparator :> $PathnameSeparator,
  $PathnameSeparator ~~ EndOfString :> "",
  $PathnameSeparator ~~ "." :> ""
};

(* this is important for macOS, which does this implicitly when you use Finder *)
accentsToMarks[str_ ? PrintableASCIIQ] := str;
accentsToMarks[str_] := CharacterNormalize[str, "NFC"];

(**************************************************************************************************)

DeclareStrict[ImportLines]

ImportLines[file_Str] := ReadList[NormalizePath @ file, Record, RecordSeparators -> "\n", NullRecords -> True]
ImportLines[file_Str, n_Int] := ReadList[NormalizePath @ file, Record, n, RecordSeparators -> "\n", NullRecords -> True];

(**************************************************************************************************)

DeclareStrict[ExportLines]

ExportLines[file_Str, lines_] := If[!StrVecQ[lines], $Failed,
  ExportUTF8[file, StrJoin @ Riffle[lines, "\n"]]];

(**************************************************************************************************)

DeclareStrict[ImportJSONString]

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

DeclareStrict[ImportJSON]

ImportJSON[path2_String] := Block[{path, expr},
  path = NormalizePath @ path2;
  expr = Quiet @ Check[ReadRawJSONFile @ path, $Failed];
  If[FailureQ[expr],
    importErrorMessage[ImportJSON, path],
    expr /. Null -> None
  ]
];

(**************************************************************************************************)

DeclareStrict[ExportJSON];

ExportJSON::badExpr = "Cannot convert `` to JSON for export to ``.";
ExportJSON[path_String, expr_] := Block[{jsonStr},
  jsonStr = WriteRawJSONString[expr, "Compact" -> 4];
  If[!StringQ[jsonStr], ReturnFailed["badExpr", expr, path]];
  ExportUTF8[NormalizePath @ path, StringReplace[jsonStr, "\\/" -> "/"]]
];

(**************************************************************************************************)

DeclareStrict[ImportMX];

ImportMX::nefile = "File `` does not exist.";
ImportMX::fail = "File `` is corrupt.";

ImportMX[path2_String] := Block[
  {System`Private`ConvertersPrivateDumpSymbol, path = NormalizePath @ path2},
  If[FailureQ[Quiet @ Check[Get @ path, $Failed]] || !MatchQ[System`Private`ConvertersPrivateDumpSymbol, _HoldComplete],
    importErrorMessage[ImportMX, path],
    First @ System`Private`ConvertersPrivateDumpSymbol
  ]
];

(**************************************************************************************************)

DeclareStrict[ExportMX];

ExportMX::fail = "Could not write expression to ``.";
ExportMX[path2_String, expr_] := Block[
  {System`Private`ConvertersPrivateDumpSymbol = HoldComplete[expr], path = NormalizePath @ path2},
  If[FailureQ @ Quiet @ Check[DumpSave[path, System`Private`ConvertersPrivateDumpSymbol], $Failed],
    Message[ExportMX::fail, path]; $Failed,
    path2
  ]
];

(**************************************************************************************************)

DeclareStrict[ImportUTF8];

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

DeclareStrict[ExportUTF8];

ExportUTF8::openFailure = "Cannot open `` for writing.";
ExportUTF8::writeFailure = "Cannot write to ``.";

ExportUTF8[path2_String ? PrintableASCIIQ, string_String] := Module[
  {path = NormalizePath @ path2, stream},
  stream = OpenWrite[path, BinaryFormat -> True];
  If[FailureQ[stream], ReturnFailed["openFailure", path]];
  Internal`WithLocalSettings[
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

DeclareStrict[ImportStringTable]

ImportStringTable::invalidSymbolTable = "`` does not contain a valid symbol table.";

ImportStringTable[path_Str] := Locals[
  text = ImportUTF8 @ path;
  If[!StrQ[text], ReturnFailed["invalidSymbolTable", path]];
  PairsToAssoc @ Map[
    StringSplit /* FirstRest,
    StringLines @ text
  ]
];

(*************************************************************************************************)

DeclareStrict[ExportStringTable]

ExportStringTable::invalidData = "`` is not an association from strings to lists of strings.";

Options[ExportStringTable] = {"Sort" -> False};

ExportStringTable[path_Str, assoc_Assoc, OptionsPattern[]] := Locals[
  sort = OptionValue["Sort"];
  len = Max @ StrLen @ Keys @ assoc;
  If[!IntQ[len], ReturnFailed["invalidData", assoc]];
  str = StrJoin @ KeyValueMap[
    {StringPadRight[#1, len + 1], Riffle[#2, " "], "\n"}&,
    If[sort, KeySort /* Map[Sort], Id] @ assoc
  ];
  If[!StrQ[str], ReturnFailed["invalidData", assoc]];
  ExportUTF8[path, str]
];

(**************************************************************************************************)

DeclareStrict[NewTemporaryFilename];

$tempDir := $tempDir = Module[{dir = FileNameJoin[{$TemporaryDirectory, "WL"}]},
  If[!FileExistsQ[dir], CreateDirectory[dir]]; dir];

NewTemporaryFilename[file_String] := PathJoin[$tempDir, file];

NewTemporaryFilename[file_String] /; StringContainsQ[file, "#"] :=
  NewTemporaryFilename @ StrRep[file, "#" -> uniqueSessionStr[]]

(**************************************************************************************************)

uniqueSessionStr[] := StrJoin[IntStr[$ProcessID], "_", IntStr[UniqueSessionID[], 10, 5]];

(**************************************************************************************************)

RunCommand::noOutput = "Command `` terminated without output, see ``.";

DeclareStrict[RunCommand];

RunCommand[binary:(_String | File[_String]), rawArgs___] := Locals[

  binPath = If[StringQ[binary], FindBinary @ binary, NormalizePath @ First @ binary];
  If[!StringQ[binPath], ReturnFailed[]];
  args = procCommandArg /@ {rawArgs};
  cmdStr = StringRiffle[Flatten[{binPath, args}], " "];

  cmdFile = NewTemporaryFilename[RandomBase36String[6] <> ".sh"];
  outFile = cmdFile <> ".out.txt";
  errFile = cmdFile <> ".err.txt";
  argStr = StringJoin[cmdStr, " 1>", outFile, " 2>", errFile];
  Export[cmdFile, argStr, "Text", CharacterEncoding -> "UTF-8"];

  exitCode = Run["/bin/bash -e " <> cmdFile];
  If[exitCode != 0, ReturnFailed[]];

  If[!FileExistsQ[outFile],
    ReturnFailed["toolNoOutput", cmd, cmdFile],
    ImportUTF8[outFile]
  ]
];

(* procCommandArg = CaseOf[
  ignored        := Nothing;
  _ -> ignored   := Nothing;
  k_String -> v_ := k <> "=" <> %[v];
  v_String       := ShellEscape @ v;
  v_Integer      := IntegerString @ v;
  r_Real         := TextString @ r;
  r_Rational     := % @ N @ r;
  False          := "false";
  True           := "true";
  File[f_]       := % @ NormalizePath @ f;
  v_             := (Message[General::badCommandArgument, v]; ThrowError[]);
,
  {ignored -> Automatic | None}
]; *)

(**************************************************************************************************)

DeclareStrict[ShellEscape];

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

DeclareStrict[FindBinary];

FindBinary[name_String] := FindBinary[name] = SelectFirst[
  PathJoin[#, name]& /@ $BinaryPaths,
  FileExistsQ,
  Message[General::binaryNotFound, name]; $Failed
];

(**************************************************************************************************)

DeclareStrict[RunAppleScript]

RunAppleScript[cmd_String] := Locals[
  file = NewTemporaryFilename["applescript.#.scpt"];
  ExportUTF8[file, cmd];
  If[Run["osascript " <> file] === 0, Null, $Failed]
];

(*************************************************************************************************)

CopyImageToClipboard[expr_] := (
  CopyToClipboard @ ImagePad[Rasterize[expr, ImageFormattingWidth -> Inf, ImageResolution -> 144], 20, White];
  expr
);

(**************************************************************************************************)

DeclareStrict[CopyTextToClipboard]

(* TODO: undertake replacements of $ScriptLetters codepoints which mathematica substitutes for private use ones *)
CopyTextToClipboard[text_Str] := Locals[
  path = NewTemporaryFilename["clipboard.#.txt"];
  ExportUTF8[path, text];
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



