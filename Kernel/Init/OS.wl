SystemExports[
  "Function",
    PathJoin, NormalizePath, NewTemporaryFilename, ShellEscape,
  "Head",
    FileList,
  "IOFunction",
    EnsureDirectory, FileData, CopyLatestFile, MoveFile, FileNameSafe, CopyFileSafe, TemporaryFileCopy, ExtractArchiveFile,
    RunAppleScript, RunCommand, FindBinary, EnsureReadableFile,
    CopyImageToClipboard, CopyTextToClipboard,
  "Variable",
    $BinaryPaths
];

PackageExports[
  "Predicate",   ForbiddenPathQ, ArchiveFileQ
];

PrivateExports[
  "MetaFn",      DefineImportFn,
  "Function",    DataPath, ImportFnHelper, ImportStreamHelper, ExportStreamHelper,
  "ControlFlow", QuietCheckCorrupt
];

SessionExports[
  "Variable", $TempDir
];

(**************************************************************************************************)

SetStrict[PathJoin, DataPath]

PathJoin[s__Str] := FileNameJoin @ {s};
PathJoin[s__Str, FileList[list_List]] := Map[FileNameJoin, Thread @ {PathJoin @ s, list}];
PathJoin[s__Str, FileList[patt_Str]]  := FileNames[patt, PathJoin @ s];

DataPath[s_Str] := PathJoin[$CoreToolsRootPath, "Data", s]; (* TODO: replace / with $PathnameSeparator on Windows *)
DataPath[spec___] := PathJoin[$CoreToolsRootPath, "Data", spec];

(**************************************************************************************************)

SetStrict @ FileNameSafe;

FileNameSafe[path_Str] := StringDelete[FileNameTake @ str, $unsafeCharRE];

$unsafeCharRE = RegularExpression["[^a-zA-Z0-9._]"];

(**************************************************************************************************)

ForbiddenPathQ[path_Str] := !StrStartsQ[path, {$HomeDirectory, $TemporaryDirectory}];
ForbiddenPathQ[_]        := True;

(**************************************************************************************************)

SetStrict @ EnsureDirectory;

EnsureDirectory[path_Str ? ForbiddenPathQ] := ThrowMsg["unsafeDir", path];
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

SetStrict[NewTemporaryFilename];

$TempDir := $TempDir = Module[{dir = FileNameJoin[{$TemporaryDirectory, "WL"}]},
  If[!FileExistsQ[dir], CreateDirectory[dir]]; dir];

NewTemporaryFilename[file_String] := PathJoin[$TempDir, file];

NewTemporaryFilename[file_String] /; StringContainsQ[file, "#"] :=
  NewTemporaryFilename @ StrRep[file, "#" -> UniqueSessionID[]]

(**************************************************************************************************)

SetStrict @ FileData;

FileData[path_Str]             := toFileDataAssoc @ sysFileInfo @ path;
FileData[path_Str, prop_Str]   := sysFileInfo[path, prop];
FileData[path_Str, props_List] := toFileDataProps[sysFileInfo[path, props], props];

toFileDataAssoc[l:{___Rule}]   := Dict @ DeleteCases[l, _Missing];
toFileDataAssoc[_]             := $Failed;

toFileDataProps[data:RuleVecP, keys_] := Lookup[data, keys];
toFileDataProps[_, _]                 := $Failed;

sysFileInfo[path_, args___] := FastQuietCheck[
  FileInformation[NormalizePath @ src, prop],
  $Failed
];

(**************************************************************************************************)

SetStrict[CopyLatestFile, syncFiles]

CopyLatestFile[src_Str, dst_Str] := syncFiles[NormalizePath @ src, NormalizePath @ dst];

syncFiles[src_Str, dst_Str] := Which[
  !FileExistsQ[src],
    Message[CopyLatestFile::missing, File @ src]; $Failed,
  System`Private`NewerFileDate[src, dst],
    copyFile[src, dst]; src,
  True,
    copyFile[dst, src]; dst
];

CopyLatestFile::missing = "Source file does not exist: ``.";

(**************************************************************************************************)

SetStrict[MoveFile, CopyFileSafe]

MoveFile[src_Str, dst_Str]     := moveFile[NormalizePath @ src, NormalizePath @ dst];
CopyFileSafe[src_Str, dst_Str] := copyFile[NormalizePath @ src, NormalizePath @ dst];

(**************************************************************************************************)

SetStrict[moveFile, copyFile, moveOrCopyFile]

moveFile[src_Str, dst_Str] := moveOrCopyFile[src, dst, RenameFile, "mv"];
copyFile[src_Str, dst_Str] := moveOrCopyFile[src, dst, CopyFile, "cp"];

moveOrCopyFile[src_Str, dst_Str, wlFn_Sym, shellFn_Str] := Which[
  src === dst,
    dst,
  PrintableASCIIQ[src] && PrintableASCIIQ[dst],
    wlFn[src, dst, OverwriteTarget -> True],
  !FailureQ[RunCommand[shellFn, src, dst]] && FileExistsQ[dst],
    dst,
  True,
    Message[General::moveCopyFailed, File @ src, File @ dst]; $Failed
];

General::moveCopyFailed = "Could not move or copy `` to ``.";

(**************************************************************************************************)

SetStrict[TemporaryFileCopy, tempFileCopy];

TemporaryFileCopy[path_Str] := tempFileCopy @ NormalizePath @ path;

tempFileCopy[path_Str] := Module[
  {copyPath = NewTemporaryFilename["copy_#_" <> safeBits[FileNameTake @ path]]},
  If[!FailureQ[copyFile[path, copyPath]] && FileExistsQ[copyPath],
    copyPath,
    Message[TemporaryFileCopy::tempCopy, File @ path]; $Failed
  ]
];

TemporaryFileCopy::tempCopy = "Could not make temporary copy of ``.";

(**************************************************************************************************)

SetStrict[EnsureReadableFile, ensureReadabale]

EnsureReadableFile[pathArg_Str] := ensureReadabale @ NormalizePath @ pathArg;

ensureReadabale[path_Str ? PrintableASCIIQ] := path;
ensureReadabale[path_Str] := tempFileCopy @ path;

(**************************************************************************************************)

$archiveExts = {".zip", ".gz", ".gzip", ".tar", ".tar.gz", ".bzip2", ".zstd", ".7z", ".rar", ".iso"};

ArchiveFileQ[path_String] := StringEndsQ[path, $archiveExts, IgnoreCase -> True];
ArchiveFileQ[_]           := False;

(**************************************************************************************************)

SetStrict[ExtractArchiveFile];

ExtractArchiveFile[pathArg_Str] := Locals[
  path = EnsureReadableFile @ pathArg;
  If[ValidFileQ[path],
    extractArchive[ExtractArchiveFile, path]
  ,
    Message[ExtractArchiveFile::missingArchive, File @ pathArg];
    $Failed
  ]
];

General::missingArchive = "Archive file `` does not exist."

(**************************************************************************************************)

archivePath[dirName_] := EnsureDirectory @ DataPath["Cache", "Archive", dirName];

extractArchive[head_, path_] := Module[
  {hashStr, dirName, dirPath, files},
  hashStr = Base36String @ FileHash @ path;
  dirName = FileBaseName[path] <> "." <> hashStr;
  dirPath = archivePath @ dirName;
  If[DirectoryQ[dirPath],
    files = FileNames[All, dirPath, Infinity];
    CachePrint["Found open archive with ", Len[files], " at ", File @ dirPath];
    If[Len[files] === 1, Return @ First @ files];
    DeleteDirectory[dirPath, DeleteContents -> True];
  ];
  CreateDirectory @ dirPath;
  CachePrint["Extracting ", path, " to ", dirPath];
  files = ExtractArchive[path, dirPath, CreateIntermediateDirectories -> False];
  If[!ListQ[files],
    Message[head::corruptArchive, File @ path];
    Return @ $Failed
  ];
  CachePrint["Obtained ", Len @ files, " files"];
  Switch[Len @ files,
    1, First @ files,
    0, Message[head::emptyArchive, File @ path]; $Failed,
    _, Message[head::multipleArchiveFiles, File @ path, File /@ files]; $Failed
  ]
];

General::corruptArchive = "Archive file `` is corrupt."
General::emptyArchive = "Archive file `` was empty."
General::multipleArchiveFiles = "Archive file `` expanded to multiple files: ``.";

(**************************************************************************************************)

SetHoldC @ QuietCheckCorrupt;

QuietCheckCorrupt[body_] := QuietCheck[body, $Corrupt];

(**************************************************************************************************)

SetStrict @ DefineImportFn;

DefineImportFn[extFn_Sym, intFn_Sym] := Then[
  _ifn           := $Invalid;
  extFn[args___] := ImportFnHelper[extFn, intFn, args];
];

ImportFnHelper[extFn_, intFn_, pathArg_, args___] := Module[
  {path, type, result},
  If[!StringQ[pathArg], Message[extFn::importArg1, File @ pathArg]; Return @ $Failed];
  path = EnsureReadableFile @ pathArg;
  type = FileType @ path;
  If[type === None,      Message[extFn::importFileMissing, File @ pathArg]; Return @ $Failed];
  If[type === Directory, Message[extFn::importFileDir,     File @ pathArg]; Return @ $Failed];
  If[ArchiveFileQ[path],
    path = extractArchive[extFn, path];
    If[FailureQ[path], Return @ $Failed];
  ];
  result = intFn[path, args];
  Switch[result,
    $Invalid,  Message[extFn::importBadArgs, HoldForm @ extFn[pathArg, args]]; $Failed,
    $Corrupt,  Message[extFn::importFileCorrupt, File @ pathArg]; $Failed,
    _,         result
  ]
];

General::importArg1        = "Import file should be a path: ``.";
General::importFileMissing = "Import file does not exist: ``.";
General::importDir         = "Import file is a directory: ``.";
General::importBadArgs     = "Bad arguments to import: ``.";
General::importFileCorrupt = "Import file is corrupt: ``.";

(**************************************************************************************************)

SetStrict[ExportStreamHelper];

ExportStreamHelper[head_Symbol, filePath_Str, streamFn_] := Module[
  {streamPath = $outputStreamPath, result = None, streamPos = None},
  If[FileExistsQ[streamPath], DeleteFile[streamPath]];
  stream = OpenWrite[streamPath, BinaryFormat -> True];
  If[Head[stream] =!= OutputStream,
    Message[head::streamWriteOpen, File @ streamPath];
    Return @ $Failed
  ];
  result = WithLocalSettings[
    Null,
    Check[streamFn[stream]; streamPos = StreamPosition @ stream, $Corrupt],
    Close[stream];
  ];
  If[MatchQ[result, $Corrupt | $Failed],
    Message[head::streamWriteError, File @ streamPath];
    Return @ $Failed
  ];
  If[!PosIntQ[streamPos] || !FileExistsQ[streamPath],
    Message[head::streamWriteEmpty, File @ streamPath];
    Return @ $Failed
  ];
  MoveFile[streamPath, filePath]
];

General::streamWriteOpen = "Cannot open `` for writing.";
General::streamWriteError = "Error while writing to ``.";
General::streamWriteEmpty = "No data written to ``.";

$outputStreamPath := $outputStreamPath = NewTemporaryFilename["tmp_output_stream"];

(**************************************************************************************************)

SetStrict[ImportStreamHelper];

ImportStreamHelper[head_Symbol, filePath_Str, streamFn_] := Module[
  {streamPath = ensureReadable @ filePath, result},
  stream = OpenRead[streamPath, BinaryFormat -> True];
  If[Head[stream] =!= InputStream,
    Message[head::streamReadOpen, File @ streamPath];
    Return @ $Failed
  ];
  WithLocalSettings[
    Null,
    Check[streamFn[stream], $Corrupt],
    Close[stream];
  ]
];

General::streamReadOpen = "Cannot open `` for reading.";

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
    Message[RunCommand::noOutput, cmd, File @ cmdFile];
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


