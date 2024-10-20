SystemExports[
  "IOFunction",
    ImportStream, ImportJSONString,
    ImportLines, ImportJSON, ImportJSONFast, ImportMX, ImportUTF8, ImportStringTable
];

(**************************************************************************************************)

DefineImportFn[ImportStream, importStream]

(**************************************************************************************************)

DefineImportFn[ImportLines, importLines]

ImportLines[file_Str]        := importLines[file];
ImportLines[file_Str, n_Int] := importLines[file, n];

importLines[file_, arg:Repeated[_Int, {0, 1}]] :=
  QuietCheckCorrupt @ ReadList[
    file, Record, arg,
    RecordSeparators -> "\n", NullRecords -> True
  ];

(**************************************************************************************************)

SetStrict[ImportJSONString]

ImportJSONString::badJSONString = "String `` does not appear to be valid JSON.";

ImportJSONString[string_Str] := IfFailed[
  QuietCheck @ jsonPostImport @ ReadRawJSONString @ string,
  Message[ImportJSONString::badJSONString, string]; $Failed
];

jsonPostImport[expr_] := ReplaceAll[expr, Null -> None];

(**************************************************************************************************)

DefineImportFn[ImportJSON, importJSON]

importJSON[path_Str] := QuietCheckCorrupt @ jsonPostImport @ ReadRawJSONFile @ path;

(**************************************************************************************************)

DefineImportFn[ImportJSONFast, importJSONFast]

importJSONFast[path_Str] := MXCachedAs[
  ImportJSONFast,
  {path, FileUnixTime @ path}, {},
  QuietCheckCorrupt @ ReadRawJSONFile[path, "JSONObjectAsList" -> True]
];

(**************************************************************************************************)

DefineImportFn[ImportMX, importMX];

importMX[path_String] := Block[
  {System`Private`ConvertersPrivateDumpSymbol},
  If[Check[Get @ path; True, False] && MatchQ[System`Private`ConvertersPrivateDumpSymbol, _HoldC],
    First @ System`Private`ConvertersPrivateDumpSymbol,
    $Corrupt
  ]
];

(**************************************************************************************************)

DefineImportFn[ImportUTF8, importUTF8];

importUTF8[path_String] := Module[
  {bytes = QuietCheck @ ReadByteArray @ path},
  Switch[bytes,
    EndOfFile,  "",
    _ByteArray, ByteArrayToString @ bytes,
    _,          $Corrupt
  ]
];

(**************************************************************************************************)

SetStrict @ ImportStream

ImportStream[pathArg_Str, streamFn_] :=
  ImportStreamHelper[ImportStream, pathArg, streamFn];

(*************************************************************************************************)

SetStrict[ImportStringTable]

DefineImportFn[ImportStringTable, importStringTable];

importStringTable[path_Str] := Module[
  {string = importUTF8 @ path},
  If[!StrQ[string], Return @ $Corrupt];
  pairs = Map[
    StringSplit /* FirstRest,
    StringLines @ StringReplace[string, "\n\t" -> " "]
  ];
  dups = Duplicates @ Col1 @ pairs;
  If[dups =!= {}, Message[ImportStringTable::duplicatesLost, dups]];
  PairsToDict @ pairs
];

ImportStringTable::duplicatesLost = "The following keys were duplicated: ``.";


