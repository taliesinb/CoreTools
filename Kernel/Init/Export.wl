SystemExports[
  "IOFunction",
    ExportStream, ExportJSONString,
    ExportLines, ExportJSON, ExportMX, ExportUTF8, ExportBytes,
    ExportStringTable, StringTableString,
  "Head",
    LiteralJSONString
];

(**************************************************************************************************)

SetStrict @ ExportStream

ExportStream[pathArg_Str, streamFn_] :=
  ExportStreamHelper[ExportStream, pathArg, streamFn];

(**************************************************************************************************)

SetStrict[ExportLines]

ExportLines[file_Str, lines_] := If[!StrVecQ[lines], $Failed,
  ExportUTF8[file, StrJoin @ Riffle[lines, "\n"]]
];

(**************************************************************************************************)

SetStrict[ExportJSON];

ExportJSON::badExpr = "Cannot convert `` to JSON for export to ``.";
ExportJSON[path_String, expr_] := Block[{jsonStr},
  jsonStr = WriteRawJSONString[expr, "Compact" -> 4];
  If[!StringQ[jsonStr], ReturnFailed["badExpr", expr, path]];
  ExportUTF8[path, StringReplace[jsonStr, "\\/" -> "/"]]
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

SetStrict @ ExportBytes;

ExportBytes[pathArg_String, bytes_ByteArray] :=
  ExportStreamHelper[ExportBytes, pathArg, BinaryWrite[#, bytes]&];

(**************************************************************************************************)

SetStrict[ExportUTF8];

ExportUTF8::openFailure = "Cannot open `` for writing.";
ExportUTF8::writeFailure = "Cannot write to ``.";

ExportUTF8[pathArg_String, string_String] :=
  ExportStreamHelper[ExportUTF8, pathArg, BinaryWrite[#, StringToByteArray @ string]&];

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

(**************************************************************************************************)

ExportJSONString[expr_, compact_:True] := exportJSON[expr, compact];

exportJSON[expr_, compact_] := WriteRawJSONString[expr, "Compact" -> compact];
exportJSON[expr_, compact_] /; VContainsQ[e, LiteralJSONString] := Block[
  {$rawStringDict = Bag[], $i = 1},
  StringReplace[
    WriteRawJSONString[expr, "Compact" -> True, "ConversionFunction" -> handleJSONRawString],
    $rawPlaceholder2 :> BagPart[$rawStringDict, $i++]
  ]
];

$rawPlaceholder = "\[FormalZ]\[FormalZ]";
$rawPlaceholder2 = "\"" <> $rawPlaceholder <> "\"";

handleJSONRawString[LiteralJSONString[raw_Str]] := (StuffBag[$rawStringDict, raw]; $rawPlaceholder);

