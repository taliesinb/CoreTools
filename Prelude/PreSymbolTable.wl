BeginPackage["Prelude`"]

PackageExports[
  "SymbolicHead",
    SymbolTableRow,

  "IOFunction",
    SymbolTableFromHeaders,
    SymbolTableFromDirectives,
    PreludeSymbolTable,
    DumpPreludeSymbolTable,

  "SpecialFunction",
    SymbolTableInit,
    SymbolTableInitString,

  "Function",
    SymbolTableGroups,
    SymbolTableFind,
    SymbolTableSymbolList,
    SymbolTableSymbolLines,
    SymbolTableSymbolString,
    SymbolTableSymbolCount,

  "MessageFunction",
    WithShadowingFixup,

  "Predicate",
    SymbolTableValidQ,

  "Variable",
    $SymbolTableKinds,
    $SymbolExportFunctions
];

Begin["`Exports`Private`"]

(*************************************************************************************************)

DeclaredHere[SymbolTableRow];

SymbolTableRow::usage            = "SymbolTableRow['kind', 'path', 'context', 'symbols'].";

SymbolTableFromHeaders::usage    = "SymbolTableFromHeaders[path, context] returns a list of SymbolTableRow objects.";
SymbolTableFromDirectives::usage = "SymbolTableFromDirectives[path, context] returns a list of SymbolTableRow objects.";

PreludeSymbolTable::usage        = "PreludeSymbolTable[] obtains all prelude system symbols.";
DumpPreludeSymbolTable::usage    = "DumpPreludeSymbolTable[] writes all prelude system symbols to SystemSymbols.wl.";

SymbolTableInit::usage           = "SymbolTableInit[table]."
SymbolTableInitString::usage     = "SymbolTableInitString[table].";

SymbolTableGroups::usage         = "SymbolTableGroups[table, key].";
SymbolTableFind::usage           = "SymbolTableFind[table, strpatt].";
SymbolTableSymbolList::usage     = "SymbolTableSymbolList[table].";
SymbolTableSymbolLines::usage    = "SymbolTableSymbolLines[table].";
SymbolTableSymbolString::usage   = "SymbolTableSymbolString[table].";
SymbolTableSymbolCount::usage    = "SymbolTableSymbolCount[table].";
SymbolTableValidQ::usage         = "SymbolTableValidQ[table].";

WithShadowingFixup::usage        = "WithShadowingFixup[body].";

$SymbolTableKinds::usage         = "$SymbolTableKinds is a list of known symbol kinds for PackageExports etc.";
$SymbolExportFunctions::usage    = "$PackageDeclSymbols are things like ExportFunction.";

(*************************************************************************************************)

$thisFile = $InputFileName;

normPath[str_String] := If[StringContainsQ[str, "~"], AbsoluteFileName @ str, str];

If[!StringQ[$PreludeDir], $PreludeDir = FileNameDrop @ normPath @ $thisFile];

dataPath[name_String] := FileNameJoin[{$PreludeDir, "Data", name}];

SetAttributes[{unprotectClearAll, notSymbolTable}, {HoldAllComplete}];
unprotectClearAll[e___] := (Unprotect[e]; ClearAll[e]; HoldComplete[e]);

setStrict[sym_Symbol] := (e_sym := Message[sym::symbolTableArgs, HoldForm @ e]; $Failed);

General::symbolTableArgs = "SymbolTable function called with invalid arguments: ``.";

(*************************************************************************************************)

DeclaredHere[$SymbolTableKinds];

$kindRules = List[
  ". Special ControlFlow Declare Define Holding Debugging IO Graphics GraphicsBox Box Message Meta Mutating Scoping Package" <-> "Function",
  ". Special Box Form Data Object Pattern StringPattern Type Symbolic Field Sort Slot Type Tag" <-> "Symbol Head",
  ". Special Cache Slot Tag" <-> "Variable",
  ". Special Box Form Graphics" <-> "Option",
  "Graphics" <-> "Directive Primitive",
  "Predicate PredicateOperator Operator"
];

enumerateSymbolKinds[] = DeleteCases["SymbolicSymbol"] @ procKindSpecList @ $kindRules;

procKindSpecList[list_List] := DeleteDuplicates @ StringDelete["."] @ Flatten @ Map[procKindSpec] @ list;

procKindSpec[str_String] := StringSplit @ str;
procKindSpec[prefix_String <-> suffix_String] := Outer[StringJoin, StringSplit @ prefix, StringSplit @ suffix];

$SymbolTableKinds = enumerateSymbolKinds[]

(*************************************************************************************************)

makeSymExportNames[pref_] := Map[StringJoin, Thread @ List[pref, $SymbolTableKinds]];

$symbolExportFnNames = Flatten @ Map[makeSymExportNames, {"Export", "Private"}];

$SymbolExportFunctions = Block[{$Context = "Prelude`"}, Symbol /@  $symbolExportFnNames];

(*************************************************************************************************)

$kindAliasRules = List[
  "DebuggingFunction"   -> "DebugFn",
  "OptionSymbol"        -> "Option",
  "ControlFlowFunction" -> "ControlFlow",
  "Operator"            -> "Op",
  "Function"            -> "Fn",
  "String"              -> "Str",
  "Pattern"             -> "Pat",
  "Symbol"              -> "Sym",
  "Primitive"           -> "Prim"
];

makeDealiasRule[long_] := Module[
  {short = StringReplace[long, $kindAliasRules]},
  If[short =!= long, short -> long, Nothing]
];

$checkedKinds := $checkedKinds = Association[
  Thread[$SymbolTableKinds -> $SymbolTableKinds],
  Map[makeDealiasRule, $SymbolTableKinds]
];

(**************************************************************************************************)

setStrict @ SymbolTableFromDirectives;

SymbolTableFromDirectives[files_List, context_String] :=
  Flatten @ Map[SymbolTableFromDirectives[#, context]&, files];

SymbolTableFromDirectives[path_String, context_String] := Block[
  {$path = path, $public = context, $private = context <> "Private`",
  $aliasBag = Internal`Bag[], lines},
  lines = FindList[path, $directiveStrs, AnchoredSearch -> True];
  lines = Select[lines, StringStartsQ[$directivePrefixSP]];
  collectLineAliases @ Select[lines, StringContainsQ["->"]];
  KeyValueMap[makeCombinedGroup, Merge[parseLineExport /@ lines, Identity]]
];

makeCombinedGroup[{kind_String, context_String}, decls_List] :=
  SymbolTableRow[kind, $path, context, StringJoin @ Riffle[decls, ", "]];

parseLineExport[line_String] := Module[
  {results, privacy, kind, decl, context},
  results = StringCases[line, $directiveRegexSR, 1];
  If[!MatchQ[results, {{_, _, _}}], Return @ Nothing];
  {privacy, kind, decl} = First @ results;
  decl = StringReplace[decl, {"->" -> ",", Repeated[" ", {2, 32}] -> " "}];
  If[StringContainsQ[decl, "`"], Message[SymbolTableFromDirectives::nonLocalExport, $path, decl]];
  context = If[privacy === "Export", $public, $private];
  {kind, context} -> decl
];

$directiveStrs     = {"Export", "Private"};
$directivePrefixSP = RegularExpression["(Export|Private)[A-Z][A-Za-z0-9]+\\["];
$directiveRegexSR  = RegularExpression["(Export|Private)([A-Z][A-Za-z0-9]+)\\[([^]]+)\\]"] :> {"$1", "$2", "$3"};
$lineAliasRegex    = RegularExpression["([$a-zA-Z0-9]+) -> ([$a-zA-Z0-9]+)"];

collectLineAliases[{}] := Null
collectLineAliases[lines_List] := Module[{ruleChunks},
  ruleChunks = Flatten @ StringCases[lines, $lineAliasRegex];
  StuffBag[$aliasBag, StringJoin["DefineAliasRules[", Riffle[ruleChunks, ", "], "]"]];
];

applyAliases[] := Block[{
  $Context = $private,
  $ContextPath = {"System`", "CoreTools`", $public},
  commands = BagPart[$aliasBag, All]},
  ToExpression[commands, InputForm];
];

SymbolTableFromDirectives::nonLocalExport = "Non-local export encountered in ``: ``.";

(*************************************************************************************************)

setStrict @ SymbolTableFromHeaders;

SymbolTableFromHeaders[pathList_List, context_String] :=
  Catch[Flatten @ Map[fromHeaders[#, context]&, pathList], badHeader]

SymbolTableFromHeaders[path_String, context_String] :=
  Catch[fromHeaders[path, context], badHeader];

fromHeaders[path_, context_] := Block[
  {header, pos, $path, $public = context, $private},
  $path = normPath @ path;
  header = ReadList[$path, Record, 1, RecordSeparators -> "(*****"];
  header = First[header, Return @ {}];
  If[StringContainsQ[header, "BeginPackage"],
    $public = findDecl[header, $begin1SR, "BeginPackage"];
    If[$public =!= context, failTable["contextMatch", $public, context]];
    $private = findDecl[header, $begin2SR, "Begin"];
  ,
    $private = "`Private`";
  ];
  If[StringStartsQ[$private, "`"], $private = $public <> StringDrop[$private, 1]];
  header = DeleteCases[""|";"] @ StringTrim @ StringSplit[header, {"]", ";"}];
  Flatten @ Map[procExports0, header]
]

findDecl[str_, patt_, name_] := First[StringCases[str, patt, 1], failExports["missingDecl", name]];

$begin1SR = RegularExpression["BeginPackage\\[\"([^\"]+)\"\\]"] :> "$1";
$begin2SR = RegularExpression["Begin\\[\"([^\"]+)\"\\]"] :> "$1";

$exportFnToContext = Association[
  "SystemExports"  -> "System`",
  "SessionExports" -> "Session`",
  "PackageExports" :> $public,
  "PrivateExports" :> $private
];

$exportFnSP = Map[StringJoin[#, "["]&, Keys @ $exportFnToContext];

(*************************************************************************************************)

setStrict @ SymbolTableFromHeaders;

SymbolTableFromHeaders::unknownKind   = "Unknown kind in \"``\": ``.";
SymbolTableFromHeaders::contextMatch  = "In \"``\", BeginPackage['``'] doesn't match ``.";
SymbolTableFromHeaders::missingDecl   = "Missing `` declaration in \"``\".";
SymbolTableFromHeaders::internalError = "Bad header file \"``\": ``.";
SymbolTableFromHeaders::corruptHeader = "Corrupt header string in \"``\": ``.";
SymbolTableFromHeaders::unknownExport = "Unknown export declaration in \"``\": ``.";

failInternal[e_] := failExports["internalError", Shallow[e]];

failExports[name_String, args___] := (
  Message[MessageName[SymbolTableFromHeaders, name], $path, args];
  Throw[$Failed, badHeader]
);

(*************************************************************************************************)

Clear[procExports0, procExports1, procExports2, checkSymbols, checkKind];

procExports0[chunk_String] :=
  Apply[procExports1, StringTrim[StringSplit[chunk, "[", 2]]];

procExports1["Begin"|"BeginPackage", symbols_String] := Nothing;

procExports1[exportFn_String, symbols_String] := procExports2[
  Lookup[$exportFnToContext, exportFn, failExports["unknownExport", exportFn]],
  symbols
];

procExports2[context_String, symbols_String] := StringCases[symbols,
  $headerArgsSP :> SymbolTableRow[
    checkKind @ "$1", $path, context,
    procExports3["$2"]
  ]
];

procExports3[symbols_String] := checkSymbols @ StringTrim[
  StringReplace[symbols, Whitespace -> " "],
  {",", " ", "\n"}..
];

checkSymbols[symbols_String] := If[
  StringFreeQ[symbols, $corruptSymbolsSP], symbols,
  failExports["corruptHeader", symbols]
];

checkKind[kind_String] := Lookup[$checkedKinds, kind, failExports["unknownKind", kind]];

$corruptSymbolsSP = RegularExpression["[$[:alnum:]] +[$[:alnum:]]"];
$headerArgsSP     = RegularExpression["\"([a-zA-Z]+)\",\\s*([$a-zA-Z0-9,\\s]+)"];

e_procExports0 := failInternal[Hold @ e];
e_procExports1 := failInternal[Hold @ e];
e_procExports2 := failInternal[Hold @ e];
e_procExports3 := failInternal[Hold @ e];
e_checkSymbols := failInternal[Hold @ e];

(*************************************************************************************************)

setStrict @ SymbolTableSymbolList;
setStrict @ SymbolTableSymbolLines;
setStrict @ SymbolTableSymbolString;
setStrict @ SymbolTableSymbolCount;

getChunks[table_] := Part[table, All, 4];

SymbolTableSymbolList[table:{__SymbolTableRow}]   := Catenate @ StringSplit[getChunks @ table, ", "]
SymbolTableSymbolLines[table:{__SymbolTableRow}]  := StringJoin @ Riffle[SymbolTableSymbolList @ table, "\n"];
SymbolTableSymbolString[table:{__SymbolTableRow}] := StringJoin @ Riffle[getChunks @ table, ", "];
SymbolTableSymbolCount[table:{__SymbolTableRow}]  := Total[StringCount[getChunks @ table, ","] + 1];

(*************************************************************************************************)

setStrict @ SymbolTableFind;

SymbolTableFind[table:{__SymbolTableRow}, pattern_] := Cases[table,
  SymbolTableRow[kind_, path_, context_, str_ /; StringContainsQ[str, pattern]] :>
    {kind, path, context, StringCases[str, pattern]}
];

(*************************************************************************************************)

setStrict @ SymbolTableGroups;

SymbolTableGroups[table:{__SymbolTableRow}, by_, fn_:"List"] :=
  GroupBy[table, Flatten @ List @ gbSpec @ by, toAgg @ fn]

toAgg[None]     := Identity;
toAgg["List"]   := SymbolTableSymbolList;
toAgg["Lines"]  := SymbolTableSymbolLines;
toAgg["String"] := SymbolTableSymbolString;
toAgg["Count"]  := SymbolTableSymbolCount;
toAgg[k_String] := Map[gbKey @ k] /* Counts;
toAgg[fn_]      := fn;

gbSpec[k_]       := gbKey[k];
gbSpec[k_ -> v_] := {gbKey @ k, gbSpec @ v};

gbKey["Kind"]    := Extract[1];
gbKey["Path"]    := Extract[2];
gbKey["Context"] := Extract[3];
gbKey["File"]    := Extract[2] /* FileNameTake;
gbKey[e_]        := Message[SymbolTableGroups::notKey, e];
SymbolTableGroups::notKey = "`` is not a valid key for grouping.";

(*************************************************************************************************)

SymbolTableValidQ[table:{__SymbolTableRow}] := Module[
  {symbols, dups, info},
  symbols = SymbolTableSymbolList @ table;
  If[!AllTrue[symbols, StringFreeQ[$invalidChars]],
    Message[SymbolTableRow::containsSpaces]; Return @ False];
  If[DuplicateFreeQ[symbols], Return @ True];
  dups = Keys @ Select[Counts @ symbols, # > 1&];
  Message[SymbolTableRow::duplicateSyms, dups];
  Scan[Print, SymbolTableFind[table, WordBoundary ~~ dups ~~ WordBoundary]]; False
];

$invalidChars = {" ",",",";","\n"};

SymbolTableValidQ[e_] := (Message[SymbolTableRow::invalidTable, Shallow[e]]; False);

SymbolTableRow::invalidTable = "Not a symbol table: ``.";
SymbolTableRow::containsSpaces = "Symbol table strings contain invalid characters.";
SymbolTableRow::duplicateSyms = "The following symbols occur multiple times: ``.";

(*************************************************************************************************)

SetAttributes[WithShadowingFixup, HoldAllComplete];

WithShadowingFixup[body_] := Internal`HandlerBlock[{"Message", $ShadowMessageHandler$}, body];

$ShadowMessageHandler$[Hold[Message[MessageName[sym_, "shdw"], s1_, s2_, s3_], _]] := With[
  {name = "Global`" <> SymbolName[Unevaluated @ sym]},
  Print["Shadowing: ", HoldForm[{s1, s2, s3}]];
  If[NameQ[name], Print["Removing ", name]; Remove[name]];
];

$ShadowMessageHandler$[h_] := Null;

(*************************************************************************************************)

mapContexts[fn_, table_] :=
  KeyValueMap[fn, GroupBy[table, Extract[3], Part[#, All, 4]&]];

mapSplitContexts[fn_, clear_, create_, table_List] := Module[
  {mask = Map[needsClearQ, table]},
  List[
    mapContexts[fn[clear],  Pick[table, mask, True]],
    mapContexts[fn[create], Pick[table, mask, False]]
  ]
];

needsClearQ[SymbolTableRow["Variable" | "SpecialVariable" | "CacheVariable", ___]] := False;
needsClearQ[_] := True;

(*************************************************************************************************)

setStrict @ SymbolTableInit;

SymbolTableInit[table:{__SymbolTableRow}] := WithShadowingFixup[
  Join[Sequence @@ Flatten[mapSplitContexts[initRow, unprotectClearAll, HoldComplete, table]], 2]
];

SymbolTableInit[table_List, clearFn_] := WithShadowingFixup[
  mapContexts[initRow[clearFn], table]
];

initRow[fn_][context_, chunks_] := Block[
  {$Context = context, $ContextPath = {"System`"}},
  ToExpression[
    StringJoin["{", Riffle[chunks, ","], "}"],
    InputForm, fn
  ]
];

(*************************************************************************************************)

setStrict @ SymbolTableInitString;

SymbolTableInitString[table:{__SymbolTableRow}] := Module[
  {result},
  result = StringJoin[
    $initHeader,
    mapSplitContexts[makeInitRow, "True", "False",  table],
    "End[];"
  ];
  If[StringQ[result], result, $Failed]
];

makeInitRow[clear_][context_, symbols_] := List[
  "create[", clear, ", \"", context, "\", \"{", Riffle[symbols, ","], "}\"]\n\n"
];

$initHeader =
"Begin[\"Prelude`Private`\"];

SetAttributes[safeClearAll, HoldAllComplete];
safeClearAll[e___] := (Unprotect[e]; ClearAll[e]);

create[clear_, context_, symbols_] := Block[
  {$ContextPath = {\"System`\", context}, $Context = context},
  ToExpression[symbols, InputForm, If[clear, safeClearAll, HoldComplete]]
];

";

(*************************************************************************************************)

PreludeSymbolTable[] := Module[
  {table, declStr},
  table = SymbolTableFromHeaders[FileNames["*.wl", $PreludeDir], "Prelude`"];
  declsStr = StringJoin @ Riffle[$symbolExportFnNames, ", "];
  Append[table, SymbolTableRow["PackageDeclaration", $thisFile, "Prelude`", declsStr]]
];

(*************************************************************************************************)

DumpPreludeSymbolTable[] := Module[
  {table, declsStr, kindStr, initStr},
  table = PreludeSymbolTable[];
  If[!SymbolTableValidQ[table], Return @ $Failed];


  kindStr = StringJoin @ KeyValueMap[toKindLine] @ SymbolTableGroups[table, "Kind", "Lines"];
  initStr = SymbolTableInitString @ table;
  List[
    Export[dataPath["PreludeKinds.m"], kindStr, "String"],
    Export[dataPath["PreludeInit.m"],  initStr, "String"]
  ]
];


toKindLine[kind_, lines_] := {"(*", kind, "*)\n\n", lines, "\n\n\n"};

(*************************************************************************************************)

End[];

EndPackage[];