SystemExports[
  "IOFunction",
    SublimeUpdateSyntax,
    SublimeViewText,
    LogToSublime
];

PrivateExports[
  "Function",
    CoreToolsSymbolKinds
  "SpecialVariable",
    $SublimePackagesPath
];

(*************************************************************************************************)

DeclareHoldAllComplete[LogToSublime]

LogToSublime[expr_] := Locals[
  path = NewTemporaryFilename["sublime_log#.txt"];
  $currentPath = path;
  $currentStream = None;
  Internal`WithLocalSettings[
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
  assoc_Assoc             := sublimeExportText @ Normal @ assoc;
];

(*************************************************************************************************)

templateFile[name_]      := DataPath["SublimeSyntax", "TemplateFiles", name];
systemSymbolFile[name_]  := DataPath["SublimeSyntax", "SystemSymbols", name];
userSymbolFile[name_]    := DataPath["SublimeSyntax", "UserSymbols", name];
staticFile[name_]        := DataPath["SublimeSyntax", "StaticFiles", name];

(*************************************************************************************************)

CoreToolsSymbolKinds[] := Locals[

  coreKinds = Prelude`Packages`PackageSymbolKinds["CoreTools`"];
  If[!symbolDictQ[coreKinds], ThrowErrorMessage["badSymbols", "CoreTools`"]];

  privateKinds = GroupBy[
    Last /@ StringSplit[Names["CoreTools`Private`*"], "`"],
    If[StringContainsQ[#, "$"], "SpecialVariable", "SpecialFunction"]&
  ];
  If[!symbolDictQ[privateKinds], ThrowErrorMessage["badSymbols", "CoreTools`Private`"]];

  Block[{$preludeKindBag = Bag[],
    System`PackageExports = savePreludeExports,
    System`SystemExports = savePreludeExports},
    Get @ $PreludeInitFile;
    preludeKinds = Merge[Catenate] @ BagPart[$preludeKindBag, All];
  ];
  If[!symbolDictQ[preludeKinds], ThrowErrorMessage["badSymbols", "prelude packages"]];

  manualKinds = Assoc[
    "SpecialVariable" -> {"$CoreToolsPath", "$CoreToolsRootPath", "$PreludeInitFile"},
    "SpecialFunction" -> {"ApplyEchoSugar"}
  ];

  Merge[{coreKinds, privateKinds, preludeKinds, manualKinds}, Apply[Union]]
];

symbolDictQ[a_] := KeysValuesTrue[a, StringQ, StringVectorQ];

DeclareHoldAll[savePreludeExports];

savePreludeExports[] := Null;
savePreludeExports[kind_String, Longest[syms___Symbol], rest___] := (
  StuffBag[$preludeKindBag, kind -> MapHold[HoldSymbolName, {syms}]];
  savePreludeExports[rest]
);

(*************************************************************************************************)

cachePackageSymbolKinds["CoreTools`"] := Null;
cachePackageSymbolKinds[context_] := Locals[
  filename = StringDelete[context, "`"] <> ".txt";
  symbolKinds = Prelude`Packages`PackageSymbolKinds[context];
  ExportStringTable[userSymbolFile[filename], symbolKinds]
];

(*************************************************************************************************)

SublimeUpdateSyntax::notInstalled = "Cannot find the \"WolframLanguage\" Sublime Text package at expected location ``.";
SublimeUpdateSyntax::messagesOccurred = "Message(s) occurred during computation, aborting.";
SublimeUpdateSyntax::invalidResult = "Did not produce valid results, aborting.";

SetDelayedInitial[$SublimePackagesPath, "~/Library/Application Support/Sublime Text/Packages"];

SublimeUpdateSyntax[] := Locals[

  targetDir = NormalizePath @ PathJoin[$SublimePackagesPath, "WolframLanguage"];
  If[!DirectoryQ[targetDir], ReturnFailed["notInstalled", targetDir]];

  (* save any loaded packages (except CoreTools) to disk, they'll be reloaded below *)
  Map[cachePackageSymbolKinds, Prelude`Packages`LoadedPackages[]];

  (* load syntax groups from syntax files, and list of internal context -> symbols *)
  builtinKinds = ImportStringTable @ systemSymbolFile @ "SystemSymbolKinds.txt";
  librarySymbolFiles = userSymbolFile @ FileList["*.txt"];
  libraryKinds = Merge[ImportStringTable /@ librarySymbolFiles, Catenate];

  coreToolsSymbolsKinds = CoreToolsSymbolKinds[];
  coreSymbols = Union @ Flatten @ Values @ coreToolsSymbolsKinds;
  libraryKinds //= Map[Complement[#, coreSymbols]&];
  KeyValueMap[KeyUnionTo[libraryKinds, #1, #2]&, coreToolsSymbolsKinds];

  (* generate strings *)
  res = Check[
    internalSymbols = ImportStringTable @ systemSymbolFile["InternalSymbols.txt"];
    preludeSymbols = GroupPairs[SymbolNameMostLast /@ Select[Names["Prelude`*`*"], StringFreeQ["`Private"]]];
    JoinTo[internalSymbols, preludeSymbols];
    {builtinRegexs, builtinDefs, builtinContext} = makeGroupsRegexpsDefs[builtinKinds, "Builtin"];
    {libraryRegexs, libraryDefs, libraryContext} = makeGroupsRegexpsDefs[libraryKinds, "Library"];
    internalDefs = KeyValueMap[makeInternalSymbolDefs, internalSymbols];
    syntaxDefinition = $syntaxTemplate @ Assoc[
      "variables"        -> StringRiffle[Join[builtinRegexs, libraryRegexs], "\n"],
      "library_symbols"  -> StringRiffle[libraryDefs, "\n"],
      "library_sym"      -> libraryContext,
      "builtin_symbols"  -> StringRiffle[builtinDefs, "\n"],
      "builtin_sym"      -> builtinContext,
      "internal_symbols" -> StringRiffle[internalDefs, "\n"]
    ];
    If[!StringQ[syntaxDefinition], ReturnFailed["invalidResult"]];
    ExportUTF8[staticFile @ "WolframLanguage.sublime-syntax", syntaxDefinition];

    symbolKinds = Merge[{builtinKinds, libraryKinds}, Catenate];
    symbolKinds["InternalSymbol"] = Flatten @ KeyValueMap[StrPre[#1][#2]&, internalSymbols];
    completions = Flatten @ KeyValueMap[makeKindCompletions, symbolKinds];
    completionsJSON = ToJSON[Assoc["scope" -> "source.wolfram", "completions" -> completions], 2];
    If[!StringQ[completionsJSON], ReturnFailed["invalidResult"]];
    ExportUTF8[staticFile @ "WolframLanguage.sublime-completions", completionsJSON];
  ,
    $Failed
  ];
  If[FailureQ[res], ReturnFailed["messagesOccurred"]];

  (* write them to disk *)
  Map[
    file |-> CopyFile[file, PathJoin[targetDir, FileNameTake @ file], OverwriteTarget -> True],
    staticFile @ FileList["WolframLanguage*"]
  ]
];

$syntaxTemplate := $syntaxTemplate =
  StringFunction @ File @ templateFile["WolframLanguage.sublime-syntax"];

(*************************************************************************************************)

$contextSymbolsDefinitionTemplate := $contextSymbolsDefinitionTemplate =
  StringFunction @ ImportUTF8 @ templateFile["SymbolDefinitionsFragment.json"];

makeInternalSymbolDefs[context_, symbols_] :=
  $contextSymbolsDefinitionTemplate[name, context, escapeDollars @ StringRiffle[symbols, "|"]];

(*************************************************************************************************)

$groupPrefix = "";
makeGroupsRegexpsDefs[kinds_, prefix_] := Locals[
  $groupPrefix = prefix;
  pair = Transpose @ KeyValueMap[makeGroup0, kinds];
  Append[pair, makeSingleContext @ StringRiffle[Last @ pair, "\n"]]
];

$addPopRule =
s:"      scope:" :>
  "      pop: true\n" <> s;

$symNotFound = "
    - match: '\\s+'
    - match: '{{symbolSegment}}{{symbolEndBoundary}}'
      pop: true
      scope: variable.local.unrecognized
";

makeSingleContext[defs_] := StrJoin[StrRep[defs, $addPopRule], $symNotFound];

makeGroup0[group:"Variable" | "SpecialVariable" | "CacheVariable", names_] :=
  MapLast[
    StringReplace["- match: '{{" -> "- match: '\\${{"],
    If[!AllTrue[names, StringStartsQ["$"]],
      Message[SublimeUpdateSyntax::badVariableSymbolNames, group, Select[names, StringStartsQ["$"] /* Not]]];
    makeGroup1[group, StringDrop[names, 1]]
  ];

SublimeUpdateSyntax::badVariableSymbolNames = "Some symbols in group `` did not start with $: ``.";

makeGroup0[group_, names_] :=
  makeGroup1[group, names];

makeGroup1[group2_, names2_] := Locals[
  scope = groupToSyntaxScope[group2];
  group = $groupPrefix <> group2;
  names = DeleteDuplicates @ names2;
  If[Length[names] >= 64,
    grouped = KeySort @ GroupBy[names, StrJoin[group, "_", StringTake[#, UpTo @ 1]]&];
    grouped //= KeyMap[StringReplace["$" -> "DOLLAR"]];
    subDefs = KeyValueMap[makeLetterSubDef, grouped];
    vars = "{{" <> # <> "}}"& /@ Keys[grouped];
    finalDef = StrJoin["  ", group, ": ", makeRegexp[vars], "\n"];
    defItems = StrJoin[subDefs, finalDef] // escapeDollars;
    parseItems = StrJoin @ Map[$parseItemT[scope, #]&, Keys @ grouped];
  ,
    defItems = StrJoin["  ", group, ": ", makeRegexp[names], "\n"];
    parseItems = $parseItemT[scope, group];
  ];
  {defItems, parseItems}
];

makeRegexp[names_] := {"(?:", StringRiffle[names, "|"], ")"};

$parseItemT = StringFunction @ "
    - match: '{{#2}}{{symbolEndBoundary}}'
      scope: #1";

(*************************************************************************************************)

escapeDollars[e_] := StringReplace[e, "$" -> "\\$"];
doubleEscapeDollars[e_] := StringReplace[e, "$" -> "\\\\$"];

makeLetterSubDef[subDefName_, strings_] :=
  StrJoin[
    "  ", subDefName, ": (?:",
    StringTake[First @ strings, UpTo @ 1],
    "(?:", StringRiffle[StringDrop[strings, UpTo @ 1], "|"],
    "))\n"
];

groupToSyntaxScope = CaseOf[
  "PackageDeclaration" := "meta.package.declaration.wolfram";
  "PackageFunction"    := "meta.package.function.wolfram";
  "Symbol"          := "constant.language.symbol.wolfram";
  "Head"            := "constant.language.head.wolfram";
  "Function"        := $$ @ "builtin";
  "OptionSymbol"    := "constant.language.symbol.option.wolfram";
  "BoxOptionSymbol" := "constant.language.symbol.option.box.wolfram";
  group_     := Which[
    StrEndsQ[group, "Symbol"],
      StrJoin["constant.language.symbol.", ToLowerCase @ StrDelete[group, "Symbol"], ".wolfram"],
    StrEndsQ[group, "Head"],
      StrJoin["constant.language.head.", ToLowerCase @ StrDelete[group, "Head"], ".wolfram"],
    True,
      igroupToSyntaxScope @ group
  ];
];

igroupToSyntaxScope[group_] := igroupToSyntaxScope[str] =
  StrJoin[
    "support.function.",
    ToLowerCase @ StringRiffle[
      CamelCaseSplit @ StringReplace[group, {"Function" -> "", "IO" -> "Io"}],
      "."
    ],
    ".wolfram"
  ];

(*************************************************************************************************)

makeKindCompletions[group_, names_] := Locals[
  kindString = {groupToKindColorProxy @ group, Lookup[$groupToSymbol, group, "f"], group};
  Map[
    name |-> <|"trigger" -> name, "contents" -> doubleEscapeDollars @ name, "kind" -> kindString|>,
    Select[names, StrLen[#] > 3&]
  ]
];

$groupToSymbol = Association[
  "PackageDeclaration"     -> "p",
  "PackageFunction"        -> "p",
  "Symbol"                 -> "s",
  "PatternSymbol"          -> "s",
  "PatternHead"            -> "s",
  "OptionSymbol"           -> "\[RightArrow]",
  "BoxOptionSymbol"        -> "\[RightArrow]",
  "FormHead"               -> "■",
  "FormSymbol"             -> "■",
  "BoxFunction"            -> "□",
  "GraphicsDirective"      -> "꠵",
  "GraphicsPrimitive"      -> "△",
  "GraphicsBoxFunction"    -> "▲",
  "Variable"               -> "$",
  "SpecialVariable"        -> "$",
  "CacheVariable"          -> "$"
];

groupToKindColorProxy = CaseOf[
  "Symbol" | "PatternSymbol" | "PatternHead"                        := "symbol";
  "Object"                                                          := "function";
  "Function" | "MutationFunction" | "ScopingFunction" | "ControlFlowFunction" := "function";
  "PackageFunction" | "PackageDeclaration" | "DebuggingFunction" | "SpecialFunction"    := "function";
  "Variable" | "SpecialVariable" | "CacheVariable"                  := "variable";
  "GraphicsBoxFunction" | "GraphicsPrimitive" |
  "GraphicsDirective" | "GraphicsFunction"                          := "navigation";
  "BoxFunction" | "FormSymbol" | "FormHead"                         := "snippet";
  _                                                                 := "symbol";
];
