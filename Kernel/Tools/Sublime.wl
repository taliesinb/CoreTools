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
    $SublimePackagesPath,
    $SublimeKindColors
];

(*************************************************************************************************)

$SublimeKindColors := $SublimeKindColors = getSublimeKindColors[];

(* CoreToolsSymbolKinds[]; *)

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

templateFile[name_]      := DataPath["Sublime", "TemplateFiles", name];
systemSymbolFile[name_]  := DataPath["Sublime", "SystemSymbols", name];
userSymbolFile[name_]    := DataPath["Sublime", "UserSymbols", name];
staticFile[name_]        := DataPath["Sublime", "StaticFiles", name];

(*************************************************************************************************)

CoreToolsSymbolKinds[] := Locals[

  coreKinds = PackageSymbolKinds["CoreTools`"];
  If[!symbolDictQ[coreKinds], ThrowMsg["badSymbols", "CoreTools`"]];

  privateSyms = Last /@ StringSplit[Names["CoreTools`Private`*"], "`"];
  privateSyms = Complement[privateSyms, Catenate @ coreKinds];
  privateKinds = GroupBy[privateSyms,
    If[StringContainsQ[#, "$"], "SpecialVariable", "SpecialFunction"]&
  ];
  If[!symbolDictQ[privateKinds], ThrowMsg["badSymbols", "CoreTools`Private`"]];

  Block[{$preludeKindBag = Bag[],
    System`PackageExports = savePreludeExports,
    System`SystemExports = savePreludeExports,
    System`PrivateExports = savePreludeExports},
    Get @ $PreludeInitFile;
    preludeKinds = Merge[Catenate] @ BagPart[$preludeKindBag, All];
  ];
  If[!symbolDictQ[preludeKinds], ThrowMsg["badSymbols", "prelude packages"]];

  manualKinds = Dict[
    "SpecialVariable" -> {"$CoreToolsPath", "$CoreToolsRootPath", "$PreludeInitFile"},
    "SpecialFunction" -> {"ApplyEchoSugar"}
  ];

  Merge[{coreKinds, privateKinds, preludeKinds, manualKinds}, Apply[Union]]
];

symbolDictQ[a_] := KeysValuesTrue[a, StringQ, StringVectorQ];

SetHoldA[savePreludeExports];

savePreludeExports[] := Null;
savePreludeExports[kind_String, Longest[syms___Symbol], rest___] := (
  StuffBag[$preludeKindBag, kind -> HoldMap[HoldSymbolName, {syms}]];
  savePreludeExports[rest]
);

(*************************************************************************************************)

cachePackageSymbolKinds["CoreTools`"] := Null;
cachePackageSymbolKinds[context_] := Locals[
  filename = StringDelete[context, "`"] <> ".wl.txt";
  symbolKinds = PackageSymbolKinds[context];
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
  builtinKinds = ImportStringTable @ systemSymbolFile @ "SystemSymbolKinds.wl.txt";
  librarySymbolFiles = userSymbolFile @ FileList["*.wl.txt"];
  libraryKinds = Merge[ImportStringTable /@ librarySymbolFiles, Catenate];

  coreToolsSymbolsKinds = CoreToolsSymbolKinds[];
  coreSymbols = Union @ Flatten @ Values @ coreToolsSymbolsKinds;
  libraryKinds //= Map[Complement[#, coreSymbols]&];
  KeyValueMap[KeyUnionTo[libraryKinds, #1, #2]&, coreToolsSymbolsKinds];

  (* generate strings *)
  res = Check[
    internalSymbols = ImportStringTable @ systemSymbolFile["InternalSymbols.wl.txt"];
    preludeSymbols = GroupPairs[NameMostLast /@ Select[Names["Prelude`*`*"], StringFreeQ["`Private"]]];
    JoinTo[internalSymbols, preludeSymbols];
    {builtinRegexs, builtinDefs, builtinContext} = makeGroupsRegexpsDefs[builtinKinds, "Builtin"];
    {libraryRegexs, libraryDefs, libraryContext} = makeGroupsRegexpsDefs[libraryKinds, "Library"];
    internalDefs = KeyValueMap[makeInternalSymbolDefs, internalSymbols];
    syntaxDefinition = $syntaxTemplate @ Dict[
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
    completionsJSON = ToJSON[Dict["scope" -> "source.wolfram", "completions" -> completions], 2];
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

makeGroup0[group:"Variable" | "SpecialVariable" | "CacheVariable" | "SlotVariable", names_] :=
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
    defItems = StrJoin["  ", group, ": ", makeRegexp[names], "\n"] // escapeDollars;
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
  "Symbol"             := "constant.language.symbol.wolfram";
  "SpecialSymbol"      := "constant.language.symbol.special.wolfram";
  "Head"               := "constant.language.head.wolfram";
  "ObjectHead"         := "support.function.object.wolfram";
  "Function"           := $$ @ "builtin";
  "OptionSymbol"       := "constant.language.symbol.option.wolfram";
  "BoxOptionSymbol"    := "constant.language.symbol.option.box.wolfram";
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
  "SpecialSymbol"          -> "s",
  "PatternHead"            -> "s",
  "OptionSymbol"           -> "\[RightArrow]",
  "BoxOptionSymbol"        -> "\[RightArrow]",
  "ObjectHead"             -> "■",
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
  "Symbol" | "PatternSymbol" | "PatternHead" | "SpecialSymbol"      := "symbol";
  "ObjectHead"                                                      := "function";
  "Function" | "MutationFunction" | "ScopingFunction" | "ControlFlow" := "function";
  "PackageFunction" | "PackageDeclaration" | "DebuggingFunction" | "SpecialFunction"    := "function";
  "Variable" | "SpecialVariable" | "CacheVariable"                  := "variable";
  "GraphicsBoxFunction" | "GraphicsPrimitive" |
  "GraphicsDirective" | "GraphicsFunction"                          := "navigation";
  "BoxFunction" | "FormSymbol" | "FormHead"                         := "snippet";
  _                                                                 := "symbol";
];
