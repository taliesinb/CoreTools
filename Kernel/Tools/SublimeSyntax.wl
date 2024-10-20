PackageExports[
  "IOFunction",
    SublimeUpdateSyntax
];

PrivateExports[
  "Function",        SystemSymbolKinds, CoreToolsSymbolKinds, SymbolKindToSyntaxScope,
  "SpecialVariable", $SublimeKindColors
];

(*************************************************************************************************)

SystemSymbolKinds[] := ImportStringTable @ systemSymbolFile @ "SymbolKinds.wlsyms";

CoreToolsSymbolKinds[] := Merge[PackageSymbolKinds /@ {"CoreTools`", "Prelude`"}, Catenate];

(*************************************************************************************************)

$SublimeKindColors := Unimplemented;

(*************************************************************************************************)

SublimeUpdateSyntax::notInstalled = "Cannot find the \"WolframLanguage\" Sublime Text package at expected location ``.";
SublimeUpdateSyntax::messagesOccurred = "Message(s) occurred during computation, aborting.";
SublimeUpdateSyntax::invalidResult = "Did not produce valid results, aborting.";
SublimeUpdateSyntax::unknownKind = "Unknown symbol kind: ``.";

SetDelayedInitial[$SublimePackagesPath, "~/Library/Application Support/Sublime Text/Packages"];

SublimeUpdateSyntax[] := Locals @ CatchMessages[

  targetDir = NormalizePath @ PathJoin[$SublimePackagesPath, "WolframLanguage"];
  If[!DirectoryQ[targetDir], ReturnFailed["notInstalled", targetDir]];

  (* save any loaded packages (except CoreTools) to disk, they'll be reloaded below *)
  Map[cachePackageSymbolKinds, PreludeLoadedPackages[]];

  (* load syntax groups from syntax files, and list of internal context -> symbols *)
  systemKinds = SystemSymbolKinds[];
  userSymbolFiles = userSymbolFile @ FileList["*.wlsyms"];
  userKinds = Merge[ImportStringTable /@ userSymbolFiles, Catenate];
  ExportStringTable[staticFile @ "LibraryKinds.wlsyms", userKinds];

  coreKinds = CoreToolsSymbolKinds[];
  coreSymbols = Union @ Flatten @ Values @ coreKinds;
  userKinds //= Map[Complement[#, coreSymbols]&];
  (* KeyValueMap[KeyUnionTo[userKinds, #1, #2]&, coreKinds]; *)

  (* generate strings *)
  res = Check[
    internalSymbols = ImportStringTable @ systemSymbolFile @ "InternalSymbols.wlsyms";
    preludeSymbols = Dict["Prelude`" -> Map[NameLast, Names @ "Prelude`*"]];
    JoinTo[internalSymbols, preludeSymbols];
    {builtinRegexs, builtinDefs, builtinContext} = makeGroupsRegexpsDefs[systemKinds,  "System"];
    {coreRegexs,    coreDefs,    coreContext}    = makeGroupsRegexpsDefs[coreKinds,    "Core"];
    {libraryRegexs, libraryDefs, libraryContext} = makeGroupsRegexpsDefs[userKinds,    "Library"];
    regexVars = Join[builtinRegexs, coreRegexs, libraryRegexs];
    internalDefs = KeyValueMap[makeInternalSymbolDefs, internalSymbols];
    syntaxDefinition = $syntaxTemplate @ Dict[
      "variables"        -> StringRiffle[regexVars, "\n"],
      "library_symbols"  -> StringRiffle[libraryDefs, "\n"],
      "library_sym"      -> libraryContext,
      "core_symbols"     -> StringRiffle[coreDefs, "\n"],
      "core_sym"         -> coreContext,
      "builtin_symbols"  -> StringRiffle[builtinDefs, "\n"],
      "builtin_sym"      -> builtinContext,
      "internal_symbols" -> StringRiffle[internalDefs, "\n"]
    ];
    If[!StringQ[syntaxDefinition], ReturnFailed["invalidResult"]];
    ExportUTF8[staticFile @ "WolframLanguage.sublime-syntax", syntaxDefinition];

    symbolKinds = Merge[{systemKinds, coreKinds, userKinds}, Catenate];
    symbolKinds["InternalSymbol"] = Flatten @ KeyValueMap[StrPre[#1][#2]&, internalSymbols];
    completions = Flatten @ KeyValueMap[makeKindCompletions, symbolKinds];
    completionsJSON = ExportJSONString[Dict["scope" -> "source.wolfram", "completions" -> completions], 2];
    If[!StringQ[completionsJSON], ReturnFailed["invalidResult"]];
    ExportUTF8[staticFile @ "WolframLanguage.sublime-completions", completionsJSON];
  ,
    $Failed
  ];
  If[FailureQ[res], ReturnFailed["messagesOccurred"]];

  (* write them to disk *)

  Map[
    file |-> CopyLatestFile[file, PathJoin[targetDir, FileNameTake @ file]],
    staticFile @ FileList["WolframLanguage*"]
  ]
];

(*************************************************************************************************)

templateFile[name_]      := DataPath["Sublime", "TemplateFiles", name];
staticFile[name_]        := DataPath["Sublime", "StaticFiles", name];

systemSymbolFile[name_]  := DataPath["System", name];
userSymbolFile[name_]    := DataPath["User", name];

(*************************************************************************************************)

$syntaxTemplate := $syntaxTemplate =
  StringFunction @ File @ templateFile["WLSyntaxTemplate.sublime-syntax"];

(*************************************************************************************************)

makeInternalSymbolDefs[context_, symbols_] :=
  $contextSymbolsDefinitionTemplate[name, context, escapeDollars @ StringRiffle[symbols, "|"]];

$contextSymbolsDefinitionTemplate := $contextSymbolsDefinitionTemplate =
  StringFunction @ ImportUTF8 @ templateFile["SymbolDefinitionsFragment.json"];

(*************************************************************************************************)

makeGroupsRegexpsDefs[kinds_, prefix_] := Locals[
  $groupPrefix = prefix;
  $originTag = Switch[prefix, "System", ".sys", "Core", ".core", _, ".user"];
  pair = Transpose @ KeyValueMap[makeGroup0, kinds];
  Append[pair, makeSingleContext @ StringRiffle[Last @ pair, "\n"]]
];

$groupPrefix = "";

$addPopRule =
s:"      scope:" :>
  "      pop: true\n" <> s;

$symNotFound = "
    - match: '\\s+'
    - match: '{{symbolSegment}}{{symbolEndBoundary}}'
      pop: true
      scope: variable.local.unrecognized
";

makeSingleContext[defs_] := StrJoin[StrRep[defs, $addPopRule], $symNotFound; ""];

makeGroup0[group_ ? (StrEndsQ["Variable"]), names_] :=
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
  If[Length[names] >= 64; True,
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

escapeDollars[e_]       := StringReplace[e, "$" -> "\\$"];
escapeDollarsTwice[e_] := StringReplace[e, "$" -> "\\\\$"];

makeLetterSubDef[subDefName_, strings_] :=
  StrJoin[
    "  ", subDefName, ": (?:",
    StringTake[First @ strings, UpTo @ 1],
    "(?:", StringRiffle[StringDrop[strings, UpTo @ 1], "|"],
    "))\n"
];

groupToSyntaxScope[group_] := groupToSyntaxScope[group] = SymbolKindToSyntaxScope[group];

(*************************************************************************************************)

$originTag = "";

SymbolKindToSyntaxScope["PackageDeclaration"] := "meta.pkg.dec";

SymbolKindToSyntaxScope["PackageFunction"]    := "meta.pkg.fn";

SymbolKindToSyntaxScope[kind_Str] := Locals[
  shortKind = Lookup[$ToShortKind, kind, ThrowMsg["unknownKind", kind]];
  tokens = ToLowerCase @ CamelCaseSplit @ shortKind;
  extraTag = If[!MemberQ[tokens, "spec"], "", tokens //= DelCases["spec"]; "spec."];
  StrJoin[extraTag, Riffle[Rev @ tokens, "."], $originTag]
];

(*************************************************************************************************)

makeKindCompletions[group_, names_] := Locals[
  kindString = {groupToKindColorProxy @ group, Lookup[$groupToSymbol, group, "f"], group};
  Map[
    name |-> Dict[
      "trigger" -> name,
      "contents" -> escapeDollars @ name,
      "kind" -> kindString
    ],
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
  "Option"                 -> "\[RightArrow]",
  "BoxOption"              -> "\[RightArrow]",
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
  "Function" | "MutatingFunction" | "Scoping" | "ControlFlow"       := "function";
  "PackageFunction" | "PackageDeclaration" | "DebuggingFunction" | "SpecialFunction" := "function";
  "Variable" | "SpecialVariable" | "CacheVariable"                  := "variable";
  "GraphicsBoxFunction" | "GraphicsPrimitive" |
  "GraphicsDirective" | "GraphicsFunction"                          := "navigation";
  "BoxFunction" | "FormSymbol" | "FormHead"                         := "snippet";
  _                                                                 := "symbol";
];

(*************************************************************************************************)

cachePackageSymbolKinds["CoreTools`"] := Null;

cachePackageSymbolKinds[context_] := Locals[
  filename = StringDelete[context, "`"] <> ".wlsyms";
  symbolKinds = PackageSymbolKinds[context];
  ExportStringTable[userSymbolFile @ filename, symbolKinds, "Split" -> 120]
];