PackageExports[
  "IOFunction",        MXCached, MXCachedAs,
  "DebuggingFunction", ExtendedDefinition,
  "Variable",          $MXCachePath
];

PrivateExports[
  "Function",          SymbolDefinitionHash,
  "IOFunction",        MXCachedInternal,
  "CacheVariable",     $SymbolDefinitionHashCache, $MXCacheInfo
];

(*************************************************************************************************)

SetInitial[$MXCachePath, PathJoin[$TemporaryDirectory, "WL", "MXCache"]];
SetInitial[$MXCacheInfo, UDict[]];

(*************************************************************************************************)

DeclaredHere[MXCached, MXCachedAs]

$CachePrinting = True;

MXCached::usage =
"MXCached[head$, args$, opts$, body$] caches to a hidden MX file."

ComplexMacroDefs[
  MXCached[args_List, opts_List, body_]             := mMXCached[MacroHead[], args, opts, body],
  MXCachedAs[head_Sym, args_List, opts_List, body_] := mMXCached[head, args, opts, body]
];

SetHoldC @ mMXCached;

mMXCached[head_, args_, opts_, body_] := Then[
  $MXCacheInfo[head] = {$CurrentPackageFileHash, cacheBasePath @ NameLast @ SymName @ head},
  HoldM @ MXCachedInternal[head, args, opts, body]
];

cacheBasePath[name_Str] := cacheBasePath[name] = PathJoin[$MXCachePath, name];

(*************************************************************************************************)

SetStrict @ SetHoldC @ MXCachedInternal;

MXCachedInternal[head_, args_, opts_, body_] := Module[
  {info, fileHash, cachePath, fnHash, optsHash, argsHash, fullHash, cacheFile, result},
  If[disableCachingQ[opts],
    CachePrint["Caching disabled by option, returning."];
    Return @ body
  ];
  CachePrint["Creating hash for call to ", head];
  Check[
    info = Lookup[$MXCacheInfo, head];
    If[!MatchQ[info, {_Int, _Str}],
      Message[MXCached::missingInfo, head];
      Return @ body
    ];
    {fileHash, cachePath} = info;
    fnHash   = SymbolDefinitionHash[head, fileHash];
    argsHash = Hash @ args;
    optsHash = Hash @ KeySort @ KeyDrop[$ignoredKeys] @ Dict[Options @ head, opts];
    fullHash = Base36Hash @ {fnHash, argsHash, optsHash};
    EnsureDirectory @ cachePath;
    CachePrint["Hashes: ", {fnHash, argsHash, optsHash}];
    cacheFile = StrJoin[fullHash, ".mx"];
  ,
    CachePrint["Error occurred."];
    Return @ $Failed
  ];
  CachePrint["Checking for cache file: ", File @ cacheFile];
  If[FileExistsQ[cacheFile],
    CachePrint["File exists, loading."];
    Return @ ImportMX @ cacheFile
  ];
  If[FailureQ[Check[result = body, $Failed]] || FailureQ[result],
    CachePrint["Computating failed."]
  ,
    CachePrint["Caching result to: ", File @ cacheFile];
    ExportMX[cacheFile, result];
  ];
  result
];

disableCachingQ[opts_] := Or[
  Lookup[opts, Caching] === False,
  Lookup[opts, Logging] === True,
  PosIntQ @ Lookup[opts, LogLevel]
];

MXCached::missingInfo = "Info missing for ``, can't cache.";

(*************************************************************************************************)

SetHoldF @ SetStrict @ SymbolDefinitionHash;

SetInitial[$SymbolDefinitionHashCache, UDict[]];

SymbolDefinitionHash[sym_Sym | Hold[sym_Sym], otherHash_:0] := Module[{value},
  val = $SymbolDefinitionHashCache[Hold[sym]];
  modTime = Lookup[Prelude`Packages`$PackageModTime, SymbolBaseContext @ sym, 0];
  Switch[val,
    {_, modTime, otherHash}, CachePrint["Current symbol hash: ", val];  Return @ First @ val,
    {_, _, _},               CachePrint["Stale symbol hash for ", HoldForm @ sym, ": ", val, " =!= ", {_, modTime, otherHash}],
    _,                       CachePrint["No symbol hash for ", HoldForm @ sym]
  ];
  fnHash = Hash @ ExtendedDefinition @ sym;
  CachePrint["Computed hash: ", fnHash];
  If[!IntQ[fnHash],
    ErrorMessage[SymbolDefinitionHash::hashFail, HoldForm @ sym],
    $SymbolDefinitionHashCache[Hold[sym]] = {fnHash, modTime, otherHash};
    fnHash
  ]
];

SymbolDefinitionHash::hashFail = "Couldn't compute hash for ``.";

(*************************************************************************************************)

SetHoldF @ ExtendedDefinition;

(* System` etc is automatically ignored, but NOT when you provide ExcludedContexts *)

ExtendedDefinition[sym_Sym | Hold[sym_Sym]] :=
  Language`ExtendedFullDefinition[sym, "ExcludedContexts" -> $excludedContexts]

$excludedContexts = {
  "System`*", "CoreTools`*", "Prelude`*", "Association`*",
  "PacletManager`*", "ExternalEvaluate`*", "Data`*", "StringPattern`*", "MathLink`*",
  "Internal`*", "Developer`*", "Language`*", "GraphComputation`*",
  "FEPrivate`*", "BoxForm`*", "Typeset`", "FrontEnd`*", "ExternalEvaluateCommon`*", "GroupTheory`*", "Random`*",
  "MathTools`*", "Tries`*", "Init`*", "NumericalMath`*", "Image`*",
  "URLUtilities`*", "Templating`*", "URLUtilities`*", "GeneralUtilities`*"
};
