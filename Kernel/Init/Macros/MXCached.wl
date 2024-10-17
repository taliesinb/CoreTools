PackageExports[
  "IOFunction",        MXCached, MXCachedAs
];

(*************************************************************************************************)

DeclaredHere[MXCached, MXCachedAs]

"MXCached[head$, args$, opts$, body$] caches to a hidden MX file."

ComplexMacroDefs[
  MXCached[args___]   := With[{head = First @ MacroHead[]}, mMXCached[head, args]],
  MXCachedAs[args___] := mMXCached[args]
];

SetHoldC @ mMXCached;

mMXCached[head_Sym, args_List, opts_List, body_] := Module[{info},
  info = List[
    $CurrentPackageFileHash,
    cacheBasePath @ NameLast @ SymName @ head
  ];
  $MXCacheInfo[head] = info;
  CachePrint["$MXCacheInfo[", head, "] = ", info];
  HoldM @ MXCachedInternal[head, args, opts, body]
];

m_mMXCached := MacroError[MXCached::invalidArguments, HoldForm @ m];

cacheBasePath[name_Str] := cacheBasePath[name] = PathJoin[$MXCachePath, name];
