PackageExports[
    "Function", UnicodeData
];

SessionExports[
  "CacheVariable",
    $UnicodeTable
];

(**************************************************************************************************)

SetStrict[UnicodeData];

UnicodeData[result_Fn, filter_Fn:None] := CatchMessages @ Locals[
  data = $UnicodeTable;
  fn = toUniFn @ result;
  If[filter =!= None,
    testFn = Echo @ toUniFn @ filter;
    MapThread[If[testFn[##], fn[##], Nothing]&, data]
  ,
    MapThread[fn, data]
  ]
];

toUniFn[fn_] := fn /. Slot[s_Str] :> RuleEval @ AssertLookup[$unicodeSlots, s];
$unicodeSlots = {"Str" -> #1, "Name" -> #2, "WLName" -> #3, "BlockName" -> #4};

(**************************************************************************************************)

SetCachedInitial[$UnicodeTable, loadUnicodeTable[]];

loadUnicodeTable[] := Locals[
  CharacterName["\[RightArrow]", "ICU_UnicodeName"];
  {codepoint, assocs} = KeysValues @ System`CharacterFunctionsDump`$letters;
  Map[
    Lookup[assocs, #]&,
    {"Character", "UnicodeName", "LongName", "UnicodeBlockName"}
  ]
];

