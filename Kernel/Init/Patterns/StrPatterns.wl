PackageExports[
  "PatternHead", StrMatchP, StrStartsP, StrContainsP, DeepStrContainsP
];

(**************************************************************************************************)

DefinePatternMacro[StrMatchP,
  StrMatchP[patt_] :> (_String ? (StringMatchQ[patt]))
]

DefinePatternMacro[StrStartsP,
  StrStartsP[patt_] :> (_String ? (StringStartsQ[patt]))
]

DefinePatternMacro[StrContainsP,
  StrContainsP[patt_] :> (_String ? (StringContainsQ[patt]))
]

DefinePatternMacro[DeepStrContainsP,
  DeepStrContainsP[patt_] :> (_ ? (ContainsQ[_String ? (StringContainsQ[patt])]))
]