PackageExports[
  "PatternHead", StrMatchP, StrStartsP, StrContainsP, DeepStrContainsP
];

(**************************************************************************************************)

PatternMacroDefs[
  StrMatchP[patt_]        := (_Str ? (StrMatchQ[patt])),
  StrStartsP[patt_]       := (_Str ? (StrStartsQ[patt])),
  StrContainsP[patt_]     := (_Str ? (StrContainsQ[patt])),
  DeepStrContainsP[patt_] := (_ ? (ContainsQ[_Str ? (StrContainsQ[patt])]))
];
