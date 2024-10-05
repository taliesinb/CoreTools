PackageExports[
  "Function",
    Id, Rev, At, Make, Len, Dims, Vals, Rep, RepRep, RepAll,
    ConstList, SelectF, PosIndex, DelCases, DelDups, DelDupsBy, Inter, Compl, DictMap,
    SysIntStr, IntDigits, Chars, FromCharCode, ToCharCode, CharRange,
    StrLen, StrJoin, StrSplit, StrCases, StrPos,
    StrTake, StrDrop, StrInsert, StrDelete,
    StrTrim, StrTrimL, StrTrimR, StrTrimLR,
    StrPadL, StrPadR,
    StrRep, StrRepPart, StrRev,
    Dist, SqrDist,
  "Option",
    DistFn
];

(*************************************************************************************************)

DefineAliasRules[
  DistFn           -> DistanceFunction
];

DefineAliasRules[
  Dist             -> EuclideanDistance,
  SqrDist          -> SquaredEuclideanDistance
];

(*************************************************************************************************)

DefineAliasRules[
  Id               -> Identity,
  Rev              -> Reverse,
  At               -> Construct,
  Make             -> Construct,
  Len              -> Length,
  Dims             -> Dimensions,
  Vals             -> Values,
  Rep              -> Replace,
  RepRep           -> ReplaceRepeated,
  RepAll           -> ReplaceAll
];

DefineAliasRules[
  ConstList        -> ConstantArray,
  SelectF          -> SelectFirst,
  PosIndex         -> PositionIndex,
  DelCases         -> DeleteCases,
  DelDups          -> DeleteDuplicates,
  DelDupsBy        -> DeleteDuplicatesBy,
  Inter            -> Intersection,
  Compl            -> Complement,
  DictMap          -> AssociationMap
];

(*************************************************************************************************)

DefineAliasRules[
  IntDigits        -> IntegerDigits,
  Chars            -> Characters,
  FromCharCode     -> FromCharacterCode,
  ToCharCode       -> ToCharacterCode,
  CharRange        -> CharacterRange,
  SysIntStr        -> IntegerString
];

DefineAliasRules[
  StrLen           -> StringLength,
  StrJoin          -> StringJoin,
  StrSplit         -> StringSplit,
  StrCases         -> StringCases,
  StrPos           -> StringPosition,
  StrTake          -> StringTake,
  StrDrop          -> StringDrop,
  StrInsert        -> StringInsert,
  StrDelete        -> StringDelete,
  StrTrim          -> StringTrim,
  StrTrimL         -> StringTrimLeft,
  StrTrimR         -> StringTrimRight,
  StrTrimLR        -> StringTrimLeftRight,
  StrPadL          -> StringPadLeft,
  StrPadR          -> StringPadRight,
  StrRep           -> StringReplace,
  StrRepPart       -> StringReplacePart,
  StrRev           -> StringReverse
];

