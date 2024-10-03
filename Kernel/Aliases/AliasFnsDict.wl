PackageExports[
  "Function",
    UCounts,
    DictSum, UDictSum, DictPlus, UDictPlus,
    InvertDict, InvertUDict,
    DictMapApply, DictMapThread, DictThread,
    UDictMap, UDictMapApply, UDictMapThread, UDictThread,
    RangeDict, RangeUDict, UDictRange, DictRange,
    ConstRules, ConstDict, ConstUDict, TrueDict, FalseDict,
    PairsToDict, RulesToDict, PairsToUDict, RulesToUDict, DictToPairs, DictToRules,
    LevelDict, OccDict, ArgDict, LeafDict, PartDict, ListDictParts
];

(*************************************************************************************************)

DefineAliasRules[
  UCounts          -> UnorderedCounts,

  DictSum          -> AssociationSum,
  UDictSum         -> UAssociationSum,
  DictPlus         -> AssociationPlus,
  UDictPlus        -> UAssociationPlus,

  InvertDict       -> InvertAssociation,
  InvertUDict      -> InvertUAssociation,

  DictMapApply     -> AssociationMapApply,
  DictMapThread    -> AssociationMapThread,
  DictThread       -> AssociationThreadOp,

  UDictMap         -> UAssociationMap,
  UDictMapApply    -> UAssociationMapApply,
  UDictMapThread   -> UAssociationMapThread,
  UDictThread      -> UAssociationThread,

  RangeDict        -> RangeAssociation,
  RangeUDict       -> RangeUAssociation,
  UDictRange       -> UAssociationRange,
  DictRange        -> AssociationRange,

  ConstRules       -> ConstantRules,
  ConstDict        -> ConstantAssociation,
  ConstUDict       -> ConstantUAssociation,
  TrueDict         -> ConstantTrueAssociation,
  FalseDict        -> ConstantFalseAssociation,

  PairsToDict      -> PairsToAssociation,
  RulesToDict      -> RulesToAssociation,
  PairsToUDict     -> PairsToUAssociation,
  RulesToUDict     -> RulesToUAssociation,
  DictToPairs      -> AssociationToPairs,
  DictToRules      -> AssociationToRules
];

DefineAliasRules[
  LevelDict        -> LevelAssociation,
  OccDict          -> OccurenceAssociation,
  ArgDict          -> ArgumentAssociation,
  LeafDict         -> LeafAssociation,
  PartDict         -> PartAssociation,
  ListDictParts    -> ListAssociationParts
];
