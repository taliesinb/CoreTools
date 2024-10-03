PackageExports[
  "MessageFunction",
    ThrowMsg, ErrorMsg, ReturnMsg,
    OptMsg, ThrowOptMsg, UnkOptMsg, ThrowUnkOpt,
  "Function",
    ToList, ToVals, ToRowVec, ToColVec, KeysVals,
    HLen, HoldLen, Len2, LenN, DimN, LenRange, RangeLen,
    MapVals, MapValsP, MapF, MapL, MapM, MapR,
    VecRep,
    Index1, VecIndex1, VecIndex1Of, VecRep, Parts, Occs, UniOccs, OccsPos, NarrowOpts,
    DelNone, DelNull, DelMissing, DelFailed, DelEmpty, DelVerb,
    AliasSymName, SymName, SymContext, SymPath,
    MakeSetD, MakeTagSetD, MakeUpSetD,
  "MutatingFunction",
    UnpackDict, PackDict, SetInherit,
  "SymbolicHead",
    Iff
];

(*************************************************************************************************)

DefineAliasRules[
  Iff              -> Equivalent
];

(*************************************************************************************************)

DefineAliasRules[
  ToList           -> Developer`ToList,
  ToVals           -> ToValues,
  ToRowVec         -> ToRowVector,
  ToColVec         -> ToColumnVector,
  KeysVals         -> KeysValues
];

(*************************************************************************************************)

DefineAliasRules[
  HoldLen          -> HoldLength,
  HLen             -> HoldLength,
  Len2             -> Length2,
  LenN             -> LengthN,
  DimN             -> DimensionN,
  LenRange         -> LengthRange,
  RangeLen         -> LengthRange
];

(*************************************************************************************************)

DefineAliasRules[
  MapVals          -> MapValues,
  MapValsP         -> MapValuesP,
  MapF             -> MapFirst,
  MapL             -> MapLast,
  MapM             -> MapMost,
  MapR             -> MapRest
];

(*************************************************************************************************)

DefineAliasRules[
  Index1           -> SelectFirstIndex,
  VecIndex1        -> FirstVectorIndex,
  VecIndex1Of      -> FirstVectorIndexOf,
  VecRep           -> VectorReplace,
  Parts            -> ExtractIndices,
  Occs             -> Occurences,
  UniOccs          -> UniqueOccurences,
  OccsPos          -> OccurencePositions,
  NarrowOpts       -> NarrowOptions
];

(*************************************************************************************************)

DefineAliasRules[
  DelNone          -> DeleteNone,
  DelNull          -> DeleteNull,
  DelMissing       -> DeleteMissing,
  DelFailed        -> DeleteFailed,
  DelEmpty         -> DeleteEmpty,
  DelVerb          -> DeleteVerbatim
];

(*************************************************************************************************)

DefineAliasRules[
  AliasSymName     -> HoldSymbolNameAlias,
  SymName          -> HoldSymbolName,
  SymContext       -> HoldSymbolContext,
  SymPath          -> HoldSymbolPath
];

(*************************************************************************************************)

DefineAliasRules[
  MakeSetD         -> MakeSetDelayed,
  MakeTagSetD      -> MakeTagSetDelayed,
  MakeUpSetD       -> MakeUpSetDelayed
];

(*************************************************************************************************)

DefineAliasRules[
  ThrowMsg     -> ThrowMessage,
  ErrorMsg     -> ErrorMessage,
  ReturnMsg    -> ReturnMessage,
  OptMsg       -> IssueOptionMessage,
  ThrowOptMsg  -> ThrowOptionMessage,
  UnkOptMsg    -> IssueUnknownOptionMessage,
  ThrowUnkOpt  -> ThrowUnknownOptionMessage
];

(*************************************************************************************************)

DefineAliasRules[
  SetInherit       -> SetInherited
];


