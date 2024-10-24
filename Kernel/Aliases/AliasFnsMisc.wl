PackageExports[
  "MessageFunction",
    ThrowMsg, ErrorMsg, ReturnMsg,
  "HoldFunction",
    HLen, EMap, HMap, HScan, HApply, HMake, HHead, HHash, HByteCount,
    AliasSymName, SymName, SymContext, SymPath,
  "IOFunction",
    DebugE, DebugP, DebugC,
  "Function",
    ToList, ToSeq, ToVals, ToRowVec, ToColVec, KeysVals, KVScan,
    Len2, LenN, DimN, LenRange, RangeLen,
    MapVals, MapValsP, MapF, MapL, MapM, MapR,
    VecRep,
    Index1, VecIndex1, VecIndex1Of, VecRep, Parts, Occs, UniOccs, OccsPos, NarrowOpts,
    DelNone, DelNull, DelMissing, DelFailed, DelEmpty, DelVerb,
  "MutatingFunction",
    UnpackDict, PackDict,
    SetInherit,
  "ControlFlow",
    IfInherit,
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
  KeysVals         -> KeysValues,
  KVScan           -> KeyValueScan
];

ToSeq[]           := Seq[];
ToSeq[List[a___]] := Seq[a];
ToSeq[a_]         := a;
ToSeq[a__]        := Apply[Seq, ToList[a]];

(*************************************************************************************************)

(* these are aliases for System functions from PreBase.wl *)
DefineAliasRules[
  HLen             -> HoldLength,
  EMap             -> EvaluateMap,
  HMap             -> HoldMap,
  HScan            -> HoldScan,
  HApply           -> HoldApply,
  HMake            -> HoldConstruct,
  HHead            -> HoldHead,
  HHash            -> HoldHash,
  HByteCount       -> HoldByteCount
];

DefineAliasRules[
  DebugE           -> DebugEcho,
  DebugC           -> DebugPrintCondition,
  DebugP           -> DPrint
];

(*************************************************************************************************)

DefineAliasRules[
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
  Occs             -> Occurrences,
  UniOccs          -> UniqueOccurrences,
  OccsPos          -> OccurrencePositions,
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
  ThrowMsg         -> ThrowMessage,
  ErrorMsg         -> ErrorMessage,
  ReturnMsg        -> ReturnMessage
];

(*************************************************************************************************)

DefineAliasRules[
  SetInherit       -> SetInherited,
  IfInherit        -> IfInherited
];


