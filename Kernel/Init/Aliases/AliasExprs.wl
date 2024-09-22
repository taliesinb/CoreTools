PackageExports[

  "SpecialFunction",
    MakeValid, MakeSealed,
    WithTimestampsPreserved,

  "MutatingFunction",
    SetNoEntryFlag,  SetValidFlag,
    HSetNoEntryFlag, HSetValidFlag,

  "Predicate",
    SealedQ, UnsealedQ,
    EntryFlagQ,   ValidFlagQ,  NoEntryFlagQ, NoValidFlagQ,
    HEntryFlagQ, HValidFlagQ, HNoEntryFlagQ, HNoValidFlagQ, MDataFlagQ,
    EvalRiskQ, NoEvalRiskQ, HMaybeFnQ, HNotFnQ, MaybeFnQ, NotFnQ,
    HasAnyCodesQ, HasNoCodesQ, HasDownCodeQ, HasOwnCodeQ, HasSubCodeQ, HasUpCodeQ, HasPrintCodeQ,
    HasAnyDefsQ,  HasNoDefsQ,  HasDownDefsQ, HasOwnDefsQ, HasSubDefsQ, HasUpDefsQ,
    HasIValueQ, HasDValueQ
];

(*************************************************************************************************)

DefineAliasRules[
  MakeValid        -> System`Private`ConstructValid,
  MakeSealed       -> System`Private`ConstructNoEntry
];

DefineAliasRules[
  SetNoEntryFlag   -> System`Private`SetNoEntry,
  SetValidFlag     -> System`Private`SetValid,
  HSetNoEntryFlag  -> System`Private`HoldSetNoEntry,
  HSetValidFlag    -> System`Private`HoldSetValid
];

(*************************************************************************************************)

DefineAliasRules[
  SealedQ          -> System`Private`HoldNoEntryQ,
  UnsealedQ        -> System`Private`HoldEntryQ
];

DefineAliasRules[
  EntryFlagQ       -> System`Private`NoEntryQ,
  ValidFlagQ       -> System`Private`ValidQ,
  NoEntryFlagQ     -> System`Private`EntryQ,
  NoValidFlagQ     -> System`Private`NotValidQ,
  HEntryFlagQ      -> System`Private`HoldEntryQ,
  HValidFlagQ      -> System`Private`HoldValidQ,
  HNoEntryFlagQ    -> System`Private`HoldNoEntryQ,
  HNoValidFlagQ    -> System`Private`HoldNotValidQ,
  MDataFlagQ       -> System`Private`MDataQ
];

(*************************************************************************************************)

DefineAliasRules[
  EvalRiskQ        -> System`Private`MightEvaluateQ,
  NoEvalRiskQ      -> System`Private`WillNotEvaluateQ,
  HMaybeFnQ        -> System`Private`MightEvaluateWhenAppliedQ,
  HNotFnQ          -> System`Private`WillNotEvaluateWhenAppliedQ
];

MaybeFnQ[f_] := HMaybeFnQ[f];
NotFnQ[f_]   := HNotFnQ[f];

(*************************************************************************************************)

DefineAliasRules[
  HasAnyCodesQ     -> System`Private`HasAnyCodesQ,
  HasAnyDefsQ      -> System`Private`HasAnyEvaluationsQ,
  HasNoCodesQ      -> System`Private`HasNoCodesQ,
  HasNoDefsQ       -> System`Private`HasNoEvaluationsQ,
  HasDownCodeQ     -> System`Private`HasDownCodeQ,
  HasDownDefsQ     -> System`Private`HasDownEvaluationsQ,
  HasOwnCodeQ      -> System`Private`HasOwnCodeQ,
  HasOwnDefsQ      -> System`Private`HasOwnEvaluationsQ,
  HasSubCodeQ      -> System`Private`HasSubCodeQ,
  HasSubDefsQ      -> System`Private`HasSubEvaluationsQ,
  HasUpCodeQ       -> System`Private`HasUpCodeQ,
  HasUpDefsQ       -> System`Private`HasUpEvaluationsQ,
  HasPrintCodeQ    -> System`Private`HasPrintCodeQ,
  HasIValueQ       -> System`Private`HasImmediateValueQ,
  HasDValueQ       -> System`Private`HasDelayedValueQ
];

(*************************************************************************************************)

DefineAliasRules[
  WithTimestampsPreserved -> Internal`WithTimestampsPreserved
];