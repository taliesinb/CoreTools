PackageExports[
  "Function",
    SplitLengths, Multisets, MultiSubsets, ReplaceSuccessive, SublistPosition,
    OneOver, Reciprocal, AbsDelta, MapThreadMin, MapThreadMax,
    UntallyInts, FastNumericIndices,
    OutermostToInnermost, InnermostToOutermost, ListJoin, ContainedSymbols
  "ControlFlow",
    FastQuietCheck, WithLocalSettings, NoMessages,
  "MutatingFunction",
    TransposeInPlace,
  "ScopingFunction",
    InheritedBlock, IBlock
];

(*************************************************************************************************)

(* Multisets[list, k] or [n, k] *)
DefineAliasRules[
  SplitLengths                 -> GroupTheory`Tools`PartitionRagged,
  Multisets                    -> GroupTheory`Tools`Multisets,
  MultiSubsets                 -> GroupTheory`Tools`MultiSubsets,
  ReplaceSuccessive            -> GroupTheory`Tools`ConsecutiveReplace,
  SublistPosition              -> GroupTheory`Tools`SublistPosition
];

(* AbsDelta: auto-broadcasts Abs[#1 - #2]& *)
DefineAliasRules[
  OneOver                      -> Internal`Reciprocal,
  Reciprocal                   -> Internal`Reciprocal,
  AbsDelta                     -> NumericalMath`AbsoluteError,
  MapThreadMin                 -> Random`Private`MapThreadMin,
  MapThreadMax                 -> Random`Private`MapThreadMax
];

DefineAliasRules[
  UntallyInts                  -> Internal`RepetitionFromMultiplicity,
  FastNumericIndices           -> Random`Private`PositionsOf,
  OutermostToInnermost         -> Internal`OutermostToInnermost,
  InnermostToOutermost         -> Internal`InnermostToOutermost,
  ListJoin                     -> Internal`JoinOrFail,
  ContainedSymbols             -> System`Utilities`SymbolList
];

DefineAliasRules[
  TransposeInPlace             -> Internal`TransposeInPlace,
  FastQuietCheck               -> Internal`UnsafeQuietCheck,
  NoMessages                   -> Internal`DeactivateMessages,
  WithLocalSettings            -> Internal`WithLocalSettings,
  InheritedBlock               -> Internal`InheritedBlock,
  IBlock                       -> Internal`InheritedBlock
];

(* JoinOrFail issues this message, but it isn't defined *)
Join::nonlist = "`` is not a list.";

