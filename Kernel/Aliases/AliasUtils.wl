PackageExports[
  "Function",
    SplitLengths, Multisets, MultiSubsets, ReplaceSuccessive, SublistPosition,
    OneOver, Reciprocal, AbsDelta, MapThreadMin, MapThreadMax,
    UntallyInts, FastNumericIndices,
    OutermostToInnermost, InnermostToOutermost, ListJoin, ContainedSymbols,
    KBatchDot, KMatrixRow, KMatrixCol,
    BitPopCount, BitReset,
    PackedFlatList,
    PackedMatrixRows,
    FindSegmentIntersections,
    IntersectionFreeSegmentsQ,
    DeleteDuplicateCoordinates,
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
  KBatchDot                    -> NDSolve`FEM`MapThreadDot,
  KMatrixRow                   -> LinearAlgebra`Private`GetMatrixRow,
  KMatrixCol                   -> LinearAlgebra`Private`GetMatrixColumn,
  BitPopCount                  -> Internal`BitCount,
  BitReset                     -> Internal`BitReset,
  FastQuietCheck               -> Internal`UnsafeQuietCheck,
  NoMessages                   -> Internal`DeactivateMessages,
  WithLocalSettings            -> Internal`WithLocalSettings,
  ReplaceAllUnheld             -> Internal`ReplaceAllUnheld,
  PackedFlatList               -> NDSolve`FEM`FlattenAll,
  PackedMatrixRows             -> NDSolve`FEM`GetElementCoordinates,
  FindSegmentIntersections     -> Region`Mesh`FindSegmentIntersections,
  IntersectionFreeSegmentsQ    -> Region`Mesh`IntersectionFreeSegmentsQ,
  DeleteDuplicateCoordinates   -> Region`Mesh`DeleteDuplicateCoordinates,
  StraightLineProgram          -> Internal`StraightLineProgram,
  Dethread                     -> NDSolve`Dethread,
  EmbeddedSymbols              -> Internal`EmbeddedSymbols,
  InheritedBlock               -> Internal`InheritedBlock,
  IBlock                       -> Internal`InheritedBlock
];

(* JoinOrFail issues this message, but it isn't defined *)
Join::nonlist = "`` is not a list.";

