PackageExports[
  "Predicate",
    SymbolQ, RealQ, NaturalQ, PositiveIntegerQ, NegativeIntegerQ, NonPositiveIntegerQ, NonNegativeIntegerQ,
    MachineRealQ, MachineIntegerQ, PositiveMachineIntegerQ, NegativeMachineIntegerQ, NonPositiveMachineIntegerQ, NonNegativeMachineIntegerQ, MachineComplexQ,
    AssociationVectorQ, ListOrAssociationQ, StringOrVectorQ, StringVectorQ, UnsafeEmptyQ, NotEmptyQ, NonEmptyQ,
    HoldSymbolQ, HoldAtomQ, HasKeyQ,

    StrQ, SymQ, IntQ, NatQ, NumQ, BoolQ, DictQ, UDictQ, ODictQ, ListDictQ,
    HStrQ, HSymQ, HIntQ, HNatQ, HNumQ, HBoolQ, HAtomQ, HEmptyQ, HNotEmptyQ, HListQ, HPackedQ, HDictQ, HUDictQ, HODictQ,
    AutoQ, InheritQ, NotAutoQ, InfQ, NotInfQ,
    ORuleQ, RuleLQ, RuleDQ,

    ZeroQ, NonZeroQ,
    ZeroIntQ, ZeroNumQ, NonZeroIntQ, NonZeroNumQ,
    PosIntQ, NegIntQ, PosRealQ, NegRealQ, PosNumQ, NegNumQ,
    NonPosIntQ, NonNegIntQ, NonPosRealQ, NonNegRealQ, NonPosNumQ, NonNegNumQ,
    UnitIntQ, UnitNumQ,
    ExtIntQ, ExtNatQ, ExtNumQ,
    ExtPosRealQ, ExtPosNumQ, ExtNonNegRealQ, ExtNonNegNumQ,

    FailedQ, FailQ, NotFailedQ, NotFailQ,
    DictOfQ, ListDictOfQ,
    AllSameLenQ, SameOKeysQ, AllSameOKeysQ,

    NEDictQ, NEDictOfQ, NEListQ, NEListOfQ, NEListDictQ, NEListDictOfQ,
    NEAllSameQ, NEAllSameByQ, NEAllSameLenQ, NEAllSameHeadsQ, NEAllSamePartsQ, NEAllSameSetsQ, NEAllSameKeysQ, NEAllSameOKeysQ,

    RuleVecQ, RuleLVecQ, ORuleVecQ, DictLikeQ,
    VecQ,    DictVecQ, PairVecQ, ListVecQ, BoolVecQ, SymVecQ, ColVecQ, StrVecQ, NatVecQ, IntVecQ, PosIntVecQ, RealVecQ, NumVecQ, ExtNumVecQ,
    AnyMatQ, DictMatQ, PairMatQ, ListMatQ, BoolMatQ, SymMatQ, ColMatQ, StrMatQ, NatMatQ, IntMatQ, PosIntMatQ, RealMatQ, NumMatQ, ExtNumMatQ,
    ArrQ,    DictArrQ, PairArrQ, ListArrQ, BoolArrQ, SymArrQ, ColArrQ, StrArrQ, NatArrQ, IntArrQ, PosIntArrQ, RealArrQ, NumArrQ, ExtNumArrQ,

    StrOrVecQ, BoolOrVecQ, SymOrVecQ, IntOrVecQ, RuleOrVecQ, RuleLOrVecQ, ORuleOrVecQ,
    IntKeysQ, StrKeysQ, DictKeysQ, SymKeysQ, IntValsQ, StrValsQ, ListValsQ, DictValsQ, BoolValsQ, SymValsQ,
    HasLenQ, HasDimsQ, SameLenQ, SameOKeysQ, AllSameLenQ, AllSameOKeysQ,
    HasDupsQ, DupFreeQ, ContainsDictQ,

    StrMatchQ, StrDelimQ, StrStartsQ, StrEndsQ, StrFreeQ, StrContainsQ, StrHasQ, ASCIIQ,

    VFreeQ, VContainsQ, ExceptionFreeQ, ComplexPresentQ, ContainsDictQ, PatternPresentQ, PatternFreeQ, CouldContainQ,
    AssocScanWhileQ, EmptyComplementQ, EmptyIntersectionQ, SyntacticNegativeQ, IntegerPartitionQ, TensorTypeQ, ValidSymbolNameQ, HashSameQ, Base64StringQ,
    ListRepQ, ContainsListRepQ,
    HasUsageQ,

  "PredicateOperator",
    LenOf, DimsOf, DictOf, ListDictOf, NEListOf, NEDictOf, NEListDictOf, RuleVecOf
];

(*************************************************************************************************)

(* TODO: Mappable? *)

(* These are defined upfront so we can use them in later pattern aliases *)

DefineAliasRules[
  SymbolQ                    -> Developer`SymbolQ,
  RealQ                      -> Developer`RealQ,
  NaturalQ                   -> Internal`NonNegativeIntegerQ,
  PositiveIntegerQ           -> Internal`PositiveIntegerQ,
  NegativeIntegerQ           -> Internal`NegativeIntegerQ,
  NonPositiveIntegerQ        -> Internal`NonPositiveIntegerQ,
  NonNegativeIntegerQ        -> Internal`NonNegativeIntegerQ,
  MachineRealQ               -> Developer`MachineRealQ,
  MachineIntegerQ            -> Developer`MachineIntegerQ,
  PositiveMachineIntegerQ    -> Internal`PositiveMachineIntegerQ,
  NegativeMachineIntegerQ    -> Internal`NegativeMachineIntegerQ,
  NonPositiveMachineIntegerQ -> Internal`NonPositiveMachineIntegerQ,
  NonNegativeMachineIntegerQ -> Internal`NonNegativeMachineIntegerQ,
  MachineComplexQ            -> Developer`MachineComplexQ
];

DefineAliasRules[
  AssociationVectorQ -> Developer`AssociationVectorQ,
  ListOrAssociationQ -> Developer`ListOrAssociationQ,
  StringOrVectorQ    -> Developer`StringOrStringVectorQ,
  StringVectorQ      -> Developer`StringVectorQ,
  UnsafeEmptyQ       -> Developer`EmptyQ,
  NotEmptyQ          -> Developer`NotEmptyQ,
  NonEmptyQ          -> Developer`NotEmptyQ,
  HoldSymbolQ        -> Developer`HoldSymbolQ,
  HoldAtomQ          -> Developer`HoldAtomQ,
  HasKeyQ            -> KeyExistsQ
];

DefineAliasRules[
  StrQ             -> StringQ,
  SymQ             -> SymbolQ,
  IntQ             -> IntegerQ,
  NatQ             -> NaturalQ,
  NumQ             -> RealValuedNumberQ,
  BoolQ            -> BooleanQ,
  DictQ            -> AssociationQ,
  UDictQ           -> UnorderedAssociationQ,
  ODictQ           -> OrderedAssociationQ,
  ListDictQ        -> ListOrAssociationQ
];

DefineAliasRules[
  HStrQ            -> HoldStringQ,
  HSymQ            -> HoldSymbolQ,
  HIntQ            -> HoldIntegerQ,
  HNatQ            -> HoldNaturalQ,
  HNumQ            -> HoldNumberQ,
  HBoolQ           -> HoldBooleanQ,
  HAtomQ           -> HoldAtomQ,
  HEmptyQ          -> HoldEmptyQ,
  HNotEmptyQ       -> HoldNotEmptyQ,
  HListQ           -> HoldListQ,
  HPackedQ         -> HoldPackedArrayQ,
  HDictQ           -> HoldAssociationQ,
  HUDictQ          -> HoldUnorderedAssociationQ,
  HODictQ          -> HoldOrderedAssociationQ
];

(**************************************************************************************************)

(* TODO: rename InfinityQ to InfiniteQ *)

DefineAliasRules[
  AutoQ            -> AutomaticQ,
  InheritQ         -> InheritedQ,
  NotAutoQ         -> NotAutomaticQ,
  InfQ             -> InfinityQ,
  NotInfQ          -> NotInfinityQ
];

DefineAliasRules[
  ORuleQ           -> OptionRuleQ,
  RuleLQ           -> RuleLikeQ,
  RuleDQ           -> RuleDelayedQ
];

(**************************************************************************************************)

DefineAliasRules[
  ZeroQ            -> ZeroNumberQ,
  NonZeroQ         -> NonZeroNumberQ
];

DefineAliasRules[
  ZeroIntQ         -> ZeroIntegerQ,
  ZeroNumQ         -> ZeroNumberQ,
  ZeroRealQ        -> ZeroRealQ,
  NonZeroIntQ      -> NonZeroIntegerQ,
  NonZeroRealQ     -> NonZeroRealQ,
  NonZeroNumQ      -> NonZeroNumberQ,

  PosIntQ          -> PositiveIntegerQ,
  NegIntQ          -> NegativeIntegerQ,
  PosRealQ         -> PositiveRealQ,
  NegRealQ         -> NegativeRealQ,
  PosNumQ          -> PositiveNumberQ,
  NegNumQ          -> NegativeNumberQ,

  NonPosIntQ       -> NonPositiveIntegerQ,
  NonNegIntQ       -> NonNegativeIntegerQ,
  NonPosRealQ      -> NonPositiveRealQ,
  NonNegRealQ      -> NonNegativeRealQ,
  NonPosNumQ       -> NonPositiveNumberQ,
  NonNegNumQ       -> NonNegativeNumberQ,

  UnitIntQ         -> UnitIntegerQ,
  UnitNumQ         -> UnitNumberQ,

  ExtIntQ          -> ExtendedIntegerQ,
  ExtNatQ          -> ExtendedNaturalQ,
  ExtNumQ          -> ExtendedNumberQ,
  ExtPosRealQ      -> ExtendedPositiveRealQ,
  ExtPosNumQ       -> ExtendedPositiveNumberQ,
  ExtNonNegRealQ   -> ExtendedNonNegativeRealQ,
  ExtNonNegNumQ    -> ExtendedNonNegativeNumberQ
];

(**************************************************************************************************)

DefineAliasRules[
  FailedQ          -> FailureQ,
  FailQ            -> FailureQ,
  NotFailedQ       -> NotFailureQ,
  NotFailQ         -> NotFailureQ
];

DefineAliasRules[
  DictOfQ          -> AssociationOfQ,
  ListDictOfQ      -> ListAssociationOfQ
];

DefineAliasRules[
  LenOf            -> LengthOf,
  DimsOf           -> DimensionsOf,
  DictOf           -> AssociationOf,
  ListDictOf       -> ListAssociationOf,
  NEListOf         -> NonEmptyListOf,
  NEDictOf         -> NonEmptyAssociationOf,
  NEListDictOf     -> NonEmptyListAssociationOf,
  RuleVecOf        -> RuleVectorOf
];

DefineAliasRules[
  NEDictQ          -> NonEmptyAssociationQ,
  NEDictOfQ        -> NonEmptyAssociationOfQ,
  NEListQ          -> NonEmptyListQ,
  NEListOfQ        -> NonEmptyListOfQ,
  NEListDictQ      -> NonEmptyListAssociationQ,
  NEListDictOfQ    -> NonEmptyListAssociationOfQ,
  NEAllSameQ       -> NonEmptyAllSameQ,
  NEAllSameByQ     -> NonEmptyAllSameByQ,
  NEAllSameLenQ    -> NonEmptyAllSameLengthQ,
  NEAllSameHeadsQ  -> NonEmptyAllSameHeadsQ,
  NEAllSamePartsQ  -> NonEmptyAllSamePartsQ,
  NEAllSameSetsQ   -> NonEmptyAllSameSetsQ,
  NEAllSameKeysQ   -> NonEmptyAllSameKeysQ,
  NEAllSameOKeysQ  -> NonEmptyAllSameOrderedKeysQ
];

(**************************************************************************************************)

DefineAliasRules[
  RuleVecQ         -> RuleVectorQ,
  RuleLVecQ        -> RuleLikeVectorQ,
  ORuleVecQ        -> OptionRuleVectorQ,
  DictLikeQ        -> AssociationLikeQ
];

DefineAliasRules[
  VecQ             -> VectorQ,
  DictVecQ         -> AssociationVectorQ,
  PairVecQ         -> PairVectorQ,
  ListVecQ         -> ListVectorQ,
  BoolVecQ         -> BooleanVectorQ,
  SymVecQ          -> SymbolVectorQ,
  ColVecQ          -> ColorVectorQ,
  StrVecQ          -> StringVectorQ,
  NatVecQ          -> NaturalVectorQ,
  IntVecQ          -> IntegerVectorQ,
  PosIntVecQ       -> PositiveIntegerVectorQ,
  RealVecQ         -> RealVectorQ,
  NumVecQ          -> NumberVectorQ,
  ExtNumVecQ       -> ExtendedNumberVectorQ
];

DefineAliasRules[
  AnyMatQ          -> AnyMatrixQ,
  DictMatQ         -> AssociationMatrixQ,
  PairMatQ         -> PairMatrixQ,
  ListMatQ         -> ListMatrixQ,
  BoolMatQ         -> BooleanMatrixQ,
  SymMatQ          -> SymbolMatrixQ,
  ColMatQ          -> ColorMatrixQ,
  StrMatQ          -> StringMatrixQ,
  NatMatQ          -> NaturalMatrixQ,
  IntMatQ          -> IntegerMatrixQ,
  PosIntMatQ       -> PositiveIntegerMatrixQ,
  RealMatQ         -> RealMatrixQ,
  NumMatQ          -> NumberMatrixQ,
  ExtNumMatQ       -> ExtendedNumberMatrixQ
];

DefineAliasRules[
  ArrQ             -> ArrayQ,
  DictArrQ         -> AssociationArrayQ,
  PairArrQ         -> PairArrayQ,
  ListArrQ         -> ListArrayQ,
  BoolArrQ         -> BooleanArrayQ,
  SymArrQ          -> SymbolArrayQ,
  ColArrQ          -> ColorArrayQ,
  StrArrQ          -> StringArrayQ,
  NatArrQ          -> NaturalArrayQ,
  IntArrQ          -> IntegerArrayQ,
  PosIntArrQ       -> PositiveIntegerArrayQ,
  RealArrQ         -> RealArrayQ,
  NumArrQ          -> NumberArrayQ,
  ExtNumArrQ       -> ExtendedNumberArrayQ
];

DefineAliasRules[
  StrOrVecQ        -> StringOrVectorQ,
  BoolOrVecQ       -> BooleanOrVectorQ,
  SymOrVecQ        -> SymbolOrVectorQ,
  IntOrVecQ        -> IntegerOrVectorQ,
  NatOrVecQ        -> NaturalOrVectorQ,
  RuleOrVecQ       -> RuleOrVectorQ,
  RuleLOrVecQ      -> RuleLikeOrVectorQ,
  ORuleOrVecQ      -> OptionRuleOrVectorQ
];

(**************************************************************************************************)

DefineAliasRules[
  IntKeysQ         -> IntegerKeysQ,
  StrKeysQ         -> StringKeysQ,
  DictKeysQ        -> AssociationKeysQ,
  SymKeysQ         -> SymbolKeysQ,
  IntValsQ         -> IntegerValuesQ,
  StrValsQ         -> StringValuesQ,
  ListValsQ        -> ListValuesQ,
  DictValsQ        -> AssociationValuesQ,
  BoolValsQ        -> BooleanValuesQ,
  SymValsQ         -> SymbolValuesQ
];

DefineAliasRules[
  HasLenQ          -> HasLengthQ,
  HasDimsQ         -> HasDimensionsQ,
  SameLenQ         -> SameLengthQ,
  SameOKeysQ       -> SameOrderedKeysQ,
  AllSameLenQ      -> AllSameLengthQ,
  AllSameOKeysQ    -> AllSameOrderedKeysQ
];

DefineAliasRules[
  HasDupsQ         -> HasDuplicatesQ,
  DupFreeQ         -> DuplicateFreeQ
];

(**************************************************************************************************)

DefineAliasRules[
  StrMatchQ        -> StringMatchQ,
  StrDelimQ        -> StringStartsEndsQ,
  StrStartsQ       -> StringStartsQ,
  StrEndsQ         -> StringEndsQ,
  StrFreeQ         -> StringFreeQ,
  StrContainsQ     -> StringContainsQ,
  StrHasQ          -> StringContainsQ,
  ASCIIQ           -> PrintableASCIIQ
];

(**************************************************************************************************)

DefineAliasRules[
  VFreeQ           -> Internal`LiterallyAbsentQ,
  VContainsQ       -> Internal`LiterallyOccurringQ,
  ExceptionFreeQ   -> Internal`ExceptionFreeQ,
  ComplexPresentQ  -> Internal`HasComplex,
  ContainsDictQ    -> ContainsAssociationQ,
  PatternPresentQ  -> Internal`PatternPresentQ,
  PatternFreeQ     -> Internal`PatternFreeQ,
  CouldContainQ    -> System`Private`CouldContainQ
];

DefineAliasRules[
  AssocScanWhileQ    -> Association`ScanWhile,
  EmptyComplementQ   -> Language`EmptyComplementQ,
  EmptyIntersectionQ -> Language`EmptyIntersectionQ,
  SyntacticNegativeQ -> Internal`SyntacticNegativeQ,
  IntegerPartitionQ  -> Internal`IntegerPartitionQ,
  TensorTypeQ        -> Internal`TensorTypeQ,
  ValidSymbolNameQ   -> Internal`SymbolNameQ,
  HashSameQ          -> Internal`HashSameQ,
  Base64StringQ      -> Developer`Base64StringQ
];

(*************************************************************************************************)

DefineAliasRules[
  ListRepQ         -> Developer`ListRepresentationQ,
  ContainsListRepQ -> Internal`ContainsListRepresentationQ,
  HasUsageQ        -> DocumentedSymbolQ
];
