SystemExports[
  "Predicate",

    ZeroIntegerQ, NonZeroIntegerQ, UnitIntegerQ, ExtendedIntegerQ, ExtendedNaturalQ, ExtendedPositiveIntegerQ,
    ZeroRealQ,   NonZeroRealQ,   PositiveRealQ,   NegativeRealQ,   NonPositiveRealQ,   NonNegativeRealQ,   UnitRealQ,   ExtendedRealQ, ExtendedPositiveRealQ, ExtendedNonNegativeRealQ,
    ZeroNumberQ, NonZeroNumberQ, PositiveNumberQ, NegativeNumberQ, NonPositiveNumberQ, NonNegativeNumberQ, UnitNumberQ, ExtendedNumberQ, ExtendedPositiveNumberQ, ExtendedNonNegativeNumberQ,

    NotMissingQ, NotFailureQ,

    UnorderedAssociationQ, OrderedAssociationQ,
    HoldUnorderedAssociationQ, HoldOrderedAssociationQ,

  "Predicate",

    AtomicQ, NonAtomicQ,
    SingleQ, DatumQ,
    HoldDatumQ, HoldSingleQ, HoldEmptyQ, HoldNotEmptyQ,
    NotHeadQ, HasHeadQ, VectorHasHeadsQ, AssociationHasHeadsQ,
    HasKeysQ, HasLengthQ, HasDimensionsQ, HasArrayDepthQ,
    HoldHasLengthQ,

    ElementQ, SameSetQ, SubsetOfQ, SupersetOfQ, IntersectsQ, NotIntersectsQ, PrefixListQ,
    SameKeysQ, SameOrderedKeysQ, SameHeadQ, SameLengthQ, SameShapeQ, SamePartsQ,
    HoldSameQ, HoldHasHeadQ, HoldSameLengthQ,

    PairQ, PairVectorQ, PairMatrixQ, PairArrayQ,

    OptionRuleQ,
    RuleVectorQ, RuleLikeVectorQ, RuleDelayedVectorQ, AssociationLikeQ, OptionRuleVectorQ,
    RangeQ, PermutedRangeQ,

  "Predicate",

    BooleanVectorQ, IntegerVectorQ, NaturalVectorQ, PositiveIntegerVectorQ, SymbolVectorQ, RealVectorQ, NumberVectorQ, ListVectorQ, ColorVectorQ, ExtendedNumberVectorQ,
    BooleanMatrixQ, IntegerMatrixQ, NaturalMatrixQ, PositiveIntegerMatrixQ, SymbolMatrixQ, RealMatrixQ, NumberMatrixQ, ListMatrixQ, ColorMatrixQ, ExtendedNumberMatrixQ, AnyMatrixQ,
    BooleanArrayQ,  IntegerArrayQ,  NaturalArrayQ,  PositiveIntegerArrayQ,  SymbolArrayQ,  RealArrayQ,  NumberArrayQ,  ListArrayQ,  ColorArrayQ,  ExtendedNumberArrayQ,
    StringMatrixQ,  AssociationMatrixQ,
    StringArrayQ,   AssociationArrayQ,

    NullQ, NoneQ, ScaledQ, AutomaticQ, InheritedQ, InfinityQ,
    NotNullQ, NotNoneQ, NotScaledQ, NotAutomaticQ, NotInfinityQ,
    NotAllSameQ, AllEqualQ, NotAllEqualQ, NotMatchQ,
    AnySameQ, NoneSameQ, AnySameByQ, NoneSameByQ,

  "Predicate",

    ZipAllTrueQ,
    AllSameQ,            NonEmptyAllSameQ,
    AllSameByQ,          NonEmptyAllSameByQ,
    AllSameLengthQ,      NonEmptyAllSameLengthQ,
    AllSameHeadsQ,       NonEmptyAllSameHeadsQ,
    AllSamePartsQ,       NonEmptyAllSamePartsQ,
    AllSameSetsQ,        NonEmptyAllSameSetsQ,
    AllSameKeysQ,        NonEmptyAllSameKeysQ,
    AllSameOrderedKeysQ, NonEmptyAllSameOrderedKeysQ,

    FalseQ, AllAreTrueQ, AnyAreTrueQ, NoneAreTrueQ, AllAreFalseQ, AnyAreFalseQ, NoneAreFalseQ,

  "Predicate",

    ListableFunctionQ, AssociationFunctionQ, PureFunctionQ,
    ContainsQ, ContainsWithinQ, FreeWithinQ, ContainsAssociationQ,
    HoldFreeQ, HoldContainsQ, HoldArgsFreeQ, HoldArgsContainQ,
    KeysTrue, ValuesTrue, KeysValuesTrue, RuleKeysTrue, RuleValuesTrue, RulesTrue,
    IntegerKeysQ, StringKeysQ, ListKeysQ, AssociationKeysQ, SymbolKeysQ,
    IntegerValuesQ, StringValuesQ, ListValuesQ, AssociationValuesQ, BooleanValuesQ, SymbolValuesQ,
    HasDuplicatesQ,
    AnyMissingQ, NoneMissingQ, AnyFailedQ, NoneFailedQ,

    ListOfQ,                 AssociationOfQ,         ListAssociationOfQ,
    NonEmptyListQ,     NonEmptyAssociationQ,   NonEmptyListAssociationQ,
    NonEmptyListOfQ, NonEmptyAssociationOfQ, NonEmptyListAssociationOfQ,
    PairOfQ, TupleOfQ, RecordOfQ, StructureOfQ, ArrayOfQ,

    BooleanOrVectorQ, SymbolOrVectorQ, IntegerOrVectorQ, NaturalOrVectorQ, RealOrVectorQ,
    RuleOrVectorQ, RuleLikeOrVectorQ, OptionRuleOrVectorQ
];

PackageExports[
  "Predicate",
    WithinUQ, WithinSQ,
    OutsideUQ, OutsideSQ,
    EmptyQ,
    Nat2Q, PosInt2Q, Int2Q, ExtNatQ, ExtIntQ, ExtPosIntQ,
    Num2Q, Num23Q, Num3Q,
    Pos2Q, Pos2ListQ, Pos2ListsQ, Pos2PairQ,
    Pos3Q, Pos3ListQ, Pos3ListsQ, Pos3PairQ,
    PosAQ, PosAListQ, PosAListsQ, PosAPairQ,
    Pos2ListOrListsQ,
    RuleLikeQ, RuleQ, RuleDelayedQ,
    PackedRealsQ, PackedIntsQ, PackedComplexQ, PackedNumsQ,
    AutoNoneQ, NotAutoNoneQ,
    HoldVFreeQ, HoldVContainsQ,
    OnePartSpecQ, MultiPartSpecQ, ExtPartSpecQ,
    PackedIntVecQ, PackedRealVecQ, PackedNumVecQ, PackedIntMatQ, PackedRealMatQ, PackedNumMatQ,
    PackedDepth1Q, PackedDepth2Q,
    PackedDepth12Q, PackedDepth2NQ, PackedDepth3NQ,
    ArrayDepth12Q, ArrayDepth2NQ, ArrayDepth3NQ,
    ArrayDepthAtLeastQ, PackedDepthAtLeastQ,
    RectListVecQ,

  "Variable",
    $IntegerPredicateFns,
    $RealPredicateFns,
    $NumberPredicateFns,
    $SymbolPredicateFns,
    $ScalarPredicateFns,
    $PosPredicateFns,
    $VectorPredicateFns,
    $MatrixPredicateFns,
    $ArrayPredicateFns,

  "MetaFunction",

    DeclarePatternPredicates,
    DefinePatternPredicateRules,
    SetVectorListableOp
];

(*************************************************************************************************)

$IntegerPredicateFns = List[ZeroIntQ, NonZeroIntQ, UnitIntegerQ, ExtendedIntegerQ, ExtendedNaturalQ, ExtendedPositiveIntegerQ];
$RealPredicateFns    = List[ZeroRealQ, NonZeroRealQ, PositiveRealQ, NegativeRealQ, NonPositiveRealQ, NonNegativeRealQ, UnitRealQ, ExtendedRealQ, ExtendedPositiveRealQ];
$NumberPredicateFns  = List[ZeroNumberQ, NonZeroNumberQ, PositiveNumberQ, NegativeNumberQ, NonPositiveNumberQ, NonNegativeNumberQ, UnitNumberQ, ExtendedNumberQ, ExtendedPositiveNumberQ];
$SymbolPredicateFns  = List[TrueQ, FalseQ, NullQ, NoneQ, AutomaticQ, InheritedQ, NotNullQ, NotNoneQ, NotAutomaticQ];

$ScalarPredicateFns = ToList[
  $IntegerPredicateFns,
  $RealPredicateFns,
  $NumberPredicateFns,
  $SymbolPredicateFns,
  InfinityQ, NotInfinityQ,
  OptionRuleQ, RuleQ, RuleLikeQ, RuleDelayedQ,
  AtomicQ, NonAtomicQ,
  DatumQ
];

$VectorPredicateFns = List[
  PairQ, PairVectorQ,
  RangeQ, PermutedRangeQ,
  SymbolVectorQ, BooleanVectorQ,
  IntegerVectorQ, NaturalVectorQ, PositiveIntegerVectorQ,
  RealVectorQ, NumberVectorQ, ExtendedNumberVectorQ,
  ListVectorQ, ColorVectorQ,
  AssociationVectorQ, StringVectorQ
];

$MatrixPredicateFns = List[
  PairMatrixQ,
  BooleanMatrixQ, IntegerMatrixQ, NaturalMatrixQ, PositiveIntegerMatrixQ,
  StringMatrixQ, SymbolMatrixQ, RealMatrixQ, NumberMatrixQ, ListMatrixQ,
  ColorMatrixQ, ExtendedNumberMatrixQ,
  AnyMatrixQ, AssociationMatrixQ
];

$ArrayPredicateFns = List[
  PairArrayQ,
  BooleanArrayQ, SymbolArrayQ,
  IntegerArrayQ, NaturalArrayQ, PositiveIntegerArrayQ, StringArrayQ,
  RealArrayQ, NumberArrayQ, ListArrayQ, ColorArrayQ, ExtendedNumberArrayQ,
  AssociationArrayQ, PackedIntsQ, PackedRealsQ
];

(*************************************************************************************************)

SetPred1[WithinUQ, WithinSQ];
SetNPred1[OutsideUQ, OutsideSQ];

WithinUQ[e_ /;  0 <= e <= 1] = True;
WithinSQ[e_ /; -1 <= e <= 1] = True;
OutsideUQ[e_ /;  0 <= e <= 1] = False;
OutsideSQ[e_ /; -1 <= e <= 1] = False;

(*************************************************************************************************)

(* the other integer predicates like PosIntQ are already kernel functions *)

DefinePatternPredicateRules[
  ZeroIntegerQ             -> ZeroIntP,
  NonZeroIntegerQ          -> NonZeroIntP,
  UnitIntegerQ             -> UnitIntP,
  ExtendedIntegerQ         -> ExtIntP,
  ExtendedNaturalQ         -> ExtNatP,
  ExtendedPositiveIntegerQ -> ExtPosIntP
];

(*************************************************************************************************)

DefinePatternPredicateRules[
  ZeroRealQ                -> ZeroRealP,
  NonZeroRealQ             -> NonZeroRealP,
  PositiveRealQ            -> PosRealP,
  NegativeRealQ            -> NegRealP,
  NonPositiveRealQ         -> NonPosRealP,
  NonNegativeRealQ         -> NonNegRealP,
  UnitRealQ                -> UnitRealP,
  ExtendedRealQ            -> ExtRealP,
  ExtendedPositiveRealQ    -> ExtPosRealP,
  ExtendedNonNegativeRealQ -> ExtNonNegRealP
];

(*************************************************************************************************)

DefinePatternPredicateRules[
  ZeroNumberQ                -> ZeroNumP,
  NonZeroNumberQ             -> NonZeroNumP,
  PositiveNumberQ            -> PosNumP,
  NegativeNumberQ            -> NegNumP,
  NonPositiveNumberQ         -> NonPosNumP,
  NonNegativeNumberQ         -> NonNegNumP,
  UnitNumberQ                -> UnitNumP,
  ExtendedNumberQ            -> ExtNumP,
  ExtendedPositiveNumberQ    -> ExtPosNumP,
  ExtendedNonNegativeNumberQ -> ExtNonNegNumP
];

(*************************************************************************************************)

NotMissingQ[_Missing] := False;
NotMissingQ[_]        := True;

(*************************************************************************************************)

NotFailureQ[_ ? FailureQ] := False;
NotFailureQ[_] := True;

(*************************************************************************************************)

SetPred1[UnorderedAssociationQ, OrderedAssociationQ]

(* this relies on the fact that Take of a UDict doesn't evaluate (and issues no messages) *)
UnorderedAssociationQ[EmptyUDict] := True;
UnorderedAssociationQ[dict:DictP] := NotEmptyQ @ Take[dict, 0];

OrderedAssociationQ[EmptyDict] := True;
OrderedAssociationQ[dict:DictP] := EmptyQ @ Take[dict, 0];

(*************************************************************************************************)

SetPred1 @ SetHoldC[HoldUnorderedAssociationQ, HoldOrderedAssociationQ]

HoldUnorderedAssociationQ[EmptyUDict] := True;
HoldUnorderedAssociationQ[dict:DictP] := NotEmptyQ @ Take[dict, 0];

HoldOrderedAssociationQ[EmptyDict]  := True;
HoldOrderedAssociationQ[dict:DictP] := EmptyQ @ Take[dict, 0];

(*************************************************************************************************)

SetPred1[OnePartSpecQ, MultiPartSpecQ, ExtPartSpecQ]

OnePartSpecQ[OnePartSpecP] = True;

MultiPartSpecQ = CaseOf[
  All                                := True;
  Span[_Int|All]                     := True;
  Span[_Int|All, _Int|All]           := True;
  Span[_Int|All, _Int|All, _Int|All] := True;
  List[OnePartSpecP..]                := True;
];

ExtPartSpecQ = CaseOf[
  All                              := True;
  (_Span | _List) ? MultiPartSpecQ := True;
  _Integer | _Key | _String        := True;
];

(*************************************************************************************************)

EmptyQ[_ ? UnsafeEmptyQ]  = True;
EmptyQ[_ ? NonEmptyQ]     = False;
EmptyQ[_]                 = True;

AtomicQ[_Dict]            = False;
AtomicQ[_ ? HAtomQ]       = True;
AtomicQ[_]                = False;

NonAtomicQ[_Dict]         = True;
NonAtomicQ[_ ? HAtomQ]    = False;
NonAtomicQ[_]             = True;

(*************************************************************************************************)

SetPred2[ListOfQ, AssociationOfQ, ListAssociationOfQ];
SetPred1[NonEmptyListQ, NonEmptyAssociationQ, NonEmptyListAssociationQ];
SetPred2[NonEmptyListOfQ, NonEmptyAssociationOfQ, NonEmptyListAssociationOfQ];

           ListOfQ[data_List,  pred_] := VectorQ[data, pred];
    AssociationOfQ[dict:DictP, pred_] := VectorQ[Values @ dict, pred];
ListAssociationOfQ[data_List,  pred_] := VectorQ[data, pred];
ListAssociationOfQ[dict:DictP, pred_] := VectorQ[Values @ dict, pred];

           NonEmptyListQ[NEListP]     = True;
    NonEmptyAssociationQ[NEDictP]     = True;
NonEmptyListAssociationQ[NEListDictP] = True;

           NonEmptyListOfQ[list:NEListP, pred_] := VectorQ[list, pred];
    NonEmptyAssociationOfQ[dict:NEDictP, pred_] := VectorQ[Values @ dict, pred];
NonEmptyListAssociationOfQ[list:NEListP, pred_] := VectorQ[list, pred];
NonEmptyListAssociationOfQ[dict:NEDictP, pred_] := VectorQ[Values @ dict, pred];

(*************************************************************************************************)

SetPred2[PairOfQ, TupleOfQ, RecordOfQ, StructureOfQ, ArrayOfQ];

PairOfQ[{a_, b_}, {t1_, t2_}] := TrueQ[t1[a] && t2[b]];
PairOfQ[{a_, b_}, t_]         := TrueQ[t[a] && t[b]];

TupleOfQ[data_List, tests_List] /; SameLengthQ[data, tests]  := AllAreTrueQ @ Bimap[tests, data];
RecordOfQ[data_Dict, tests_Dict] /; SameKeysQ[data, tests] := AllAreTrueQ @ Bimap[Vals @ tests, Lookup[data, Keys @ tests]];

StructureOfQ[data_List, tests_List]   := TupleOfQ[data, tests];
StructureOfQ[data_Dict, tests_Dict] := RecordOfQ[data, tests];

ArrayOfQ[data_List, test_]     := TensorQ[data, test];
ArrayOfQ[data_, test_, {}]     := test @ data;
ArrayOfQ[data_, test_, shape_] := HasDimensionsQ[data, shape] && TensorQ[data, test];

(*************************************************************************************************)

DeclareSeqScan[DefinePatternPredicateRules];

DefinePatternPredicateRules[sym_Symbol -> patt_] := (
  sym[patt] := True;
  sym[_]    := False;
);

DeclarePatternPredicates::notP = "Pattern symbols `` should end in Q.";
DeclarePatternPredicates::notQ = "Pattern symbols `` did not appear to exist.";
DeclarePatternPredicates[syms___Symbol] := Locals[
  predicateSyms = {syms};
  predicateNames = Map[HoldSymbolPath, predicateSyms];
  If[AnyAreFalseQ @ StringEndsQ[predicateNames, "Q"], ReturnMsg["notP", predicateNames]];
  patternNames = StringAppend[StringDrop[predicateNames, -1], "P"];
  If[!AllTrue[patternNames, NameQ], ReturnMsg["notQ", Select[patternNames, NameQ /* Not]]];
  MapThread[setPattPred, {predicateSyms, patternNames}];
];

setPattPred[sym_, pattName_Str] := With[{patt = Symbol @ pattName},
  sym[patt] = True;
  sym[_] = False;
];

(*************************************************************************************************)

SetPred1[SingleQ, DatumQ];

SingleQ[e_] := Len[e] === 1;
DatumQ[DatumP] := True;

(*************************************************************************************************)

SetHoldC @ SetPred1[HoldSingleQ, HoldDatumQ, HoldEmptyQ, HoldNotEmptyQ];

HoldSingleQ[_[_]]          = True;
HoldDatumQ[DatumP]         = True;
HoldEmptyQ[EmptyP]         = True;
HoldNotEmptyQ[NonEmptyP]   = True;

(*************************************************************************************************)

DeclarePatternPredicates[Nat2Q, Int2Q, PosInt2Q];
DeclarePatternPredicates[Num2Q, Num3Q, Num23Q];

(*************************************************************************************************)

$PosPredicateFns = List[
  Pos2Q, Pos2ListQ, Pos2ListsQ, Pos2PairQ,
  Pos3Q, Pos3ListQ, Pos3ListsQ, Pos3PairQ,
  PosAQ, PosAListQ, PosAListsQ, PosAPairQ,
  Pos2ListOrListsQ
];

SetPred1[Pos2Q, Pos2ListQ, Pos2ListsQ, Pos2PairQ];
SetPred1[Pos3Q, Pos3ListQ, Pos3ListsQ, Pos3PairQ];
SetPred1[PosAQ, PosAListQ, PosAListsQ, PosAPairQ];

Pos2Q[{_, _} ? PackedNumVecQ] = True;
Pos2Q[{NumP, NumP}] = True;

Pos3Q[{_, _, _} ? PackedNumVecQ] = True;
Pos3Q[{NumP, NumP, NumP}] = True;

PosAQ[{_, _} ? PackedNumVecQ] = True;
PosAQ[{_, _, _} ? PackedNumVecQ] = True;
PosAQ[{NumP, NumP}] = True;
PosAQ[{NumP, NumP, NumP}] = True;
PosAQ[{Repeated[_ ? RealValuedNumericQ, {2, 3}]}] := True;

Pos2ListQ[a_List] := NumMatQ[a] && LenN[a] === 2;
Pos3ListQ[a_List] := NumMatQ[a] && LenN[a] === 3;
PosAListQ[a_List] := NumMatQ[a] && 2 <= LenN[a] <= 3;

Pos2ListsQ[a_List ? PackedQ] := PackedNumsQ[a, 3] && LenN[a] === 2;
Pos3ListsQ[a_List ? PackedQ] := PackedNumsQ[a, 3] && LenN[a] === 3;
PosAListsQ[a_List ? PackedQ] := PackedNumsQ[a, 3] && 2 <= LenN[a] <= 3;
Pos2ListsQ[a_List] := VectorQ[a, Pos2ListQ];
Pos3ListsQ[a_List] := VectorQ[a, Pos3ListQ];
PosAListsQ[a_List] := VectorQ[a, Pos2ListQ] || VectorQ[a, Pos3ListQ];

Pos2PairQ[a_List] := NumArrQ[a] && SameQ[Dims @ a, {2, 2}];
Pos3PairQ[a_List] := NumArrQ[a] && SameQ[Dims @ a, {2, 3}];
PosAPairQ[a_List] := NumArrQ[a] && MatchQ[Dims @ a, {2, 2|3}];

(*************************************************************************************************)

SetPred1 @ Pos2ListOrListsQ;

(* TODO: use CoordinateDimension here *)
Pos2ListOrListsQ[a_List] := Pos2ListsQ[a] || Pos2ListQ[a];

(*************************************************************************************************)

HasHeadQ[e_, h_]       := MatchQ[e, ToBlankP @ h];
HasHeadQ[h_]           := MatchQ[ToBlankP @ h];

NotHeadQ[e_, h_]       := MatchQ[e, Except @ ToBlankP @ h];
NotHeadQ[h_]           := MatchQ[Except @ ToBlankP @ h];

VectorHasHeadsQ[e_, h_] := MatchQ[e, List @ ToBlankNullSeqP @ h];
VectorHasHeadsQ[h_]     := MatchQ[List @ ToBlankNullSeqP @ h];

SetCurry2 @ SetPred2 @ AssociationHasHeadsQ

AssociationHasHeadsQ[a_Dict, h_] := MatchQ[Values @ a, List @ ToBlankNullSeqP @ h];

(*************************************************************************************************)

SetCurry2 @ SetPred2[HasKeysQ, HasLengthQ, HasDimensionsQ, HasArrayDepthQ]

HasKeysQ[dict_Dict, keys_List] := Len[dict] === Len[keys] && SameQ[Keys @ dict, keys];

HasLengthQ[expr_, n_Integer] := Len[expr] === n;

HasDimensionsQ[_, {}] := True;
HasDimensionsQ[expr_, dims_List] := MatchQ[Dimensions[expr, Len @ dims], dims];

HasArrayDepthQ[expr_, depth_] := MatchQ[ArrayDepth @ expr, depth];

HasDuplicatesQ[expr_] := Not @ DuplicateFreeQ @ expr;

SetHoldC @ SetPred2 @ HoldHasLengthQ;

HoldHasLengthQ[e_, n_] := Len[NoEval @ e] == n;
HoldHasLengthQ[n_] := With[{n2 = n}, HoldCompFn[FmE, HoldHasLengthQ[FmE, n]]];

(*************************************************************************************************)

SetPred1[RuleQ, RuleLikeQ, RuleDelayedQ, AssociationLikeQ]

RuleQ[_Rule]                       = True;
RuleLikeQ[_Rule | _RuleDelayed]    = True;
RuleDelayedQ[_RuleDelayed]         = True;
AssociationLikeQ[_Dict ? DictQ]    = True;
AssociationLikeQ[_List ? RuleVecQ] = True;

(*************************************************************************************************)

SetCurry2 @ SetPred2[KeysTrue, ValuesTrue, RuleKeysTrue]

KeysTrue[dict_Dict, kTest_]   := VectorQ[Keys @ dict, kTest];
ValuesTrue[dict_Dict, vTest_] := VectorQ[Values @ dict, vTest];

RuleKeysTrue[rules:{___Rule}, kTest_]   := VectorQ[Keys @ rules, kTest];
RuleValuesTrue[rules:{___Rule}, vTest_] := VectorQ[Values @ rules, vTest];

(*************************************************************************************************)

SetCurry23 @ SetPred3[KeysValuesTrue, RulesTrue]

KeysValuesTrue[dict_Dict, kTest_, vTest_] :=
  VectorQ[Keys @ dict, kTest] && VectorQ[Values @ dict, vTest];

RulesTrue[rules:{___Rule}, kTest_, vTest_] :=
  VectorQ[Keys @ rules, kTest] && VectorQ[Values @ rules, vTest];

(* using AssocScanWhileQ turns out to be slower than extracting keys and doing VectorQ...
UNLESS we expect an early fail, maybe we can have a head LikelyFalse or LikelyTrue that
indicates this! *)

(*
KeysTrue[dict_Dict, kTest_] := AssocScanWhileQ[dict, First /* kTest];
ValuesTrue[dict_Dict, vTest_] := AssocScanWhileQ[dict, Last /* vTest];
KeysValuesTrue[dict_Dict, kTest_, vTest_] := AssocScanWhileQ[dict, rule |-> kTest[First @ rule] && vTest[Last @ rule]];
*)

(*************************************************************************************************)

SetPred1[IntegerKeysQ, StringKeysQ, ListKeysQ, AssociationKeysQ, SymbolKeysQ]

IntegerKeysQ[dict_Dict]       :=     IntegerVectorQ @ Keys @ dict;
StringKeysQ[dict_Dict]        :=      StringVectorQ @ Keys @ dict;
ListKeysQ[dict_Dict]          :=        ListVectorQ @ Keys @ dict;
AssociationKeysQ[dict_Dict]   := AssociationVectorQ @ Keys @ dict;
SymbolKeysQ[dict_Dict]        :=      SymbolVectorQ @ Keys @ dict;

SetPred1[IntegerValuesQ, StringValuesQ, ListValuesQ, AssociationValuesQ, BooleanValuesQ]

IntegerValuesQ[dict_Dict]     :=     IntegerVectorQ @ Values @ dict;
StringValuesQ[dict_Dict]      :=      StringVectorQ @ Values @ dict;
ListValuesQ[dict_Dict]        :=        ListVectorQ @ Values @ dict;
AssociationValuesQ[dict_Dict] := AssociationVectorQ @ Values @ dict;
BooleanValuesQ[dict_Dict]     :=     BooleanVectorQ @ Values @ dict;
SymbolValuesQ[dict_Dict]      :=      SymbolVectorQ @ Values @ dict;

(*************************************************************************************************)

SetHoldA[HoldVFreeQ, HoldVContainsQ]

HoldVFreeQ[e_, h_]     := VFreeQ[NoEval @ e, NoEval @ h];
HoldVContainsQ[e_, h_] := VContainsQ[NoEval @ e, NoEval @ h];

(*************************************************************************************************)

ElementQ::usage = "ElementQ[elem$, {e$1, e$2, $$}] returns True if any of the e$i is the same elem$ occurs verbatim in expr$.";

SetCurry2 @ SetPred2[ElementQ]

ElementQ[elem_, set_] := MemberQ[set, Verbatim @ elem];

(*************************************************************************************************)

SetPred2[SameSetQ, SubsetOfQ, SupersetOfQ, IntersectsQ, NotIntersectsQ, PrefixListQ, SameKeysQ, SameOrderedKeysQ, SamePartsQ]

(* Note:
SubsetQ is super slow! it does a lot of heads checking and doesn't special case SameTest etc.
DisjointQ is the same.
*)

SameSetQ[a_List, b_List] := EmptyComplementQ[a, b] && EmptyComplementQ[b, a];
SubsetOfQ[a_List, b_List] := EmptyComplementQ[a, b];
SupersetOfQ[a_List, b_List] := EmptyComplementQ[b, a];
IntersectsQ[a_List, b_List] := !EmptyIntersectionQ[a, b];
NotIntersectsQ[a_List, b_List] := EmptyIntersectionQ[a, b];

PrefixListQ[a_List, b_List] := Length[a] <= Length[b] && a === Take[b, Length @ a];

SameKeysQ[a_Dict, b_Dict]        := Len[a] === Len[b] && SameSetQ[Keys @ a, Keys @ b];
SameOrderedKeysQ[a_Dict, b_Dict] := Len[a] === Len[b] && SameQ[Keys @ a, Keys @ b];

SamePartsQ[a_List, b_List] := Len[a] === Len[b];
SamePartsQ[a_Dict, b_Dict] := Len[a] === Len[b] && Keys[a] === Keys[b];
SamePartsQ[a_, b_] := Head[a] === Head[b] && Len[a] === Len[b];

(*************************************************************************************************)

SetCurry1[SameHeadQ, SameLengthQ, SameShapeQ]

SameHeadQ[a_, b_] := Head[a] === Head[b];
SameLengthQ[a_, b_] := Length[a] === Length[b];
SameShapeQ[a_, b_] := SamePartsQ[a, b];

(*************************************************************************************************)

SetPred1[PairQ, PairVectorQ, PairMatrixQ, PairArrayQ]

PairQ[{_, _}]         := True;
PairVectorQ[arr_List] := Length2[arr] === 2;
PairMatrixQ[arr_List] := DimensionN[arr, 3] === 2;
PairArrayQ[arr_List]  := LengthN[arr] === 2;

(*************************************************************************************************)

RangeQ[list_]                 := PermutedRangeQ[list] && OrderedQ[list];
PermutedRangeQ[list_]         := IntVecQ[list] && MinMax[list] === {1, Length @ list};

(*************************************************************************************************)

DeclareSeqScan @ setPackedIsFalse;
setPackedIsFalse[sym_Sym] := Then[
  Set[sym[_ ? PackedQ], False],
  Set[sym[{}], True]
];

(*************************************************************************************************)

SetPred1[RuleVectorQ, RuleDelayedVectorQ, RuleLikeVectorQ]
setPackedIsFalse[RuleLikeVectorQ];

RuleVectorQ[{___Rule}]         := True;
RuleDelayedVectorQ[{___RuleD}] := True;
RuleLikeVectorQ[l:RuleLVecP]   := VectorQ[l, RuleLikeQ];

hasBoolsQ[e_] := CouldContainQ[list, True] || CouldContainQ[list, False];

(*************************************************************************************************)

DeclaredHere[BooleanVectorQ, SymbolVectorQ, ColorVectorQ];
setPackedIsFalse[BoolVecQ, SymVecQ, ColVecQ];

BoolVecQ[list_ ? hasBoolsQ]     := MatchQ[list, {True|False...}];
SymVecQ[list_]                  := SymbolQ[First[list]] && MatchQ[list, {___Symbol}];
ColVecQ[list_]                  := VectorQ[list, ColorQ];

DeclaredHere[IntegerVectorQ, NaturalVectorQ, PositiveIntegerVectorQ, RealVectorQ, NumberVectorQ, ExtendedNumberVectorQ];

IntVecQ[list_ ? PackedQ]        := PackedIntVecQ[list];
IntVecQ[list_]                  := VectorQ[list, IntQ];

NatVecQ[list_ ? PackedQ]        := PackedIntVecQ[list] && NonNegative @ Min @ list;
NatVecQ[list_]                  := VectorQ[list, NatQ];

PosIntVecQ[list_ ? PackedQ]     := PackedIntVecQ[list] && Positive @ Min @ list;
PosIntVecQ[list_]               := VectorQ[list, PosIntQ];

RealVecQ[list_ ? PackedQ]       := PackedRealVecQ @ list;
RealVecQ[list_]                 := VectorQ[list, RealQ];

NumVecQ[list_ ? PackedQ]        := PackedNumVecQ @ list;
NumVecQ[list_]                  := VectorQ[list, NumQ];

ExtNumVecQ[list_ ? PackedQ]     := PackedNumVecQ @ list;
ExtNumVecQ[list_]               := VectorQ[list, ExtendedNumberQ];

DeclaredHere[ListVectorQ];

ListVecQ[{}]                    := True;
ListVecQ[list_ ? PackedQ]       := ArrayDepth2NQ @ list;
ListVecQ[list_List]             := ListQ[First[list]] && MatchQ[list, {___List}];

(*************************************************************************************************)

DeclaredHere[BooleanMatrixQ, SymbolMatrixQ, StringMatrixQ, ColorMatrixQ, AssociationMatrixQ];
setPackedIsFalse[BoolMatQ, SymMatQ, StrMatQ, ColMatQ, DictMatQ];

BoolMatQ[list_ ? hasBoolsQ]     := MatrixQ[list, BooleanQ];
StrMatQ[list_]                  := MatrixQ[list, StringQ];
SymMatQ[list_]                  := MatrixQ[list, SymbolQ];
ColMatQ[list_]                  := MatrixQ[list, ColorQ];
DictMatQ[list_]                 := MatrixQ[list, DictQ];

DeclaredHere[IntegerMatrixQ, NaturalMatrixQ, PositiveIntegerMatrixQ, RealMatrixQ, NumberMatrixQ, ExtendedNumberMatrixQ];

IntMatQ[list_ ? PackedQ]        := PackedIntMatQ[list];
IntMatQ[list_]                  := MatrixQ[list, IntQ];

NatMatQ[list_ ? PackedQ]        := PackedIntMatQ[list] && NonNegative @ Min @ list;
NatMatQ[list_]                  := MatrixQ[list, NatQ];

PosIntMatQ[list_ ? PackedQ]     := PackedIntMatQ[list] && Positive @ Min @ list;
PosIntMatQ[list_]               := MatrixQ[list, PosIntQ];

RealMatQ[list_ ? PackedQ]       := PackedRealMatQ @ list;
RealMatQ[list_]                 := MatrixQ[list, RealQ];

NumMatQ[list_ ? PackedQ]        := PackedNumMatQ @ list;
NumMatQ[list_]                  := MatrixQ[list, NumQ];

ExtNumMatQ[list_ ? PackedQ]     := PackedNumMatQ @ list;
ExtNumMatQ[list_]               := MatrixQ[list, ExtendedNumberQ];

DeclaredHere[ListMatrixQ, AnyMatrixQ];

ListMatQ[list_ ? PackedQ]       := ArrayDepth3NQ @ list;
ListMatQ[list_]                 := MatrixQ[list, ListQ];

AnyMatQ[{} | {{}}]              := True;
AnyMatQ[a_ ? PackedQ]           := Not @ PackedDepth1Q @ a;
AnyMatQ[list_List]              := Length[Dimensions[list, 2]] == 2;
AnyMatQ[_]                      := False;

(*************************************************************************************************)

DeclaredHere[BooleanArrayQ, SymbolArrayQ, RealArrayQ, NumberArrayQ, ColorArrayQ, StringArrayQ, AssociationArrayQ];

setPackedIsFalse[BoolArrQ, SymArrQ, ColArrQ, StrArrQ, DictArrQ];

BoolArrQ[list_ ? hasBoolsQ]     := TensorQ[list, BooleanQ];
SymArrQ[list_]                  := TensorQ[list, SymbolQ];
ColArrQ[list_]                  := TensorQ[list, ColorQ];
StrArrQ[list_]                  := TensorQ[list, StringQ];
DictArrQ[list_]                 := TensorQ[list, DictQ];

DeclaredHere[IntegerArrayQ, NaturalArrayQ, PositiveIntegerArrayQ, RealArrayQ, NumberArrayQ, ExtendedNumberArrayQ];

IntArrQ[list_ ? PackedQ]        := PackedIntsQ[list];
IntArrQ[list_]                  := TensorQ[list, IntQ];

NatArrQ[list_ ? PackedQ]        := PackedIntsQ[list] && NonNegative @ Min @ list;
NatArrQ[list_]                  := TensorQ[list, NatQ];

PosIntArrQ[list_ ? PackedQ]     := PackedIntsQ[list] && Positive @ Min @ list;
PosIntArrQ[list_]               := TensorQ[list, PosIntQ];

RealArrQ[list_ ? PackedQ]       := PackedRealsQ @ list;
RealArrQ[list_]                 := TensorQ[list, RealQ];

NumArrQ[list_ ? PackedQ]        := PackedNumsQ @ list;
NumArrQ[list_]                  := TensorQ[list, NumQ];

ExtNumArrQ[list_ ? PackedQ]     := PackedNumsQ @ list;
ExtNumArrQ[list_]               := TensorQ[list, ExtendedNumberQ];

DeclaredHere[ListArrayQ];

ListArrQ[list_]                 := ListVectorQ[list];

(*************************************************************************************************)

SetPred1[PackedIntsQ, PackedRealsQ, PackedComplexQ, PackedNumsQ];

PackedIntsQ[list_]      := PackedQ[list, Int];
PackedRealsQ[list_]     := PackedQ[list, Real];
PackedComplexQ[list_]   := PackedQ[list, Complex];

PackedNumsQ[list_]      := PackedQ[list, Int] || PackedQ[list, Real];
PackedNumsQ[list_, n_]  := PackedQ[list, Int, n] || PackedQ[list, Real, n];

SetPred1[PackedIntVecQ, PackedRealVecQ, PackedNumVecQ, PackedIntMatQ, PackedRealMatQ, PackedNumMatQ];

PackedIntVecQ[list_]    := PackedQ[list, Int, 1];
PackedRealVecQ[list_]   := PackedQ[list, Real, 1];
PackedNumVecQ[list_]    := PackedQ[list, Int, 1] || PackedQ[list, Real, 1];

PackedIntMatQ[list_]    := PackedQ[list, Int, 2];
PackedRealMatQ[list_]   := PackedQ[list, Real, 2];
PackedNumMatQ[list_]    := PackedQ[list, Int, 2] || PackedQ[list, Real, 2];

(*************************************************************************************************)

SetPred2[ArrayDepthAtLeastQ, PackedDepthAtLeastQ]

ArrayDepthAtLeastQ[arr_, n_] := ArrayQ[arr, _ ? (GreaterEqualThan[n])];
PackedDepthAtLeastQ[arr_ ? PackedQ, n_] := Depth[arr] > n;

(*************************************************************************************************)

DeclaredHere[PackedDepth1Q, PackedDepth2Q, PackedDepth12Q, PackedDepth2NQ, PackedDepth3NQ, ArrayDepth12Q, ArrayDepth2NQ, ArrayDepth3NQ];
SetPred1[PackedDepth1Q, PackedDepth2Q, PackedDepth12Q, PackedDepth2NQ, PackedDepth3NQ]

PackedDepth1Q[(_List ? PackedQ) ? VectorQ] = True;
PackedDepth2Q[(_List ? PackedQ) ? MatrixQ] = True;
PackedDepth12Q[(_List ? PackedQ) ? ArrayDepth12Q] = True;
PackedDepth2NQ[(_List ? PackedQ) ? ArrayDepth2NQ] = True;
PackedDepth3NQ[(_List ? PackedQ) ? ArrayDepth3NQ] = True;

ArrayDepth12Q[a_] := Array[a, 1|2];
ArrayDepth2NQ[a_] := ArrayDepth[a] >= 2;
ArrayDepth3NQ[a_] := ArrayDepth[a] >= 3;

(*************************************************************************************************)

SetPred1[RectListVecQ];

RectListVecQ[list_List]         := NoMessages @ IntQ @ Part[Dims[arr, 2], 2];

(*************************************************************************************************)

SetPred1[OptionRuleQ, OptionRuleVectorQ];

OptionRuleQ[_List]          = False;
OptionRuleQ[_ ? ORuleTreeQ] = True;

OptionRuleVectorQ[(_List ? ORuleTreeQ) ? VecQ] = True;

(*************************************************************************************************)

SetPred1[NullQ, NoneQ, ScaledQ, AutomaticQ, InheritedQ, AutoNoneQ, InfinityQ]
SetNPred1[NotNullQ, NotNoneQ, NotScaledQ, NotAutomaticQ, NotAutoNoneQ, NotInfinityQ]

ScaledQ[Scaled[NumP]]     = True;
NullQ[Null]               = True;
NoneQ[None]               = True;
AutomaticQ[Auto]          = True;
InheritedQ[Inherited]     = True;
AutoNoneQ[None | Auto]    = True;
InfinityQ[Infinity]       = True;

NotNullQ[Null]            = False;
NotScaledQ[Scaled[NumP]]  = False;
NotNoneQ[None]            = False;
NotAutomaticQ[Auto]       = False;
NotAutoNoneQ[None | Auto] = False;
NotInfinityQ[Infinity]    = False;

(*************************************************************************************************)

ZipAllTrueQ[fn_, dict1_Dict, dict2_Dict] := And[
  SameOrderedKeysQ[dict1, dict2],
  iZipAllTrueQ[fn, dict1, dict2]
];

ZipAllTrueQ[fn_, expr1_, expr2_] := And[
  Head[expr1] === Head[expr2],
  Len[expr1] === Len[expr2],
  iZipAllTrueQ[fn, expr1, expr2]
];

iZipAllTrueQ[fn_, expr1_, expr2_] := Catch[
  Do[
    If[fn[Part[expr1, i], Part[expr2, i]] =!= True, Throw @ False],
    {i, 1, Len @ expr1}
  ];
  True
];

(*************************************************************************************************)

AllSameQ[e_]         := SameQ @@ e;
NonEmptyAllSameQ[e_] := NonEmptyQ[e] && AllSameQ[e];

NotAllSameQ[e_] := Not[SameQ @@ e];

AllEqualQ[e_]    := Equal @@ e;
NotAllEqualQ[e_] := Not[Equal @@ e];

NotMatchQ[a_, b_] := !MatchQ[a, b];
NotMatchQ[b_][a_] := !MatchQ[a, b];

(*************************************************************************************************)

AnySameQ = CaseOf[
  EmptyP   := False;
  SingleP  := False;
  {a_, b_} := f[a] === f[b];
  list_    := NonEmptyQ[list] && !Apply[UnsameQ, list];
];

NoneSameQ[list_] := Apply[UnsameQ, list];

(*************************************************************************************************)

AllSameByQ = CaseOf[
  $[EmptyP, _]      := True;
  $[SingleP, _]     := True;
  $[{a_, b_}, f_]   := f[a] === f[b];
  $[list_List, f_]  := AllTrue[Rest @ list, f /* SameAs[f @ First @ list]];
  $[expr_, f_]      := NonEmptyQ[expr] && AllTrue[Rest @ Level[expr, 1], f /* SameAs[f @ First @ expr]];
];

NonEmptyAllSameByQ[e_ ? EmptyQ, _] := False;
NonEmptyAllSameByQ[e_, f_]         := AllSameByQ[e, f];

(* TODO: make these do the minimum of tests *)
AnySameByQ = CaseOf[
  $[EmptyP, _]    := False;
  $[SingleP, _]   := False;
  $[{a_, b_}, f_] := f[a] === f[b];
  $[expr_, f_]    := NonEmptyQ[expr] && !DuplicateFreeQ[f /@ expr];
];

NoneSameByQ = CaseOf[
  $[EmptyP, _]    := True;
  $[SingleP, _]   := True;
  $[{a_, b_}, f_] := f[a] =!= f[b];
  $[expr_, f_]    := DuplicateFreeQ[f /@ expr];
];

(*************************************************************************************************)

rectQ[expr_] := Length[Dimensions[expr, 2, AllowedHeads -> All]] == 2;

cheapAllSameQ[fn_, expr_] := Apply[SameQ, MapValues[fn, expr]];

(* does not allow them to all have length 0 *)
iAllSameLengthQ = CaseOf[
  list_List ? PackedQ := ArrayDepth[list] > 1;
  expr_ ? SingleQ     := NonAtomicQ @ First @ expr;
  expr_               := rectQ[expr] || (Depth[expr] > 2 && cheapAllSameQ[Length, expr]);
];

iAllSameHeadsQ = CaseOf[
  SingleP         := False;
  _List ? PackedQ := True;
  expr_           := cheapAllSameQ[Head, list];
];

iAllSamePartsQ = CaseOf[
  list_List ? PackedQ := ArrayDepth[list] > 1;
  expr_ ? SingleQ     := NonAtomicQ @ First @ expr;
  dicts_ ? DictVecQ   := Apply[SameQ, Len /@ dicts] && Apply[SameQ, Keys @ dicts];
  expr_               := rectQ @ expr;
];

iAllSameSetsQ = CaseOf[
  expr_ ? SingleQ := ListQ @ First @ expr;
  {a_, b_}        := SameSetQ[a, b];
  list_List       := sameSetVectorQ @ list;
  dict_Dict       := sameSetVectorQ @ Values @ dict;
  expr_           := sameSetVectorQ @ Level[expr, 1];
];

sameSetVectorQ[list_] := AnyMatrixQ[list] && Apply[SameQ, Sort /@ list];

iAllSameKeysQ = CaseOf[
  expr_ ? SingleQ    := DictQ @ First @ expr;
  {a_Dict, b_Dict}   := SameKeysQ[a, b];
  list_List          := sameKeysDictVecQ @ list;
  dict_Dict          := sameKeysDictVecQ @ Values @ dict;
  expr_              := sameKeysDictVecQ @ Level[expr, 1];
];

sameKeysDictVecQ[dicts_] := And[
  AssociationVectorQ @ dicts,
  Apply[SameQ, Len /@ dicts],
  Apply[SameQ, Sort /@ Keys[dicts]]
];

iAllSameOrderedKeysQ = CaseOf[
  expr_ ? SingleQ    := DictQ @ First @ expr;
  {a_Dict, b_Dict}   := SameOrderedKeysQ[a, b];
  list_List          := sameOrderedKeysAssocVectorQ @ list;
  dict_Dict          := sameOrderedKeysAssocVectorQ @ Values @ dict;
  expr_              := sameOrderedKeysAssocVectorQ @ Level[expr, 1];
];

sameOrderedKeysAssocVectorQ[dicts_] := And[
  AssociationVectorQ @ dicts,
  Apply[SameQ, Len /@ dicts],
  Apply[SameQ, Keys @ dicts]
];

(*************************************************************************************************)

AllSameLengthQ[_ ? EmptyQ]        := True;
AllSameHeadsQ[_ ? EmptyQ]         := True;
AllSamePartsQ[_ ? EmptyQ]         := True;
AllSameSetsQ[_ ? EmptyQ]          := True;
AllSameKeysQ[_ ? EmptyQ]          := True;
AllSameOrderedKeysQ[_ ? EmptyQ]   := True;

AllSameLengthQ[expr_]      := iAllSameLengthQ @ expr;
AllSameHeadsQ[expr_]       := iAllSameHeadsQ @ expr;
AllSamePartsQ[expr_]       := iAllSamePartsQ @ expr;
AllSameSetsQ[expr_]        := iAllSameSetsQ @ expr;
AllSameKeysQ[expr_]        := iAllSameKeysQ @ expr;
AllSameOrderedKeysQ[expr_] := iAllSameOrderedKeysQ @ expr;

(*************************************************************************************************)

NonEmptyAllSameLengthQ[_ ? EmptyQ]      := False;
NonEmptyAllSameHeadsQ[_ ? EmptyQ]       := False;
NonEmptyAllSamePartsQ[_ ? EmptyQ]       := False;
NonEmptyAllSameSetsQ[_ ? EmptyQ]        := False;
NonEmptyAllSameKeysQ[_ ? EmptyQ]        := False;
NonEmptyAllSameOrderedKeysQ[_ ? EmptyQ] := False;

NonEmptyAllSameLengthQ[expr_]      := iAllSameLengthQ @ expr;
NonEmptyAllSameHeadsQ[expr_]       := iAllSameHeadsQ @ expr;
NonEmptyAllSamePartsQ[expr_]       := iAllSamePartsQ @ expr;
NonEmptyAllSameSetsQ[expr_]        := iAllSameSetsQ @ expr;
NonEmptyAllSameKeysQ[expr_]        := iAllSameKeysQ @ expr;
NonEmptyAllSameOrderedKeysQ[expr_] := iAllSameOrderedKeysQ @ expr;

(*************************************************************************************************)

SetPred1[FalseQ, AllAreTrueQ, AnyAreTrueQ, NoneAreTrueQ, AllAreFalseQ, AnyAreFalseQ, NoneAreFalseQ]

FalseQ[False] := True;
AllAreTrueQ[e:ListDictP]   := AllTrue[e, TrueQ];
AnyAreTrueQ[e:ListDictP]   := AnyTrue[e, TrueQ];
NoneAreTrueQ[e:ListDictP]  := NoneTrue[e, TrueQ];
AllAreFalseQ[e:ListDictP]  := AllTrue[e, EqualTo[False]];
AnyAreFalseQ[e:ListDictP]  := AnyTrue[e, EqualTo[False]];
NoneAreFalseQ[e:ListDictP] := NoneTrue[e, EqualTo[False]];

(**************************************************************************************************)

SetPred1[ListableFunctionQ, AssociationFunctionQ, PureFunctionQ]

ListableFunctionQ[sym_Symbol] := ListableFunctionQ[sym] = MemberQ[Attributes @ sym, Listable];
ListableFunctionQ[HoldP[Fn[___, Listable | {___, Listable, ___}]]] := True;
ListableFunctionQ[c_RightComposition | c_Composition] := AllTrue[c, ListableFunctionQ];

AssociationFunctionQ[HoldP[Fn[_Sym | _List, ___]]] := False;
AssociationFunctionQ[fn_Fn] := ContainsQ[fn, Slot[_Str]];

PureFunctionQ[_Fn] := True;

(**************************************************************************************************)

DeclareDeclare @ SetVectorListableOp;

SetVectorListableOp[sym_Symbol] := ListableFunctionQ[_sym] = True;

(**************************************************************************************************)

SetCurry2[ContainsQ]

ContainsQ[e_, p_] := !FreeQ[e, p];

ContainsAssociationQ[e_] := And[VContainsQ[e, Association], !FreeQ[e, DictP]];

SetCurry2[FreeWithinQ, ContainsWithinQ]

FreeWithinQ[e_, p_, n_:2]     :=  FreeQ[e, p, {n, Inf}];
ContainsWithinQ[e_, p_, n_:2] := !FreeQ[e, p, {n, Inf}];

SetHoldF[HoldFreeQ, HoldContainsQ, HoldArgsFreeQ, HoldArgsContainQ]

HoldFreeQ[e_, p_]     :=  FreeQ[NoEval @ e, p];
HoldContainsQ[e_, p_] := !FreeQ[NoEval @ e, p]

HoldArgsFreeQ[_[a___], p_]    :=  FreeQ[NoEval @ a, p];
HoldArgsContainQ[_[a___], p_] := !FreeQ[NoEval @ a, p];
HoldArgsFreeQ[_, _] := True;
HoldArgsContainQ[_, _] := True;

(**************************************************************************************************)

SetPred1[AnyMissingQ, NoneMissingQ, AnyFailedQ, NoneFailedQ]

AnyMissingQ[e:ListDictP] := MemberQ[e, _Missing];
AnyFailedQ[e:ListDictP]  := AnyTrue[e, FailureQ];

NoneMissingQ[e:ListDictP] := !MemberQ[e, _Missing];
NoneFailedQ[e:ListDictP]  := NoneTrue[e, FailureQ];

(**************************************************************************************************)

SetPred2 @ SetHoldC[HoldSameQ, HoldHasHeadQ]

HoldSameQ[a_, a_] := True;
HoldHasHeadQ[h_[___], h_] := True;
HoldHasHeadQ[h_] := HoldCompFn[FmE, HoldHasHeadQ[FmE, h]];

(**************************************************************************************************)

(* StringOrVectorQ is an alias to Developer`StringOrStringVectorQ *)
SetPred1[BooleanOrVectorQ, SymbolOrVectorQ, IntegerOrVectorQ, NaturalOrVectorQ, RealOrVectorQ]

BooleanOrVectorQ[BoolOrVecP] = True;
SymbolOrVectorQ[SymOrVecP]   = True;
IntegerOrVectorQ[IntOrVecP]  = True;
NaturalOrVectorQ[NatOrVecP]  = True;
RealOrVectorQ[RealOrVecP]    = True;

(**************************************************************************************************)

SetPred1[RuleOrVectorQ, RuleLikeOrVectorQ, OptionRuleOrVectorQ]

RuleOrVectorQ[_Rule]                   = True;
RuleOrVectorQ[_List ? RuleVectorQ]     = True;

RuleLikeOrVectorQ[_Rule | RuleD]       = True;
RuleLikeOrVectorQ[_ ? RuleLikeVectorQ] = True

OptionRuleOrVectorQ[e_List]      := OptionQ[e] && VectorQ[e];
OptionRuleOrVectorQ[_ ? OptionQ] := True;

