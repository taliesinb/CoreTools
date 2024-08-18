SystemExports[
  "Predicate",

    ZeroQ, ZeroIntegerQ, NonZeroQ, NonZeroIntegerQ,
    OnePartSpecQ, MultiPartSpecQ, ExtPartSpecQ,

    UnorderedAssociationQ, OrderedAssociationQ,

    AtomicQ, NonAtomicQ,

    SingleQ, DatumQ,
    HoldDatumQ, HoldSingleQ, HoldEmptyQ, HoldNotEmptyQ,
    HasHeadQ, VectorHasHeadsQ, AssociationHasHeadsQ,
    HasKeysQ, HasLengthQ, HasDimensionsQ, HasArrayDepthQ,
    HoldHasLengthQ,

    ElementQ, SameSetQ, SubsetOfQ, SupersetOfQ, IntersectsQ, NotIntersectsQ, PrefixListQ,
    SameKeysQ, SameOrderedKeysQ, SameHeadQ, SameLengthQ, SameShapeQ, SamePartsQ,
    HoldSameQ, HoldHasHeadQ, HoldSameLengthQ,

    ExtendedNumberQ, UnitNumberQ, PositiveRealQ,
    PairQ, PairVectorQ, PairMatrixQ, PairArrayQ,

    OptionRuleQ,
    RuleVectorQ, RuleLikeVectorQ, RuleDelayedVectorQ, AssociationLikeQ, OptionRuleVectorQ,
    RangeQ, PermutedRangeQ,
    ArrayDepthAtLeastQ, PackedArrayDepthAtLeastQ,

    BooleanVectorQ, IntegerVectorQ, NaturalVectorQ, PositiveIntegerVectorQ, SymbolVectorQ, RealVectorQ, NumberVectorQ, ListVectorQ, ColorVectorQ, ExtendedNumberVectorQ,
    BooleanMatrixQ, IntegerMatrixQ, NaturalMatrixQ, PositiveIntegerMatrixQ, SymbolMatrixQ, RealMatrixQ, NumberMatrixQ, ListMatrixQ, ColorMatrixQ, ExtendedNumberMatrixQ, AnyMatrixQ,
    BooleanArrayQ,  IntegerArrayQ,  NaturalArrayQ,  PositiveIntegerArrayQ,  SymbolArrayQ,  RealArrayQ,  NumberArrayQ,  ListArrayQ,  ColorArrayQ,  ExtendedNumberArrayQ,
    StringMatrixQ,  AssociationMatrixQ
    StringArrayQ,   AssociationArrayQ,

    NullQ, NoneQ, AutomaticQ, InheritedQ, InfinityQ,
    NotNullQ, NotNoneQ, NotAutomaticQ, NotInfinityQ,
    NotAllSameQ, AllEqualQ, NotAllEqualQ, NotMatchQ,
    AnySameQ, NoneSameQ, AnySameByQ, NoneSameByQ,

    AllSameQ,            NonEmptyAllSameQ,
    AllSameByQ,          NonEmptyAllSameByQ,
    AllSameLengthQ,      NonEmptyAllSameLengthQ,
    AllSameHeadsQ,       NonEmptyAllSameHeadsQ,
    AllSamePartsQ,       NonEmptyAllSamePartsQ,
    AllSameSetsQ,        NonEmptyAllSameSetsQ,
    AllSameKeysQ,        NonEmptyAllSameKeysQ,
    AllSameOrderedKeysQ, NonEmptyAllSameOrderedKeysQ,

    FalseQ, AllAreTrueQ, AnyAreTrueQ, NoneAreTrueQ, AllAreFalseQ, AnyAreFalseQ, NoneAreFalseQ,

    ListableFunctionQ,
    ContainsQ, ContainsWithinQ, FreeWithinQ, ContainsAssociationQ,
    HoldFreeQ, HoldContainsQ, HoldArgsFreeQ, HoldArgsContainQ,
    KeysTrue, ValuesTrue, KeysValuesTrue, RuleKeysTrue, RuleValuesTrue, RulesTrue,
    IntegerKeysQ, StringKeysQ, ListKeysQ, AssociationKeysQ, SymbolKeysQ,
    IntegerValuesQ, StringValuesQ, ListValuesQ, AssociationValuesQ, BooleanValuesQ, SymbolValuesQ,
    HasDuplicatesQ,
    AnyMissingQ, NoneMissingQ, AnyFailedQ, NoneFailedQ, NotFailureQ,
    HoldAssociationQ, HoldPackedArrayQ,

    ListOfQ,                 AssociationOfQ,         ListAssociationOfQ,
    NonEmptyListQ,     NonEmptyAssociationQ,   NonEmptyListAssociationQ,
    NonEmptyListOfQ, NonEmptyAssociationOfQ, NonEmptyListAssociationOfQ,
    PairOfQ, TupleOfQ, RecordOfQ, StructureOfQ, ArrayOfQ,

    BooleanOrVectorQ, SymbolOrVectorQ, IntegerOrVectorQ, RuleOrVectorQ, RuleLikeOrVectorQ, OptionRuleOrVectorQ
];

PackageExports[
  "Predicate",
    EmptyQ,
    Nat2Q, PosInt2Q, ExtNatQ, ExtIntQ, ExtPosIntQ,
    Num2Q, Num23Q, Num3Q,
    Pos2Q, Pos2ListQ, Pos2ListsQ, Pos2PairQ,
    Pos3Q, Pos3ListQ, Pos3ListsQ, Pos3PairQ,
    PosAQ, PosAListQ, PosAListsQ, PosAPairQ,
    Pos2ListOrListsQ,
    AutoQ,
    RuleLikeQ, RuleQ, RuleDelayedQ, PackedRealsQ, PackedIntsQ,
    AutoNoneQ, NotAutoNoneQ,
    HoldVFreeQ, HoldVContainsQ,

  "MetaFunction",

    DeclarePatternPredicates,
    DefinePatternPredicateRules,
    DeclareVectorListableOp
];

(*************************************************************************************************)

SetPred1[ZeroQ, ZeroIntegerQ, NonZeroQ, NonZeroIntegerQ]

ZeroQ[ZeroP]          = True;
ZeroIntegerQ[0]       = True;

NonZeroQ[ZeroP]       = False;
NonZeroQ[NumP]        = True;

NonZeroIntegerQ[0]    = False;
NonZeroIntegerQ[_Int] = True;

(*************************************************************************************************)

SetPred1[UnorderedAssociationQ, OrderedAssociationQ]

UnorderedAssociationQ[EmptyUDict] := True;
UnorderedAssociationQ[dict:DictP] := NotEmptyQ @ Take[dict, 0];

OrderedAssociationQ[EmptyDict] := True;
OrderedAssociationQ[dict:DictP] := EmptyQ @ Take[dict, 0];

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

AtomicQ[_Dict]           = False;
AtomicQ[_ ? HoldAtomQ]    = True;
AtomicQ[_]                = False;

NonAtomicQ[_Dict]        = True;
NonAtomicQ[_ ? HoldAtomQ] = False;
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
  ZipScan[setPattPred, predicateSyms, patternNames];
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

HoldSingleQ[_[_]]        = True;
HoldDatumQ[DatumP]       = True;
HoldEmptyQ[EmptyP]       = True;
HoldNotEmptyQ[NonEmptyP] = True;

(*************************************************************************************************)

DeclarePatternPredicates[Nat2Q, PosInt2Q, ExtNatQ, ExtIntQ, ExtPosIntQ];
DeclarePatternPredicates[Num2Q, Num3Q, Num23Q];

(*************************************************************************************************)

SetPred1[Pos2Q, Pos2ListQ, Pos2ListsQ, Pos2PairQ];
SetPred1[Pos3Q, Pos3ListQ, Pos3ListsQ, Pos3PairQ];
SetPred1[PosAQ, PosAListQ, PosAListsQ, PosAPairQ];

Pos2Q[{NumP, NumP}] = True;
Pos3Q[{NumP, NumP, NumP}] = True;
PosAQ[{NumP, NumP}] = True;
PosAQ[{NumP, NumP, NumP}] = True;
PosAQ[{Repeated[_ ? RealValuedNumericQ, {2, 3}]}] := True;

Pos2ListQ[a_List] := MatrixQ[a, RealValuedNumberQ] && Last[Dims @ a] == 2;
Pos3ListQ[a_List] := MatrixQ[a, RealValuedNumberQ] && Last[Dims @ a] == 3;
PosAListQ[a_List] := MatrixQ[a, RealValuedNumberQ] && 2 <= Last[Dims @ a] <= 3;

Pos2ListsQ[a_List] := (ArrayQ[a, 3, RealValuedNumberQ] && Last[Dims @ a] == 2) || VectorQ[a, Pos2ListQ];
Pos3ListsQ[a_List] := (ArrayQ[a, 3, RealValuedNumberQ] && Last[Dims @ a] == 3) || VectorQ[a, Pos3ListQ];
PosAListsQ[a_List] := (ArrayQ[a, 3, RealValuedNumberQ] && 2 <= Last[Dims @ a] <= 3) || VectorQ[a, Pos2ListQ] || VectorQ[a, Pos3ListQ];

Pos2PairQ[a_List] := VectorQ[a, RealValuedNumberQ] && SameQ[dims @ a, {2, 2}];
Pos3PairQ[a_List] := VectorQ[a, RealValuedNumberQ] && SameQ[dims @ a, {2, 3}];
PosAPairQ[a_List] := VectorQ[a, RealValuedNumberQ] && MatchQ[dims @ a, {2, 2|3}];

(*************************************************************************************************)

SetPred1 @ Pos2ListOrListsQ;

Pos2ListOrListsQ[a_List] := Pos2ListsQ[a] || Pos2ListQ[a];

(*************************************************************************************************)

HasHeadQ[e_, h_]       := MatchQ[e, ToBlank @ h];
HasHeadQ[h_]           := MatchQ[ToBlank @ h];

VectorHasHeadsQ[e_, h_] := MatchQ[e, List @ ToBlankNullSequence @ h];
VectorHasHeadsQ[h_]     := MatchQ[List @ ToBlankNullSequence @ h];

SetCurry2 @ SetPred2 @ AssociationHasHeadsQ

AssociationHasHeadsQ[a_Dict, h_] := MatchQ[Values @ a, List @ ToBlankNullSequence @ h];

(*************************************************************************************************)

SetCurry2 @ SetPred2[HasKeysQ, HasLengthQ, HasDimensionsQ, HasArrayDepthQ]

HasKeysQ[dict_Dict, keys_List] := Len[dict] === Len[key] && SameQ[Keys @ dict, keys];

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

RuleQ[_Rule]                    = True;
RuleLikeQ[_Rule | _RuleDelayed] = True;
RuleDelayedQ[_RuleDelayed]      = True;

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

SetCurry2 @ SetPred2[ElementQ]

ElementQ[elem_, set_] := MemberQ[set, Verbatim @ elem];

(*************************************************************************************************)

SetPred2[SameSetQ, SubsetOfQ, SupersetOfQ, IntersectsQ, NotIntersectsQ, PrefixListQ, SameKeysQ, SameOrderedKeysQ, SamePartsQ]

(* Note:
SubsetQ is super slow! it does a lot of heads checking and doesn't special case SameTest etc.
DisjointQ is the same.
*)

SameSetQ[a_List, b_List] := Language`EmptyComplementQ[a, b] && Language`EmptyComplementQ[b, a];
SubsetOfQ[a_List, b_List] := Language`EmptyComplementQ[a, b];
SupersetOfQ[a_List, b_List] := Language`EmptyComplementQ[b, a];
IntersectsQ[a_List, b_List] := !Language`EmptyIntersectionQ[a, b];
NotIntersectsQ[a_List, b_List] := Language`EmptyIntersectionQ[a, b];

PrefixListQ[a_List, b_List] := Length[a] <= Length[b] && a === Take[b, Length @ a];

SameKeysQ[a_Dict, b_Dict]        := Len[a] === Len[b] && SameSetQ[Keys @ a, Keys @ b];
SameOrderedKeysQ[a_Dict, b_Dict] := Len[a] === Len[b] && SameQ[Keys @ a, Keys @ b];

SamePartsQ[a_List, b_List] := Len[a] === Len[b];
SamePartsQ[a_Dict, b_Dict] := Len[a] === Len[b] && Keys[a] === Keys[b];
SamePartsQ[a_, b_] := Head[a] === Head[b] && Len[a] === Len[b];

(*************************************************************************************************)

DeclareCurry1[SameHeadQ, SameLengthQ, SameShapeQ]

SameHeadQ[a_, b_] := Head[a] === Head[b];
SameLengthQ[a_, b_] := Length[a] === Length[b];
SameShapeQ[a_, b_] := SamePartsQ[a, b];

(*************************************************************************************************)

SetPred1[ExtendedNumberQ];

ExtendedNumberQ[ExtNumP] := True;
UnitNumberQ[a_]          := RealValuedNumberQ[a] && 0 <= a <= 1;
PositiveRealQ[a_]        := RealQ[a] && Positive[a];

(*************************************************************************************************)

SetPred1[PairQ, PairVectorQ, PairMatrixQ, PairArrayQ]

PairQ[{_, _}]         := True;
PairVectorQ[arr_List] := Length2[arr] === 2;
PairMatrixQ[arr_List] := MatrixQ[arr, PairQ];
PairArrayQ[arr_List]  := LengthN[arr] === 2;

(*************************************************************************************************)

RangeQ[list_]                 := PermutedRangeQ[list] && OrderedQ[list];
PermutedRangeQ[list_]         := VectorQ[list, IntegerQ] && MinMax[list] == {1, Length @ list};

(*************************************************************************************************)

SetPred1[RuleVectorQ, RuleDelayedVectorQ]

RuleVectorQ[{___Rule}]               := True;
RuleDelayedVectorQ[{___RuleDelayed}] := True;
RuleLikeVectorQ[e_]                  := VectorQ[e, RuleLikeQ];

(*************************************************************************************************)

BooleanVectorQ[list_]         := VectorQ[list, BooleanQ];
IntegerVectorQ[list_]         := VectorQ[list, IntegerQ];
NaturalVectorQ[list_]         := VectorQ[list, NaturalQ];
PositiveIntegerVectorQ[list_] := VectorQ[list, PositiveIntegerQ];
SymbolVectorQ[list_]          := VectorQ[list, SymbolQ];
RealVectorQ[list_]            := VectorQ[list, RealQ];
NumberVectorQ[list_]          := VectorQ[list, RealValuedNumberQ];
ListVectorQ[list_]            := PackedArrayQ[list] || VectorQ[list, ListQ]; (* firts check avoids unpacking *)
ColorVectorQ[list_]           := VectorQ[list, ColorQ];
ExtendedNumberVectorQ[list_]  := VectorQ[list, RealValuedNumberQ] || VectorQ[list, ExtendedNumberQ];

(*************************************************************************************************)

BooleanMatrixQ[list_]         := MatrixQ[list, BooleanQ];
IntegerMatrixQ[list_]         := MatrixQ[list, IntegerQ];
NaturalMatrixQ[list_]         := MatrixQ[list, NaturalQ];
PositiveIntegerMatrixQ[list_] := MatrixQ[list, PositiveIntegerQ];
StringMatrixQ[list_]          := MatrixQ[list, StringQ];
SymbolMatrixQ[list_]          := MatrixQ[list, SymbolQ];
RealMatrixQ[list_]            := MatrixQ[list, RealQ];
NumberMatrixQ[list_]          := MatrixQ[list, RealValuedNumberQ];
ListMatrixQ[list_]            := PackedArrayDepthAtLeastQ[list, 2] || MatrixQ[list, ListQ];
ColorMatrixQ[list_]           := MatrixQ[list, ColorQ];
ExtendedNumberMatrixQ[list_]  := MatrixQ[list, RealValuedNumberQ] || MatrixQ[list, ExtendedNumberQ];

AnyMatrixQ[{} | {{}}]         := True;
AnyMatrixQ[list_List]         := Length[Dimensions[list, 2]] == 2;
AnyMatrixQ[_]                 := False;

AssociationMatrixQ[list_]     := MatrixQ[list, AssociationQ];

(*************************************************************************************************)

BooleanArrayQ[list_]          := TensorQ[list, BooleanQ];
IntegerArrayQ[list_]          := TensorQ[list, IntegerQ];
NaturalArrayQ[list_]          := TensorQ[list, NaturalQ];
PositiveIntegerArrayQ[list]   := TensorQ[list, PositiveIntegerQ];
StringArrayQ[list_]           := TensorQ[list, StringQ];
SymbolArrayQ[list_]           := TensorQ[list, SymbolQ];
RealArrayQ[list_]             := TensorQ[list, RealQ];
NumberArrayQ[list_]           := TensorQ[list, RealValuedNumberQ];
ListArrayQ[list_]             := PackedArrayDepthAtLeastQ[list, 2] || TensorQ[list, ListQ];
ColorArrayQ[list_]            := TensorQ[list, ColorQ];
ExtendedNumberArrayQ[list_]   := TensorQ[list, RealValuedNumberQ] || TensorQ[list, ExtendedNumberQ];

AssociationArrayQ[list_]      := TensorQ[list, AssociationQ];

PackedIntsQ[list_]  := PackedQ[list] && IntegerArrayQ[list];
PackedRealsQ[list_] := PackedQ[list] && RealArrayQ[list];

(*************************************************************************************************)

ArrayDepthAtLeastQ[arr_, n_]         := ArrayQ[arr, _ ? (GreaterEqualThan[n])];
PackedArrayDepthAtLeastQ[arr_, n_]   := PackedArrayQ[arr] && Depth[arr] > n;

(*************************************************************************************************)

SetPred1[OptionRuleQ, OptionRuleVectorQ];

OptionRuleQ[ORuleP]             = True;
OptionRuleVectorQ[ORuleVecP]    = True;

(*************************************************************************************************)

SetPred1[NullQ, NoneQ, AutomaticQ, InheritedQ, AutoNoneQ, InfinityQ]
SetNPred1[NotNullQ, NotNoneQ, NotAutomaticQ, NotAutoNoneQ, NotInfinityQ]

NullQ[Null]               = True;
NoneQ[None]               = True;
AutomaticQ[Auto]          = True;
InheritedQ[Inherited]     = True;
AutoNoneQ[None | Auto]    = True;
InfinityQ[Infinity]       = True;

NotNullQ[Null]            = False;
NotNoneQ[None]            = False;
NotAutomaticQ[Auto]       = False;
NotAutoNoneQ[None | Auto] = False;
NotInfinityQ[Infinity]    = False;

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
  expr_ ? SingleQ    := AssociationQ @ First @ expr;
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
  expr_ ? SingleQ    := AssociationQ @ First @ expr;
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

SetPred1[ListableFunctionQ]

ListableFunctionQ[sym_Symbol] := ListableFunctionQ[sym] = MemberQ[Attributes @ sym, Listable];
ListableFunctionQ[HoldPattern[Function[___, Listable | {___, Listable, ___}]]] := True;
ListableFunctionQ[c_RightComposition | c_Composition] := AllTrue[c, ListableFunctionQ];

(**************************************************************************************************)

DeclareDeclare @ DeclareVectorListableOp;

DeclareVectorListableOp[sym_Symbol] := ListableFunctionQ[_sym] = True;

(**************************************************************************************************)

SetCurry2[ContainsQ]

ContainsQ[e_, p_] := !FreeQ[e, p];

ContainsAssociationQ[e_] := And[VContainsQ[e, Association], !FreeQ[e, _Dict ? Developer`HoldAtomQ]];

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

NotFailureQ[_ ? FailureQ] := False;
NotFailureQ[_] := True;

(**************************************************************************************************)

SetHoldC[HoldSameQ, HoldAssociationQ, HoldPackedArrayQ, HoldHasHeadQ]
SetPred1[HoldAssociationQ, HoldPackedArrayQ]
SetPred2[HoldSameQ, HoldHasHeadQ]

HoldSameQ[a_, a_] := True;
HoldAssociationQ[_Dict ? HoldAtomQ] := True;
HoldPackedArrayQ[arr_List] := PackedArrayQ @ NoEval @ arr;

HoldHasHeadQ[h_[___], h_] := True;
HoldHasHeadQ[h_] := HoldCompFn[FmE, HoldHasHeadQ[FmE, h]];

(**************************************************************************************************)

(* StringOrVectorQ is an alias to Developer`StringOrStringVectorQ *)
SetPred1[BooleanOrVectorQ, SymbolOrVectorQ, IntegerOrVectorQ, RuleOrVectorQ, RuleLikeOrVectorQ, OptionRuleOrVectorQ]

BooleanOrVectorQ[True|False]             = True;
BooleanOrVectorQ[_List ? BooleanVectorQ] = True;

SymbolOrVectorQ[_Sym ? HoldSymbolQ]    = True;
SymbolOrVectorQ[_List ? SymbolVectorQ] = True;

IntegerOrVectorQ[_Int]                   = True;
IntegerOrVectorQ[_List ? IntegerVectorQ] = True;

RuleOrVectorQ[_Rule]               = True;
RuleOrVectorQ[_List ? RuleVectorQ] = True;

RuleLikeOrVectorQ[_Rule | RuleD]       = True;
RuleLikeOrVectorQ[_ ? RuleLikeVectorQ] = True

OptionRuleOrVectorQ[e_List]      := OptionQ[e] && VectorQ[e];
OptionRuleOrVectorQ[_ ? OptionQ] := True;


