SystemExports[
  "Operator",
    TakeOp, DropOp, ClipOp, DistanceOp, PartOp, ReplaceAllOp, ReplaceRepeatedOp,
    TimesOp, DivideOp, PlusOp, SubtractOp,
    ThreadTimesOp, ThreadDivideOp, ThreadPlusOp, ThreadSubtractOp,
    DotOp, DotRightOp, AffineOp,
    ModOp, MinOp, MaxOp,
    JoinOp,
    UnionOp, ComplementOp, IntersectionOp,
    SubscriptOp,
    LookupOp,
    IfOp, ConstructOp,
    ConstOp,
  "Variable",
    $Operators,
  "PredicateOperator",
    AndOp, OrOp, NotOp,
    AllSameBy, AnySameBy, NoneSameBy,
    UnsameAs,
    LengthOf, DimensionsOf, KeysOf,
    VectorOf, MatrixOf, ArrayOf,
    ListOf, AssociationOf, ListAssociationOf,
    NonEmptyListOf, NonEmptyAssociationOf, NonEmptyListAssociationOf,
    RuleVectorOf, HeadOf, VectorHeadOf,
    PairOf, RecordOf, TupleOf, StructureOf,
    IntBetween, RealBetween, NumBetween,
  "OptionSymbol",
    DefaultFunction
];

(**************************************************************************************************)

DefineOperator2Rules[
  TakeOp            -> Take,
  DropOp            -> Drop,
  ClipOp            -> Clip,
  DistanceOp        -> EuclideanDistance,
  PartOp            -> Part,
  ReplaceAllOp      -> ReplaceAll,
  ReplaceRepeatedOp -> ReplaceRepeated,
  TimesOp           -> Times,
  DivideOp          -> Divide,
  PlusOp            -> Plus,
  SubtractOp        -> Subtract,
  ThreadTimesOp     -> ThreadTimes,
  ThreadDivideOp    -> ThreadDivide,
  ThreadPlusOp      -> ThreadPlus,
  ThreadSubtractOp  -> ThreadSubtract,
  DotOp             -> Dot,
  MinOp             -> Min,
  MaxOp             -> Max,
  JoinOp            -> Join,
  UnionOp           -> Union,
  ComplementOp      -> Complement,
  IntersectionOp    -> Intersection
]

DeclareListableOperator[
  DotOp, DotRightOp,
  TimesOp, DivideOp, PlusOp, SubtractOp,
  ThreadTimesOp, ThreadDivideOp, ThreadPlusOp, ThreadSubtractOp
]

DefineOperator1Rules[
  DotRightOp -> Dot
]

(**************************************************************************************************)

DefineOperator1Rules[UnsameAs -> UnsameQ]

DefineOperator2Rules[LengthOf -> HasLengthQ, DimensionsOf -> HasDimensionsQ, KeysOf -> HasKeysQ]
DefineOperator2Rules[AllSameBy -> AllSameByQ, AnySameBy -> AnySameByQ, NoneSameBy -> NoneSameByQ]

(**************************************************************************************************)

DefineOperator2Rules[
  ListOf                    -> VectorQ,
  AssociationOf             -> AssociationOfQ,
  ListAssociationOf         -> ListAssociationOfQ,
  NonEmptyListOf            -> NonEmptyListOfQ,
  NonEmptyAssociationOf     -> NonEmptyAssociationOfQ,
  NonEmptyListAssociationOf -> NonEmptyListAssociationOfQ
];

(**************************************************************************************************)

AssociationOf[ktest_, vtest_][assoc_]         := KeysValuesTrue[assoc, ktest, vtest];
NonEmptyAssociationOf[ktest_, vtest_][assoc_] := NotZeroLenQ[assoc] && KeysValuesTrue[assoc, ktest, vtest];

RuleVectorOf[vtest_][rules_]         := RuleValuesTrue[rules, vtest];
RuleVectorOf[ktest_, vtest_][rules_] := RulesTrue[rules, ktest, vtest];

(**************************************************************************************************)

DefineOperator2Rules[VectorOf -> VectorQ, MatrixOf -> MatrixQ]

ArrayOf[test_][data_]         := ArrayOfQ[data, test];
ArrayOf[test_, shape_][data_] := ArrayOfQ[data, test, shape];

(**************************************************************************************************)

PairOf[a_, b_]     := PairOf[{a, b}];
PairOf[preds_][e_] := PairOfQ[e, preds];

TupleOf[preds:Repeated[_, {2, Inf}]] := TupleOf[List[preds]];
TupleOf[preds_List][e_]              := TupleOfQ[e, preds];

RecordOf[preds__Rule]      := RecordOf @ UAssoc @ preds;
RecordOf[preds_Assoc][e_]  := RecordOfQ[e, preds];

StructureOf[preds_][e_]    := StructureOfQ[e, preds];

(**************************************************************************************************)

IntBetween[a_, b_][i_]  := IntegerQ[i] && a <= i <= b;
RealBetween[a_, b_][i_] := RealQ[i]    && a <= i <= b;
NumBetween[a_, b_][i_]  := NumberQ[i]  && a <= i <= b;

(**************************************************************************************************)

VectorHeadOf[h_][e_] := MatchQ[e, {___h}];
HeadOf[h_][e_]       := MatchQ[e, _h];

(**************************************************************************************************)

SubscriptOp[s_][e__] := Subscript[s, e];

(**************************************************************************************************)

AffineOp[matrix_] := DotRightOp[Transpose @ ToPackedArray @ matrix];
AffineOp[matrix_, {(0|0.)..}] := DotRightOp @ Transpose @ ToPackedArray @ matrix;
AffineOp[matrix_, vector_] := DotRightOp[Transpose @ ToPackedArray @ matrix, vector];

(**************************************************************************************************)

ModOp[n_][e_]      := If[NumericQ[e], Mod[e, n, 0], e];
ModOp[n_, m_][e_]  := If[NumericQ[e], Mod[e, n, m], e];
ModOp[Infinity]    = Identity;
ModOp[Infinity, _] = Identity;

(**************************************************************************************************)

e_AndOp[arg_] := AllTrue[e, #[arg]&];
e_OrOp[arg_] := AnyTrue[e, #[arg]&];
NotOp[f_][expr_] := Not @ f @ expr;

(**************************************************************************************************)

LookupOp[a_][key_] := Lookup[a1, key];
LookupOp[a__][key_] := ChainedLookup[{a}, key];

(**************************************************************************************************)

IfOp[test_, trueFn_][input_] := If[test[input], trueFn[input], Null, Null];
IfOp[test_, trueFn_, falseFn_][input_] := If[test[input], trueFn[input], falseFn[input], Null];
IfOp[test_, trueFn_, falseFn_, otherFn_][input_] := If[test[input], trueFn[input], falseFn[input], otherFn[input]];

(**************************************************************************************************)

ConstOp[c_][___] := c;

(**************************************************************************************************)

ConstructOp[head_, f_][x_] := head[f[x]];
ConstructOp[head_, f_, g_][x_] := head[f[x], g[x]];
ConstructOp[head_, f_, g_, h_][x_] := head[f[x], g[x], h[x]];
ConstructOp[head_, f_, g_, h_, j_][x_] := head[f[x], g[x], h[x], j[x]];

