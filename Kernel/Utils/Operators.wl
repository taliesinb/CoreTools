SystemExports[
  "Operator",
    PartOp, PartOfOp,
    TakeOp, DropOp, ClipOp, DistanceOp, ReplaceAllOp, ReplaceRepeatedOp,
    TimesOp, DivideOp, PlusOp, SubtractOp,
    ThreadTimesOp, ThreadDivideOp, ThreadPlusOp, ThreadSubtractOp,
    DotOp, DotRightOp, AffineOp,
    ModOp, MinOp, MaxOp,
    JoinOp,
    UnionOp, ComplementOp, IntersectionOp,
    SubscriptOp,
    LookupOp,
    IfOp, ApplyIfOp, WhichOp,
    ConstructOp,
    ConstOp,
    RiffleOp,
    ArrayTableOp,
    ThenOp,
    OnArrayOp, FlipXOp, FlipYOp, Rot90LOp, Rot90ROp,

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
  "Option",
    DefaultFunction
];

(**************************************************************************************************)

{$flipX, $flipY} = OnArrayOp /@ ThreadTimesOp /@ ToPacked /@ {{-1, 1}, {1, -1}};
{$rot90R, $rot90L} = OnArrayOp /@ AffineOp /@ ToPacked /@ {{{0,1},{1,0}}, {{0,-1},{-1,0}}};

OnArrayOp[f_][{}] := {};
OnArrayOp[f_][arr_List ? ArrayQ] := f @ arr;
f_OnArrayOp[arr_List] := Map[f, arr];

FlipXOp[e_List] := $flipX @ e;
FlipYOp[e_List] := $flipY @ e;
Rot90ROp[e_List] := $rot90L @ e;
Rot90LOp[e_List] := $rot90R @ e;

(**************************************************************************************************)

(* operator form of MaybePart / SafePart *)
PartOp::usage = "PartOp[p$$][e$] gives Part[e$, p$] or $Failed. It curries the part."
PartOp[p___][e_] := FastQuietCheck @ Part[e, p];

PartOfOp::usage =
"PartOfOp[e$][p$] gives Part[e$, p$] or $Failed. It curries the expression.
PartOfOp[e$, p1$$][p2$$] gives Part[e$, p1$$, p2$$]."
PartOfOp[e_][p___]        := FastQuietCheck @ Part[e, p];
PartOfOp[e_, p1__][p2___] := FastQuietCheck @ Part[p, p1, p2];

(**************************************************************************************************)

DeclaredHere[TakeOp, DropOp, ClipOp, ReplaceAllOp, ReplaceRepeatedOp]

DefineOperator2Rules[
  TakeOp            -> Take,
  DropOp            -> Drop,
  ClipOp            -> Clip,
  ReplaceAllOp      -> ReplaceAll,
  ReplaceRepeatedOp -> ReplaceRepeated
];

DeclaredHere[DistanceOp, TimesOp, DivideOp, PlusOp, SubtractOp]
DeclaredHere[ThreadTimesOp, ThreadDivideOp, ThreadPlusOp, ThreadSubtractOp]
DeclaredHere[DotOp, MinOp, MaxOp, JoinOp, UnionOp, ComplementOp, IntersectionOp]

DefineOperator1Rules[
  DistanceOp        -> EuclideanDistance,
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

(**************************************************************************************************)

SetListableOp[
  DotOp, DotRightOp,
  TimesOp, DivideOp, PlusOp, SubtractOp,
  ThreadTimesOp, ThreadDivideOp, ThreadPlusOp, ThreadSubtractOp
]

DefineOperator2Rules[
  DotRightOp -> Dot
]

AffineOp[matrix_] := DotRightOp[Transpose @ ToPacked @ matrix];
AffineOp[matrix_, {(0|0.)..}] := DotRightOp @ Transpose @ ToPacked @ matrix;
AffineOp[matrix_, vector_] := DotRightOp[Transpose @ ToPacked @ matrix, vector];

(**************************************************************************************************)

DeclaredHere[UnsameAs, LengthOf, DimensionsOf, KeysOf, AllSameBy, AnySameBy, NoneSameBy]

DefineOperator1Rules[UnsameAs -> UnsameQ]
DefineOperator2Rules[LengthOf -> HasLengthQ, DimensionsOf -> HasDimensionsQ, KeysOf -> HasKeysQ]
DefineOperator2Rules[AllSameBy -> AllSameByQ, AnySameBy -> AnySameByQ, NoneSameBy -> NoneSameByQ]

(**************************************************************************************************)

DeclaredHere[ListOf, AssociationOf, ListAssociationOf, NonEmptyListOf, NonEmptyAssociationOf, NonEmptyListAssociationOf]

DefineOperator2Rules[
  ListOf                    -> VectorQ,
  AssociationOf             -> AssociationOfQ,
  ListAssociationOf         -> ListAssociationOfQ,
  NonEmptyListOf            -> NonEmptyListOfQ,
  NonEmptyAssociationOf     -> NonEmptyAssociationOfQ,
  NonEmptyListAssociationOf -> NonEmptyListAssociationOfQ
];

(**************************************************************************************************)

(* 2-arg DVs for AssociationOf, NonEmptyAssociationOf in addition to 1-arg ones from above *)
AssociationOf[ktest_, vtest_][assoc_]         := KeysValuesTrue[assoc, ktest, vtest];
NonEmptyAssociationOf[ktest_, vtest_][assoc_] := NonEmptyQ[assoc] && KeysValuesTrue[assoc, ktest, vtest];

RuleVectorOf[vtest_][rules_]         := RuleValuesTrue[rules, vtest];
RuleVectorOf[ktest_, vtest_][rules_] := RulesTrue[rules, ktest, vtest];

(**************************************************************************************************)

DeclaredHere[VectorOf, MatrixOf]

DefineOperator2Rules[VectorOf -> VectorQ, MatrixOf -> MatrixQ]

ArrayOf[test_][data_]         := ArrayOfQ[data, test];
ArrayOf[test_, shape_][data_] := ArrayOfQ[data, test, shape];

(**************************************************************************************************)

PairOf[a_, b_]             := PairOf[{a, b}];
PairOf[preds_][e_]         := PairOfQ[e, preds];

TupleOf[preds:BlankSeq2]   := TupleOf[List[preds]];
TupleOf[preds_List][e_]    := TupleOfQ[e, preds];

RecordOf[preds__Rule]      := RecordOf @ UDict @ preds;
RecordOf[preds_Dict][e_]   := RecordOfQ[e, preds];

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

ModOp[n_][e_]      := If[NumericQ[e], Mod[e, n, 0], e];
ModOp[n_, m_][e_]  := If[NumericQ[e], Mod[e, n, m], e];
ModOp[Infinity]    := Identity;
ModOp[Infinity, _] := Identity;

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

ApplyIfOp[test_, appFn_, rest___][input_] := If[test[input], appFn[input, rest], input, input];

(**************************************************************************************************)

WhichOp[][input_] := input;
WhichOp[test1_, appFn_, rest___][input_] := If[test1[input], appFn @ input, WhichOp[rest][input]];

(**************************************************************************************************)

ConstOp[c_][___] := c;

(**************************************************************************************************)

RiffleOp[r_][list_] := Riffle[list, r];

(**************************************************************************************************)

SetHoldF[ArrayTableOp]

ArrayTableOp[body_][args___] := ArrayTable[body, args];

SetHoldA[ThenOp]

ThenOp[body___][___] := Then[body];

(**************************************************************************************************)

ConstructOp[head_, f_][x_] := head[f[x]];
ConstructOp[head_, f_, g_][x_] := head[f[x], g[x]];
ConstructOp[head_, f_, g_, h_][x_] := head[f[x], g[x], h[x]];
ConstructOp[head_, f_, g_, h_, j_][x_] := head[f[x], g[x], h[x], j[x]];

