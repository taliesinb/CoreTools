SystemExports[
  "Function",
    Lerp, Avg, Multiply,
    PlusOne, MinusOne, OneMinus, OneOver,
    Unthread, SequenceLength, Birange, LengthRange, RangeLength,
    SequenceFirst, SequenceSecond, SequenceThird, SequenceLast, SequenceMost, SequenceRest, SequenceReverse,
    FlatList,
    DropWhile, CommonPrefix, CommonPrefixLength, CommonSuffix, CommonSuffixLength,
    IndexOf,
    VectorIndices, FirstVectorIndex, VectorIndicesOf, FirstVectorIndexOf,
    ExtractIndices, SortedCounts, SortedCountsBy,
    DuplicateIndices, DuplicateIndicesBy,
    Duplicates, DuplicatesBy,
    DeleteNone, DeleteNull,
    GatherAgainst, CombineAgainst, CombineBy,
    ApplyWindowed, ApplyWindowedCyclic, MapWindowed, MapWindowedCyclic, MapTuples, ApplyTuples,
    ListRiffle, ScalarRiffle,
  "Head",
    Unsequence
];

PackageExports[
  "Function",
    Args, HoldArgs,
  "MutatingFunction",
    JoinTo, UnionTo, ReplaceAllIn, ReplaceRepeatedIn,
  "Function",
    ReplaceAllList,
    SelectDiscard, Discard, SelectFirstIndex,
    EnsurePair,
    HoldLength,
    NewCollector, FromCollector,
  "ControlFlowFunction",
    ThenNull, ThenPart, ThenFail, ThenFailEval, Then1, Then2, Then3,
  "Head",
    CollectorFn,
  "Variable",
    $UnthreadEnabled
];

(*************************************************************************************************)

DeclareStrict[NewCollector, FromCollector]
DeclareListable[FromCollector];

NewCollector[]       := CollectorFn[Bag[]];
NewCollector[n_Int]  := Table[NewCollector[], n];

FromCollector[CollectorFn[b_Bag]] := BagPart[b, All];

CollectorFn::badArguments = "`` is not valid."
e:(_CollectorFn[___])           := ReturnMsg["badArguments", HoldForm @ e];
CollectorFn[bag_][item_]        := StuffBag[bag, item];
CollectorFn[bag_][item_, n_Int] := StuffBag[bag, item, n];

(*************************************************************************************************)

DeclareHoldAllComplete[HoldLength];

HoldLength[e_] := Length @ NoEval @ e;

(*************************************************************************************************)

Multiply[e_] := Apply[Times, e];

(*************************************************************************************************)

Args[dict_Dict ? HoldAtomQ] := Values @ dict;
Args[list_List]             := list;
Args[_[args___]]            := List[args];
Args[_]                     := $Failed;

DeclareHoldAllComplete[HoldArgs];

HoldArgs[dict_Dict ? HoldAtomQ] := Level[dict, 1, HoldComplete];
HoldArgs[_[args___]]            := HoldComplete[args];
HoldArgs[_]                     := $Failed;

(*************************************************************************************************)

FlatList[a_List] := Flatten @ a;
FlatList[a___]   := Flatten @ List @ a;

(*************************************************************************************************)

DeclareCurry2[SelectDiscard, Discard, Occurences, FirstOccurence]

SelectDiscard[assoc_Assoc, fn_] := pickTF[assoc, Map[fn /* TrueQ, Values @ assoc]];
SelectDiscard[list_List, fn_] := pickTF[list, Map[fn /* TrueQ, list]];

pickTF[thing_, mask_] := {Pick[thing, mask, True], Pick[thing, mask, False]};

Discard[list_, crit_] := Select[list, Function[e, crit[e] =!= True]];

(*************************************************************************************************)

SortedCounts[list_] := ReverseSort @ Counts[list];
SortedCounts[list_, n_Int] := Take[ReverseSort @ Counts[list], All, UpTo[n]];

SortedCountsBy[list_, f_] := ReverseSort @ CountsBy[list, f];
SortedCountsBy[list_, f_, n_Int] := Take[ReverseSort @ CountsBy[list, f], All, UpTo[n]];

(**************************************************************************************************)

Avg[] := 0;
Avg[a_] := a;
Avg[a_, b_] := (a + b)/2;
Avg[a_, b_, c_] := (a + b + c)/3;
Avg[args__] := Mean[{args}];

(*************************************************************************************************)

Lerp[a_, b_, f_] := a * (1 - f) + b * f;
Lerp[a_, b_, f_List] := Lerp[a, b, #]& /@ f;

Lerp[a_, b_, Into[0]] := {};
Lerp[a_, b_, Into[1]] := (a + b) / 2;
Lerp[a_, b_, Into[2]] := {a, b};
Lerp[a_, b_, Into[n_]] := Lerp[a, b, Range[0, 1, 1/(n-1)]]

Lerp[n_][a_, b_] := Lerp[a, b, n];

(*************************************************************************************************)

General::notPair = "Expected a single item or a pair, not ``.";
EnsurePair = CaseOf[
  a:Except[_List] := {a, a};
  {a_, b_}        := {a, b};
  a_              := ThrowErrorMessage["notPair", a];
];

General::badPairValue = "Value of item or pair `` did not satisfy ``.";
EnsurePair[a_, test_] := Ensure[EnsurePair @ a, VectorOf[test], ThrowErrorMessage["badPairValue", a, test]];

(*************************************************************************************************)

PlusOne[e_] := e + 1;
MinusOne[e_] := e - 1;
OneMinus[e_] := 1 - e;
OneOver[e_] := 1 / e;

(*************************************************************************************************)

ExportFunction[ReplaceAllList]

ReplaceAllList[expr_, rules_] := Locals[
  positions = Position[expr, toRepLHS @ rules];
  Switch[positions,
    {},   {},
    {_},  replaceListAt[expr, rules, P1 @ positions],
    _,    Catenate @ Map[pos |-> replaceListAt[expr, rules, pos], positions]
  ]
];

replaceListAt[expr_, rules_, {}] := ReplaceList[expr, rules];

replaceListAt[expr_, rules_, pos_] := Locals[
  Map[
    result |-> ReplacePart[expr, pos -> result],
    ReplaceList[Extract[expr, pos], rules]
  ]
];

ReplaceAllList[rules_][expr_] := ReplaceAllList[expr, rules];

toRepLHS = CaseOf[
  rules_List := Alternatives @@ Map[toRepLHS, rules];
  lhs_ -> _  := lhs;
  lhs_ :> _  := lhs;
];

(*************************************************************************************************)

DeclareHoldFirst[JoinTo, UnionTo, ReplaceAllIn, ReplaceRepeatedIn];

JoinTo[e_, r_]            := Set[e, Join[e, r]];
UnionTo[e_, r_]           := Set[e, Union[e, r]];
ReplaceAllIn[e_, r_]      := Set[e, ReplaceAll[e, r]];
ReplaceRepeatedIn[e_, r_] := Set[e, ReplaceRepeated[e, r]];

(**************************************************************************************************
`Unthread[{e_1, e_2, ..}]` turns the outer expression into a list of expressions, one for each `e_i`.
* not just `List` but any head is supported.
*)

$UnthreadEnabled = True;

DeclareHoldAllComplete[unthreadThroughQ];
unthreadThroughQ[Set | SetDelayed | Hold | CoreToolsSequence | CoreToolsHold | Print | ToBoxes | MakeBoxes] := False;
unthreadThroughQ[_] := $UnthreadEnabled;

Unthread /: Rule[lhs_, Unthread[rhs_]] :=
  Unthread @ Map[lhs -> #&, rhs];

Unthread /: head_Symbol[l___, Unthread[a_], r___] /; unthreadThroughQ[head] := With[
  {u = Unique["\[FormalO]"]},
  Map[u |-> head[l, u, r], a]
];

Unthread[a_, 0] := a;

Unthread /: head_Symbol[l___, Unthread[a_, n_Int], r___] := With[
  {u = Unique["\[FormalO]"]},
  Construct[Unthread, Map[u |-> head[l, u, r], a], n-1]
];

(*************************************************************************************************)

DeclareHoldAllComplete[ThenNull, ThenPart, ThenFail, ThenFailEval, Then1, Then2, Then3]
DeclareSequenceHold[Unsequence]

ThenNull[e___] := Then[e, Null];
ThenFail[e___] := Then[e, $Fail];
ThenFailEval[e___] := Then[e, FailEval];
ThenPart[n_Int, e___] := Part[Unsequence[e], n];

Then1[e___] := P1 @ Unsequence[e];
Then2[e___] := P2 @ Unsequence[e];
Then3[e___] := P3 @ Unsequence[e];

(*************************************************************************************************)

SequenceFirst[e_, ___] := e;
SequenceSecond[_, e_, ___] := e;
SequenceThird[_, _, e_, ___] := e;
SequenceLast[___, e_]  := e;
SequenceMost[e___, _]  := e;
SequenceRest[_, e___]  := e;
SequenceReverse[e___]  := Seq @@ Reverse[{e}];

(*************************************************************************************************)

SequenceLength[]        := 0;
SequenceLength[_]       := 1;
SequenceLength[_, _]    := 2;
SequenceLength[_, _, _] := 3;
s_SequenceLength        := Length[Unevaluated @ s];

(*************************************************************************************************)
Birange::usage = "Birange[a, b]` gives `Range[a,b]` or `Range[b, a, -1]` as appropriate."

Birange[a_, b_]     := Range[a, b, Sign[b - a]];
Birange[a_, b_, d_] := Range[a, b, Sign[b - a] * d];

(*************************************************************************************************)

LengthRange::usaage = "LengthRange[e]` gives `{ 1, 2, .., Length[e] }`."
LengthRange[expr_] := Range @ Length @ expr;
RangeLength[expr_] := Range @ Length @ expr;

(*************************************************************************************************)

DeclareHoldRest[SelectFirstIndex]
DeclareCurry2[SelectFirstIndex]

SelectFirstIndex[assoc_Association, fn_, default_:None] := Module[{fn2 = fn},
  Association`ScanWhile[assoc, Function[z, If[fn2 @ Last @ z, Return[First @ z, Module], True, True]]];
  default
];

SelectFirstIndex[list_, fn_, default_:None] := Module[{fn2 = fn},
  MapIndexed[Function[{z, i}, If[fn2 @ z, Return[First @ i, Module], Null, Null]], list];
  default
];

(**************************************************************************************************
`DropWhile[{e$1, e$2, $$}, f$]` drops the initial elements `e$i` that all yield `f$[ei$] = True`.
*)

DeclareCurry2[DropWhile]

DropWhile[list_, f_] := Drop[list, LengthWhile[list, f]];

(**************************************************************************************************
`CommonPrefix[{e$1, e$2, $$}]` gives the expression that is the longest common prefix of all the `e$i`.
`CommonSuffix[{e$1, e$2, $$}]` gives the expression that is the longest common suffix of all the `list$i`.
*)

CommonPrefix[list_List] := commonPrefixSuffix[list, 1];
CommonSuffix[list_List] := commonPrefixSuffix[list, -1];

commonPrefixSuffix[{}, _] := {};
commonPrefixSuffix[{e_}, _] := e;
commonPrefixSuffix[list_, mult_] := Take[First @ list, mult * commonPrefixSuffixLen[list, mult]];

(**************************************************************************************************
`CommonPrefixLength[{e$1, e$2, $$}]` gives the length of the expression that is the longest common prefix of all the `e$i`.
`CommonSuffixLength[{e$1, e$2, $$}]` gives the length of the expression that is the longest common suffix of all the `e$i`.
*)

CommonPrefixLength[list_List] := commonPrefixSuffixLen[list, 1];
CommonSuffixLength[list_List] := commonPrefixSuffixLen[list, -1];

commonPrefixSuffixLen[{}, _] := 0;
commonPrefixSuffixLen[{list_}, _] := Length @ list;
commonPrefixSuffixLen[list_, mult_] := Module[
  {min = Min @ Map[Length, list]},
  Do[
    If[NotAllSameQ[Part[list, All, mult * n]], Return[n-1, Module]],
    {n, min}
  ];
  min
];

(**************************************************************************************************)

DeclareStrict[IndexOf];
DeclareHoldRest[IndexOf];

IndexOf[EmptyP, _, else_] := else;
IndexOf[expr_ ? IntVecQ, elem_ ? IntQ, else_] := First[FastNumericIndices[expr, elem, 1], else];
IndexOf[expr_ ? NonZeroDepthQ, elem_, else_] := FirstPosition[expr, Verbatim[elem], else, {1}];

(**************************************************************************************************)

DeclareCurry2[VectorIndicesOf, FirstVectorIndexOf]

VectorIndicesOf[x_, n_Integer] := TryEval @ Replace[FastNumericIndices[x, n], _Failure | $Failed -> Fail]
VectorIndicesOf[x_, n_] := Flatten @ Position[x, Verbatim[n], {1}];

FirstVectorIndexOf[x_, n_Integer] := TryEval @ First[Replace[FastNumericIndices[x, n, 1], _Failure | $Failed -> {Fail}], None];
FirstVectorIndexOf[x_, n_] := First @ FirstPosition[x, Verbatim[n], {None}, {1}];

(**************************************************************************************************)

DeclareCurry2[VectorIndices, FirstVectorIndex]
DeclareHoldRest[FirstVectorIndex, iFirstVectorIndex]

VectorIndices::usage =
"VectorIndices[list$, test$] returns the i$ for which test$[e$i] gives True.
It uses special kernel code for common numeric predicates like Positive, Negative, EqualTo[$$], GreaterThan[$$], etc.
VectorIndices[test$] is the operator form of VectorIndices.
"

FirstVectorIndex::usage =
"FirstVectorIndex[list$, test$] returns the i$ for which test[e$i] gives True.
It uses special kernel code for common numeric predicates like Positive, Negative, EqualTo[$$], GreaterThan[$$], etc.
FirstVectorIndex[list$, test$, def$] returns default$ if no element passes.
FirstVectorIndex[test$] is the operator form of FirstVectorIndex.
"

VectorIndices[EmptyP, _] := {};
VectorIndices[vec_, EqualTo[r_Integer]] /; IntegerVectorQ[vec] := FastNumericIndices[vec, r];

FirstVectorIndex[vec_, pred_, def_:None] :=
  iFirstVectorIndex[vec, Evaluate @ pred, def];

iFirstVectorIndex[vec_, EqualTo[r_Integer], else_] /; IntegerVectorQ[vec] :=
  First[FastNumericIndices[vec, r, 1], else];

(**************************************************************************************************)

vecIndsDef[lhs_ :> numVecPred[rhs_, test_]] := Hold[
  VectorIndices[$$_, lhs],
  RuleCondition @ Replace[FastNumericIndices[rhs, test], _Failure | $Failed -> Fail]
];

vecFirstIndDef[lhs_ :> numVecPred[rhs_, test_]] := Hold[
  iFirstVectorIndex[$$_, lhs, else_],
  RuleCondition @ First[
    Replace[FastNumericIndices[rhs, test, 1], _Failure | $Failed -> {Fail}],
    else
  ]
];

defineVectorPredicates[list_List] :=
  MapApply[SetDelayed, Join[vecIndsDef /@ list, vecFirstIndDef /@ list] /. HoldPattern[NumQ] -> RealValuedNumberQ];

defineVectorPredicates[{
                 Positive :> numVecPred[$$,          Positive],
                 Negative :> numVecPred[$$,          Negative],
              NonPositive :> numVecPred[$$,          NonPositive],
              NonNegative :> numVecPred[$$,          NonNegative],
       UnequalTo[r_?NumQ] :> numVecPred[Abs[$$ - r], Positive],
         EqualTo[r_?NumQ] :> numVecPred[Abs[$$ - r], NonPositive],
   LessEqualThan[r_?NumQ] :> numVecPred[$$ - r,      NonPositive],
GreaterEqualThan[r_?NumQ] :> numVecPred[$$ - r,      NonNegative],
        LessThan[r_?NumQ] :> numVecPred[$$ - r,      Negative],
     GreaterThan[r_?NumQ] :> numVecPred[$$ - r,      Positive],
   Between[{a_?NumQ, b_}] :> numVecPred[Clip[$$ - a, {0, b-a}, {-1,-1}], NonNegative]
}];

VectorIndices[expr_, test_] :=
  Pick[Range @ Length @ expr, MapVals[test, expr], True];

iFirstVectorIndex[expr_, test_, def_] := Module[{i = 0},
  Scan[elem |-> If[i++; test @ elem, Return[i, Module], Null, Null], expr];
  def
];

(**************************************************************************************************)

ExtractIndices::usage =
"ExtractIndices[expr$, {i$1, i$2, $$}] gives a list of parts of expr$, where the i$ are non-negative integers.
ExtractIndices[expr$, array$] assumes array$ is a structure whose leaves are non-negative integers."

ExtractIndices = CaseOf[

  $[expr_, index_Int ? NonNegativeMachineIntegerQ] := Part[expr, index];

  $[expr_, vector_List /; VectorQ[vector, NonNegativeMachineIntegerQ]] :=
    Part[expr, vector];

  $[expr_, matrix_List /; MatrixQ[matrix, NonNegativeMachineIntegerQ]] :=
    Map[vector |-> Part[expr, vector], matrix];

  $[expr_, listVec_ ? ListVecQ] :=
    Map[list |-> $[expr, list], listVec];
];

DeclareStrict[ExtractIndices];

(**************************************************************************************************)

Duplicates[list_List] := DeleteCases[{_}] @ Gather[list];
DuplicateIndices[list_List | list_Assoc] := DeleteCases[{_}] @ Values @ PositionIndex @ list;

DeclareCurry2[DuplicateIndicesBy, DuplicatesBy]

DuplicateIndicesBy[list_List | list_Assoc, fn_] := DuplicateIndices @ Map[fn, list];
DuplicatesBy[list_List, fn_] := DeleteCases[{_}] @ GatherBy[list, fn];

(**************************************************************************************************)

DeleteNone[e_] := DeleteCases[e, None];
DeleteNull[e_] := DeleteCases[e, Null];

(**************************************************************************************************)

DeclareStrict[GatherAgainst]

GatherAgainst::usage =
"GatherAgainst[expr$, against$] gathers expr$ into sublists whose corresponding values in against$ are equal."

GatherAgainst[expr_, against_] /; SameLenQ[expr, against] :=
  KeyValueMap[indices |-> Part[expr, indices], PositionIndex @ against];

(**************************************************************************************************)

DeclareStrict[CombineAgainst]

CombineAgainst::usage = "
CombineAgainst[expr$, against$, f$] returns a list of f$[a$, {e$i, e$j, $$}], where the e$ are parts of \
expr$ that correspond to the value a$ in against$.
"

CombineAgainst[expr_, against_, fn_:Id] /; SameLenQ[expr, against] :=
  KeyValueMap[{key, indices} |-> fn[key, Part[expr, indices]], PositionIndex @ against];

(**************************************************************************************************)

DeclareCurry23[CombineBy]

CombineBy::usage =
"CombineBy[expr$, f$, g$] returns a list of g$[k$, {e$i, e$j, $$}], where the e$ are parts of \
expr$ for which f$[e$] gives k$.
CombineBy[f$, g$] is the operator form of CombineBy.
"

CombineBy[expr_, f_, g_] := KeyValueMap[g, GroupBy[expr, f]];

(**************************************************************************************************)

ApplyWindowed::usage = "ApplyWindowed[f$, {e$1, e$2, $$, e$n}] gives {f$[e$1, e$2], f$[e$2, e$3], $$, f$[e$(n-1), e$n]}."

MapWindowed[f_, list_]             := f /@ Partition[list, 2, 1];
MapWindowed[f_, list_, n_]         := f /@ Partition[list, n, 1];

ApplyWindowed[f_, list_]           := f @@@ Partition[list, 2, 1];
ApplyWindowed[f_, list_, n_]       := f @@@ Partition[list, n, 1];

MapWindowedCyclic[f_, list_]       := f /@ Partition[list, 2, 1, 1];
MapWindowedCyclic[f_, list_, n_]   := f /@ Partition[list, n, 1, 1];

ApplyWindowedCyclic[f_, list_]     := f @@@ Partition[list, 2, 1, 1];
ApplyWindowedCyclic[f_, list_, n_] := f @@@ Partition[list, n, 1, 1];

(**************************************************************************************************)

DeclareCurry1[ApplyTuples, MapTuples]

MapTuples[f_, pairs_]       := Map[f, Tuples @ pairs];
MapTuples[f_, pairs_, n_]   := Map[f, Tuples[pairs, n]];

ApplyTuples[f_, pairs_]     := f @@@ Tuples[pairs];
ApplyTuples[f_, pairs_, n_] := f @@@ Tuples[pairs, n];

(**************************************************************************************************)

(* TODO: is this meaningfully different from AtIndices? *)

(* "
MapIndices[f$, {i$1, i$2, $$},  {e$1, e$2, $$}] applies f$ selectively on elements e$(i$1), e$(i$2), $$.
MapIndices[f$, indices$] is the operator form of MapIndices.
* indices that don't exist are skipped.
"

MapIndices[f_, {}, expr_] := expr;

MapIndices[f_, indicesLists:{__List}, expr_] :=
  MapIndices[f, #, expr]& /@ indicesLists;

MapIndices[f_, indices_, expr_] :=
  SafeMapAt[f, expr, List /@ indices];

MapIndices[f_, indices_][expr_] := MapIndices[f, indices, expr];
 *)

(**************************************************************************************************)

ListRiffle[list_List, {}] := list;
ListRiffle[list_List, riffleList_List] := Locals[
  riff = PadRight[riffleList, Len[list], L @ riffleList];
  Most @ Catenate @ Transpose[{list, riff}]
];

ScalarRiffle[list_List, scalar_] :=
  Most @ Catenate @ Transpose[{list, ConstList[scalar, Len @ list]}];
