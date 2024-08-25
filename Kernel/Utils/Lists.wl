SystemExports[
  "Function",
    CountUnique, CountUniqueBy, ToUnique,
    Lerp, Avg, Multiply,
    PlusOne, MinusOne, OneMinus, OneOver,
    Unthread, SequenceLength, Birange, LengthRange, RangeLength,
    SequenceNothing, SequenceFirst, SequenceSecond, SequenceThird, SequenceLast, SequenceMost, SequenceRest, SequenceReverse,
    SequenceFirstSecond, SequenceSecondFirst,
    FlatList,
    DropWhile, CommonPrefix, CommonPrefixLength, CommonSuffix, CommonSuffixLength,
    IndexOf,
    VectorIndices, FirstVectorIndex, VectorIndicesOf, FirstVectorIndexOf,
    ExtractIndices, SortedCounts, SortedCountsBy,
    DuplicateIndices, DuplicateIndicesBy,
    Duplicates, DuplicatesBy,
    DeleteNone, DeleteNull, DeleteFailed, DeleteEmpty, DeleteVerbatim,
    GatherAgainst, CombineAgainst, CombineBy,
    ApplyWindowed, ApplyWindowedCyclic, MapWindowed, MapWindowedCyclic, MapTuples, ApplyTuples,
    ListRiffle, ScalarRiffle,
    TrimRight, TrimLeft,
    ReplaceIndices, ConstantReplaceIndices
];

PackageExports[
  "Function",
    Args,
    Clip2,
  "ControlFlowFunction",
    HoldArgs,
  "MutatingFunction",
    JoinTo, UnionTo, ReplaceAllIn, ReplaceRepeatedIn,
  "Function",
    SelectDiscard, Discard, SelectFirstIndex, PickTrueFalse,
    EnsurePair,
  "Variable",
    $UnthreadEnabled
];

(*************************************************************************************************)

DecUElemDispatch1N @ SetHoldR @ ToUnique;

ToUnique[list_List, else_] := ToUnique[list, else, else];
ToUnique[list_List, notUnique_:None, isEmpty_:None] := If[SameQ @@ list, First[list, isEmpty], notUnique];

(*************************************************************************************************)

DecUElemDispatch1 @ CountUnique;

CountUnique[list_List] := Len @ Union @ list;
CountUnique[expr_]     := Len @ Union @ HoldArgs @ expr;

DecUElemDispatch12 @ SetCurry2 @ CountUniqueBy;

CountUniqueBy[list_List, fn_] := Len @ Union @ Map[fn, list];
CountUniqueBy[expr_, fn_]     := Len @ Union @ MapApply[fn, HoldArgs @ expr];

(*************************************************************************************************)

SetCurry2[TrimRight, TrimLeft]

TrimRight[e:EmptyP, _] := e;
TrimRight[e_, d_]      := If[MatchQ[Last[e, $dummy], d], Most @ e, e];

TrimLeft[e:EmptyP, _] := e;
TrimLeft[e_, d_]      := If[MatchQ[First[e, $dummy], d], Rest @ e, e];

(*************************************************************************************************)

SetCurry23[Clip2]

Clip2[x_, a_, b_] := Clip[x, {a, b}];

(*************************************************************************************************)

DecUElemDispatch1 @ Multiply;

Multiply[e_] := Apply[Times, e];

(*************************************************************************************************)

DecOElemDispatch1[Args, HoldArgs];

Args[dict_Dict ? HoldAtomQ] := Values @ dict;
Args[list_List]             := list;
Args[_[args___]]            := List[args];
Args[_]                     := $Failed;

SetHoldC[HoldArgs]

HoldArgs[dict_Dict ? HoldAtomQ] := Level[dict, 1, HoldComplete];
HoldArgs[_[args___]]            := HoldComplete[args];
HoldArgs[_]                     := $Failed;

(*************************************************************************************************)

FlatList[a_List] := Flatten @ a;
FlatList[a___]   := Flatten @ List @ a;

(*************************************************************************************************)

DecFullDispatch12[SelectDiscard, Discard];

SetCurry2[SelectDiscard, Discard]

SelectDiscard[assoc_Dict, fn_] := PickTrueFalse[assoc, Map[fn /* TrueQ, Values @ assoc]];
SelectDiscard[list_List, fn_] := PickTrueFalse[list, Map[fn /* TrueQ, list]];

PickTrueFalse[thing_, mask_] := {Pick[thing, mask, True], Pick[thing, mask, False]};

Discard[list_, crit_] := Select[list, Function[e, crit[e] =!= True]];

(*************************************************************************************************)

DecUElemDispatch1N[SortedCounts, SortedCountsBy]

SortedCounts[list_] := ReverseSort @ Counts[list];
SortedCounts[list_, n_Int] := Take[ReverseSort @ Counts[list], All, UpTo[n]];

SortedCountsBy[list_, f_] := ReverseSort @ CountsBy[list, f];
SortedCountsBy[list_, f_, n_Int] := Take[ReverseSort @ CountsBy[list, f], All, UpTo[n]];

(**************************************************************************************************)

DecUElemDispatch1[Avg];

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
  a_              := ThrowMsg["notPair", a];
];

General::badPairValue = "Value of item or pair `` did not satisfy ``.";
EnsurePair[a_, test_] := Ensure[EnsurePair @ a, VectorOf[test], ThrowMsg["badPairValue", a, test]];

(*************************************************************************************************)

PlusOne[e_] := e + 1;
MinusOne[e_] := e - 1;
OneMinus[e_] := 1 - e;
OneOver[e_] := 1 / e;

(*************************************************************************************************)

ConstantReplaceIndices::usage =
"ConstantReplaceIndices[expr$, indices$, value$]` replaces the parts at various indices with a single value value$.
* indices can be All, Span[$$], or List[p$1, p$2, $$].";

(* TODO: support Broadcast *)
ConstantReplaceIndices[expr_, indices_, value_List] := ReplacePart[expr, Map[List, indices] -> value];
ConstantReplaceIndices[expr_, indices_, value_]     := setParts[expr, indices, value];

(*************************************************************************************************)

ReplaceIndices::usage =
"ConstantReplaceIndices[expr$, indices$, value$]` replaces the parts at various indices with value$.
* indices can be All, Span[$$], or List[p$1, p$2, $$].";

SetStrict @ ReplaceIndices;

General::replaceLengthMismatch = "List of indices to replace has length `` but values have length ``."
ReplaceIndices[expr_, indices_List, values_List] /; SameLenQOrThrow[indices, values, "replaceLengthMismatch"] :=
  setParts[expr, indices, values];

setParts[expr_, parts_, value_] := Locals[expr2 = expr; Part[expr2, parts] = value; expr2];

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
Birange::usage = "Birange[a, b]` gives `Range[a,b]` or `Range[b, a, -1]` as appropriate."

Birange[a_, b_]     := Range[a, b, Sign[b - a]];
Birange[a_, b_, d_] := Range[a, b, Sign[b - a] * d];

(*************************************************************************************************)

LengthRange::usaage = "LengthRange[e]` gives `{ 1, 2, .., Length[e] }`."
LengthRange[expr_] := Range @ Length @ expr;
RangeLength[expr_] := Range @ Length @ expr;

(*************************************************************************************************)

DecOElemDispatch13[SelectFirstIndex]

SetHoldR[SelectFirstIndex]
SetCurry2[SelectFirstIndex]

SelectFirstIndex[assoc_Dict, fn_, default_:None] := Module[{fn2 = fn},
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

DecFullDispatch12 @ SetCurry2[DropWhile]

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

DecOElemDispatch13 @ SetHoldR @ SetStrict @ IndexOf;

IndexOf[EmptyDataP, _, else_] := else;
IndexOf[expr_ ? IntVecQ, elem_ ? IntQ, else_] := First[FastNumericIndices[expr, elem, 1], else];
IndexOf[expr_, elem_, else_] := FirstPosition[expr, Verbatim[elem], else, {1}];

(**************************************************************************************************)

DecOElemDispatch12 @ SetCurry2[VectorIndicesOf, FirstVectorIndexOf]

VectorIndicesOf[x_, n_Integer] := MaybeEval @ Replace[FastNumericIndices[x, n], _Failure | $Failed -> FailEval]
VectorIndicesOf[x_, n_] := Flatten @ Position[x, Verbatim[n], {1}];

FirstVectorIndexOf[x_, n_Integer] := MaybeEval @ First[Replace[FastNumericIndices[x, n, 1], _Failure | $Failed -> {FailEval}], None];
FirstVectorIndexOf[x_, n_] := First @ FirstPosition[x, Verbatim[n], {None}, {1}];

(**************************************************************************************************)

SetCurry2[VectorIndices, FirstVectorIndex]

VectorIndices::usage =
"VectorIndices[list$, test$] returns the i$ for which test$[e$i] gives True.
It uses special kernel code for common numeric predicates like Positive, Negative, EqualTo[$$], GreaterThan[$$], etc.
VectorIndices[test$] is the operator form of VectorIndices.
"

VectorIndices[EmptyDataP, _] := {};
VectorIndices[vec_, EqualTo[r_Integer]] /; IntegerVectorQ[vec] := FastNumericIndices[vec, r];

(**************************************************************************************************)

SetHoldR[FirstVectorIndex, iFirstVectorIndex]

FirstVectorIndex::usage =
"FirstVectorIndex[list$, test$] returns the i$ for which test[e$i] gives True.
It uses special kernel code for common numeric predicates like Positive, Negative, EqualTo[$$], GreaterThan[$$], etc.
FirstVectorIndex[list$, test$, def$] returns default$ if no element passes.
FirstVectorIndex[test$] is the operator form of FirstVectorIndex.
"

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
ExtractIndices[expr$, array$] assumes array$ is a structure whose leaves are non-negative integers.
Parts is an alias for ExtractIndices."

ExtractIndices = CaseOf[

  $[expr_, index_Int ? NonNegativeMachineIntegerQ] := Part[expr, index];

  $[expr_, vector_List /; VectorQ[vector, NonNegativeMachineIntegerQ]] :=
    Part[expr, vector];

  $[expr_, matrix_List /; MatrixQ[matrix, NonNegativeMachineIntegerQ]] :=
    Map[vector |-> Part[expr, vector], matrix];

  $[expr_, listVec_ ? ListVecQ] :=
    Map[list |-> $[expr, list], listVec];
];

DecFullDispatch1 @ SetStrict @ ExtractIndices;

(**************************************************************************************************)

DecOElemDispatch1 @ Duplicates;
DecUElemDispatch1 @ DuplicateIndices;

Duplicates[list_List]            := DeleteCases[{_}] @ Gather[list];
DuplicateIndices[list:ListDictP] := DeleteCases[{_}] @ Values @ PositionIndex @ list;

(**************************************************************************************************)

DecOElemDispatch12 @ DuplicatesBy;
DecUElemDispatch12 @ DuplicateIndicesBy;

SetCurry2[DuplicateIndicesBy, DuplicatesBy]

DuplicatesBy[list_List, fn_]            := DeleteCases[{_}] @ GatherBy[list, fn];
DuplicateIndicesBy[list:ListDictP, fn_] := DuplicateIndices @ Map[fn, list];

(**************************************************************************************************)

DecFullDispatch1[DeleteNone, DeleteNull, DeleteFailed, DeleteEmpty, DeleteVerbatim];

DeleteNone[e_]         := DeleteCases[e, None];
DeleteNull[e_]         := DeleteCases[e, Null];
DeleteFailed[e_]       := DeleteCases[e, $Failed];
DeleteEmpty[e_]        := DeleteCases[e, EmptyP];
DeleteVerbatim[e_, v_] := DeleteCases[e, Verbatim @ v];

(**************************************************************************************************)

SetStrict[GatherAgainst]

GatherAgainst::usage =
"GatherAgainst[expr$, against$] gathers expr$ into sublists whose corresponding values in against$ are equal."

GatherAgainst[expr_, against_] /; SameLenQ[expr, against] :=
  KeyValueMap[indices |-> Part[expr, indices], PositionIndex @ against];

(**************************************************************************************************)

SetStrict[CombineAgainst]

CombineAgainst::usage = "
CombineAgainst[expr$, against$, f$] returns a list of f$[a$, {e$i, e$j, $$}], where the e$ are parts of \
expr$ that correspond to the value a$ in against$.
"

CombineAgainst[expr_, against_, fn_:Id] /; SameLenQ[expr, against] :=
  KeyValueMap[{key, indices} |-> fn[key, Part[expr, indices]], PositionIndex @ against];

(**************************************************************************************************)

SetCurry23[CombineBy]

CombineBy::usage =
"CombineBy[expr$, f$, g$] returns a list of g$[k$, {e$i, e$j, $$}], where the e$ are parts of \
expr$ for which f$[e$] gives k$.
CombineBy[f$, g$] is the operator form of CombineBy.
"

CombineBy[expr_, f_, g_] := KeyValueMap[g, GroupBy[expr, f]];

(**************************************************************************************************)

MapWindowed::usage =
"MapWindowed[f$, {e$1, e$2, $$, e$n}] gives {f$[{e$1, e$2}], f$[{e$2, e$3}], $$, f$[{e$(n-1), e$n]}}.
MapWindowed[f$, list$, n$] provides tuples of length n$ to f$."

ApplyWindowed::usage =
"ApplyWindowed[f$, {e$1, e$2, $$, e$n}] gives {f$[e$1, e$2], f$[e$2, e$3], $$, f$[e$(n-1), e$n]}.
ApplyWindowed[f$, list$, n$] provides tuples of length n$ to f$."

MapWindowedCyclic::usage = "MapWindowedCyclic is like MapWindowed but considers the list to be cyclic."
ApplyWindowedCyclic::usage = "ApplyWindowedCyclic is like ApplyWindowed but considers the list to be cyclic."

MapWindowed[f_, list_]             := f /@ Partition[list, 2, 1];
MapWindowed[f_, list_, n_]         := f /@ Partition[list, n, 1];

ApplyWindowed[f_, list_]           := f @@@ Partition[list, 2, 1];
ApplyWindowed[f_, list_, n_]       := f @@@ Partition[list, n, 1];

MapWindowedCyclic[f_, list_]       := f /@ Partition[list, 2, 1, 1];
MapWindowedCyclic[f_, list_, n_]   := f /@ Partition[list, n, 1, 1];

ApplyWindowedCyclic[f_, list_]     := f @@@ Partition[list, 2, 1, 1];
ApplyWindowedCyclic[f_, list_, n_] := f @@@ Partition[list, n, 1, 1];

(**************************************************************************************************)

MapTuples::usage =
"MapTuples[f$, {list$1, list$2, $$}] produces a list of f$[{e$1, e$2, $$}] where each e$i is taken from list$i.
MapTuples[f$, list$, n$] uses n$ elements from a single list as arguments for each f$."

ApplyTuples::usage =
"ApplyTuples[f$, {list$1, list$2, $$}] produces a list of f$[e$1, e$2, $$] where each e$i is taken from list$i.
ApplyTuples[f$, list$, n$] uses n$ elements from a single list as arguments for each f$."

MapTuples[f_, lists_]       := Map[f, Tuples @ lists];
MapTuples[f_, list_, n_]   := Map[f, Tuples[list, n]];

ApplyTuples[f_, lists_]     := f @@@ Tuples[lists];
ApplyTuples[f_, lists_, n_] := f @@@ Tuples[lists, n];

(**************************************************************************************************)

ListRiffle::usage =
"ListRiffle[list$, {r$1, r$2, $$, r$n}] riffles elements of list$ with each r$i in turn, repeating the final r$n if neccessary."

ListRiffle[list_List, {}] := list;
ListRiffle[list_List, riffleList_List] := Locals[
  riff = PadRight[riffleList, Len[list], Last @ riffleList];
  Most @ Catenate @ Transpose[{list, riff}]
];

ScalarRiffle::usage =
"ScalarRiffle[list$, r$] riffles elements of list$ with r$.
Unlike Riffle, ScalarRiffle does not behave differently if r$ is itself a list."

ScalarRiffle[list_List, scalar_] :=
  Most @ Catenate @ Transpose[{list, ConstList[scalar, Len @ list]}];
