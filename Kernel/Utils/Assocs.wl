SystemExports[
  "Function",
    ListAssociationParts,
    InvertAssociation, InvertUnorderedAssociation, ReverseRules, MapRules,
    KeyMapValueMap, ValueMap, KeysValues,
    ToValues, ToRuleList,
    RuleThread, RuleUnthread, UnorderedAssociationThread,
    RangeRules, RangeAssociation, RangeUnorderedAssociation,
    RulesRange, AssociationRange, UnorderedAssociationRange,
    ConstantRules, ConstantAssociation, ConstantUnorderedAssociation,
    ConstantTrueAssociation, ConstantFalseAssociation,
    TrueRules,
    UnorderedAssociationMapApply, UnorderedAssociationMapThread, UnorderedAssociationMap,
    AssociationMapThread, AssociationMapApply,
    PairsToAssociation, PairsToUnorderedAssociation, AssociationToPairs,
    RulesToAssociation, RulesToUnorderedAssociation, AssociationToRules,
    GroupPairs, GroupAgainst, CombineBy, CombineAgainst, MergeAssocations,
    AssociationSum, UnorderedAssociationSum,
    AssociationPlus, UnorderedAssociationPlus,
    UnorderedCounts,
    LevelIndex, PadAssociation,
    AssociationThreadOp
];

PackageExports[
  "Function",
    Bind, UBind,
  "MutatingFunction",
    CachedTo,
    BindTo,
    InternTo, InternUniqueTo,
    KeyApplyTo, KeyIncrement, KeyDecrement,
    KeyAddTo,   KeySubFrom,   KeyTimesBy,   KeyDivideBy,
    KeyUnionTo, KeyJoinTo,    KeyAppendTo,  KeyPrependTo, KeyBindTo, KeyUBindTo,
  "MetaFunction",
    DefineKeywiseOperator1, DefineKeywiseOperator2,
  "Function",
    EnsureODict
];

(**************************************************************************************************)

EnsureODict::usage =
"EnsureODict[UnorderedAssociation[$$]] returns Association[$$].
EnsureODict has no effect on other expressions."

(* because some functions like Extract don't work on UnorderedAssociation *)
EnsureODict[e:DictP] := Dict @ e;
EnsureODict[e_]      := e

(**************************************************************************************************)

ListAssociationParts::usage =
"ListAssociationParts[<|k$1 -> v$1, k$2 -> v$2, $$|>] returns {Key[k$1], Key[k$2], $$}.
ListAssociationParts[{e$1, e$2, $$, e$n}] returns {1, 2, $$, n$}."

SetStrict @ ListAssociationParts;

ListAssociationParts[dict_Dict] := Keys[dict, Key];
ListAssociationParts[list_List] := RangeLen @ list;

(**************************************************************************************************)

SetStrict @ PadAssociation;

PadAssociation::usage =
"PadAssociation[assoc$, keys$, default$] pads assoc$ to have keys$ if not present, with value default$."

PadAssociation[dict_Dict, keys_, val_] :=
  Join[dict, ConstDict[Complement[keys, Keys @ dict], val]];

(**************************************************************************************************)

ReverseRules::usage =
"ReverseRules[{k$1 -> v$1, k$2 -> v$2, $$}] returns {v$1 -> k$1, v$2 -> k$2, $$}.
ReverseRules[<|k$1 -> v$1, k$2 -> v$2, $$|>] returns <|v$1 -> k$1, v$2 -> k$2, $$|>."

SetStrict[ReverseRules]

ReverseRules[dict_Dict]          := Dict @ Map[Reverse, Normal @ dict];
ReverseRules[rules_ ? RuleLVecQ] := Map[Reverse, rules];

(**************************************************************************************************)

InvertAssociation::usage =
"InvertAssociation[<|k$1 -> v$1, k$2 -> v$2, $$|>] returns <|v$1 -> k$1, v$2 -> k$2, $$|>.
InvertAssociation throws an error if a unique inverse does not result."

InvertUnorderedAssociation::usage =
"InvertUnorderedAssociation[<|k$1 -> v$1, k$2 -> v$2, $$|>] returns UnorderedAssociation[v$1 -> k$1, v$2 -> k$2, $$].
InvertUnorderedAssociation throws an error if a unique inverse does not result."

SetStrict[InvertAssociation, InvertUnorderedAssociation];

InvertAssociation::notUnique = "Cannot uniquely invert association ``.";
InvertAssociation[dict:DictP] := Module[
  {res = ReverseRules @ dict},
  If[Len[res] == Len[dict], res,
    ErrorMsg[InvertAssociation::notUnique, dict]]];

InvertUnorderedAssociation::notUnique = "Cannot uniquely invert association ``.";
InvertUnorderedAssociation[dict:DictP] := Module[
  {res = UDict @ Reverse[Normal @ dict, 2]},
  If[Len[res] == Len[dict], res,
    ErrorMsg[InvertAssociation::notUnique, dict]]];

(**************************************************************************************************)

(* TODO: rename to ApplyRules? *)
DecFullDispatch2 @ SetCurry2 @ MapRules;

MapRules::usage =
"MapRules[f$, <|k$1 -> v$1, k$2 -> v$2, $$|>] returns {f$[k$1, v$1], f$[k$2, v$2], $$}.
MapRules does the same for a list of rules, where it applies f$ to the pair of LHS and RHS of each rule."

MapRules[fn_, dict_Dict]      := KeyValueMap[fn, dict];
MapRules[fn_, list:RuleLVecP] := MapApply[fn, rules];
MapRules[_, _]                := InternalError;

(**************************************************************************************************)

KeyMapValueMap::usage =
"KeyMapValueMap[f$k, f$v, <|k$1 -> v$1, $$|>] returns <|f$k[k$1] -> f$v[v$1], $$|>.
KeyMapValueMap also works on lists of rules, where it applies f$k to the LHS and f$v to the RHS of each rule."

DecFullDispatch3 @ SetCurry12 @ KeyMapValueMap;

KeyMapValueMap[kfn_, vfn_, dict_Dict] := KeyMap[kfn,  Map[vfn,     dict]];
KeyMapValueMap[kfn_, vfn_, ruls_List] := MapCol1[kfn, MapCol2[vfn, ruls]];
KeyMapValueMap[_, _, _]               := InternalError;

(**************************************************************************************************)

ValueMap::usage =
"ValueMap[f$, <|k$1 -> v$1, k$2 -> v$2, $$|>] returns <|k$1 -> f[v$1], k$2 -> f$[v$2]|>.
ValueMap also works on lists of rules, where it applies f$ to the RHS of each rule."

DecFullDispatch2 @ SetCurry12 @ ValueMap;

ValueMap[vfn_, dict_Dict] := Map[vfn, dict];
ValueMap[vfn_, ruls_List] := MapCol2[vfn, dict];
ValueMap[_, _, _]         := InternalError;

(**************************************************************************************************)

KeysValues::usage =
"KeysValues[<|k$1 -> v$1, k$2 -> v$2, $$|>] returns {{k$1, k$2, $$}, {v$1, v$2, $$}}.
KeysValues also works on a list of rules."

DecFullDispatch1 @ SetStrict @ KeysValues;

KeysValues[data:DictLikeP] := {Keys @ data, Values @ data};
KeysValues[_]              := InternalError;

(**************************************************************************************************)

(* TODO: rename MaybeValues. It's actually just a narrower Args *)
ToValues::usage =
"ToValues[<|k$1 -> v$1, k$2 -> v$2, $$|>] returns {v$1, v$2, $$}.
ToValues[{v$1, v$2, $$}] returns {v$1, v$2, $$}."

DecFullDispatch1 @ SetStrict @ ToValues;

ToValues[list_List] := list;
ToValues[dict_Dict] := Values @ dict;

(**************************************************************************************************)

ToRuleList::usage =
"ToRuleList[<|k$1 -> v$1, $$|>] returns {k$1 -> v$1, $$}.
ToRuleList[{k$1 -> v$1, $$}] returns {k$1 -> v$1, $$}."

DecFullDispatch1 @ SetStrict @ ToRuleList;

ToRuleList[dict_Dict]     := Normal @ dict;
ToRuleList[list:RuleVecP] := list;
ToRuleList[_]             := InternalError;

ToRuleList[]              := {};
ToRuleList[specs__]       := Join @ Map[ToRuleList, NoEval @ specs];

(**************************************************************************************************
`UnorderedAssociationThread[{key_i}_i, {val_i}_i]` gives the unordered association `<|key_i -> val_i|>_i`.
`RuleThread[{key_i}_i, {val_i}_i]` gives the rule list `{key_i -> val_i}_i`.
?*)

AssociationThreadOp::usage =
"AssociationThreadOp[{k$1, k$2, $$}, {v$1, v$2, $$}] returns <|k$1 -> v$2, k$2 -> v$2, $$|>.
AssociationThreadOp[keys$] is the operator form of AssociationThreadOp."

UnorderedAssociationThread::usage =
"UnorderedAssociationThread[{k$1, k$2, $$}, {v$1, v$2, $$}] returns UnorderedAssociation[k$1 -> v$2, k$2 -> v$2, $$].
UnorderedAssociationThread[keys$] is the operator form of UnorderedAssociationThread."

RuleThread::usage =
"RuleThread[{k$1, k$2, $$}, {v$1, v$2, $$}] returns {k$1 -> v$2, k$2 -> v$2, $$}.
RuleThread[keys$] is the operator form of RuleThread."

RuleUnthread::usage =
"RuleUnthread[{k$1 -> v$2, k$2 -> v$2, $$}] returns {k$1, k$2, $$} -> {v$1, v$2, $$}."

SetCurry1[AssociationThreadOp, UnorderedAssociationThread, RuleThread];

(* unlike AssociationThread this fn can curry *)
AssociationThreadOp[keys_, vals_] := AssociationThread[keys, vals];

UnorderedAssociationThread[keys_List, vals_List]  :=
  UDict @ Thread[keys -> vals];

RuleThread[keys_List, values_List] /; Len[keys] === Len[values] :=
  MapThread[Rule, {keys, values}];

RuleUnthread[rules_List] := Thread[rules, Rule];

(**************************************************************************************************
`RangeAssociation[{key_i}_i]` gives the association `<|i -> val_i|>_i`.
`AssociationRange[{val_i}_i]` gives the association `<|key_i -> i|>_i`.
?*)

RangeAssociation[vals_List] := AssociationThread[LengthRange @ vals, vals];
AssociationRange[keys_List] := AssociationThread[keys, LengthRange @ keys];

RangeUnorderedAssociation[vals_List] := UnorderedAssociationThread[LengthRange @ vals, vals];
UnorderedAssociationRange[keys_List] := UnorderedAssociationThread[keys, LengthRange @ keys];

(**************************************************************************************************
`RangeRules[{key_i}_i]` gives the rule list `{key_i -> i}_i`.
`RulesRange[{val_i}_i]` gives the rule list `{i -> val_i}_i`.
?*)

RangeRules[vals_] := RuleThread[LengthRange @ vals, vals];
RulesRange[keys_] := RuleThread[keys, LengthRange @ keys];

(**************************************************************************************************
`ConstantRules[{key_i}_i, c]` gives the constant rules `{key_i -> c}_i`.
`ConstantAssociation[{key_i}_i, c]` gives the constant association `<|key_i -> c|>_i`.
`ConstantUnorderedAssociation[{key_i}_i, c]` gives the unordered constant association `<|key_i -> c|>_i`.
?*)

SetCurry1[ConstantRules, ConstantAssociation, ConstantUnorderedAssociation];

ConstantRules[keys_List, constant_List] := Map[key |-> Rule[key, constant], keys];
ConstantRules[keys_List, constant_] := Thread @ Rule[keys, constant];
ConstantAssociation[keys_List, constant_] := DictThread[keys, ConstantArray[constant, Len @ keys]];
ConstantUnorderedAssociation[keys_List, constant_] := UDictThread[keys, ConstantArray[constant, Len @ keys]];
ConstantTrueAssociation[keys_List]  := ConstantUnorderedAssociation[keys, True];
ConstantFalseAssociation[keys_List] := ConstantUnorderedAssociation[keys, False];

(*************************************************************************************************)

TrueRules = CaseOf[
  {}        := {};
  {a_}      := List[a -> True];
  list_List := Thread @ Rule[list, True];
];

(**************************************************************************************************
`AssociationMapThread[fn, <|key_i -> val_i|>_i]` gives the association `<|.., val_i -> i, ..|>`.
?*)

AssociationMapApply::usage = "AssociationMapApply[f$, <|k$1 -> v$1, k$2 -> v$2, $$|>] returns <|f$[k$1, v$1], f$[k$2, v$2], $$|>."
UnorderedAssociationMapApply::usage = "UnorderedAssociationMapApply[f$, <|k$1 -> v$1, k$2 -> v$2, $$|>] returns UnorderedAssociation[f$[k$1, v$1], f$[k$2, v$2], $$]."

SetCurry1[AssociationMapApply, UnorderedAssociationMapApply]

AssociationMapApply[fn_, dict_Dict]          :=  Dict @ MapApply[fn, Normal @ dict];
UnorderedAssociationMapApply[fn_, dict_Dict] := UDict @ MapApply[fn, Normal @ dict];

(*************************************************************************************************)

AssociationMapThread::usage = "AssociationMapThread[f$, <|k$1 -> {a$1, a$2, $$}, k$2 -> {b$1, b$2, $$}, $$|>] returns {f$[a$1, b$1, $$], f$[a$2, b$2, $$], $$}."
UnorderedAssociationMapThread::usage = "UnorderedAssociationMapThread[f$, <|k$1 -> {a$1, a$2, $$}, k$2 -> {b$1, b$2, $$}, $$|>] returns {f$[a$1, b$1, $$], f$[a$2, b$2, $$], $$}."

SetCurry1[AssociationMapThread, UnorderedAssociationMapThread]

AssociationMapThread[fn_, dict_Dict]          := With[{keys = Keys @ dict}, Map[val |-> fn[ DictThread[keys, val]], Transpose @ Values @ dict]];
UnorderedAssociationMapThread[fn_, dict_Dict] := With[{keys = Keys @ dict}, Map[val |-> fn[UDictThread[keys, val]], Transpose @ Values @ dict]];

(*************************************************************************************************)

UnorderedAssociationMap::usage =
"UnorderedAssociationMap[f$, {k$1, k$2, $$}] returns UnorderedAssociation[k$1 -> f$[k$1], k$2 -> f$[k$2], $$].
UnorderedAssociationMap[f$, <|k$1 -> v$1, k$2 -> v$2, $$|>] returns UnorderedAssociation[f$[k$1 -> v$1], f$[k$2 -> v$2]]."

DecFullDispatch2 @ SetCurry1[UnorderedAssociationMap]

UnorderedAssociationMap[fn_, list_List]  := UDict @ Map[z |-> Rule[z, fn[z]], list];
UnorderedAssociationMap[fn_, dict:DictP] := UDict @ Map[fn, Normal @ assoc];
UnorderedAssociationMap[_, expr_] := RuleCondition[Message[AssociationMap::invrp, expr]; Fail];

(*************************************************************************************************)

AssociationToRules::usage = "AssociationToRules[<|k$1 -> v$1, k$2 -> v$2, $$|>]  returns {k$1 -> v$1, k$2 -> v$2, $$}."
RulesToAssociation::usage = "AssociationToRules[{k$1 -> v$1, k$2 -> v$2, $$}] returns <|k$1 -> v$1, k$2 -> v$2, $$|>."
AssociationToPairs::usage = "AssociationToPairs[<|k$1 -> v$1, k$2 -> v$2, $$|>] returns {{k$1, v$1}, {k$2, v$2}, $$}."
PairsToAssociation::usage = "AssociationToRules[{{k$1, v$1}, {k$2, v$2}, $$}] returns <|k$1 -> v$1, k$2 -> v$2, $$|>."
RulesToUnorderedAssociation::usage = "RulesToUnorderedAssociation[{k$1 -> v$1, k$2 -> v$2, $$}] returns UnorderedAssociation[k$1 -> v$1, k$2 -> v$2, $$]."
PairsToUnorderedAssociation::usage = "PairsToUnorderedAssociation[{{k$1, v$1}, {k$2, v$2}, $$}] returns UnorderedAssociation[k$1 -> v$1, k$2 -> v$2, $$]."

SetStrict[AssociationToRules, AssociationToPairs, PairsToAssociation, RulesToAssociation, PairsToUnorderedAssociation, RulesToUnorderedAssociation]

AssociationToRules[dict_Dict] := Normal @ dict;
AssociationToPairs[dict_Dict] := Transpose @ {Keys @ dict, Values @ dict};
AssociationToRules::badArguments = AssociationToPairs::badArguments = "First argument was not an association: ``.";

         PairsToAssociation[list_ ? PairVectorQ] :=          AssociationThread @@ Transpose @ list;
PairsToUnorderedAssociation[list_ ? PairVectorQ] := UnorderedAssociationThread @@ Transpose @ list;
PairsToAssociation::badArguments = PairsToUnorderedAssociation::badArguments = "First argument was not a list of pairs: ``.";

         RulesToAssociation[list_List ? RuleLikeVectorQ] :=          Association @ list;
RulesToUnorderedAssociation[list_List ? RuleLikeVectorQ] := UnorderedAssociation @ list;
RulesToAssociation::badArguments = RulesToUnorderedAssociation::badArguments = "First argument was not a list of pairs: ``.";

(**************************************************************************************************)

SetStrict @ GroupPairs;

GroupPairs::usage =
"GroupPairs[{{key$i, val$i}}_i]` yields `<|key_i -> {val_{i1}, val_{i2}, ..}|>_i` where the val_{ij} are grouped by matching `key_i`.
GroupPairs[pairs$, f$] aggregates the resulting lists of values using f$.
GroupPairs[pairs$]` effectively gives `GroupBy[expr$, First -> Last]`.
The transposed version of this is GroupAgainst."

GroupPairs[list_ ? PairVectorQ]     := GroupBy[list, First -> Last];
GroupPairs[list_ ? PairVectorQ, f_] := GroupBy[list, First -> Last, f];

(**************************************************************************************************)

SetStrict @ GroupAgainst;

GroupAgainst::usage =
"GroupAgainst[expr$, against$] returns an association whose keys are unique values of against$, and \
whose values are the corresponding lists of parts of expr$.
GroupAgainst[expr$, against$, f$] aggregates the resulting lists of values using f$.
This is the transposed version of GroupPairs."

GroupAgainst[expr_, against_, fn_:Id] /; SameLenQ[expr, against] :=
  Map[indices |-> fn[Part[expr, indices]], PositionIndex @ against];

(**************************************************************************************************)

SetCurry1 @ MergeAssocations;

MergeAssocations::usage = "MergeAssocations[f$, {assoc$1, assoc$2}] returns {$f[k$, {v$1, v$2, $$}], $$}."

MergeAssocations[fn_, assocs_] := KeyValueMap[fn, Merge[assocs, Id]];

(*************************************************************************************************)

AssociationSum::usage = "AssociationSum[assocs$] returns <|k$ -> Total[{v$1, v$2, $$}], $$|>."
UnorderedAssociationSum::usage = "UnorderedAssociationSum[assocs$] returns UnorderedAssociation[k$ -> Total[{v$1, v$2, $$}], $$]."

AssociationSum = CaseOf[
  {}       := EmptyDict;
  {e_Dict} := Dict @ e;
  e_List   := Merge[e, Total];
  r_Dict   := Dict[r];
  r_Rule   := Dict[r];
];

UnorderedAssociationSum = CaseOf[
  {}       := EmptyUDict;
  {e_Dict} := UDict @ e;
  e_List   := UDict @ Merge[e, Total];
  r_Dict   := UDict[r];
  r_Rule   := UDict[r];
];

(*************************************************************************************************)

AssociationPlus::usage =
"AssociationPlus[assoc$1, assoc$2, $$] returns <|k$ -> Total[{v$1, v$2, $$}], $$|>.
Any of the assoc$i can be rules or lists of rules."

UnorderedAssociationPlus::usage = "
UnorderedAssociationPlus[assoc$1, assoc$2, $$] returns UnorderedAssociation[k$ -> Total[{v$1, v$2, $$}], $$].
Any of the assoc$i can be rules or lists of rules."

AssociationPlus[]               := EmptyDict;
AssociationPlus[as___]          := Merge[{as}, Total];

UnorderedAssociationPlus[]      := EmptyUDict;
UnorderedAssociationPlus[as___] := UDict @ Merge[{as}, Total];

(*************************************************************************************************)

UnorderedCounts::usage = "UnorderedCounts[list$] returns UnorderedAssociation[e$1 -> n$1, $$]."

UnorderedCounts[e_] := UDict @ Counts @ e;

(*************************************************************************************************)

LevelIndex::usage =
"LevelIndex[expr$, n$] returns an association mapping expressions occuring at level n$ in expr$ and the position i$ for in which it occurred (in Part[expr$, i$]).
LevelIndex[expr$, 1] is equivalent to PositionIndex."

DecFullDispatch12 @ LevelIndex;

(* this is like PositionIndex, but at arbitrary levels *)
LevelIndex[expr_, 1] := PositionIndex @ expr;
LevelIndex[expr_, level:PosIntP] := Module[
  {index = Bag[]},
  ScanP[
    {subExpr, part} |-> Scan[
      subSubExpr |-> StuffBag[index, subSubExpr -> part],
      subExpr,
      {level - 1}
    ],
    expr
  ];
  Merge[BagPart[index, All], Id]
];

(**************************************************************************************************)

SetHoldA @ CachedTo;

CachedTo[sym_, key2_, value_, test_:NotFailureQ] := Module[{key = key2, res},
  Lookup[sym, Key @ key, If[test[res = value], sym[key] = res, $Failed, $Failed]]
];

(**************************************************************************************************)

SetHoldF @ SetCurry1[KeyApplyTo, KeyIncrement, KeyDecrement];

KeyApplyTo[lhs_, key_, fn_, mfn_:KeyAbsentFn] := Set[lhs[key], fn @ Lookup[lhs, key, mfn[key]]];
KeyIncrement[lhs_, key_] := Set[lhs[key], Lookup[lhs, key, 0] + 1];
KeyDecrement[lhs_, key_] := Set[lhs[key], Lookup[lhs, key, 0] - 1];

(**************************************************************************************************)

InternTo::usage =
"InternTo[sym$, key$] returns the ID for key$, or creates and adds a new ID and returns that.
IDs are consecutive integers starting at 1.
InternTo[sym$] is the operator form of InternTo."

SetHoldF @ SetCurry1 @ InternTo;

InternTo[lhs_, key_] := Lookup[lhs, key, addLenKey[lhs, key]];

SetHoldF @ addLenKey;
addLenKey[lhs_, Auto] := With[{n = Len[lhs] + 1}, lhs[n] = n];
addLenKey[lhs_, key_] := lhs[key] = Len[lhs] + 1;

(**************************************************************************************************)

SetHoldF @ SetCurry13 @ InternUniqueTo;

InternUniqueTo::usage =
"InternUniqueTo[sym$, key$, fn$] creates and adds a new ID for key$ and returns it.
If key$ has already been interned, fn$[key$, id$] is evaluated.
InternUniqueTo[sym$, fn$] is the operator form of InternUniqueTo."

InternUniqueTo[lhs_, key_, errorFn_] /; KeyExistsQ[lhs, key] := errorFn[key, lhs[key]];
InternUniqueTo[lhs_, key_, _]                                := addLenKey[lhs, key];

(**************************************************************************************************)

BindTo::usage =
"BindTo[sym$, entries$] updates an association sym$ with new entries.
entries$ can be a single rule, a list of rules, or an association.
BindTo is effectively the same as AssociateTo.
BindTo[sym$] is the operator form of BindTo."

SetStrict @ SetHoldF @ BindTo;

BindTo[lhs_, rule:RuleLP] := AssociateTo[lhs, rule];
BindTo[lhs_, dict_Dict]   := AssociateTo[lhs, dict];
BindTo[lhs_, rules_List]  := AssociateTo[lhs, rules];

(**************************************************************************************************)

Bind::usage =
"Bind[assoc$, entries$] appends new entries to an association.
Bind is like a non-mutating form of BindTo."

UBind::usage =
"UBind[assoc$, entries$] appends new entries to an unordered association.
UBind is like a non-mutating form of BindTo."

SetStrict[Bind, UBind];

Bind[dict_Dict, rule:RuleLP]  := Append[dict, rule];
Bind[dict_Dict, data:DictLP]  := Append[dict, data];

UBind[dict_Dict, rule:RuleLP] := Append[dict, rule];
UBind[dict_Dict, data:DictLP] := UDict[dict, data];

(**************************************************************************************************)

        DeclaredHere[KeyAddTo, KeySubFrom, KeyTimesBy, KeyDivideBy, KeyUnionTo, KeyJoinTo, KeyAppendTo, KeyPrependTo]
SetCurry1 @ SetHoldF[KeyAddTo, KeySubFrom, KeyTimesBy, KeyDivideBy, KeyUnionTo, KeyJoinTo, KeyAppendTo, KeyPrependTo]

DefineKeywiseOperator1[sym_, def_, fn_] := SetDelayed[sym[lhs_, key_, arg_], Set[lhs[key], fn[Lookup[lhs, Key @ key, def], arg]]];
DefineKeywiseOperator2[sym_, def_, fn_] := SetDelayed[sym[lhs_, key_, arg_], Set[lhs[key], fn[arg, Lookup[lhs, Key @ key, def]]]];

DefineKeywiseOperator1[KeyAddTo,     0,  Plus]
DefineKeywiseOperator1[KeySubFrom,   0,  Subtract]
DefineKeywiseOperator1[KeyTimesBy,   1,  Times]
DefineKeywiseOperator1[KeyDivideBy,  1,  Divide]
DefineKeywiseOperator1[KeyUnionTo,   {}, Union]
DefineKeywiseOperator1[KeyJoinTo,    {}, Join]
DefineKeywiseOperator1[KeyAppendTo,  {}, Append]
DefineKeywiseOperator1[KeyPrependTo, {}, Prepend]

(**************************************************************************************************)

DeclaredHere[KeyBindTo, KeyUBindTo]
    SetHoldF[KeyBindTo, KeyUBindTo]

DefineKeywiseOperator1[KeyBindTo,  Dict[],   Bind]
DefineKeywiseOperator1[KeyUBindTo, UDict[], UBind]
