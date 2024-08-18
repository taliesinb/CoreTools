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
    UnorderedAssociationMapApply, UnorderedAssociationMapThread, UnorderedAssociationMap,
    AssociationMapThread, AssociationMapApply,
    PairsToAssociation, PairsToUnorderedAssociation, AssociationToPairs,
    RulesToAssociation, RulesToUnorderedAssociation, AssociationToRules,
    GroupPairs, GroupAgainst, CombineBy, CombineAgainst, MergeAssocations,
    LevelIndex, PadAssociation,
    AssociationThreadOp
];

PackageExports[
  "Function",
    Bind, UBind,
  "MutatingFunction",
    CachedTo,
    BindTo,
    KeyApplyTo, KeyIncrement, KeyDecrement, KeyIndex, KeyIndexUnique,
    KeyAddTo,   KeySubFrom,   KeyTimesBy,   KeyDivideBy,
    KeyUnionTo, KeyJoinTo,    KeyAppendTo,  KeyPrependTo, KeyBindTo, KeyUBindTo,
  "MetaFunction",
    DefineKeywiseOperator1, DefineKeywiseOperator2,
  "Function",
    EnsureODict
];

(**************************************************************************************************)

(* because some functions like Extract don't work on UnorderedAssociation *)
EnsureODict = CaseOf[
  e:DictP := Dict @ e;
  e_      := e
];

(**************************************************************************************************)

SetStrict @ ListAssociationParts;

ListAssociationParts[dict_Dict] := Keys[dict, key];
ListAssociationParts[list_List] := RangeLen @ list;

(**************************************************************************************************)

PadAssociation[dict_Dict, keys_, val_] :=
  Join[dict, ConstDict[Complement[keys, Keys @ dict], val]];

(**************************************************************************************************)

SetStrict[ReverseRules, InvertAssociation, InvertUnorderedAssociation];

ReverseRules[dict_Dict]                := Dict @ Map[Reverse, Normal @ dict];
ReverseRules[rules_ ? RuleLVecQ] := Map[Reverse, rules];

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

SetCurry1 @ MapRules;

MapRules[fn_, dict_Dict]      := KeyValueMap[fn, dict];
MapRules[fn_, list:RuleLVecP] := MapApply[fn, rules];
MapRules[_, _]                := InternalError;

(**************************************************************************************************)

SetCurry12 @ KeyMapValueMap;

KeyMapValueMap[kfn_, vfn_, dict_Dict] := KeyMap[kfn,  Map[vfn,     dict]];
KeyMapValueMap[kfn_, vfn_, ruls_List] := MapCol1[kfn, MapCol2[vfn, ruls]];
KeyMapValueMap[_, _, _] := InternalError;

(**************************************************************************************************)

SetCurry12 @ ValueMap;

ValueMap[vfn_, dict_Dict] := Map[vfn, dict];
ValueMap[vfn_, ruls_List] := MapCol2[vfn, dict];
ValueMap[_, _, _]         := InternalError;

(**************************************************************************************************)

SetStrict @ KeysValues;

KeysValues[data:DictLikeP] := {Keys @ data, Values @ data};
KeysValues[_]              := InternalError;

(**************************************************************************************************)

SetStrict @ ToValues;

ToValues[list_List] := list;
ToValues[dict_Dict] := Values @ dict;

(**************************************************************************************************)

SetStrict @ ToRuleList;

ToRuleList[dict_Dict]     := Normal @ dict;
ToRuleList[list:RuleVecP] := list;
ToRuleList[_]             := InternalError;

ToRuleList[]              := {};
ToRuleList[specs__]       := Join @ Map[ToRuleList, NoEval @ specs];

(**************************************************************************************************
`UnorderedAssociationThread[{key_i}_i, {val_i}_i]` gives the unordered association `<|key_i -> val_i|>_i`.
`RuleThread[{key_i}_i, {val_i}_i]` gives the rule list `{key_i -> val_i}_i`.
?*)

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

(**************************************************************************************************
`AssociationMapThread[fn, <|key_i -> val_i|>_i]` gives the association `<|.., val_i -> i, ..|>`.
?*)

SetCurry1[AssociationMapApply, AssociationMapThread]
SetCurry1[UnorderedAssociationMapApply, UnorderedAssociationMapThread]

AssociationMapApply[fn_, dict_Dict]          :=  Dict @ Map[fn, Normal @ dict];
UnorderedAssociationMapApply[fn_, dict_Dict] := UDict @ Map[fn, Normal @ dict];

AssociationMapThread[fn_, dict_Dict]          := With[{keys = Keys @ dict}, Map[val |-> fn[ DictThread[keys, val]], Transpose @ Values @ dict]];
UnorderedAssociationMapThread[fn_, dict_Dict] := With[{keys = Keys @ dict}, Map[val |-> fn[UDictThread[keys, val]], Transpose @ Values @ dict]];

(*************************************************************************************************)

SetCurry1[UnorderedAssociationMap]

UnorderedAssociationMap[fn_, list_List]  := UDict @ Map[z |-> Rule[z, fn[z]], list];
UnorderedAssociationMap[fn_, dict:DictP] := UDict @ Map[fn, Normal @ assoc];
UnorderedAssociationMap[_, expr_] := RuleCondition[Message[AssociationMap::invrp, expr]; Fail];

(*************************************************************************************************)

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

GroupPairs::usage = "
GroupPairs[{{key$i, val$i}}_i]` yields `<|key_i -> {val_{i1}, val_{i2}, ..}|>_i` where the val_{ij} are grouped by matching `key_i`.
GroupPairs[pairs$, f$] aggregates the resulting lists of values using f$.
GroupPairs[pairs$]` effectively gives `GroupBy[expr$, First -> Last]`.
The transposed version of this is GroupAgainst."

GroupPairs[list_ ? PairVectorQ]     := GroupBy[list, First -> Last];
GroupPairs[list_ ? PairVectorQ, f_] := GroupBy[list, First -> Last, f];

(**************************************************************************************************)

SetStrict @ GroupAgainst;

GroupAgainst::usage = "
GroupAgainst[expr$, against$] returns an association whose keys are unique values of against$, and \
whose values are the corresponding lists of parts of expr$.
GroupAgainst[expr$, against$, f$] aggregates the resulting lists of values using f$.
"

GroupAgainst[expr_, against_, fn_:Id] /; SameLenQ[expr, against] :=
  Map[indices |-> fn[Part[expr, indices]], PositionIndex @ against];

(**************************************************************************************************
`MergeAssocations[XXX]` yields `XXX`.
?*)

SetCurry1[MergeAssocations]

MergeAssocations[fn_, assocs_] := KeyValueMap[fn, Merge[assocs, Id]];

(*************************************************************************************************)

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

SetHoldF @ SetCurry1 @ KeyIndex;
SetHoldF @ SetCurry13 @ KeyIndexUnique;

KeyIndex[lhs_, key_] := Lookup[lhs, key, addLenKey[lhs, key]];

KeyIndexUnique[lhs_, key_, errorFn_] /; KeyExistsQ[lhs, key] := errorFn[key, lhs[key]];
KeyIndexUnique[lhs_, key_, _] := addLenKey[lhs, key];

SetHoldF @ addLenKey;
addLenKey[lhs_, Auto] := With[{n = Len[lhs] + 1}, lhs[n] = n];
addLenKey[lhs_, key_] := lhs[key] = Len[lhs] + 1;

(**************************************************************************************************)

SetStrict @ SetHoldF @ BindTo;

BindTo[lhs_, rule:RuleLP] := AssociateTo[lhs, rule];
BindTo[lhs_, dict_Dict]   := AssociateTo[lhs, dict];
BindTo[lhs_, rules_List]  := AssociateTo[lhs, rules];

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
