SystemExports[
  "Function",
    ListAssociationParts,
    InvertAssociation, InvertUnorderedAssociation, ReverseRules, MapRules, KeyMapValueMap, KeysValues, ToValues,
    RuleThread, RuleUnthread, UnorderedAssociationThread,
    RangeRules, RangeAssociation, RangeUnorderedAssociation,
    RulesRange, AssociationRange, UnorderedAssociationRange,
    ConstantRules, ConstantAssociation, ConstantUnorderedAssociation, ConstantTrueAssociation,
    UnorderedAssociationMapApply, UnorderedAssociationMapThread, UnorderedAssociationMap,
    AssociationMapThread, AssociationMapApply,
    PairsToAssociation, PairsToUnorderedAssociation, AssociationToPairs,
    RulesToAssociation, RulesToUnorderedAssociation, AssociationToRules,
    GroupPairs, GroupAgainst, CombineBy, CombineAgainst, MergeAssocations,
    LevelIndex, PadAssociation
];

PackageExports[
  "MutatingFunction",
    CachedTo,
    KeyApplyTo, KeyIncrement, KeyDecrement, KeyIndex, KeyIndexUnique,
    KeyAddTo, KeySubtractFrom, KeyTimesBy, KeyDivideBy,
    KeyUnionTo, KeyJoinTo, KeyAppendTo, KeyPrependTo, KeyAssociateTo,
  "MetaFunction",
    DefineKeywiseOperator1, DefineKeywiseOperator2,
  "Function",
    AssocThread, EnsureOAssoc
];

(**************************************************************************************************)

(* because some functions like Extract don't work on UnorderedAssociation *)
EnsureOAssoc[e_Assoc ? HAssocQ] := Assoc @ e;
EnsureOAssoc[e_] := e;

(**************************************************************************************************)

DeclareStrict[ListAssociationParts]

ListAssociationParts[dict_Dict] := Keys[dict, key];
ListAssociationParts[list_List] := RangeLen @ list;

(**************************************************************************************************)

PadAssociation[assoc_, keys_, val_] := Join[
  assoc,
  ConstAssoc[Complement[keys, Keys @ assoc], val]
];

(**************************************************************************************************)

DeclareStrict[ReverseRules, InvertAssociation, InvertUnorderedAssociation];

ReverseRules[dict_Dict]                := Dict @ Map[Reverse, Normal @ dict];
ReverseRules[rules_ ? RuleLikeVectorQ] := Map[Reverse, rules];

InvertAssociation::notUnique = "Cannot uniquely invert association ``.";
InvertAssociation[assoc_ ? AssociationQ] := Module[
  {res = ReverseRules @ assoc},
  If[Len[res] == Len[assoc], res,
    ErrorMsg[InvertAssociation::notUnique, assoc]]];

InvertUnorderedAssociation::notUnique = "Cannot uniquely invert association ``.";
InvertUnorderedAssociation[assoc_ ? AssociationQ] := Module[
  {res = UDict @ Reverse[Normal @ assoc, 2]},
  If[Len[res] == Len[assoc], res,
    ErrorMsg[InvertAssociation::notUnique, assoc]]];

(**************************************************************************************************)

DeclareCurry1[MapRules]

MapRules[fn_, assoc_Assoc] := KeyValueMap[fn, assoc];
MapRules[fn_, rules:RuleVecP] := MapApply[fn, rules];

(**************************************************************************************************)

DeclareCurry12[KeyMapValueMap]

KeyMapValueMap[kfn_, vfn_, assoc_Assoc] := KeyMap[kfn, Map[vfn, assoc]];
KeyMapValueMap[kfn_, vfn_, rules:RuleVecP] := MapCol1[kfn, MapCol2[vfn, rules]];

(**************************************************************************************************)

DeclareStrict[KeysValues]

KeysValues[list_List ? RuleLikeVectorQ] := {Keys @ assoc, Values @ assoc};
KeysValues[assoc_Assoc]                 := {Keys @ assoc, Values @ assoc};

(**************************************************************************************************)

DeclareStrict[ToValues]

ToValues[list_List]   := list;
ToValues[assoc_Assoc] := Values @ assoc;

(**************************************************************************************************
`UnorderedAssociationThread[{key_i}_i, {val_i}_i]` gives the unordered association `<|key_i -> val_i|>_i`.
`RuleThread[{key_i}_i, {val_i}_i]` gives the rule list `{key_i -> val_i}_i`.
?*)

DeclareCurry1[AssocThread, UnorderedAssociationThread, RuleThread];

(* unlike AssociationThread this fn can curry *)
AssocThread[keys_, vals_] := AssociationThread[keys, vals];

UnorderedAssociationThread[keys_List, vals_List]  :=
  UnorderedAssociation @ Thread[keys -> vals];

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

DeclareCurry1[ConstantRules, ConstantAssociation, ConstantUnorderedAssociation];

ConstantRules[keys_List, constant_List] := Map[key |-> Rule[key, constant], keys];
ConstantRules[keys_List, constant_] := Thread @ Rule[keys, constant];
ConstantAssociation[keys_List, constant_] := AssocThread[keys, ConstantArray[constant, Len @ keys]];
ConstantUnorderedAssociation[keys_List, constant_] := UAssociation @ ConstantAssociation[keys, constant];
ConstantTrueAssociation[keys_List] := ConstantUnorderedAssociation[keys, True];

(**************************************************************************************************
`AssociationMapThread[fn, <|key_i -> val_i|>_i]` gives the association `<|.., val_i -> i, ..|>`.
?*)

DeclareCurry1[AssociationMapApply, AssociationMapThread]
DeclareCurry1[UnorderedAssociationMapApply, UnorderedAssociationMapThread]

AssociationMapApply[fn_, assoc_Assoc]          :=  Assoc @ Map[fn, Normal @ assoc];
UnorderedAssociationMapApply[fn_, assoc_Assoc] := UAssoc @ Map[fn, Normal @ assoc];

AssociationMapThread[fn_, assoc_Assoc]          := With[{keys = Keys @ assoc}, Map[val |-> fn[ AssocThread[keys, val]], Transpose @ Values @ assoc]];
UnorderedAssociationMapThread[fn_, assoc_Assoc] := With[{keys = Keys @ assoc}, Map[val |-> fn[UAssocThread[keys, val]], Transpose @ Values @ assoc]];

(*************************************************************************************************)

DeclareCurry1[UnorderedAssociationMap]

UnorderedAssociationMap[fn_, expr_List] := UAssoc @ Map[z |-> Rule[z, fn[z]], expr];
UnorderedAssociationMap[fn_, assoc_Assoc ? AssociationQ] := UAssoc @ Map[fn, Normal @ assoc];
UnorderedAssociationMap[_, expr_] := RuleCondition[Message[AssociationMap::invrp, expr]; Fail];

(*************************************************************************************************)

DeclareStrict[AssociationToRules, AssociationToPairs, PairsToAssociation, RulesToAssociation, PairsToUnorderedAssociation, RulesToUnorderedAssociation]

AssociationToRules[assoc_Assoc] := Normal @ assoc;
AssociationToPairs[assoc_Assoc] := Transpose @ {Keys @ assoc, Values @ assoc};
AssociationToRules::badArguments = AssociationToPairs::badArguments = "First argument was not an association: ``.";

         PairsToAssociation[list_ ? PairVectorQ] :=          AssociationThread @@ Transpose @ list;
PairsToUnorderedAssociation[list_ ? PairVectorQ] := UnorderedAssociationThread @@ Transpose @ list;
PairsToAssociation::badArguments = PairsToUnorderedAssociation::badArguments = "First argument was not a list of pairs: ``.";

         RulesToAssociation[list_List ? RuleVectorQ] := Association @ list;
RulesToUnorderedAssociation[list_List ? RuleVectorQ] := UnorderedAssociation @ list;
RulesToAssociation::badArguments = RulesToUnorderedAssociation::badArguments = "First argument was not a list of pairs: ``.";

(**************************************************************************************************)

DeclareStrict[GroupPairs]

GroupPairs::usage = "
GroupPairs[{{key$i, val$i}}_i]` yields `<|key_i -> {val_{i1}, val_{i2}, ..}|>_i` where the val_{ij} are grouped by matching `key_i`.
GroupPairs[pairs$, f$] aggregates the resulting lists of values using f$.
GroupPairs[pairs$]` effectively gives `GroupBy[expr$, First -> Last]`.
The transposed version of this is GroupAgainst."

GroupPairs[list_ ? PairVectorQ]     := GroupBy[list, First -> Last];
GroupPairs[list_ ? PairVectorQ, f_] := GroupBy[list, First -> Last, f];

(**************************************************************************************************)

DeclareStrict[GroupAgainst]

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

DeclareCurry1[MergeAssocations]

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

DeclareHoldAll[CachedTo]

CachedTo[sym_, key2_, value_, test_:NotFailureQ] := Module[{key = key2, res},
  Lookup[sym, Key @ key, If[test[res = value], sym[key] = res, $Failed, $Failed]]
];

(**************************************************************************************************)

DeclareHoldFirst @ DeclareCurry1[KeyApplyTo, KeyIncrement, KeyDecrement];

KeyApplyTo[lhs_, key_, fn_, mfn_:KeyAbsentFn] := Set[lhs[key], fn @ Lookup[lhs, key, mfn[key]]];
KeyIncrement[lhs_, key_] := Set[lhs[key], Lookup[lhs, key, 0] + 1];
KeyDecrement[lhs_, key_] := Set[lhs[key], Lookup[lhs, key, 0] - 1];

(**************************************************************************************************)

DeclareHoldFirst @ DeclareCurry1 @ KeyIndex;
DeclareHoldFirst @ DeclareCurry13 @ KeyIndexUnique;

KeyIndex[lhs_, key_] := Lookup[lhs, key, addLenKey[lhs, key]];

KeyIndexUnique[lhs_, key_, errorFn_] /; KeyExistsQ[lhs, key] := errorFn[key, lhs[key]];
KeyIndexUnique[lhs_, key_, _] := addLenKey[lhs, key];

DeclareHoldFirst @ addLenKey;
addLenKey[lhs_, Auto] := With[{n = Len[lhs] + 1}, lhs[n] = n];
addLenKey[lhs_, key_] := lhs[key] = Len[lhs] + 1;

(**************************************************************************************************)

DeclareHoldFirst[KeyAddTo, KeySubtractFrom, KeyTimesBy, KeyDivideBy, KeyUnionTo, KeyJoinTo, KeyAppendTo, KeyPrependTo, KeyAssociateTo];
   DeclareCurry1[KeyAddTo, KeySubtractFrom, KeyTimesBy, KeyDivideBy, KeyUnionTo, KeyAppendTo, KeyPrependTo, KeyJoinTo];

DefineKeywiseOperator1[sym_, {def_, fn_}] := SetDelayed[sym[lhs_, key_, arg_], Set[lhs[key], fn[Lookup[lhs, key, def], arg]]];
DefineKeywiseOperator2[sym_, {def_, fn_}] := SetDelayed[sym[lhs_, key_, arg_], Set[lhs[key], fn[arg, Lookup[lhs, key, def]]]];

DefineKeywiseOperator1[KeyAddTo,        {0, Plus}]
DefineKeywiseOperator1[KeySubtractFrom, {0, Subtract}]
DefineKeywiseOperator1[KeyTimesBy,      {1, Times}]
DefineKeywiseOperator1[KeyDivideBy,     {1, Divide}]
DefineKeywiseOperator1[KeyUnionTo,      {{}, Union}]
DefineKeywiseOperator1[KeyJoinTo,       {{}, Join}]
DefineKeywiseOperator1[KeyAppendTo,     {{}, Append}]
DefineKeywiseOperator1[KeyPrependTo,    {{}, Prepend}]
DefineKeywiseOperator1[KeyAssociateTo,  {Assoc[], Assoc}];