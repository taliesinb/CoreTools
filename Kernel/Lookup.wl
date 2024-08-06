SystemExports[
  "Function",
    LookupKeys, ChainedLookup,
    OptionRules, HoldOptionRules,
    LookupOptions, HoldLookupOptions,
    MissingApply,
    OptionKeys, OptionValueRules, OptionValueList,
    GraphOptions,
  "Head",
    ChainedRules, OptionKey,
  "Predicate",
    OptionKeyQ
];

PackageExports[
  "Operator",
    DefaultOptionValueFn,
  "Function",
    JoinOptions,
    LookupOption, KeyAbsentFn,
    TakeOptions, NarrowOptions
];

(*************************************************************************************************)

DeclareStrict[OptionKeys, OptionKeyQ]

OptionKeys[sym_Sym] := Keys @ Options @ sym;
OptionKeyQ[sym_Sym, key_] := KeyExistsQ[Options @ sym, key];

(*************************************************************************************************)

DeclareStrict[OptionValueRules, OptionValueList]

(* TODO: head_Symbol -> inheritList *)
OptionValueRules[head_Symbol, rules___] :=
  DeleteDuplicatesBy[ToList[rules, Options @ head], First];

OptionValueList[head_Symbol, rules___] :=
  OptionValue[head, FlatList @ rules, OptionKeys @ head];

(*************************************************************************************************)

DeclareStrict @ JoinOptions;

JoinOptions[args___List] := DeleteDuplicatesBy[Join[args], First];

(*************************************************************************************************)

DeclareCurry2[TakeOptions];

TakeOptions[sym_Sym,   ref_Sym]  := TakeOptions[Options @ sym, ref];
TakeOptions[opts_List, ref_Sym]  := Seq @@ FilterRules[Flatten @ {opts}, Options @ ref];

NarrowOptions /: sym_[l___, NarrowOptions[m__], r___] := sym[l, TakeOptions[{m}, sym], r];
NarrowOptions[] := Sequence[];

(*************************************************************************************************)

DeclareCurry1[MissingApply];

MissingApply[fn_, e_] := e;
MissingApply[fn_, Missing[_, k_]] := fn[k];
MissingApply[fn_, e_List ? PackedQ] := e;
MissingApply[fn_, e_List] := VectorReplace[e, Missing[_, k_] :> fn[k]];

(*************************************************************************************************)

DeclareStrict[LookupKeys]

LookupKeys[assoc:AssocLikeP, keys_List, fn_] := MissingApply[fn, Lookup[assoc, keys]];

(*************************************************************************************************)

KeyAbsentFn[key_] := Missing["KeyAbsent", key];

(*************************************************************************************************)

ChainedRules::usage =
"ChainedRules[obj$1, obj$2, $$] represents a series of associations or rule lists to be looked up in sequence.
ChainedRules[$$][key$] performs a lookup of key$.
ChainedRules[$$][{key$1, key$2, $$}] performs a lookup of all keys
* a MissingFn[fn$] is also a valid object.
* lookup is performed by ChainedLookup[{obj$1, obj$2, $$}, keys].
";

a_ChainedRules[key_] := ChainedLookup[a, key];
a_ChainedRules[key_List] := ChainedLookup[a, key];

(*************************************************************************************************)

ChainedLookup::usage =
"ChainedLookup[objs$, key$] looks up key in each obj$ until a value is found.
ChainedLookup[objs$, {key$1, key$2, $$}] looks up multiple keys.
ChainedLookup[$$, fn$] applies fn$[key$] to obtain values for keys not present in any object.
* each obj$ can be an association, list of rules, a ChainedRules object.
* values that are present but are Missing will also be chained.
* the final object can be a Function that will be applied to missing keys.
"

DeclareStrict[ChainedLookup]

ChainedLookup[cr_ChainedRules, args__] := chainLookup[List @@ cr, args];
ChainedLookup[objs_List, keys_] := chainLookup[objs, keys];
ChainedLookup[objs_List, keys_, fn_] := chainLookup[Append[objs, Fn[z, fn[z]]], keys];

chainLookup[objs_, keys_List]     := Apply[chainLookupN, objs][keys];
chainLookupN[obj_, rest__][keys_] := MissingApply[chainLookup1[rest], Lookup[obj, keys]];
chainLookupN[obj_Fn][keys_]       := Map[obj, keys];
chainLookupN[obj_][keys_]         := Lookup[obj, keys];

chainLookup[objs_, Key[key_] | key_] := Apply[chainLookup1, objs][key]
chainLookup1[obj_, rest__][key_]     := SubMissing[obj[key], chainLookup1[rest][key]];
chainLookup1[obj_Fn][key_]           := obj[key];
chainLookup1[obj_][key_]             := Lookup[obj, Key @ key];

(*************************************************************************************************)

OptionRules[_[___, r:RuleLikeP...]] := {r};
OptionRules[_] := $Failed;

DeclareHoldAllComplete[HoldOptionRules];
HoldOptionRules[_[___, r:RuleLikeP...]] := HoldComplete[r];
HoldOptionRules[_] := $Failed;

(*************************************************************************************************)

OptionKey::usage = "
OptionKey[key$] represents a key in an object with non-rule arguments.
OptionKey[key$, default$] represents a key with a held default value.
OptionKey[$$][obj$] will find the key in the obj$ if present and return its value.
* the lookup is performed by LookupOptions.
";

DeclareHoldRest[OptionKey];
OptionKey[key_][expr_]       := LookupOptions[expr, key];
OptionKey[key_, def_][expr_] := LookupOptions[expr, key, def&];

(*************************************************************************************************)

DefaultOptionValueFn[head_Symbol][key_] := OptionValue[head, key];

(*************************************************************************************************)

LookupOptions::usage = "
LookupOptions[expr$, key$] looks up an option in an expr$ with non-rule arguments.
LookupOptions[expr$, {key$1, key$2, $$}] looks up multiple keys.
LookupOptions[$$, fn$] uses fn$[key$] to obtain defaults for any keys not found.
* expr$ can also be an association.
* expr$ can also be a Graph, etc.
* individual keys can also be Key[$$] or OptionKey[$$] expressions."

LookupOptions[graph_Graph, key_, fn_:DefaultOptionValueFn[Graph]] :=
  LookupOptions[GraphOptions @ graph, key, fn];

LookupOptions[expr_, key_, fn_:KeyAbsentFn] := lookupOpts[expr, key, fn];

DeclareHoldFirst[HoldLookupOptions];

HoldLookupOptions[expr_, key_, fn_:KeyAbsentFn] := lookupOpts[expr, key, fn];

DeclareHoldAllComplete[lookupOpts, lookupOptKey];

(* TODO: extract the rule sequence part to save multiple traversals *)
lookupOpts[expr_, keys_List, fn_] := Map[key |-> lookupOptKey[expr, key, fn], keys];
lookupOpts[expr_, key_, fn_] := lookupOptKey[expr, key, fn];

lookupOptKey[expr_, OptionKey[key_, def_], _] := lookupOptKey[expr, key, def&];
lookupOptKey[expr_, (OptionKey|Key)[key_], fn_] := lookupOptKey[expr, key, fn];
lookupOptKey[assoc_ ? HoldAssociationQ, key_, fn_] := Lookup[assoc, key, fn[key]];
lookupOptKey[expr_, key_, fn_] := lookupExprKey1[expr, key, fn];

lookupExprKey1[expr_, key_, fn_] :=
  FirstCase[NoEval @ expr, (Rule|RuleDelayed)[Verbatim[key], val_] :> val, fn[key]];
(* lookupExprKey1[g_Graph,  *)

lookupOptKey[expr_ ? HAtomQ, _, _] := ErrorMsg[LookupOptions::notCompoundExpression, HoldForm @ expr];
LookupOptions::notCompoundExpression = "Expression `` was not a compound expression or association."

(*************************************************************************************************)

GraphOptions[g_Graph] := Options @ g;