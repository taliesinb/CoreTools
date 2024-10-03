SystemExports[
  "Function",
    LookupKeys, ChainedLookup,
    OptionRules, HoldOptionRules,
    LookupOptions, LookupOptionsAs, HoldLookupOptions,
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
    KeyAbsentFn,
    TakeOptions,
    NarrowOptions
];

(*************************************************************************************************)

SetStrict[OptionKeys, OptionKeyQ]

OptionKeys::usage = "OptionKeys[sym$] yields the keys of Options[sym$]."
OptionKeyQ::usage = "OptionKeyQ[sym$, key$] gives True if key$ is an option name of sym$."

OptionKeys[sym_Sym] := Keys @ Options @ sym;
OptionKeyQ[sym_Sym, key_] := KeyExistsQ[Options @ sym, key];

(*************************************************************************************************)

SetStrict[OptionValueRules, OptionValueList]

OptionValueRules::usage =
"OptionValueRules[sym$, options$$] yields a list of rules by mering options for sym$ with explicit \
options.
* keys in options$$ override the defaults for sym$.
* see OptionValueList."

(* TODO: head_Symbol -> inheritList *)
OptionValueRules[head_Symbol, rules___] :=
  JoinOptions[rules, Options @ head];


OptionValueList::usage =
"OptionValueList[sym$, options$$] yields the values for options of sym$ in their natural order.
* values are obtained from options$$, but defaults come from Options[sym$].
* see OptionValueRules."

OptionValueList[head_Symbol, rules___] :=
  OptionValue[head, ToRules @ rules, OptionKeys @ head];

(*************************************************************************************************)

SetStrict @ JoinOptions;

JoinOptions::usage =
"JoinOptions[opts$$] joins together multiple lists of option lists or single rules."

JoinOptions[args___] := DeleteDuplicatesBy[ToList[args], First];

(*************************************************************************************************)

SetCurry2 @ TakeOptions;

TakeOptions::usage =
"TakeOptions[sym$, ref$] yields a Sequence of those options of sym$ that match an option for ref$.
TakeOptions[{opts$$}, ref$] does the same for explicit options.
* use NarrowOption[$$] to do this within a function application."

TakeOptions[sym_Sym,   ref_Sym]  := TakeOptions[Options @ sym, ref];
TakeOptions[opts_List, ref_Sym]  := Seq @@ FilterRules[Flatten @ {opts}, Options @ ref];

TakeOptions::usage =
"NarrowOptions[opts$$] yields a Sequence of options that are appropriate for an external function \
application."

NarrowOptions /: sym_[l___, NarrowOptions[m__], r___] := sym[l, TakeOptions[{m}, sym], r];
NarrowOptions[] := Sequence[];

(*************************************************************************************************)

SetCurry1 @ MissingApply;

MissingApply::usage =
"MissingApply[fn$, expr$] replaces a missing key with fn$[key$] at outer level or level 1, and \
otherwise has no effect.
MissingApply[fn$] is the operator form of MissingApply."

MissingApply[fn_, e_] := e;
MissingApply[fn_, Missing[_, k_]] := fn[k];
MissingApply[fn_, e_List ? PackedQ] := e;
MissingApply[fn_, e_List] := VectorReplace[e, Missing[_, k_] :> fn[k]];

(*************************************************************************************************)

SetStrict @ LookupKeys;

LookupKeys::usage =
"LookupKeys[dict$, keys$, fn$] looks up a list of keys$, evaluating fn$[key$] for missing keys."

LookupKeys[assoc:DictLikeP, keys_List, fn_] := MissingApply[fn, Lookup[assoc, keys]];

(*************************************************************************************************)

KeyAbsentFn::usage =
"KeyAbsentFn[key$] returns Missing['KeyAbsent', key$]."

KeyAbsentFn[key_] := Missing["KeyAbsent", key];

(*************************************************************************************************)

ChainedRules::usage =
"ChainedRules[obj$1, obj$2, $$] represents a series of associations or rule lists to be looked up in sequence.
ChainedRules[$$][key$] performs a lookup of key$.
ChainedRules[$$][{key$1, key$2, $$}] performs a lookup of all keys
* a MissingFn[fn$] is also a valid object.
* lookup is performed by ChainedLookup[{obj$1, obj$2, $$}, keys].";

a_ChainedRules[key_]     := ChainedLookup[a, key];
a_ChainedRules[key_List] := ChainedLookup[a, key];

(*************************************************************************************************)

ChainedLookup::usage =
"ChainedLookup[{obj$1, obj$2, $$}, key$] looks up key in each successive obj$i until a value is found.
ChainedLookup[objs$, {key$1, key$2, $$}] looks up multiple keys.
ChainedLookup[$$, fn$] applies fn$[key$] to obtain values for keys not present in any object.
* each obj$ can be an association, list of rules, a ChainedRules object.
* values that are present but are Missing will also be chained.
* the final object can be a Function that will be applied to missing keys."

SetStrict @ ChainedLookup;

ChainedLookup[cr_ChainedRules, args__] := chainLookup0[List @@ cr, args];
ChainedLookup[objs_List, keys_]        := chainLookup0[objs, keys];
ChainedLookup[objs_List, keys_, fn_]   := chainLookup0[Append[objs, Fn[z, fn[z]]], keys];

chainLookup0[objs_, keys_List]         := Apply[chainLookupN, objs][keys];
chainLookupN[obj_, rest__][keys_]      := MissingApply[chainLookup1[rest], Lookup[obj, keys]];
chainLookupN[obj_Fn][keys_]            := Map[obj, keys];
chainLookupN[obj_][keys_]              := Lookup[obj, keys];

chainLookup0[objs_, Key[key_] | key_]  := Apply[chainLookup1, objs][key]
chainLookup1[obj_, rest__][key_]       := IfMissing[obj[key], chainLookup1[rest][key]];
chainLookup1[obj_Fn][key_]             := obj[key];
chainLookup1[obj_][key_]               := Lookup[obj, Key @ key];

(*************************************************************************************************)

OptionRules::usage = "OptionRules[head$[$$, options$$]] yields {options$$}."

OptionRules[_[___, r:RuleLP...]] := {r};
OptionRules[_] := $Failed;

HoldOptionRules::usage = "HoldOptionRules[head$[$$, options$$]] yields HoldComplete[options$$]."

SetHoldC @ HoldOptionRules;

HoldOptionRules[_[___, r:RuleLP...]] := HoldComplete[r];
HoldOptionRules[_] := $Failed;

(*************************************************************************************************)

OptionKey::usage =
"OptionKey[key$] represents a key in an object with non-rule arguments.
OptionKey[key$, default$] represents a key with a held default value.
OptionKey[$$][obj$] will find the key in the obj$ if present and return its value.
* the lookup is performed by LookupOptions.";

SetHoldR @ OptionKey;

OptionKey[key_][expr_]       := LookupOptions[expr, key];
OptionKey[key_, def_][expr_] := LookupOptions[expr, key, def&];

(*************************************************************************************************)

DefaultOptionValueFn::usage =
"DefaultOptionValueFn[head$][key$] looks up OptionValue[head$, key$]."

DefaultOptionValueFn[head_Symbol][key_] := OptionValue[head, key];

(*************************************************************************************************)

SetStrict @ LookupOptionsAs;

LookupOptionsAs::usage =
"LookupOptionsAs[dict$, keys$, head$] looks up a list of option symbols in dict$, but falls back on Options[head$].";

LookupOptionsAs[EmptyP, keys_List, head_Sym]         := Lookup[Options @ head, keys];
LookupOptionsAs[opts:ListDictP, keys_List, head_Sym] := MissingApply[DefaultOptionValueFn @ head, Lookup[opts, keys]];

(*************************************************************************************************)

LookupOptions::usage =
"LookupOptions[expr$, key$] looks up an option in an expr$ with non-rule arguments.
LookupOptions[expr$, {key$1, key$2, $$}] looks up multiple keys.
LookupOptions[$$, fn$] uses fn$[key$] to obtain defaults for any keys not found.
* expr$ can also be an association.
* expr$ can also be a Graph, etc.
* individual keys can also be Key[$$] or OptionKey[$$] expressions."

LookupOptions[graph_Graph, key_, fn_:DefaultOptionValueFn[Graph]] :=
  LookupOptions[GraphOptions @ graph, key, fn];

LookupOptions[expr_, key_, fn_:KeyAbsentFn] := lookupOpts[expr, key, fn];

SetHoldC[lookupOpts, lookupOptKey];

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

SetHoldF @ HoldLookupOptions;

HoldLookupOptions::usage = "HoldLookupOptions[expr$, $$] is like LookupOptions but does not evaluate exrp$."

HoldLookupOptions[expr_, key_, fn_:KeyAbsentFn] := lookupOpts[expr, key, fn];

(*************************************************************************************************)

GraphOptions[g_Graph] := Options @ g;