PackageExports[
  "DataHead",      Store,
  "Predicate",     StoreQ, StoreKeyQ,
  "PatternSymbol", StoreP,
  "SpecialFn",    StoreNew, KeyStoreNew, StoreClone,
  "Function",
    StoreGet, StoreGetRaw, StoreDict, StoreKeys, StoreValues, ExpandStores,
    StorePairs, StoreAt, StoreScan, StoreMap,
  "MutatingFunction",
    KeyStoreAdd, KeyStoreAddMany,
    StoreAdd, StoreSet, StoreDrop, StoreGetPut, StorePut, StoreApply
];

(*************************************************************************************************)

Store::usage = "Store is an alias for HashTable, but has its own richer API with more checking."

StoreAdd::usage =
"StoreAdd[dict, key, val] adds key -> value.
Messages if key is already present."

StoreGetRaw::usage =
"StoreGetRaw[dict, key] returns the value for entry key.
Messages if key is absent."

StoreSet::usage =
"Like StoreAdd, but doesn't fail if key already present.
Returns the previous value.";

StoreDrop::usage = "StoreDrop[dict, key] removes the entry for key.
Messages if key is not present."

(*************************************************************************************************)

DefineAliasRules[
  Store          -> HashTable
];

DefineAliasRules[
  StoreQ         -> HashTableQ,
  StoreKeyQ      -> HashTableContainsQ
];

DefineAliasRules[
  StoreAdd       -> HashTableAdd,
  StoreSet       -> HashTableSet,
  StoreDrop      -> HashTableRemove
];

DefineAliasRules[
  StoreGetRaw    -> HashTableGet,
  StoreClone     -> HashTableClone,
  StoreDict      -> HashTableToDict,
  StoreKeys      -> HashTableKeys,
  StoreValues    -> HashTableValues
];

(*************************************************************************************************)

SetStrict @ StoreNew;

StoreNew::usage =
"StoreNew[entries] makes a Store containing entries, which can be an association a list of rules, or a list of pairs."

StoreNew[]          := HashTable[];
StoreNew[{}]        := HashTable[];
StoreNew[dict_Dict] := HashTable[dict];
StoreNew[list_List] := If[PairVecQ[list] || RuleVecQ[list], HashTable[1, list], Message[StoreNew::badArg, e]; $Failed];
StoreNew[e_]        := (Message[StoreNew::badArg, e]; $Failed);

e_StoreNew := Message[StoreNew::badArguments, HoldForm @ e];
StoreNew::badArg = "First argument should be a Dict, list of rules, or list of pairs.";

(*************************************************************************************************)

SetStrict @ KeyStoreNew;

KeyStoreNew::usage =
"KeyStoreNew[items] makes a Store containing only keys, with Null values."

KeyStoreNew[]          := HashTable[];
KeyStoreNew[{}]        := HashTable[];
KeyStoreNew[keys_List] := HashTable[1, Thread @ {keys, Null}];

e_KeyStoreNew := Message[StoreNew::badArguments, HoldForm @ e];

(*************************************************************************************************)

SetStrict[KeyStoreAdd, KeyStoreAddMany];

KeyStoreAdd::usage = "KeyStoreAdd[dict, key] adds key to a store, with Null value.";
KeyStoreAddMany::usage = "KeyStoreAddMany[dict, keys] adds several keys to a store, with Null values.";

KeyStoreAdd[s_Store, k_] :=
  HashTableSet[s, NoEval @ k, Null];

KeyStoreAddMany[s_Store, keys_List] :=
  Scan[HashTableSet[s, #, Null]&, keys];

(*************************************************************************************************)

SetStrict @ SetHoldR @ StoreGet;

StoreGet::usage =
"StoreGet[store, key] returns the value for entry key, or None if not present.
StoreGet[dict, key, def] uses the given default."

StoreGet[s_Store, k_, def_:None] :=
  FastQuietCheck[HashTableGet[s, k], def];

(*************************************************************************************************)

SetStrict @ SetHoldR @ StoreGetPut;

StoreGetPut::usage =
"StoreGetPut[dict, key, def] returns the value for entry key, or sets it def if not present."

StoreGetPut[s_Store, k_, def_] :=
  FastQuietCheck[HashTableGet[s, k], StorePut[h, def]];

(*************************************************************************************************)

SetStrict @ SetHoldR @ StoreApply;

StoreApply::usage =
"StoreApply[dict, key, fn] updates key to have value fn[key].
StoreApply[dict, key, fn, fresh] uses fresh as the value for non-existent key (default is None).
The old value is returned."

StoreApply[s_Store, k_, fn_, def_:None] :=
  HashTableSet[s, k, fn[FastQuietCheck[HashTableGet[s, k], def]]];

(*************************************************************************************************)

SetStrict @ StoreAt;

StoreAt::usage =
"StoreAt[dict, key, fn1] evaluates fn1[value] if key exists.
StoreAt[dict, key, fn1, fn2] evaluates fn2[] if key does not exist."

StoreAt[s_Store, k_, fn1_, fn2:NullFn] :=
  Construct @@ FastQuietCheck[{fn1, HashTableGet[s, k]}, {fn2}];

(*************************************************************************************************)

SetStrict @ StorePut;

StorePut::usage = "Like StoreAdd, but doesn't fail if key already present, returning the latest value.";

StorePut[s_Store, k_, v_] := Then[HashTableSet[s, k, v], v];

(*************************************************************************************************)

SetStrict @ StorePairs;

StorePairs::usage = "StorePairs[store] returns {{k, v}, ...}.";

StorePairs[s_Store] := Thread @ List[StoreKeys @ s, StoreValues @ s];

(*************************************************************************************************)

SetStrict @ StoreScan;

StoreScan::usage = "StoreScan[f, store] evaluates f[k, v] in sequence.";

StoreScan[f_, s_Store] := KeyValueScan[f, StoreDict @ s];

(*************************************************************************************************)

SetStrict @ StoreMap;

StoreMap::usage = "StoreMap[f, store] returns {f[k, v], ...}.";

StoreMap[f_, s_Store] := MapThread[f, {StoreKeys @ s, StoreValues @ s}];

(*************************************************************************************************)

DefinePatternRules[
  StoreP -> _Store ? StoreQ
];

(*************************************************************************************************)

ExpandStores[expr_] := ReplaceRepeated[expr, s:StoreP :> RuleEval[StoreDict[ht]]];
