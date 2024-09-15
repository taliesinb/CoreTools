PackageExports[

  "DataHead",
    Bag,
    Store, HashTable,
    PackedTree,

  "MutatingFunction",
    StuffBag,
    HashTableAdd, HashTableSet, HashTableMapAt, HashTableRemove,
    StoreAdd, StoreSet, StoreGetPut, StorePut, StorePairs, StoreApply, StoreAt, StoreDrop,
    StoreScan, StoreMap,
    GlobalWeakTablePut,

  "Function",
    BagPart, BagLength,
    ExpandBags,

    HashTableGet, HashTableClone, HashTableToDict, HashTableKeys, HashTableValues,
    StoreNew, StoreGet, StoreGetPut, StoreGetRaw, StoreClone, StoreDict, StoreKeys, StoreValues,
    ExpandStores,
    GlobalWeakTableGet,

    ToPacked, FromPacked, PackedType, ToPackedInts, ToPackedReals,

    ToPackedTree, DimsTree, DimsProd, FlatProd,

  "FormHead",
    PackedForm,

  "Predicate",
    HashTableQ, HashTableContainsQ,
    StoreQ, StoreKeyQ,
    PackedQ, HPackedQ,

  "PatternSymbol",
    StoreP, PackedP
];

(*************************************************************************************************)

DefineAliasRules[
  Bag                -> Internal`Bag,
  HashTable          -> System`Utilities`HashTable,
  Store              -> System`Utilities`HashTable,
  PackedTree         -> NumericalMath`Derivatives`PackedExpression
];

(*************************************************************************************************)
(*************************************************************************************************)

DefineAliasRules[
  StuffBag             -> Internal`StuffBag,
  BagPart              -> Internal`BagPart,
  BagLength            -> Internal`BagLength
];

ExpandBags[expr_] := ReplaceRepeated[expr, bag_Bag :> RuleEval[BagPart[bag, All]]];

(**************************************************************************************************)
(*************************************************************************************************)

General::hasht = "Argument `` of `` is not a HashTable.";
General::hashtv = "Argument `` of `` is not a HashTable with values.";

HashTable::usage =
"HashTable[dict] makes a new HashTable (alias Store) containing entries of dict.
HashTable[0] makes a set-like hash table, which only supports testing for membership.
HashTable[1, {{k, v}, ...}] or HashTable[1, {k -> v, ...}] creates a table from entries.
HashTable[] makes an empty hash table."

System`Utilities`HashTableGet::noget = "Key `` does not exist in table ``.";
System`Utilities`HashTableAdd::noadd = "Key `` already exist in table ``.";
System`Utilities`HashTableRemove::norem = "Key `` does not exist in table ``.";

HashTableAdd::usage =
"HashTableAdd[set, key] adds a key.\nHashTableAdd[dict, key, val] adds key -> value.
Messages if key is already present."

HashTableMapAt::usage =
"HashTableMapAt[dict, key, fn] updates key to have value fn[key].
HashTableMapAt[dict, key, fn, fresh] uses fresh as the value for non-existent key (default is Null).
The old value is returned.
Unfortunately it does not actually evaluate the function!"

HashTableGet::usage =
"HashTableGet[dict, key] returns the value for entry key.
Messages if key is absent."

HashTableSet::usage =
"Like HashTableAdd, but doesn't fail if key already present, returning the previous value (or Null).";

HashTableRemove::usage = "StoreDrop[dict, key] removes the entry for key.
Messages if key is not present."

(*************************************************************************************************)

DefineAliasRules[
  HashTableQ         -> System`Utilities`HashTableQ,
  HashTableContainsQ -> System`Utilities`HashTableContainsQ
];

DefineAliasRules[
  HashTableAdd       -> System`Utilities`HashTableAdd,
  HashTableSet       -> System`Utilities`HashTableSet,
  HashTableMapAt     -> System`Utilities`HashTableMapAt,
  HashTableRemove    -> System`Utilities`HashTableRemove
];

DefineAliasRules[
  HashTableGet       -> System`Utilities`HashTableGet,
  HashTableClone     -> System`Utilities`HashTableClone,
  HashTableToDict    -> System`Utilities`HashTableToAssociation,
  HashTableKeys      -> System`Utilities`HashTableKeys,
  HashTableValues    -> System`Utilities`HashTableValues
];

(**************************************************************************************************)
(*************************************************************************************************)

StoreNew::usage =
"StoreNew[entries] makes a Store (alias for HashTable) containing entries, which can \
be an association a list of rules, or a list of pairs."

StoreNew[]          := HashTable[];
StoreNew[dict_Dict] := HashTable[dict];
StoreNew[list_List] := HashTable[1, list];
StoreNew[e_]        := (Message[StoreNew::badArg, e]; $Failed);

(*************************************************************************************************)

ExpandStores[expr_] := ReplaceRepeated[expr, ht_HashTable :> RuleEval[HashTableToDict[ht]]];

(*************************************************************************************************)

StoreGet::usage =
"StoreGet[store, key] returns the value for entry key, or None if not present.
StoreGet[dict, key, def] uses the given default."

Attributes[StoreGet] = {HoldRest};
StoreGet[s_, k_, def_:None] := FastQuietCheck[HashTableGet[s, k], def];

(*************************************************************************************************)

StoreGetPut::usage =
"StoreGetPut[dict, key, def] returns the value for entry key, or sets it def if not present."

Attributes[StoreGetPut] = {HoldRest};
StoreGetPut[s_, k_, def_] := FastQuietCheck[HashTableGet[s, k], StorePut[h, def]];

(*************************************************************************************************)

StoreApply::usage =
"StoreApply[dict, key, fn] updates key to have value fn[key].
StoreApply[dict, key, fn, fresh] uses fresh as the value for non-existent key (default is None).
The old value is returned."

StoreApply[s_, k_, fn_, def_:None] :=
  HashTableSet[s, k, fn[FastQuietCheck[HashTableGet[s, k], def]]];

(*************************************************************************************************)

StoreAt::usage =
"StoreAt[dict, key, fn1] evaluates fn1[value] if key exists.
StoreAt[dict, key, fn1, fn2] evaluates fn2[] if key does not exist.
"

StoreAt[s_, k_, fn1_, fn2:NullFn] :=
  Construct @@ FastQuietCheck[{fn1, HashTableGet[s, k]}, {fn2}];

(*************************************************************************************************)

StorePut::usage =
"Like StoreAdd, but doesn't fail if key already present.
Returns the latest value.";

StorePut[s_, k_, v_] := Then[HashTableSet[s, k, v], v];

(*************************************************************************************************)

StorePairs::usage =
"StorePairs[store] returns {{k, v}, ...}.";

StorePairs[s_] := Thread @ List[StoreKeys @ s, StoreValues @ s];

(*************************************************************************************************)

StoreScan::usage =
"StoreScan[f, store] evaluates f[k, v] in sequence.";

StoreScan[f_, s_] := KeyValueScan[f, StoreDict @ s];

(*************************************************************************************************)

StoreMap::usage =
"StoreMap[f, store] returns {f[k, v], ...}.";

StoreMap[f_, s_] := MapThread[f, {StoreKeys @ s, StoreValues @ s}];

(*************************************************************************************************)

StoreAdd::usage =
"StoreAdd[set, key] adds a key.\nStoreAdd[dict, key, val] adds key -> value.
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
(**************************************************************************************************)

DefineAliasRules[
  GlobalWeakTablePut   -> System`Utilities`ExprLookupAdd,
  GlobalWeakTableGet   -> System`Utilities`ExprLookup
];

(*************************************************************************************************)
(**************************************************************************************************)

(* Developer` PackedArray utilities *)
DefineAliasRules[
  ToPacked             -> Developer`ToPackedArray,
  FromPacked           -> Developer`FromPackedArray,
  PackedType           -> Internal`PackedArrayType,
  PackedForm           -> Developer`PackedArrayForm,
  PackedQ              -> Developer`PackedArrayQ
];

HPackedQ[arr_List]  := Developer`PackedArrayQ @ NoEval @ arr;

ToPackedInts[arr_]  := ToPacked[arr, Integer];
ToPackedReals[arr_] := ToPacked[N @ arr, Real];

(*************************************************************************************************)
(**************************************************************************************************)

(* ToPackedTree: packing of tree into a special expression, not sure how we map elements yet *)
DefineAliasRules[
  ToPackedTree         -> NumericalMath`Derivatives`ToPackedExpression,
  DimsTree             -> NumericalMath`Derivatives`RaggedDimensions,
  DimsProd             -> NumericalMath`Derivatives`NumberOfElements,
  FlatProd             -> FlatProduct
];

(*************************************************************************************************)
(**************************************************************************************************)

DefinePatternRules[
  StoreP             -> _HashTable ? System`Utilities`HashTableQ,
  PackedP            -> _List ? HPackedQ
];

