PackageExports[

  "DataHead",
    Bag,
    HashTable,
    PackedTree,
    InternalData,
    BoolFn,

  "MutatingFunction",
    StuffBag,
    HashTableAdd, HashTableSet, HashTableMapAt, HashTableRemove,
    GlobalWeakTablePut,

  "Function",
    BagPart, BagLength, ExpandBags,
    HashTableGet, HashTableClone, HashTableToDict, HashTableKeys, HashTableValues, ExpandHashTables,
    GlobalWeakTableGet,
    ToPacked, FromPacked, PackedType, PackedSize, DimsSize, ToPackedInts, ToPackedReals,
    ToPackedTree, DimsTree, DimsTreeProd, FlatProd,

  "FormHead",
    PackedForm,

  "Predicate",
    HashTableQ, HashTableContainsQ,
    StoreQ, StoreKeyQ,

  "PatternSymbol",
    HashTableP, PackedP
];

(*************************************************************************************************)

DefineAliasRules[
  Bag                -> Internal`Bag,
  HashTable          -> System`Utilities`HashTable,
  PackedTree         -> NumericalMath`Derivatives`PackedExpression,
  InternalData       -> System`Private`InternalData,
  BoolFn             -> BooleanFunction
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
"HashTable[dict] makes a new HashTable containing entries of dict.
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

HashTableRemove::usage = "HashTableRemove[dict, key] removes the entry for key.
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
  PackedForm           -> Developer`PackedArrayForm
];

ToPackedInts[arr_]  := ToPacked[arr, Integer];
ToPackedReals[arr_] := ToPacked[N @ arr, Real];

DimsSize[arr_]             := Times @@ Dimensions[NoEval @ arr]
PackedSize[arr_ ? PackedQ] := DimsSize @ arr;
PackedSize[_]              := None;

(*************************************************************************************************)
(**************************************************************************************************)

(* ToPackedTree: packing of tree into a special expression, not sure how we map elements yet *)
DefineAliasRules[
  ToPackedTree         -> NumericalMath`Derivatives`ToPackedExpression,
  DimsTree             -> NumericalMath`Derivatives`RaggedDimensions,
  DimsTreeProd         -> NumericalMath`Derivatives`NumberOfElements,
  FlatProd             -> FlatProduct
];

(*************************************************************************************************)
(**************************************************************************************************)

DefinePatternRules[
  HashTableP           -> _HashTable ? HashTableQ,
  PackedP              -> _List ? HPackedQ
];

(*************************************************************************************************)

ExpandHashTables[expr_] := ReplaceRepeated[expr, ht:HashTableP :> RuleEval[HashTableToDict @ ht]];
