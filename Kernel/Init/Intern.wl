PackageExports[
  "Function",     ToInternedData,
  "Function",     Interning,     InterningUnique,    InterningListDict,
  "DataHead",     InternedData,  InternedUniqueData, InternedListDictData
];

PrivateExports[
  "Operator", InterningFn, InterningUniqueFn, InterningListDictFn
];

(**************************************************************************************************)

Interning[index_:EmptyDict] :=
  Module[{ind = index}, InterningFn[ind]];

InterningUnique[index_:EmptyDict, msgName_:"duplicateItem"] :=
  Module[{ind = index}, InterningUniqueFn[ind, ThrowMsg[msgName, #]&]];

InterningListDict[kindex_:Auto, vindex_:Auto] :=
  Module[{keyFn = toIntFn @ index, valFn = toIntFn @ index}, InterningListDictFn[keyFn, valFn]];

toIntFn[Auto]          = Interning[];
toIntFn[f_InterningFn] = f;

General::duplicateItem = "Item `` occurred more than once.";

(**************************************************************************************************)

SetHoldF[InterningFn, InterningUniqueFn];

InterningFn[index_][item_] := InternTo[index, item];

InterningUniqueFn[index_, msgFn_][item_] := InternUniqueTo[index, item, msgFn];

InterningListDictFn[_,      valFn_][list_List] := {1, Len @ list, Map[valFn, list]};
InterningListDictFn[keyFn_, valFn_][dict_Dict] := {2, Map[keyFn, Keys @ dict], Map[valFn, Vals @ dict]};

(**************************************************************************************************)

ToInternedData = CaseOf[
  InterningFn[index_]                 := InternedData[index];
  InterningUniqueFn[index_, _]        := InternedUniqueData[index];
  InterningListDictFn[keyFn_, valFn_] := InternedListDictData[ToInternedData @ keyFn, ToInternedData @ valFn];
];
