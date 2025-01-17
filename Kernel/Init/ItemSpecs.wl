SystemExports[
  "Option",
    GroupSettings
];

PackageExports[
  "Function",
    ParseItemOptions,
    CanonicalizeSpec,
  "Option",
    ItemGroups,
    ItemData,
    UseBroadcast,
    Canonicalization,
    MethodResolution,
    ItemMessageFn,
  "DataHead",
    ItemSpec,
  "Operator",
    MethodResolutionFn,
    CanonicalizationFn
];

(**************************************************************************************************)

SetStrict @ ParseItemOptions;

"ParseItemOptions[head$, descriptions$, items$, spec$, opts$] yields an association.
* each description is a ItemSpec[$$], see its usage for more info.
* if UseBroadcast is True, the result may be a Broadcast[value, len], or a list.
* if UseBroadcast is False, the result is always a list.

* ItemData -> symbol$ specifies that symbol$ is the option key to obtain auxilliary item data.
* ItemGroups -> symbol$ specifies that symbol$ is the option key used to obtain item groups.
* GroupSettings -> symbol$s specifies that symbol$ the option key used to obtain settings for these groups.
* The special option Theme, if present, is made available as item data under the key 'Theme'.

* The ItemGroups option provided by the ultimate user should is treated like any other specification: \
  * it can be a list whose length is the number of items.
  * it can be a function assigning items to groups.
  * it can be a dictionary.
  * it can be a string key present in ItemData.
  * it can be a set of rules that assign to groups via pattern matching.
* The GroupSettings option provided by the ultimate user should be a list of rules or a dictionary whose \
keys are valid options in the spec list,."

Options[ParseItemOptions] = {
  ItemData      -> None,
  UseBroadcast  -> True,
  ItemGroups    -> None,
  GroupSettings -> GroupSettings
};

ParseItemOptions[head_, itemSpecs_List, items_, userOpts_, metaOpts___Rule] := Locals[

  UnpackSymbolsAs[ParseItemOptions, {metaOpts}, $useBroadcast, groupSettings, itemGroups, itemData];

  $head = head; $items = items; $len = Len @ items;
  $theme := $theme = Lookup[$opts, Theme, getDefaultTheme @ head];
  $opts = If[DictQ @ userOpts, userOpts, UDict @ Rev @ userOpts]; (* TODO: RuleDict *)

  $data = toItemData @ itemData;
  If[notDataKeyQ["Name"],  $data["Name"] = items];
  If[notDataKeyQ["Index"], $data["Index"] = Range @ $len];

  DPrint["Parsing items for ", head];
  DPrint["  * MetaOpts = ", MsgArgForm @ {metaOpts}];

  $bcast = UDict[];
  $output = Dict @ Map[applyItemSpec1, itemSpecs];

  applyGroupSpecs[itemGroups, groupSettings, itemSpecs];

  If[$DebugPrinting, DebugGroup["Results:",
    KeyValueScan[DPrint[#1, ": ", MsgArgForm @ #2]&, $output]
  ]];

  $output
];

getDefaultTheme[head_] := $ActiveThemes;

(**************************************************************************************************)

toItemData = CaseOf[
  None | Auto := Dict[];
  s_Sym       := $ @ Lookup[$userOpts, itemData, Dict[]];
  spec_       := AssertOptVal[ItemData, spec, DictQ]
];

hasDataKeyQ[k_] := HasKeyQ[$data, k];
notDataKeyQ[k_] := NotKeyQ[$data, k];

evalDictFn[fn_]            := CheckedDictMapThread[fn, $data, throwKey]

evalDataFn[dict_Dict, key_] /; KeyExistsQ[dict, DefV] := Lookup[dict, getData @ key, dict[DefV]];
evalDataFn[dict_Dict, key_] := AssertLookupList[dict, getData @ key, "missingDataFnKey", $key];
evalDataFn[fn_, keys_List]  := MapThread[fn, getData @ keys];
evalDataFn[fn_, key_]       := Map[fn, getData @ key];

getData = CaseOf[
  ItemData  := ZipDictLists @ $data;
  keys_List := throwMissing /@ Lookup[$data, keys];
  key_Key   := throwMissing @ Lookup[$data, key];
  key_      := throwMissing @ $data @ key;
];

throwMissing[Missing[_, "Theme"]] := $theme;
throwMissing[Missing[_, key_]] := throwKey[key];
throwMissing[e_] := e;

General::missingDataFnKey = "While processing `3`: data function requested keys `1`. Present keys: `2`.";

(**************************************************************************************************)

throwKey[key_] := ThrowMsg["missingItemDataKey", $key, key, Keys @ $data];

General::missingItemDataKey = "While processing ``: no key `` present in provided item data. Present keys: ``.";

(**************************************************************************************************)

SetStrict[applyItemSpec1, applyItemSpec2, parseItemSpec];

applyItemSpec1[spec:ItemSpec[key_, ___]] := Block[
  {$key = key},
  applyItemSpec2 @ parseItemSpec @ spec
];

applyItemSpec2[{testFn_, globalI_, localI_, finalFn_, useBroad_}] := Locals @ DebugGroup[Key @ $key,

  useBroadcast = useBroad;
  SetInherit[useBroadcast, $useBroadcast];
  AssociateTo[$bcast, $key -> useBroadcast];

  DPrint["TestFn = ", MsgArgForm @ testFn];

  InheritVar[$DebugPrinting];
  spec = Lookup[$opts, $key, Inherited];
  spec = applySpecFns @ spec;
  DPrint["Spec = ", MsgArgForm @ spec];

  SetInherit[spec, globalI];

  scalar = localI;
  vector = Null;

  Which[
    spec === {} || spec === EmptyDict,        DPrint["[Empty]"];
      Null
    ,
    MatchQ[spec, Broadcast[_]],               DPrint["[Broadcast]"];
      scalar = finalFn @ First @ spec
    ,
    RuleLVecQ @ spec,                         DPrint["[Rules]"];
      {scalar, vector} = applyRules[localI, spec]
    ,
    ListQ[spec] && Len[spec] == $len,         DPrint["[List]"];
      vector = spec
    ,
    ListQ @ spec,                             DPrint["[PaddedList]"];
      vector = PadRight[spec, $len, scalar]
    ,
    DictQ @ spec,                             DPrint["[Dict]"];
      scalar = getUserDefault @ spec;
      vector = Lookup[spec, $items, scalar]
    ,
    TrueQ @ testFn @ spec,                    DPrint["[CanonScalar]"];
      scalar = spec
    ,
    StrQ[spec] && (NoneQ[testFn] || !TrueQ[testFn[spec]]), DPrint["[StrKey]"];
      vector = getData @ spec
    ,
    MaybeFnQ @ spec,                          DPrint["[Fn]"];
      vector = Map[spec, $items]
    ,
    True,                                     DPrint["[NonCanonScalar]"];
      scalar = spec;
  ];
  If[useBroadcast && NotNullQ[vector] && AllSameQ[vector],
    DPrint["(uniform)"];
    scalar = First @ vector;
    vector = Null
  ];
  If[NullQ[vector],
    DPrint["Scalar: ", scalar];
    vector = If[useBroadcast, Broadcast, ConstList][finalFn @ scalar, $len]
  ,
    DPrint["Vector: ", scalar];
    vector = If[finalFn === Id, vector, Map[finalFn, vector]]
  ];

  $key -> vector
];

(**************************************************************************************************)

getUserDefault[inherit_, rules_List] :=
  Replace[$nomatch$ -> inherit] @ Replace[ToList[rules, DefV -> $nomatch$]] @ DefV;

getUserDefault[inherit_, dict_Dict] :=
  Replace[$nomatch$ -> inherit] @ Lookup[dict, DefV, $nomatch$];

applyRules[inherit_, rules_] := Locals[
  scalar = Replace[DefV, ToList[rules, DefV -> $nomatch$]];
  If[scalar === $nomatch$, scalar = inherit];
  vector = VectorReplace[$items, ToList[rules, _ -> $nomatch$]];
  If[MatchQ[vector, {$nomatch$..}],
    ErrorMessage[$head -> "noRulesMatched", $key];
    vector = Null;
  ,
    vector = VectorReplace[vector, $nomatch$ :> scalar];
  ];
  {scalar, vector}
];

General::noRulesMatched = "None of the rules specified for `` matched any items. Using defaults.";

(**************************************************************************************************)

applySpecFns = CaseOf[

  DebugRules[expr_] := Then[
    $DebugPrinting = True;
    $ @ expr
  ];

  fn_ ? DictFnQ := Then[
    DPrint["[DictFn]: ", fn];
    evalDictFn @ fn
  ];

  Rule[key1_, rule_Rule] := Then[
    DPrint["[NestedFn]: ", key1, " then ", rule];
    Block[{$data = getData[key1]}, applySpecFns[rule]]
  ];

  Rule[key_, fn_ ? MaybeFnQ] := Then[
    DPrint["[KeyFn]: ", fn, " @ ", key];
    evalDataFn[fn, key]
  ];

  k_Key := Then[
    DPrint["[Key]: ", k];
    getData @ k
  ];

  spec_ := spec;
];

(**************************************************************************************************)

"ItemSpec[key$, testFn$, default$, finalFn$?]

* default$ is the per-item default used to resolve Inherited if one can't be obtained from Options.
* because options give the possibility of theming, we only resort to the ItemSpec default in worst-case
scenarios.

* DefaultValue, however, always refers this default value.

* Automatic gets no special treatment, as it is best handled downstream.

* testFn$ can be None, which says that any values are allowed, but Inherited will still be resolved.

* additional options:
| UseBroadcast       | False | whether to return Broadcast where possible |
| MethodResolution   | fn    | apply to strings (often a Dict) |
| Canonicalization   | {}    | rules to canonicalize particular specs |
| ItemMessageFn      | fn    | should issue a non-generic message |
* if Broadcast is True, and all broadcast values are the same, Broadcast[value] will be returned.
* if Broadcast is Inherited, it is obtained from ParseItemOptions itself.

* multi-item specs (ListP, RuleListP, DictP, Key[_], Rule[_, FnP], DictFnP) are always expanded.

* Global specs are resolved (to fixed point) as follows:
  * DefaultValue is resolved to the ItemSpec default, always
  * Inherited is resolved to the global option/theme setting unless testFn accepts Inherited
  * Canonicalization rule replacements are applied
  * Strings are converted via MethodResolution (if any)

* Local (per-item) specs are resolved as above, except that Inherited is resolved to:
  * the per-item default, which may come from options or from the ItemSpec
  * the ItemSpec default otherwise"

Options[ItemSpec] = {
  UseBroadcast     -> Inherited,
  MethodResolution -> None,
  Canonicalization -> {},
  ItemMessageFn    -> None
};

parseItemSpec[ItemSpec[key_, testFn_, opts___Rule]] :=
  parseItemSpec[ItemSpec[key, testFn, None, Id, opts]];

parseItemSpec[ItemSpec[key_, testFn_, default_, opts___Rule]] :=
  parseItemSpec[ItemSpec[key, testFn, default, None, opts]];

parseItemSpec[spec:ItemSpec[key_, testFn_, defaultValue_, postFn_, opts___Rule]] := Locals @ DebugGroup[
  spec,

  optionValue = OptionValue[$head, key];
  inheritOk = TrueQ @ testFn @ Inherited;
  optionOk = TrueQ @ testFn @ optionValue;

  globalInherit = Which[
    inheritOk, DPrint["Global Inherit unchanged"];                  Inherited,
    optionOk,  DPrint["Global inherit from Opts = ", optionValue];  optionValue,
    True,      DPrint["Global inherit from DefV = ", defaultValue]; defaultValue
  ];
  localInherit = Which[
    optionOk,  DPrint["Items inherit from Opts = ", optionValue];    optionValue,
    inheritOk, DPrint["Items Inherit unchanged"];                    Inherited,
    True,      DPrint["Items inherit from DefV = ", defaultValue];  defaultValue
  ];

  UnpackSymbolsAs[ItemSpec, List @ opts, canonicalization, methodResolution, useBroadcast, itemMessageFn];

(*
  If[NotNoneQ[methodResolution], DPrint["MethodResultion = ", methodResolution]];
  methodResolution //= MethodResolutionFn;

   globalFn = makeCanonFn[testFn, canonicalization, defaultValue, globalInheritValue, methodResolution];
  itemFn = makeCanonFn[testFn, canonicalization, defaultValue, localInheritValue, methodResolution];
 *)
  finalFn = toFinalizerFn[testFn, IfNone[postFn, Id], itemMessageFn];
  If[finalFn =!= Id, DPrint["FinalFn = ", finalFn]];

  List[testFn, globalInherit, localInherit, finalFn, useBroadcast]
];

(**************************************************************************************************)

toFinalizerFn = CaseOf[
  $[None,    postFn_, msgFn_] := postFn;
  $[TrueFn,  postFn_, _]      := postFn;
  $[testFn_, postFn_, None]   := FmA |-> If[TrueQ @ testFn @ FmA, postFn @ FmA, ThrowOptVal[$key, FmA, testFn]];
  $[testFn_, postFn_, msgFn_] := FmA |-> If[TrueQ @ testFn @ FmA, postFn @ FmA, msgFn[$key, FmA]; ThrowRawException[]];
];

(**************************************************************************************************)

makeCanonFn[canonSpec_, default_, inherit_, method_] := Locals[
  canonRules = DelCases[sym_ -> sym_] @ ToList[
    canonSpec,
    DefaultValue -> default,
    Inherited -> inherit,
    FnRule[_String, method]
  ];
  CanonicalizationFn[canonRules, Id]
];

(**************************************************************************************************)

CanonicalizeSpec[value_, rules__Rule] := CanonicalizationFn[{rules}] @ value;

CanonicalizationFn[rules_, final_:Id][value_] := final @ FixedPoint[Replace[rules], value, 5];

(**************************************************************************************************)

SetCurry1[MethodResolutionFn]

MethodResolutionFn[None]           := Id;
MethodResolutionFn[spec_, val_]    := val;
MethodResolutionFn[spec_, str_Str] := resolveStrSpec[str, spec];

resolveStrSpec = CaseOf[
  (* $[str_, Rule[spec_, then_]] := then @ $[spec, str]; *)
  $[str_, dict_Dict]          := Lookup[dict, str, throwStrSpec[str, dict]];
  $[str_, fn_]                := IfFailed[fn @ str, throwStrSpec @ str];
];

General::notKnownStringSpec1 = "`` is not a known named setting for ``. Valid named settinngs include ``.";
General::notKnownStringSpec2 = "`` is not a known named setting for ``.";
throwStrSpec[str_]        := ThrowMsg["notKnownStringSpec2", str, $key];
throwStrSpec[str_, dict_] := ThrowMsg["notKnownStringSpec1", str, $key, LitStrRow @ Keys @ dict];

(**************************************************************************************************)

SetStrict @ applyGroupSpecs;

noneOrEmptyP = None | Auto | EmptyP;

applyGroupSpecs[None, _, _] := Null
applyGroupSpecs[assignOpt_Sym, settingsOpt_Sym, itemSpecs_] := Locals @ DebugGroup["Group specs:",
  {assignSpec, settingsSpec} = OptionValue[$head, Normal @ $opts, {assignOpt, settingsOpt}];

  If[MatchQ[assignSpec, noneOrEmptyP], DPrint["No group assignment spec"]; Return[]];
  If[MatchQ[settingsSpec, noneOrEmptyP], DPrint["No group settings"]; Return[]];

  settingsDict = Dict[settingsSpec];
  If[!DictQ[settingsDict], ThrowOptVal[settingsOpt, settingsDict]];
  DPrint["Group settings:\n\t", MsgArgForm @ settingsDict];

  $specKeys = Col1 @ itemSpecs;
  $specInd = DictThread[$specKeys, itemSpecs];
  AssociateTo[$opts, assignOpt -> assignSpec];

  assignItemSpec = ItemSpec[assignOpt, groupAssignmentQ, {}, ToList, UseBroadcast -> False, ItemMessageFn -> badGroupAssignment];
  itemGroups = P2 @ applyItemSpec1 @ assignItemSpec;
  itemGroupPos = LevelIndex[itemGroups, 2];
  If[EmptyQ[itemGroupPos], DPrint["Empty group assignments"]; Return[]]; (* no groups, all empty lists *)
  DPrint["Item groups: ", MsgArgForm @ itemGroups];

  AssertSubsetOfQ[Keys @ itemGroupPos, Keys @ settingsDict, "unknownGroups"];
  $touched = UDict[];
  KeyValueMap[applySingleGroupSpec[settingsDict], itemGroupPos];

  Scan[
    key |-> If[TrueQ @ $bcast[key], KeyApplyTo[$output, key, ToBroadcast]],
    Keys @ $touched
  ]
];

applySingleGroupSpec[settingsDict_][groupName_, indices_] := Locals @ DebugGroup[
  {"Group ", groupName, " at indices ", indices, ":"},

  $gIndices = indices; $gLen = Len @ indices;

  groupSettingsSpec = Lookup[settingsDict, groupName, InternalError];
  groupSettings = Switch[groupSettingsSpec,
    noneOrEmptyP,     Return[],
    RuleP | RuleVecP, Dict @ groupSettingsSpec,
    _Dict,            Null,
    _,                ThrowMsg["invalidGroupSettings", groupName, groupSettingsSpec]
  ];

  Block[{$opts = groupSettings},
    itemSpecs = AssertLookupList[$specInd, Keys @ groupSettings, "unknownGroupSettingKeys"];
    DPrint["Group assiment spec: ", assignItemSpec];
    If[$gLen === $len,
      DPrint["Replacing all items by group settings"];
      Scan[applyItemSpec1 /* replaceOldSettings, itemSpecs]
    ,
      Block[{
        $items = Part[$items, indices],
        $data = Part[$data, All, indices],
        $len = $gLen},
        Scan[applyItemSpec1 /* mergeNewSettings, itemSpecs];
      ]
    ]
  ];
];

replaceOldSettings[key_ -> new_] :=
  Set[$output[key], new];

mergeNewSettings[key_ -> new_] := Then[
  DPrint["merging new settings for ", Key[key], ":\n\t", new];
  $touched[key] = True;
  KeyApplyTo[$output, key, old |-> replaceParsedSpecs[old, new]]
];

SetPred1[groupAssignmentQ];

groupAssignmentQ[s_Str] := notDataKeyQ @ s;
groupAssignmentQ[DatumP | {DatumP...}] := True;

badGroupAssignment[head_, e_] := ErrorMessage[$head -> "badGroupAssignment", $key, e];

General::badGroupAssignment = "Not a valid group assignment for option ``: ``.";
General::unknownGroups = "Unknown groups: ``. Declared groups are: ``.";
General::unknownGroupSettingKeys = "Group setting keys `` aren't elements one of ``."
General::invalidGroupSettings = "Invalid settings for group ``: ``."

replaceParsedSpecs = CaseOf[
  $[old_Broadcast, new_]     := $[FromBroadcast @ old, new];
  $[old_List, new_Broadcast] := ConstantReplaceIndices[old, $gIndices, P1 @ new];
  $[old_List, new_List]      := ReplaceIndices[old, $gIndices, new];
];