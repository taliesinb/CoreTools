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
  "Head",
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
keys are valid options in the spec list,.
"

Options[ParseItemOptions] = {
  ItemData      -> None,
  UseBroadcast  -> True,
  ItemGroups    -> None,
  GroupSettings -> GroupSettings
};

ParseItemOptions[head_, itemSpecs_List, items_, userOpts_, metaOpts___Rule] := Locals[
  UnpackSymbolsAs[ParseItemOptions, metaOpts, $useBroadcast, groupSettings, itemGroups, itemData];
  $ispecHead = head;
  $ispecItems = items;
  theme = Lookup[userOpts, Theme, getDefaultTheme @ head];
  $ispecItemData = Which[
    NoneQ[itemData], Dict[],
    DictQ[itemData], itemData,
    SymQ[itemData], Lookup[userOpts, itemData, Dict[]],
    True, ThrowOptVal[ItemData, itemData]
  ];
  If[theme =!= None, BindTo[$ispecItemData, "Theme" -> theme]];
  $ispecBcast = UDict[];
  $ispecOpts = If[DictQ @ userOpts, userOpts, UDict @ Rev @ userOpts]; (* TODO: RuleDict *)
  $ispecLen = Len @ items;
  DPrint["Parsing items"];
  $iresults = Dict @ Map[parseMultiItemSpec, itemSpecs];
  applyGroupSpecs[itemGroups, groupSettings, itemSpecs];
  If[$DebugPrinting, DebugGroup["Results:",
    KeyValueScan[DPrint[#1, ": ", MsgArgForm @ #2]&, $iresults]
  ]];
  $iresults
];

getDefaultTheme[head_] := Lookup[Options @ head, Theme, None];

SetStrict @ applyGroupSpecs;

noneOrEmptyP = None | Auto | EmptyP;

applyGroupSpecs[None, _, _] := Null
applyGroupSpecs[assignOpt_Sym, settingsOpt_Sym, itemSpecs_] := Locals @ DebugGroup["Group specs:",
  {assignSpec, settingsSpec} = OptionValue[$ispecHead,
    Normal @ $ispecOpts,
    {assignOpt, settingsOpt}];
  If[MatchQ[assignSpec, noneOrEmptyP], DPrint["No group assignment spec"]; Return[]];
  If[MatchQ[settingsSpec, noneOrEmptyP], DPrint["No group settings"]; Return[]];
  settingsDict = Dict[settingsSpec];
  If[!DictQ[settingsDict], ThrowOptVal[settingsOpt, settingsDict]];
  DPrint["Group settings:\n\t", MsgArgForm @ settingsDict];
  $specKeys = Col1 @ itemSpecs; $specInd = DictThread[$specKeys, itemSpecs];
  AssociateTo[$ispecOpts, assignOpt -> assignSpec];
  assignItemSpec = ItemSpec[assignOpt, groupAssignmentQ, {}, ToList, UseBroadcast -> False];
  itemGroups = P2 @ parseMultiItemSpec @ assignItemSpec;
  itemGroupPos = LevelIndex[itemGroups, 2];
  If[EmptyQ[itemGroupPos], DPrint["Empty group assignments"]; Return[]]; (* no groups, all empty lists *)
  DPrint["Item groups: ", MsgArgForm @ itemGroups];
  AssertSubsetOfQ[Keys @ itemGroupPos, Keys @ settingsDict, "unknownGroups"];
  $touched = UDict[];
  KeyValueMap[applySingleGroupSpec[settingsDict], itemGroupPos];
  Scan[key |-> If[TrueQ @ $ispecBcast[key], KeyApplyTo[$iresults, key, ToBroadcast]], Keys @ $touched]
];

applySingleGroupSpec[settingsDict_][groupName_, indices_] := Locals @ DebugGroup[
  {"Group ", groupName, " at indices ", indices, ":"},
  $gIndices = indices;
  groupSettingsSpec = Lookup[settingsDict, groupName, InternalError];
  groupSettings = Switch[groupSettingsSpec,
    noneOrEmptyP,     Return[],
    RuleP | RuleVecP, Dict @ groupSettingsSpec,
    _Dict,            Null,
    _,                ThrowMsg["invalidGroupSettings", groupName, groupSettingsSpec]
  ];
  Block[{$ispecOpts = groupSettings},
    itemSpecs = AssertLookupList[$specInd, Keys @ groupSettings, "unknownGroupSettingKeys"];
      DPrint["Group assiment spec: ", assignItemSpec];

    If[Len[indices] === $ispecLen,
      DPrint["Replacing all items by group settings"];
      Scan[parseMultiItemSpec /* replaceOldSettings, itemSpecs]
    ,
      Block[{
        $ispecItems = Part[$ispecItems, indices],
        $ispecItemData = Part[$ispecItemData, All, indices],
        $ispecLen = Len[indices]},
      Scan[parseMultiItemSpec /* mergeNewSettings, itemSpecs];
      ]
    ]
  ];
];

replaceOldSettings[key_ -> new_] := $iresults[key] = new;
mergeNewSettings[key_ -> new_] := Then[
  DPrint["merging new settings for ", Key[key], ":\n\t", new];
  $touched[key] = True;
  KeyApplyTo[$iresults, key, old |-> replaceParsedSpecs[old, new]]
];

General::unknownGroups = "Unknown groups: ``. Declared groups are: ``.";

SetPred1[groupAssignmentQ];

groupAssignmentQ[s_Str] := !KeyExistsQ[$ispecItemData, s];
groupAssignmentQ[DatumP | {DatumP...}] := True;

General::unknownGroupSettingKeys = "Group setting keys `` aren't elements one of ``."
General::invalidGroupSettings = "Invalid settings for group ``: ``."

replaceParsedSpecs = CaseOf[
  $[old_Broadcast, new_]     := $[FromBroadcast @ old, new];
  $[old_List, new_Broadcast] := ConstantReplaceIndices[old, $gIndices, P1 @ new];
  $[old_List, new_List]      := ReplaceIndices[old, $gIndices, new];
];

(**************************************************************************************************)

ItemSpec::usage =
"ItemSpec[key$, testFn$, default$?, finalFn$?]
* additional options:
| UseBroadcast       | False | whether to return Broadcast where possible |
| MethodResolution   | fn    | apply to strings (often a Dict) |
| Canonicalization   | {}    | rules to canonicalize particular specs |
* if Broadcast is Auto, and all broadcast values are the same, the scalar value will be \
set to this and the vector value removed.
* if Broadcast is Inherited, it is obtained from ParseItemOptions itself."

Options[ItemSpec] = {
  UseBroadcast     -> Inherited,
  MethodResolution -> None,
  Canonicalization -> {},
  ItemMessageFn    -> None
};

ItemSpec[key_, test_, opts___Rule]       := ItemSpec[key, test, Inherited, Id, opts];
ItemSpec[key_, test_, def_, opts___Rule] := ItemSpec[key, test, def, Id, opts];

SetStrict[parseMultiItemSpec];

parseMultiItemSpec[ItemSpec[key_, testFn_, defaultValue_, finalFn_, opts___Rule]] := Locals @ DebugGroup[Key @ key,

  UnpackSymbolsAs[ItemSpec, opts, canonicalization, methodResolution, useBroadcast, itemMessageFn];
  SetInherit[useBroadcast, $useBroadcast];
  AssociateTo[$ispecBcast, key -> useBroadcast];
  $ispecKey = key;
  spec = Lookup[$ispecOpts, key, Inherited];
  inheritValue = OptionValue[$ispecHead, key];
  methodResolution //= MethodResolutionFn;
  specRules = ToList[canonicalization, Auto -> defaultValue, Inherited -> inheritValue, FnRule[_String, methodResolution]];
  resolveFn = CanonicalizationFn[specRules, finalCheckFn = finalCheck[testFn, finalFn, itemMessageFn]];
  vectorResult = None;
  If[DictFnQ[spec],
    DPrint["Applying dict function ", spec];
    spec = CheckedDictMapThread[spec, $ispecItemData, missingDictKey];
  ];
  If[MatchQ[spec, Rule[_, _ ? MaybeFnQ]],
    DPrint["Applying function ", spec];
    spec = applyDataFn[$ispecItems, spec]];
  Which[
    spec === {} || spec === EmptyDict,
      DPrint["Empty"];
      scalarResult = resolveFn @ Auto,
    MatchQ[spec, Broadcast[_]],
      DPrint["Broadcast"];
      scalarResult = finalCheckFn @ First @ spec,
    RuleLVecQ[spec],
      DPrint["Rules = ", MsgArgForm @ spec];
      scalarResult = resolveFn @ Replace[DefaultValue, ToList[spec, DefaultValue -> Auto]];
      vectorResult = resolveFn /@ VectorReplace[$ispecItems, ToList[spec, _ -> scalarResult]],
    ListQ[spec],
      DPrint["List = ", MsgArgForm @ spec];
      scalarResult = resolveFn @ Auto;
      vectorResult = PadRight[resolveFn /@ spec, $ispecLen, scalarResult],
    DictQ[spec],
      DPrint["Dict = ", MsgArgForm @ spec];
      scalarResult = resolveFn @ Lookup[spec, DefaultValue, Auto];
      vectorResult = Lookup[spec, $ispecItems, scalarResult],
    StrQ[spec] && !TrueQ[testFn @ spec],
      DPrint["String = ", MsgArgForm @ spec];
      DPrint[resolveFn];
      vectorResult = Map[resolveFn] @ throwMissing @ Lookup[$ispecItemData, spec],
    MaybeFnQ[spec] && !TrueQ[testFn @ spec],
      DPrint["Fn = ", MsgArgForm @ spec];
      scalarResult = resolveFn @ Auto;
      vectorResult = Map[spec /* resolveFn, $ispecItems],
    True,
      DPrint["Scalar = ", MsgArgForm @ spec];
      scalarResult = resolveFn @ spec
  ];
  If[useBroadcast,
    If[NotNoneQ[vectorResult] && AllSameQ[vectorResult],
      scalarResult = P1 @ vectorResult;
      vectorResult = None
    ],
    SetNone[vectorResult, ConstList[scalarResult, $ispecLen]]
  ];
  If[vectorResult =!= None,
    DPrint["Vector: ", MsgArgForm @ vectorResult],
    DPrint["Scalar: ", MsgArgForm @ scalarResult]
  ];
  result = IfNone[vectorResult, Broadcast[scalarResult, $ispecLen]];
  key -> result
];

applyDataFn[items_, "Name" -> fn_] /; !KeyExistsQ[$ispecItemData, "Name"] := Map[fn, items];
applyDataFn[items_, keys_List -> fn_] := MapThread[fn, throwMissing /@ Lookup[$ispecItemData, keys]];
applyDataFn[items_, key_ -> fn_]      := Map[fn, throwMissing @ Lookup[$ispecItemData, key]];
applyDataFn[items_, ItemData -> fn_]  := Map[fn, ZipDictLists @ $ispecItemData];

missingDictKey[key_] := ThrowMsg["missingItemDataKey", $ispecKey, key, Keys @ $ispecItemData];

throwMissing[Missing[_, key_]] := missingDictKey[key];
throwMissing[e_] := e;

General::missingItemDataKey = "While processing ``: no key `` present in provided item data. Present keys: ``.";

(* TODO: Validated[...] head, i can use above in RuleLVecQ to avoid resolving the scalarResult twice *)
finalCheck[test_, final_, msgFn_][value_] := If[
  TrueQ @ test[value], final @ value,
  If[msgFn =!= None, msgFn[$ispecKey, value]];
  ThrowOptVal[$ispecKey, value, test]
];

(**************************************************************************************************)

CanonicalizeSpec[value_, rules__Rule] := CanonicalizationFn[{rules}] @ value;

CanonicalizationFn[rules_, final_:Id][value_] := final @ FixedPoint[Replace[rules], value, 5];

(**************************************************************************************************)

SetCurry1[MethodResolutionFn]

MethodResolutionFn[None] := Id;
MethodResolutionFn[spec_, val_]    := val;
MethodResolutionFn[spec_, str_Str] := resolveStrSpec[str, spec];

resolveStrSpec = CaseOf[
  (* $[str_, Rule[spec_, then_]] := then @ $[spec, str]; *)
  $[str_, dict_Dict]          := Lookup[dict, str, throwStrSpec[str, dict]];
  $[str_, fn_]                := IfFailed[fn @ str, throwStrSpec @ str];
];

General::notKnownStringSpec1 = "`` is not a known named setting for ``. Valid named settinngs include ``.";
General::notKnownStringSpec2 = "`` is not a known named setting for ``.";
throwStrSpec[str_]        := ThrowMsg["notKnownStringSpec2", str, $ispecKey];
throwStrSpec[str_, dict_] := ThrowMsg["notKnownStringSpec1", str, $ispecKey, LitStrRow @ Keys @ dict];
