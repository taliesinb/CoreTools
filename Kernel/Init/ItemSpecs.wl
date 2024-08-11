SystemExports[
  "OptionSymbol",
    GroupSettings
];

PackageExports[
  "Function",
    ParseItemOptions,
    CanonicalizeSpec,
  "OptionSymbol",
    ItemGroups,
    ItemData,
    UseBroadcast,
    Canonicalization,
    MethodResolution,
  "Head",
    ItemSpec,
  "Operator",
    MethodResolutionFn,
    CanonicalizationFn
];

(**************************************************************************************************)

SetStrict @ ParseItemOptions;

ParseItemOptions::usage =
"ParseItemOptions[head$, descriptions$, items$, spec$, opts$] yields an association.
* each description is a ItemSpec[$$], see its usage for more info.
* if UseBroadcast is True, the result may be a Broadcast[value, len], or a list.
* if UseBroadcast is False, the result is always a list.
* ItemData -> symbol$ specifies that symbol$ is the option key to obtain auxilliary item data.
* ItemGroups -> symbol$g specifies that symbol$ is the option key used to obtain item groups.
* GroupSettings -> symbol$s specifies that symbol$ the option key used to obtain settings for these groups.
"

Options[ParseItemOptions] = {
  ItemData      -> None,
  UseBroadcast  -> True,
  ItemGroups    -> None,
  GroupSettings -> GroupSettings
};

ParseItemOptions[head_, itemSpecs_List, items_, userOpts_, metaOpts___Rule] := Locals[
  UnpackOptionsAs[ParseItemOptions, metaOpts, $useBroadcast, groupSettings, itemGroups, itemData];
  $ispecHead = head;
  $ispecItems = items;
  $ispecItemData = If[NoneQ[itemData], Dict[], Lookup[userOpts, itemData, Dict[]]];
  $ispecBcast = UDict[];
  $ispecOpts = If[DictQ @ userOpts, userOpts, UDict @ Rev @ userOpts]; (* TODO: RuleDict *)
  $ispecLen = Len @ items;
  DPrint["Parsing items"];
  $iresults = Dict @ Map[parseMultiItemSpec, itemSpecs];
  applyGroupSpecs[itemGroups, groupSettings, itemSpecs];
  $iresults
];

SetStrict @ applyGroupSpecs;

noneOrEmptyP = None | Auto | EmptyP;

applyGroupSpecs[None, _, _] := Null
applyGroupSpecs[assignOpt_Sym, settingsOpt_Sym, itemSpecs_] := Locals[
  {assignSpec, settingsSpec} = OptionValue[$ispecHead,
    Normal @ $ispecOpts,
    {assignOpt, settingsOpt}];
  DPrint["Parsing group specs"];
  If[MatchQ[assignSpec, noneOrEmptyP] || MatchQ[settingsSpec, noneOrEmptyP], Return[]];
  settingsDict = Dict[settingsSpec];
  If[!DictQ[settingsDict], ThrowOptionError[settingsOpt, settingsDict]];
  $specKeys = Col1 @ itemSpecs; $specInd = DictThread[$specKeys, itemSpecs];
  AssociateTo[$ispecOpts, assignOpt -> assignSpec];
  assignItemSpec = ItemSpec[assignOpt, groupAssignmentQ, {}, ToList, UseBroadcast -> False];
  itemGroups = P2 @ parseMultiItemSpec @ assignItemSpec;
  itemGroupPos = LevelIndex[itemGroups, 2];
  If[EmptyQ[itemGroupPos], Return[]]; (* no groups, all empty lists *)
  SubsetOfQOrThrow[Keys @ itemGroupPos, Keys @ settingsDict, "unknownGroups"];
  $touched = UDict[];
  KeyValueMap[applySingleGroupSpec[settingsDict], itemGroupPos];
  Scan[key |-> If[TrueQ @ $ispecBcast[key], KeyApplyTo[$iresults, key, ToBroadcast]], Keys @ $touched]
];

applySingleGroupSpec[settingsDict_][groupName_, indices_] := Locals[
  $gIndices = indices;
  groupSettingsSpec = Lookup[settingsDict, groupName, InternalError];
  groupSettings = Switch[groupSettingsSpec,
    noneOrEmptyP,     Return[],
    RuleP | RuleVecP, Dict @ groupSettingsSpec,
    _Dict,            Null,
    _,                ThrowMsg["invalidGroupSettings", groupName, groupSettingsSpec]
  ];
  Block[{$ispecOpts = groupSettings},
    itemSpecs = LookupListOrThrow[$specInd, Keys @ groupSettings, "unknownGroupSettingKeys"];
    If[Len[indices] === $ispecLen,
      Scan[parseMultiItemSpec /* replaceOldSettings, itemSpecs]
    ,
      Block[{$ispecItems = Part[$ispecItems, indices]},
      Scan[parseMultiItemSpec /* mergeNewSettings, itemSpecs];
      ]
    ]
  ];
];

replaceOldSettings[key_ -> new_] := $iresults[key] = new;
mergeNewSettings[key_ -> new_] := Then[
  $touched[key] = True;
  KeyApplyTo[$iresults, key, old |-> replaceParsedSpecs[old, new]]
];

General::unknownGroups = "Unknown groups: ``. Declared groups are: ``.";

SetPred1[groupAssignmentQ];
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
  Canonicalization -> {}
};

ItemSpec[key_, test_, opts___Rule]       := ItemSpec[key, test, Inherited, Id, opts];
ItemSpec[key_, test_, def_, opts___Rule] := ItemSpec[key, test, def, Id, opts];

SetStrict[parseMultiItemSpec];

parseMultiItemSpec[ItemSpec[key_, testFn_, defaultValue_, finalFn_, opts___Rule]] := Locals[
  DPrint["Parsing item: ", key];
  UnpackOptionsAs[ItemSpec, opts, canonicalization, methodResolution, useBroadcast];
  SetInherit[useBroadcast, $useBroadcast];
  AssociateTo[$ispecBcast, key -> useBroadcast];
  $ispecKey = key;
  spec = Lookup[$ispecOpts, key, Inherited];
  inheritValue = OptionValue[$ispecHead, key];
  methodResolution //= MethodResolutionFn;
  specRules = ToList[canonicalization, Auto -> defaultValue, Inherited -> inheritValue, FnRule[_String, methodResolution]];
  resolveFn = CanonicalizationFn[specRules, finalCheckFn = finalCheck[testFn, finalFn]];
  vectorResult = None;
  If[MatchQ[spec, Rule[_, _ ? MaybeFnQ]],
    spec = applyDataFn[$ispecItems, spec]];
  Which[
    spec === {},
      scalarResult = resolveFn @ Auto,
    MatchQ[spec, Broadcast[_]],
      scalarResult = finalCheckFn @ First @ spec,
    RuleLVecQ[spec],
      scalarResult = resolveFn @ Replace[DefaultValue, ToList[spec, DefaultValue -> Auto]];
      vectorResult = resolveFn /@ VectorReplace[$ispecItems, ToList[spec, _ -> scalarResult]],
    ListQ[spec],
      scalarResult = resolveFn @ Auto;
      vectorResult = PadRight[resolveFn /@ spec, $ispecLen, scalarResult],
    DictQ[spec],
      scalarResult = resolveFn @ Lookup[spec, DefaultValue, Auto];
      vectorResult = Lookup[spec, $ispecItems, scalarResult],
    MaybeFnQ[spec] && !TrueQ[testFn @ spec],
      scalarResult = resolveFn @ Auto;
      vectorResult = Map[spec /* resolveFn, $ispecItems],
    True,
      scalarResult = resolveFn @ spec
  ];
  If[useBroadcast,
    If[NotNoneQ[vectorResult] && AllSameQ[vectorResult],
      scalarResult = P1 @ vectorResult;
      vectorResult = None
    ],
    SetNone[vectorResult, ConstList[scalarResult, $ispecLen]]
  ];
  result = SubNone[vectorResult, Broadcast[scalarResult, $ispecLen]];
  key -> result
];

applyDataFn[items_, "Name" -> fn_] /; !KeyExistsQ[$ispecItemData, "Name"] := Map[fn, items];
applyDataFn[items_, key_ -> fn_]  := Map[fn, LookupOrThrow[$ispecItemData, key, "missingItemDataKey"]];

General::missingItemDataKey = "No key `` present in provided item data.";

(* TODO: Validated[...] head, i can use above in RuleLVecQ to avoid resolving the scalarResult twice *)
finalCheck[test_, final_][value_] :=
  If[test[value], final[value], ThrowOptionError[$ispecKey, value]];

(**************************************************************************************************)

CanonicalizeSpec[value_, rules__Rule] := CanonicalizationFn[{rules}] @ value;

CanonicalizationFn[rules_, final_:Id][value_] := final @ FixedPoint[Replace[rules], value, 5];

(**************************************************************************************************)

DeclareCurry1[MethodResolutionFn]

MethodResolutionFn[None] := Id;
MethodResolutionFn[spec_, val_]    := val;
MethodResolutionFn[spec_, str_Str] := resolveStrSpec[str, spec];

resolveStrSpec = CaseOf[
  (* $[str_, Rule[spec_, then_]] := then @ $[spec, str]; *)
  $[str_, dict_Dict]          := Lookup[dict, str, throwStrSpec[str, dict]];
  $[str_, fn_]                := SubFailed[fn @ str, throwStrSpec @ str];
];

General::notKnownStringSpec1 = "`` is not a known named setting for ``. Valid named settinngs include ``.";
General::notKnownStringSpec2 = "`` is not a known named setting for ``.";
throwStrSpec[str_]        := ThrowMsg["notKnownStringSpec2", str, $ispecKey];
throwStrSpec[str_, dict_] := ThrowMsg["notKnownStringSpec1", str, $ispecKey, LiteralCommaStringForm @ Keys @ dict];
