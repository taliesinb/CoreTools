SystemExports[
  "Head",
    Repeating, Under, Over,
  "OptionSymbol",
    ItemGroups,
    GroupSettings,
    Broadcasting,
    UseBroadcast,
    Canonicalization,
    MethodResolution
];

PackageExports[
  "Function",
    ParsePadding,
    ParsePart,
    ParseListSpec,
    ParseCyclicSpec,
    LookupSide,
    ParseAlignment,
    ParseItemOptions,
    BoundsToSize,
  "Variable",
    $SideToCoords,
    $CoordsToSide,
    $SideToRadians
    $SideToUnitCoords,
  "Head",
    ItemSpec,
  "Operator",
    MethodResolutionFn,
    CanonicalizationFn
];

(**************************************************************************************************)

ParsePadding::usage =
"ParsePadding[spec$] standardizes a padding specification spec$.
* ParsePadding returns {{l$, r$}, {b$, t$}}.
* ParsePadding accepts the following forms:
| None | no padding |
| n$ | pad by n$ on all sides |
| {h$, v$} | pad by h$ horizontally and v$ vertically |
| {{l$, r$}, {b$, t$}} | explicit padding |
| {Left -> l$, $$} | per-side padding |
* Sides can be Horizontal or Vertical to indicate both sides.
"

ParsePadding = CaseOf[
  All                          := All;
  None                         := {{0, 0}, {0, 0}};
  p:NumP                       := N @ {{p, p}, {p, p}};
  c:Num2P                      := N @ c;
  s:{Num2P, Num2P}             := N @ s;
  rule_Rule                    := % @ {rule};
  rules:{Rule[sideP, NumP]...} := N @ LookupSide[rules, {{Left, Right}, {Bottom, Top}}];
  _                            := $Failed;
,
  {sideP -> ExtSideP|Horizontal|Vertical|All}
];

(**************************************************************************************************)

LookupSide[rules_, sides_List] :=
  Map[LookupSide[rules, #]&, sides];

LookupSide[rules_, side_] :=
  Lookup[rules, side, Lookup[rules, toSideClass @ side, Lookup[rules, All, 0]]];

LookupSide[rules_, side_] :=
  Lookup[rules, side,
  Lookup[rules, toSideClass @ side,
  Lookup[rules, toMultiClassC @ side,
  Lookup[rules, toMultiClassA @ side,
  Lookup[rules, All, 0]]]]];

toMultiClassC = <|Bottom -> BottomLeft, Left -> TopLeft, Top -> TopRight, Right -> BottomRight|>;
toMultiClassA = <|Bottom -> BottomRight, Left -> BottomLeft, Top -> TopLeft, Right -> TopRight|>;

LookupSide[rules_][side_] :=
  LookupSide[rules, side];

toSideClass = CaseOf[
  Left|Right := Horizontal;
  Bottom|Top := Vertical;
  other_     := Null;
]

(**************************************************************************************************)

$SideToCoords = <|
  Left        -> {-1,  0},
  Right       -> { 1,  0},
  Top         -> { 0,  1},
  Above       -> { 0,  1},
  Bottom      -> { 0, -1},
  Below       -> { 0, -1},
  BottomLeft  -> {-1, -1},
  BottomRight -> { 1, -1},
  TopLeft     -> {-1,  1},
  TopRight    -> { 1,  1},
  Center      -> { 0,  0}
|>

$CoordsToSide = InvertAssociation @ KeyDrop[$SideToCoords, {Below, Above}];

$SideToUnitCoords = Map[(# + 1)/2.&, $SideToCoords]

$SideToRadians = <|
  Left        ->  4/4 * Pi,
  TopLeft     ->  3/4 * Pi,
  Top         ->  2/4 * Pi,
  TopRight    ->  1/4 * Pi,
  Right       ->  0,
  BottomRight -> -1/4 * Pi,
  Bottom      -> -2/4 * Pi,
  BottomLeft  -> -3/4 * Pi
|>;

(**************************************************************************************************)

SetStrict @ ParseItemOptions;

ParseItemOptions::usage =
"ParseItemOptions[head$, descriptions$, items$, spec$, opts$] yields an association.
* each description is a ItemSpec[$$], see its usage for more info.
* if Broadcasting is True or Auto, the result may be a Broadcast[value, len], or a list.
* if Broadcasting is False, the result is always a list.
* ItemGroups -> symbol$g specifies that symbol$ the option key used to obtain item groups.
* GroupSettings -> symbol$s specifies that symbol$ the option key used to obtain settings for these groups.
"

Options[ParseItemOptions] = {
  UseBroadcast  -> True,
  ItemGroups    -> None,
  GroupSettings -> GroupSettings
};

ParseItemOptions[head_, itemSpecs_List, items_, userOpts_, metaOpts___Rule] := Locals[
  UnpackOptionsAs[ParseItemOptions, metaOpts, $useBroadcast, groupSettings, itemGroups];
  $ispecHead = head;
  $ispecItems = items;
  $ispecBcast = UDict[];
  $ispecOpts = If[DictQ @ userOpts, userOpts, UDict @ Rev @ userOpts]; (* TODO: RuleDict *)
  $ispecLen = Len @ items;
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
  UnpackOptionsAs[ItemSpec, opts, canonicalization, methodResolution, useBroadcast];
  SetInherit[useBroadcast, $useBroadcast];
  AssociateTo[$ispecBcast, key -> useBroadcast];
  $ispecKey = key;
  spec = Lookup[$ispecOpts, key, Inherited];
  inheritValue = OptionValue[$ispecHead, key];
  methodResolution //= MethodResolutionFn;
  specRules = ToList[canonicalization, Auto -> defaultValue, Inherited -> inheritValue, FnRule[_String, methodResolution]];
  resolveFn = CanonicalizationFn[specRules, finalCheck[testFn, finalFn]];
  vectorResult = None;
  Which[
    spec === {},
      scalarResult = resolveFn @ Auto,
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

(* TODO: Validated[...] head, i can use above in RuleLVecQ to avoid resolving the scalarResult twice *)
finalCheck[test_, final_][value_] :=
  If[test[value], final[value], ThrowOptionError[$ispecKey, value]];

(**************************************************************************************************)

Canonicalize[value_, rules__Rule] := CanonicalizationFn[{rules}] @ value;

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

(**************************************************************************************************)

DeclareStrict[BoundsToSize]

BoundsToSize[{{x1_, x2_}, {y1_, y2_}}] := {x2 - x1, y2 - y1};

(**************************************************************************************************)

ParsePart::invalidPartSpecification = "`` is not a valid part specification.";
With[{sp = sP = Except[0, _Int | All]},
ParsePart = CaseOf[
  $[_,                      0]     := {};
  $[All,                    n_Int] := Range[n];
  $[i_Int,                  n_Int] := spanL[n, {i}];
  $[is_List ? IntVecQ,      n_Int] := spanL[n, is];
  $[Span[a:sP],             n_Int] := span1[n, a];
  $[Span[a:sP, b:sP],       n_Int] := span2[n, a, b];
  $[Span[a:sP, b:sP, c:sP], n_Int] := span3[n, a, b, c];
  $[spec_,                  n_Int] := (Message[ParsePart::invalidPartSpecification, spec]; {});
]];

spanL[n_, p_ ? PosIntVecQ] := Select[p, LessEqualThan[n]];
spanL[n_, p_]              := Map[i |-> Which[-n <= i < 0, i + n + 1, 0 < i <= n, i, True, Nothing], p];

span1[n_, All]            := Range[n];
span1[n_, a_ ? Negative]  := Range[Max[n + a + 1, 1], n];
span1[n_, a_ ? Positive]  := Range[1, Min[n, a]];

span2[n_, a_, b_]         := clippedRange[n, clipL[n, a], clipR[n, b], 1];
span3[n_, a_, b_, c_]     := clippedRange[n, clipL[n, a], clipR[n, b], c];

clipL[n_, All]            := 1;
clipL[n_, x_ ? Negative]  := n + x + 1;
clipL[n_, x_ ? Positive]  := x;

clipR[n_, All]            := n;
clipR[n_, x_ ? Negative]  := n + x + 1;
clipR[n_, x_ ? Positive]  := x

(**************************************************************************************************)

clippedRange[n_, a_, b_, All] :=
  clippedRange[n, a, b, If[b < a, -1, 1]];

clippedRange[n_, a_, b_, c_ ? Negative] := If[
  a < b || a < 1 || b > n, {},
  Range[Clip[a, {1, n}], Clip[b, {1, n}], c]
];

clippedRange[n_, a_, b_, c_] := If[
  a > b || a > n || b < 1, {},
  Range[Clip[a, {1, n}], Clip[b, {1, n}], c]
];

(**************************************************************************************************)

General::badAligOrCoords = "Setting `` -> `` is not a symbolic side or coordinate."

ParseAlignment = CaseOf[
  Seq[{x:NumP, y:NumP}, _, _] := {x, y};
  Seq[side_Symbol, h_, o_]    := Lookup[$SideToUnitCoords, side, Message[MessageName[h, "badAligOrCoords"], o, side]; {0, 0}];
  Seq[spec_, h_, o_]          := (Message[MessageName[h, "badAligOrCoords"], o, spec]; {0, 0})
]

(**************************************************************************************************)

ParseListSpec::usage =
"ParseListSpec[spec$, n$] fills out a vector of n$ specs from a simple list specification spec$.
ParseListSpec[n$] is the operator form of ParseListSpec.
* if spec$ is a non-list, it is simply repeated.
* if spec$ is a list, the last element is repeated as necessary.
"

General::noSpecItemToRepeat = "Empty specification.";

ParseListSpec[item_, n_Int]     := ConstList[item, n];
ParseListSpec[{}, n_]           := ThrowMsg["noSpecItemToRepeat"];
ParseListSpec[list_List, n_Int] := PadRight[list, n, Last @ list];

(**************************************************************************************************)

ParseCyclicSpec::usage =
"ParseCyclicSpec[spec$, n$] fills out a vector of n$ specs from a cyclic specification spec$.
ParseCyclicSpec[n$] is the operator form of ParseCyclicSpec.
* it mirrors how e.g. %RowAlignments is implemented for %GridBox.
* Automatic will be used to fill if a too-short list is given.
* $Failed will be returned if it is an invalid spec.
* the more helpful spec %ConstListing can be used.
* additionally, a list of rules can be used that send positions to specs.
"

DeclareCurry2[ParseCyclicSpec]

ParseCyclicSpec[spec_, n_] := Locals[$n = n; parseCyclic @ spec];

parseCyclic = CaseOf[
  rules:RuleVecP                 := VectorReplace[Range @ $n, Append[compNeg /@ rules, _ -> Auto]];
  spec:{DatumP..}                := PadRight[spec, $n, Auto];
  Repeating[mid__]               := repSpec[{}, {mid}, {}];
  spec:DatumP                    := ConstList[spec, $n];
  {{spec:DatumP}}                := ConstList[spec, $n];
  {l___, {}, r___}               := repSpec[{l}, {Auto}, {r}];
  {l___, Repeating[mid__], r___} := repSpec[{l}, {mid}, {r}];
  {l___, mid:{DatumP..}, r___}   := repSpec[{l}, mid, {r}];
  _                              := $Failed;
];

compNeg[head_[lhs_, rhs_]] := With[
  {lhs2 = lhs /. n_Int ? Negative :> (n + $n + 1)},
  head[lhs2, rhs]
];

repSpec[l_, mid_, r_] := Locals[
  n2 = $n - Len[l] - Len[r];
  If[n2 < 0, Return @ parseCyclic @ Join[l, r]];
  mid2 = TakeOp[n2] @ Catenate @ ConstList[mid, Ceiling[n2 / Len[mid]]];
  Join[l, mid2, r]
];

