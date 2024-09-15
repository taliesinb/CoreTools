PackageExports[
  "Head",
    Trie,
  "Function",
    TrieNew,
    TrieLeaves, TriePart, TrieCount,
    TrieScanNodes, TrieScanLeaves,
    TrieLists,
    ExpandTrieNodes,
  "MutatingFunction",
    TrieAdd,
  "FormHead",
    TrieExprForm, TrieGridForm,
  "Predicate",
    TrieSameQ, TrieEqualQ,
    TrieQ, HoldTrieQ
];

PrivateExports[
  "Variable",         $EnableTrieFormatting,
  "Function",         NewTNode, LeafTNode, NewTChain,
  "Function",         TCount, TLeafs,  TEntries,
  "MutatingFunction", TIncrement, TInsert
];

(*************************************************************************************************)

SetPred1[HoldTrieQ, TrieQ];
SetHoldC[HoldTrieQ];

HoldTrieQ[Trie[_Store ? StoreQ]] := True;
    TrieQ[Trie[_Store ? StoreQ]] := True;

(*************************************************************************************************)

(* DefineSimpleMacro[TCount,     TCount[t_]         :> StoreGet[t, Tries`C]]
DefineSimpleMacro[TLeaf,    TLeaf[t_]        :> StoreGet[t, Tries`L]]
 *)
CoreBoxes[Tries`C] := StyleBox["C", $Gray];
CoreBoxes[Tries`L] := StyleBox["L", $Red];

(*************************************************************************************************)

SetStrict[TEntries, TCount, TLeafs];

TCount[t_]    := StoreGet[t, Tries`C];
TCount[None]  := 0;

TLeafs[t_]   := BagPart[StoreGet[t, Tries`L], All];
TLeafs[None] := {};

TEntries[node_Store] := KeyDrop[StoreDict @ node, {Tries`L, Tries`C}];
TEntries[None]       := UDict[];

(*************************************************************************************************)

TIncrement[node_, count_] :=
  Then[StoreSet[node, Tries`C, StoreGetRaw[node, Tries`C] + count], node];

TInsert[node_, ids_, count_] := Then[
  StuffBag[StoreGet[node, Tries`L], ids, 1],
  TIncrement[node, count]
];

(*************************************************************************************************)

NewTNode[ids_, count_, subs___] := StoreNew[{{Tries`L, Internal`Bag[ids]}, {Tries`C, count}, subs}];
LeafTNode[ids_]                 := NewTNode[ids, Len @ ids];
UnaryTNode[key_, sub_, count_]  := NewTNode[{}, count, {key, sub}];

(*************************************************************************************************)

NewTChain[seq_, ids_]          := NewTChain[seq, ids, Len @ ids];
NewTChain[{}, ids_, count_]    := NewTNode[ids, count];
NewTChain[seq_, ids_, count_]  := Fold[chainStep[count], NewTNode[ids, count], Reverse @ seq];
chainStep[count_][node_, key_] := NewTNode[{}, count, {key, node}];

(*************************************************************************************************)

TrieNew[] := Trie @ NewTNode[{}, 0];

TrieNew[list_List] := Locals[
  node = NewTNode[{}, 0];
  ScanIndexed[trieAdd[node, #1, #2]&, list];
  Trie @ node
];

(* TODO: optimized implementation that sorts the list once, and steps through it,
maintaining a stack of nodes. each new list element is compared from the left to current prefix, when
we hit a change, we create a singelton for the remainder and add it to that stack slot.
*)

(*************************************************************************************************)

TrieAdd[trie:Trie[node_], seq_List]       := Then[trieAdd[node, seq, TCount[node] + 1], trie];
TrieAdd[trie:Trie[node_], seq_List, ids_] := Then[trieAdd[node, seq, ids], trie];

toCount[ids_List] := Len @ ids;
toCount[_] := 1;

trieAdd[node_, {}, ids_]   := TInsert[node, ids, toCount @ ids];
trieAdd[node_, seq_, ids_] := Block[
  {$ids = ids, $cnt = toCount @ ids, $node = node, $val},
  iTrieAdd @ seq
];

iTrieAdd[{}]      := TInsert[$node, $ids, $cnt];
iTrieAdd[es_]     := iTrieAdd[First @ es, Rest @ es];
iTrieAdd[e1_, er_] := Then[
  TIncrement[$node, $cnt],
  If[StoreQ[$next = StoreGet[$node, e1]],
    $node = $next; iTrieAdd @ er,
    StoreSet[$node, e1, NewTChain[er, $ids, $cnt]]
  ]
];

(*************************************************************************************************)

TrieLeaves[Trie[node_], list_List] := Block[
  {$leaves},
  Fold[collectLeaves, $node, list];
  BagPart[$leaves, All]
];

collectLeaves[None, _] = None;
collectLeaves[node_, key_] := Then[
  StuffBag[$leaves, TLeafs @ node, 1],
  StoreGet[node, key]
];

(*************************************************************************************************)

TrieScan[fn_, Trie[node_]]            := trieScanP[fn, node];
TrieScan[fn_, Trie[node_], path_List] := trieScanP[fn, triePart[node, path]];

trieScanP[fn_, node_]         := Locals[$fn = fn; $path = path; scanNode @ trie]
scanNode[node_]               := Then[$fn[node, $path], StoreScan[node, scanEntries]];
scanEntries[key_, node_Store] := Block[{$path = Append[$path, key]}, nodeScan @ node];

(*************************************************************************************************)

TrieLists[Trie[node_]]            := trieSeqs @ node;
TrieLists[Trie[node_], path_List] := trieSeqs @ triePart[node, path];

trieSeqs[None] := {};
trieSeqs[node_] := Module[{bag},
  $lists = Bag[]; $path = {};
  nodeSeqs @ node;
  BagPart[$lists, All]
];

nodeSeqs[node_] := StoreScan[entrySeqs, node];

entrySeqs = CaseOf[
  $[Tries`L, b_]  := If[BagLength[b] > 0, StuffBag[$lists, $path -> BagPart[b, All]]];
  $[Tries`C, _]   := Null;
  $[key_, node_]  := Block[{$path = Append[$path, key]}, nodeSeqs @ node];
];

(*************************************************************************************************)

TriePart[trie_Trie, {}]          := trie;
TriePart[Trie[node_], path_List] := toTrie @ triePart[node, path];

toTrie[None] := None;
toTrie[node_] := Trie @ node;

triePart[None, _]        := None;
triePart[node_, {}]      := node;
triePart[node_, {e1_}]   := StoreGet[node, e1];
triePart[node_, es_List] := Fold[StoreGet, node, es];

(*************************************************************************************************)

TrieCount[Trie[node_]]            := TCount @ trie;
TrieCount[Trie[node_], path_List] := TCount @ triePart[node, path];

(*************************************************************************************************)

SetPred2[TrieSameQ, TrieEqualQ]

 TrieSameQ[Trie[n1_], Trie[n2_]] := nodeSameQ[n1, n2];
TrieEqualQ[Trie[n1_], Trie[n2_]] := nodeEqualQ[n1, n2];

nodeSameQ[a_, b_]  := compareNodes[nodeSameQ, False, a, b];
nodeEqualQ[a_, b_] := compareNodes[nodeEqualQ, True, a, b];

compareNodes[fn_, ignoreLeaves_, a_, b_] := And[
  SameQ[TCount @ a, TCount @ b],
  ignoreLeaves || SameQ[TLeafs @ a, TLeafs @ b],
  ZipAllTrueQ[fn, TEntries @ a, TEntries @ b]
];

(*************************************************************************************************)

SetInitial[$EnableTrieFormatting, True];

CoreBoxes[t_Trie ? HoldTrieQ] := If[$EnableTrieFormatting,
  NiceObjectBoxes["TNode", {trieGridBoxes @ t}, .2],
  trieExprBoxes @ t
];

CoreBoxes[TrieGridForm[t_Trie ? HoldTrieQ]] := trieGridBoxes @ t;

CoreBoxes[TrieExprForm[expr_]] := Block[{$EnableTrieFormatting = False}, MakeBoxes @ expr];

(*************************************************************************************************)

ExpandTrieNodes[expr_] := ExpandBags @ ExpandStores @ expr;

trieExprBoxes[Trie[node_Store]] :=
  FnBracketBoxOp["Trie"][ToBoxes @ ExpandTrieNodes @ node];

(*************************************************************************************************)

trieGridBoxes[t_Trie ? HoldTrieQ] := Block[
  {$ngDepth = 0, $ngHeight = 0, $ngMaxHeight = 8},
  nodeGridBoxes[1, Dict[
    Tries`C -> Null,
    Tries`L -> Bag[],
    "\[FilledSmallCircle]" -> First[t]
  ]]
];

nodeGridBoxes[e_] := NiceTooltipBox[StyleBox["invalid", Red], ToBoxes @ Head[e]];

nodeGridBoxes[node_Store] := nodeGridBoxes[TCount @ node, StoreDict @ node];

nodeGridBoxes[counts_, dict_Dict] := Locals @ BlockIncrement[$ngDepth,
  If[counts === 0, Return @ "\[EmptySquare]"];
  If[$ngDepth > 8, Return @ "\[Ellipsis]"];
  $ngHeight = 0;
  entries = KeyDrop[dict, {Tries`L, Tries`C}];
  entries = KeyValueMap[trieGridSubnodeBox, entries];
  leafs = BagPart[dict[Tries`L], All];
  If[leafs =!= {},
    leafStrs = ToString /@ Take[leafs, UpTo[8]];
    If[Len[leafs] > 8, AppendTo[leafStrs, ".."]];
    leafBox = StyleBox[SansFontBox @ StrJoin @ Riffle[leafStrs, " "], $LightRed];
    AppendTo[entries, {leafBox, "\[SpanFromLeft]"}];
  ];

  GridBox[
    entries,
    ColumnAlignments -> {{Left}}, GridFrameMargins -> 0,
    RowAlignments -> {{Baseline}},
    ColumnSpacings -> 1, RowSpacings -> Join[ConstList[1, Len[ns]], {0}],
    BaselinePosition -> {{1,1}, Baseline}
  ]
];

trieGridSubnodeBox[key_, node_] := Locals[
  If[$ngHeight > $ngMaxHeight, Return @ Nothing];
  subGrid = Block[
    {$ngMaxHeight = $ngMaxHeight - $ngHeight},
    nodeGridBoxes @ node
  ];
  subHeight = If[MatchQ[subGrid, _GridBox], Len @ P1 @ subGrid, 1];
  $ngHeight += subHeight;
  counts = TCount @ node;
  List[
    SubscriptBox[BoldBox @ ToBoxes @ key, ToBoxes @ counts],
    subGrid
  ]
];
