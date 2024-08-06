PackageExports[
  "MutatingFunction",
    PutRef, PutRefVal,
  "Function",
    GetRefVal,
    FindRefs, FindRefDepths, ScanRefs, SortRefs, KillRefs,
    ClearRefs, AllRefs,
    RefSort, RefOrdering, RefGraph, RefGraphRules,
  "SpecialHead",
    Ref, RefVal, RefGhost,
  "Predicate",
    RefQ, RefListQ, RefDictQ, RefListDictQ, ContainsRefQ,
  "GraphicsFunction",
    RefPanel, DynamicRefPanel,
  "Symbol",
    RefVisited, RefNovel, RefRoot,
  "CacheVariable",
    $Refs, $RefCols, $RefSortCache, $HashToRef
];

PrivateExports[
  "BoxFunction",
  MakeRefBoxes
];

(**************************************************************************************************)

ClearRefs[] := Then1[Len @ $Refs, $Refs = $RefCols = $HashToRef = UDict[]];

If[!DictQ[$Refs], ClearRefs[]];

AllRefs[] := Keys @ $Refs;

(**************************************************************************************************)

DeclareHoldAllComplete[RefVal]

PutRef[val_] := PutRefVal @ RefVal @ val;

PutRefVal[rval_RefVal] := findRef[rval, Hash @ rval];

findRef[rval_, hash_] := Lookup[$HashToRef, hash, $HashToRef[hash] = makeRef[rval, hash]];
makeRef[rval_, hash_] := With[
  {ref = ConstructNoEntryExpr[Ref, hash]},
  $RefCols[ref] = UniqueColor[Len @ $Refs];
  $Refs[ref] = rval; ref
];

PutRef::badArguments = "Incorrect PutRef: ``.";
PutRefVal::badArguments = "Incorrect PutRef: ``.";

(**************************************************************************************************)

CoreBoxes[ref_Ref ? ExprNoEntryQ] := MakeRefBoxes[ref];

MakeRefBoxes[ref_] := If[!$UseCoreBoxFormatting,
  FnBracketBoxOp["Ref"][hashBoxes @ ref],
  NiceTooltipBox[
    ClickBox[circleBox @ ref, printRefCell @ ref],
    DynamicBox[ColumnBox @ {hashBoxes @ ref, valBoxes @ ref}, TrackedSymbols :> {}]
  ]
];

printRefCell[Ref[hash_]] := PrintInputCell @ Hold @ GetRefVal @ $HashToRef @ hash;
circleBox[ref_] := StyleBox["\[FilledCircle]", Lookup[$RefCols, ref, Gray]];

valBoxes[ref_] := valBoxes2 @ Lookup[$Refs, ref, None];
valBoxes2[None] := "<<Missing>>";
valBoxes2[RefVal[expr_]] := CodePaneBoxes[expr, {UpTo[200], UpTo[50]}];

hashBoxes[_] := "?";
hashBoxes[ref:Ref[hash_Int]] := StyleBox[
  DQuotedString @ Base36String[hash, 6],
  FontWeight -> "SemiBold",
  FontSize -> 12,
  FontColor -> Lookup[$RefCols, ref, Gray],
  FontFamily -> "Source Code Pro", Gray
];

(**************************************************************************************************)

DynamicRefPanel[] := Dynamic[
  Replace[RefPanel[], Except[_Framed] :>  "---"],
  SynchronousUpdating -> False, TrackedSymbols :> {$Refs}
];

RefPanel[] := DisableCoreBoxInteractivity @ Framed[
  RawGrid[
    KeyValueMap[{k, v} |-> {RawBoxes @ hashBoxes @ k, Style[v, "Output"]}, $Refs],
    ColumnAlignments -> Left, ColumnSpacings -> 2, RowSpacings -> 2
  ],
  Background -> White
];

(**************************************************************************************************)

GetRefVal = CaseOf[
  r_Ref     := Lookup[$Refs, r, badKey @ r];
  r:{__Ref} := Lookup[$Refs, r, badKey @ r];
  e_        := ErrorMsg["notRefOrList", e];
];

badKey[r_]     := ErrorMsg[General::missingRef, r];
badKey[r_List] := ErrorMsg[General::missingRef, Compl[Keys @ $Refs, r]];
General::notRefOrList = "Expected Ref or list of these: ``."
General::missingRef = "Failed to find ``."

(**************************************************************************************************)

DeclarePredicate1[RefQ, RefListQ, RefDictQ, RefListDictQ]

RefQ[_Ref ? ExprNoEntryQ]         := True;
RefListQ[{___Ref ? ExprNoEntryQ}] := True;
RefDictQ[d_Dict]                  := DictOfQ[d, RefQ];
RefListDictQ[l_List]              := RefListQ @ l;
RefListDictQ[d_Dict]              := RefDictQ @ l;

ContainsRefQ[expr_] := !FreeQ[expr, _Ref ? ExprNoEntryQ];

(**************************************************************************************************)

FindRefs[expr_]      := Keys @ internalScanRefs[Hold, expr];
FindRefDepths[expr_] := internalScanRefs[Hold, expr];

(**************************************************************************************************)

internalScanRefs[fn_, expr_, root_] := Module[
  {visitDepth = Assoc[], parent = root, visit, value, depth = 0, depth2},
  visit = Function[
    If[ExprEntryQ[#1] || IntQ[Lookup[visitDepth, #1]],
      fn[#1, RefVisited, parent, depth]
    ,
      visitDepth[#1] = depth;
      value = GetRefVal @ #1;
      fn[#1, RefNovel, parent, depth];
      BlockSet[parent, #1, BlockIncrement[depth, FreeQ[value, _Ref ? #0]]]
    ];
    False
  ];
  FreeQ[expr, _Ref ? visit];
  visitDepth
];

(**************************************************************************************************)

RefGraphRules[expr_, root_:None] := Locals[
  minD = If[NoneQ @ root, 1, 0];
  verts = Keys @ Collecting[edges, internalScanRefs[
    {ref, status, parent, d} |-> If[d >= minD, edges[parent -> ref]],
    expr, root
  ]];
  {verts, edges}
];

RefGraph[expr_, root:Except[_Rule]:None, opts___Rule] := Locals[
  {verts, edges} = RefGraphRules[expr, root];
  Graph[verts, edges, opts,
    PlotTheme -> "CoreRefGraph",
    GraphLayout -> {"LayeredDigraphEmbedding", "Orientation" -> Left}
  ]
];

DefineGraphTheme["CoreRefGraph", {
  ThemeParent -> "Core",
  VertexShapeFunction -> "Name"
}];

(**************************************************************************************************)

RefOrdering::usage =
"RefOrdering[{ref$1, ref$2, $$}] returns the ordering that puts the Ref[$$] in canonical order.
* This order depends only on the actual content (RefVal[$$]) of the refs and their \
recurrent topology.
"

(*
given a list of Refs, look up all their values,
for each, obtain a pure shape to hash, and a list of indices of their children.
then, n times, compute new hash for each ref by pulling prior hashes from children, then folding
those hashes into current ref hash. this ensures each ref will have a hash that is uniquely
determined by the shapes of its n-reachable children. since n is the longest possible cycle,
distinguished nodes will have distinguished hashes by then.

we can use these as a canonical order to break ties after sorting by shape. technically the
iterated hahses are enough on their own, but first using shape and then canon hash order is...
safer?
*)

RefSort[refs_List] := Part[refs, RefOrdering @ refs];

(* RefOrdering[refs_List] := CachedTo[$RefOrderingCache, refs, refOrdering @ refs];
$RefOrderingCache = UDict[];
 *)

RefOrdering[refs_List] := refOrdering @ refs;

refOrdering[ourRefs_] := Locals[
  graph = RefGraph[ourRefs, None];
  refs = VertexList @ graph;
  shapeL = Map[val |-> KillRefs[val, TrueFn], GetRefVal @ refs];
  hashL0 = Hash /@ shapeL;
  hashFn = {hash, subHashes} |-> If[subHashes === {}, hash, Hash @ {hash, subHashes}];
  iters = Len[refs] + 1;
  hashL1 = GraphFold[graph, DebugRules[hashFn, {LabelFunction -> hashInts, ImageSize -> 600}], hashL0, -iters];
  ourInd = VertexIndex[graph, ourRefs];
  ourHashL = Part[hashL1, ourInd];
  ourShapeL = Part[shapeL, ourInd];
  Ordering @ Flip @ List[ourShapeL, ourHashL]
];

hashInts[expr_] := expr /. (i_Int /; i > 1000) :> RuleEval @ PixelHash @ i;

(**************************************************************************************************)

KillRefs[expr_, test_] :=
  Locals @ ReplaceAll[i = 1; expr, _Ref ? test :> RuleEval @ RefGhost[i++]];

(**************************************************************************************************)

(*
OLD KNOWN-CORRECT IMPLEMENTATION:

knownRefQ[r_Ref ? ExprNoEntryQ] := KeyExistsQ[$refI, r];

echoHF[f_][a_, b_] := Locals[
  h = f[a, b]; Print @ hashInts[{a, b} -> h];
  h
];

toExprShape[ref_] := ReplaceAll[val, i = 0; _Ref ? knownRefQ :> RuleEval @ RefGhost[i++]];

refOrdering2[refs_] := Locals[
  vals = GetRefVal @ refs;
  $refI = DictRange @ refs;
  {shapeL, hashL, kidsIL} = Flip @ ZipMap[toINode, refs, vals];
  (* Scan[Print, RuleThread[refs, Flip @ {shapeL, hashInts @ hashL, kidsIL}]]; *)
  range = RangeLen @ refs;
  iters = Len[refs] + 1;
  Do[
    hashL = ZipMap[
      {hash, kidsI, i} |-> echoHF[extHash][hash, Part[hashL, kidsI]],
      hashL, kidsIL, range
    ];,
    {iters}
  ];
  hashOrd = Ordering @ Flip[{shapeL, hashL}];
  Ordering @ Flip @ List[shapeL, hashL]
];

extHash[h_, {}] := h;
extHash[h_, l_List] := Hash[{h, l}];
zextHash[h_, _] := h;

toINode[ref_, val_] := Locals[
  shape = ReplaceAll[val, i = 1; _Ref ? knownRefQ :> RuleEval @ RefGhost[i++]];
  hash = Hash @ shape;
  kids = Occs[val, _Ref ? knownRefQ];
  Print[ref -> kids];
  kidsI = Lookup[$refI, kids]; (* TODO: dont just use level 1 kids, use all kids *)
  {shape, hash, kidsI}
];

 *)
