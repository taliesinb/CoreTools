SystemExports[
  "Head",
    Repeating, Under, Over
];

PackageExports[
  "Function",
    ParsePadding,
    ParsePart,
    ParseListSpec,
    ParseCyclicSpec,
    ParseSide,
    LookupSide,
    ParseAlignment,
    ParseItemOptions,
    BoundsToSize,
    SideToRadians,
  "Variable",
    $SideToCoords,
    $CompassToCoords,
    $ExtSideToCoords,
    $SideToSidePair,
    $SideToRadians,
    $SideToUnitCoords,
    $SymbolicPointSizes
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
  rules:{Rule[side, NumP]...}  := N @ LookupSide[rules, {{Left, Right}, {Bottom, Top}}];
  _                            := $Failed;
,
  {side -> ExtSideP|Horizontal|Vertical|All}
];

(**************************************************************************************************)

(* TODO: allow Lef, Top, etc to specify *part* of an existing side pair *)

ParseSide[side_Sym] :=
  Lookup[$SideToSidePair, side, ThrowMsg["invalidSide", side]];

$SideToSidePair = Dict[
  Lef   -> {Lef, Cen},
  Rig   -> {Rig, Cen},
  Top   -> {Cen, Top},
  Bot   -> {Cen, Bot},
  Cen   -> {Cen, Cen},
  TopL  -> {Lef, Top},
  BotL  -> {Lef, Bot},
  TopR  -> {Rig, Top},
  BotR  -> {Rig, Bot},
  Above -> {Top, Cen},
  Below -> {Bot, Cen}
];

General::invalidSide = "`` is not one of Top, Left, etc.";

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

$CompassToCoords = Dict[
  TopL   -> {-1,  1},
  TopC   -> { 0,  1},
  TopR   -> { 1,  1},
  CenR   -> { 1,  0},
  BotR   -> { 1, -1},
  BotC   -> { 0, -1},
  BotL   -> {-1, -1},
  CenL   -> {-1,  0}
];

(**************************************************************************************************)

$SideToCoords = Dict[
  Top    -> { 0,  1},
  Rig    -> { 1,  0},
  Bot    -> { 0, -1},
  Lef    -> {-1,  0},
  Cen    -> { 0,  0}
];

(**************************************************************************************************)

$ExtSideToCoords = Dict[
  $CompassToCoords,
  $SideToCoords,
  Above  -> { 0,  1},
  Below  -> { 0, -1}
];

(**************************************************************************************************)

$SideToUnitCoords = Map[(# + 1)/2.&, $ExtSideToCoords]

(**************************************************************************************************)

$SymbolicPointSizes = Dict[
  Tiny        -> 2,
  Small       -> 3,
  (* MediumSmall -> 4, *)
  Medium      -> 5,
  (* MediumLarge -> 6, *)
  Large       -> 7
  (* Huge        -> 10 *)
];

(**************************************************************************************************)

SetStrict[SideToRadians];

SideToRadians[side:(_Symbol | _List)] := Lookup[$SideToRadians, side, ThrowMsg["badSide", side]];

$SideToRadians = Dict[
  Left        ->  4/4 * Pi,
  TopLeft     ->  3/4 * Pi,
  Top         ->  2/4 * Pi,
  TopRight    ->  1/4 * Pi,
  Right       ->  0,
  BottomRight -> -1/4 * Pi,
  Bottom      -> -2/4 * Pi,
  BottomLeft  -> -3/4 * Pi
];

(**************************************************************************************************)

SetStrict[BoundsToSize]

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

SetCurry2[ParseCyclicSpec]

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

