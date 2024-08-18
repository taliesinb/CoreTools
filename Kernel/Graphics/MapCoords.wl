PrivateExports[
  "GraphicsFunction", MapGPrimCoords, MapGBoxCoords, ConvertGBoxesToGPrims,
  "SpecialVariable",  $MapGPrimRules, $MapGBoxRules, $ConvertGPrimToGBoxRules, $ConvertGBoxToPrimRules
];

(**************************************************************************************************)

MapGPrimCoords[{posFn_, posListFn_}, expr_] := Locals[
  $posFn = posFn; $posListFn = posListFn;
  ReplaceAll[expr, $mapGPrimsDispatch]
]

MapGPrimCoords[Threaded[posFn_] | (posFn_ ? ListableFunctionQ), expr_] := Locals[
  $posFn = $posListFn = posFn;
  ReplaceAll[expr, $mapGPrimsDispatch]
]

MapGPrimCoords[posFn_, expr_] := Locals[
  $posFn = posFn; $posListFn = Map[posFn];
  ReplaceAll[expr, $mapGPrimsDispatch]
];

MapGPrimCoords[{x_, y_} ? PosAQ, expr_] :=
  MapGPrimCoords[
    DotRightOp @ ToPacked @ Transpose @ {{1, 0, x}, {0, 1, y}},
    expr
  ];

$mapGPrimsDispatch := $mapGPrimsDispatchActual;

$mapGPrimsDispatchActual := $mapGPrimsDispatchActual = Dispatch @ $MapGPrimRules;

(**************************************************************************************************)

MapGBoxCoords::usage =
"MapGBoxCoords[fn$, boxes$] is like %MapGPrimCoords but operates on primitive boxes."

MapGBoxCoords[args___] := Block[
  {$mapGPrimsDispatch = $mapGBoxesDispatch},
  MapGPrimCoords[args]
];

$mapGBoxesDispatch := $mapGBoxesDispatch = Dispatch @ $MapGBoxRules;

(**************************************************************************************************)

primPattern[str_] := ToAltPattern @ GSigToGPrims[str];

(* TODO: handle rotating a rectangle *)
(* TODO: handle rotating curves! *)
$MapGPrimRules := With[{
  posPos      = primPattern["Pos,Pos"],
  posDel      = primPattern["Pos,PosDelta"],
  posRad      = primPattern["Pos,Radius"],
  posIRad     = primPattern["Pos?Radius"],
  pos         = primPattern["Pos!Radius"],
  posListRad  = primPattern["PosList,Radius | PosPair,Radius | Curve,Radius"],
  posList     = primPattern["PosList!Radius | PosPair!Radius | Curve!Radius"],
  posLists    = primPattern["PosLists?Radius"],
  opaque      = primPattern["Opaque"],
  posRules    = primPattern["PosRules,Primitives"],
  opaquePos   = primPattern["Opaque,Pos|Primitives,Pos"]}, {

  (FmE:opaque)    [FmA___]                              :> FmE[FmA],

  (FmH:posPos)    [FmV:PosAP,      FmW:PosAP,  FmA___]  :> RuleEval @ Make[FmH, $posFn @ FmV, $posFn @ FmW,       FmA],
  (FmH:posDel)    [FmV:PosAP,      FmD:PosAP,  FmA___]  :> RuleEval @ Make[FmH, Seq @@      posDeltaFn[FmV, FmD], FmA],
  (FmH:posRad)    [FmV:PosAP,      FmR_,       FmA___]  :> RuleEval @ Make[FmH, $posFn @ FmV, posRadFn[FmV, FmR], FmA],
  (FmH:posIRad)   [FmV:PosAP]                           :> RuleEval @ Make[FmH, $posFn @ FmV                         ],
  (FmH:pos)       [FmV:PosAP,                  FmA___]  :> RuleEval @ Make[FmH, $posFn @ FmV,                     FmA],

  (FmH:posListRad)[FmM:PosAListP,  FmR_,       FmA___]  :> RuleEval @ Make[FmH, $posListFn  @ FmM, posRadFn[P1 @ FmM, FmR], FmA],
  (FmH:posList)   [FmM:PosAListP,              FmA___]  :> RuleEval @ Make[FmH, $posListFn  @ FmM, fixupNestedCurves @ FmA],

  (FmH:posLists)  [FmN:PosAListsP,             FmA___]  :> RuleEval @ Make[FmH, $posListFn /@ FmN,                            FmA],
  (FmH:posRules)  [FmR_List,       FmP_,       FmA___]  :> RuleEval @ Make[FmH, posRulesFn  @ FmR, FmP /. $mapGPrimsDispatch, FmA],

   Text[FmT_, FmV:PosAP, FmO_,       FmD:PosAP, FmA___] :> RuleEval @ With[{FmQ = posDeltaFn[FmV, FmD]},  Make[Text, FmT, P1 @ FmQ, FmO,      P2 @ FmQ, FmA]],
  Inset[FmT_, FmV:PosAP, FmO_, FmP_, FmD:PosAP, FmA___] :> RuleEval @ With[{FmQ = posDeltaFn[FmV, FmD]}, Make[Inset, FmT, P1 @ FmQ, FmO, FmP, P2 @ FmQ, FmA]],

  (FmH:opaquePos) [FmO_, FmV:PosAP, FmA___]             :> RuleEval @ Make[FmH, FmO, $posFn @ FmV, FmA]

}];

(* we set up this dispatch so that we know (and test) whether to call
$posFn or $posListFn for heads that are dual-use, e.g.
* Line is $posList OR $posLists
* Point is $pos or $posList)
they will get dispatched to the right case based on a pattern test --
order is important to test the more specific cases first
*)

mixedCoords[a_] := !PackedQ[a] && ContainsQ[a, _Real] && ContainsQ[a, _Int];

fixupNestedCurves[e___] := ReplaceAll[
  NoEval[e],
  (FmH:(BezierCurve | BSplineCurve | BSplineCurve | BSplineCurveBox))[FmM_ ? mixedCoords, FmA___] :>
  RuleEval @ Make[FmH, Map[ignoreIntFn[$posFn], FmM], FmA]
];

ignoreIntFn[fn_][i_Int] := i;
ignoreIntFn[fn_][e_] := fn[e];

(**************************************************************************************************)

posRulesFn[e_] := VectorReplace[e, Rule[c:PosAP, o_] :> Rule[$posFn[c], o]];
posDeltaFn[a_, d_] := With[{a1 = $posFn[a]}, {a1, $posFn[a + d] - a1}];

posRadFn[v_, r_] := If[Len[v] == 2, rad2Fn @ r, rad3Fn @ r];

rad2Fn = CaseOf[
  r:NumP            := radDist2[{r, 0}] * Sign[r]; (* the sign is for ElbowCurve, which can take a negative value *)
  {x:NumP, y:NumP}  := {radDist2[{x, 0}] * Sign[x], radDist2[{0, y}] * Sign[y]}; (* for {rx, ry} e.g. CenteredRectangle -- but doesn't do the right thing for horizontal stretching on HorizontalCurve *)
  e_                := e;
];

radDist2[v_] := Dist[$posFn @ v, $posFn @ {0, 0}];

rad3Fn = CaseOf[
  r:NumP                   := radDist3[{r, 0, 0}] * Sign[r];
  {x:NumP, y:NumP, z:NumP} := {radDist3[{x, 0, 0}] * Sign[x], radDist3[{0, y, 0}] * Sign[y], radDist3[{0, 0, z}] * Sign[z]};
  e_                       := e;
];
radDist3[v_] := Dist[$posFn @ v, $posFn @ {0, 0, 0}];

(**************************************************************************************************)

ConvertGBoxesToGPrims[gboxes_] := ReplaceAll[gboxes, $convertGBoxToGPrimDispatch];

$convertGBoxToGPrimDispatch := $convertGBoxToGPrimDispatch = Dispatch @ Normal @ $ConvertGBoxToGPrimRules;

$ConvertGBoxToGPrimRules := Normal @ $GBoxToGPrim;

(**************************************************************************************************)

$ConvertGPrimToGBoxRules := Block[{nonBoxablePrims},
  nonBoxablePrims = Complement[$GPrimSyms, Keys @ $GPrimToGBoxes, GSigToGPrims["Primitives?Pos"]];
  Join[
    Normal[P1 /@ $GPrimToGBoxes],
    Map[Blank[#] -> Null&, nonBoxablePrims]
  ]
];

$MapGBoxRules := Locals[
  rules = Normal @ $mapGPrimsDispatchActual;
  boxifyingRules = $ConvertGPrimToGBoxRules;
  rules //= ReplaceAll[boxifyingRules];
  rules //= ReplaceAll[Verbatim[Alt][a_] :> a];
  rules //= Select[FreeQ[Verbatim[Alt[]]]];
  rules //= Map[addBoxConstruct];
  rules // simplifyRules
];

addBoxConstruct[other_] := other;
addBoxConstruct[RuleDelayed[lhs_, RuleEval[h[args___]]]] :=
  RuleDelayed[lhs, RuleEval @ Construct[h, args]];

(**************************************************************************************************)

ExtractPrimitiveCoordinates::usage = "ExtractPrimitiveCoordinates[g$] returns a list of primitive coordinates.";
ExtractPrimitiveBoxCoordinates::usage = "ExtractPrimitiveBoxCoordinates[g$] returns a list of primitive box coordinates.";

ExtractPrimitiveCoordinates[prims_] := iExtractCoords[prims, MapGPrimCoords];
ExtractPrimitiveBoxCoordinates[prims_] := iExtractCoords[prims, MapGBoxCoords];

iExtractCoords[prims_, fn_] := Locals[
  points = Bag[];
  fn[{StuffBag[points, #]&, StuffBag[points, #, 1]&}, prims];
  BagPart[points, All]
];
