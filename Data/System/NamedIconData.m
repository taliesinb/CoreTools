SetStrict @ makeIconData;

makeIconData[$bounded$[prims_, a_, ___], key_] :=
  makeIconData[prims, key, a];

makeIconData[prims_, key_] :=
  makeIconData[prims, key, {$u, $u, $u}];

makeIconData[prims2_, name_, align_] := Locals[
  prims = N @ prims2;
  Check[
    boxes = ToGraphicsBoxes @ prims;
    boxes3D = ToGraphics3DBoxes @ prims;
    solid = solidPrimitiveQ @ prims;
  ,
    ErrorPrint["Error while generating icon: ", name];
  ];
  GIconData[prims, boxes, boxes3D, align, solid]
];

(**************************************************************************************************)

solidPrimitiveQ = CaseOf[
  e_List       := $ @ P1 @ e;
  _FilledCurve := True;
  _Polygon     := True;
  _Disk        := True;
  _Rectangle   := True;
  _            := False;
]

(**************************************************************************************************)

SetListable[makeArrowHead];
makeArrowHead[name_ -> head_[upperPath2_]] := Locals[
  upperPath = upperPath2;
  (* center the primitive *)
  {x1, x2} = MinMax @ Col1[upperPath];
  dx = -Avg[x1, x2];
  upperPath = VecTrans[{dx, 0}] @ upperPath;
  lowerPath = VecReflectV @ upperPath;
  symmetricCurve = toJoinedCurve[head @ upperPath, head @ Rev @ Most @ lowerPath];
  {x, {y1, y2}} = CoordinateBounds @ upperPath;
  l = If[Part[upperPath, 1, 2] == 0, P11[upperPath], P1 @ x];
  r = If[Part[upperPath, -1, 2] == 0, PN1[upperPath], PN @ x];
  lr = {l, r};
  List[
    name                    -> $bounded$[symmetricCurve, {x, {-y2, y2}, lr}],
    StrJoin["Upper", name]  -> $bounded$[head[upperPath], {x, {0, y2}, lr}],
    StrJoin["Lower", name]  -> $bounded$[head[lowerPath], {x, {-y2, 0}, lr}]
  ]
];

simplifyLine = CaseOf[
  Line[{l___, a_, a_, r___}]        := $ @ Line @ {l, a, r};
  $[Line[{f_, m___, l_}] /; f == l] := EmptyPolygon @ {f, m};
  other_                            := other;
];

toJoinedCurve[Line[c1_], Line[c2_]] := simplifyLine @ Line @ Join[c1, c2];
toJoinedCurve[JoinedCurve[list_], JoinedCurve[list2_]] := JoinedCurve[Join[list1, list2]];
toJoinedCurve[JoinedCurve[list_], elem_] := JoinedCurve[Append[list, elem]];
toJoinedCurve[c1_, c2_] := JoinedCurve @ {c1, c2};

(**************************************************************************************************)

SetListable[makeClosed];

makeClosed[name_ -> (curve_ ? isClosedQ)] := name -> curve;

makeClosed[name_ -> $bounded$[curve_, b_]] := List[
  name                    -> $bounded$[curve, ReplacePart[b, {3, 1} -> Part[b, 3, 2]]],
  StrJoin["Closed", name] -> $bounded$[toClosedCurve[curve], b]
];

toClosedCurve[curve_] := Locals[
  p1 = curvePart[curve, 1];
  pn = curvePart[curve, -1];
  Switch[{p1, pn},

    (* symmetric *)
    {{x_, _?Positive}, {x_, _?Negative}},
      If[Head[curve] === JoinedCurve,
        Append[curve, CurveClosed -> True],
        toJoinedCurve[curve, Line @ {p1}]
      ],

    (* open *)
    {_,  {_, 0|0.}},
      toJoinedCurve[curve, Line @ {{P1 @ p1, 0}, p1}],

    _,
    None
  ]
];

(**************************************************************************************************)

SetListable[makeFilled]

makeFilled[rule_] := rule;

makeFilled[name_ -> curve_ ? isClosedQ] := List[
  name                       -> curve,
  StrJoin["Filled", StrTrimL[name, "Closed"]] -> toFilledCurve[curve]
];

toFilledCurve = CaseOf[
  $bounded$[c_, a_] := $bounded$[toFilledCurve @ c, a];
  l_Line            := Polygon @@ simplifyLine @ l;
  p_Polygon         := p;
  e_EmptyPolygon    := Polygon @@ e;
  b_BezierCurve     := FilledCurve @ List @ b;
  j_JoinedCurve     := FilledCurve @@ j;
];

(**************************************************************************************************)

isClosedQ = CaseOf[
  $bounded$[c_, _]                    := $ @ c;
  JoinedCurve[_, CurveClosed -> True] := True;
  _Polygon | _EmptyPolygon            := True;
  curve_                              := curvePart[curve, 1] == curvePart[curve, -1];
];

curvePart = CaseOf[
  Seq[JoinedCurve[elems_], part_] := $[Part[elems, part], part];
  Seq[head_[path_], part_]        := Part[path, part]
];

(**************************************************************************************************)

$arrowIcons := With[
  {cr$0 = 0.00000000, cr$1 = 1.00005519, cr$2 = 0.55342686, cr$3 = 0.99873585},
  {hookPoints = {{0, 1-cr$1}, {-cr$2, 1-cr$3}, {-cr$3, 1-cr$2}, {-cr$1, 1}, {-cr$3, 1+cr$2}, {-cr$2, 1+cr$3}, {cr$0-0.2, 1+cr$1}}/2}, {

  makeFilled @ makeClosed @ makeArrowHead @ {
  "CurvedArrow"        -> BezierCurve @ {{-1.0, 1.0}, {-0.9, 0.25}, {0, 0}},
  "GentleCurveArrow"   -> BezierCurve @ {{-1.0, 1.0}, {-0.5, 0.25}, {0, 0}},
  "Triangle"           -> Line @ {{-0.724145, 1.}, {1., 0.}},
  "NarrowTriangle"     -> Line @ {{-0.724145, 0.724145}, {1., 0.}},
  "Arrow"              -> Line @ {{-1, 1}, {0, 0}},
  "Diamond"            -> Line @ {{-1, 0}, {0, 0.6}, {1, 0}},
  "Kite"               -> Line @ {{-0.53, 0}, {-1., 0.89}, {1., 0}}
  },

  setRightAligned @ makeArrowHead @ {
  "Hook"               -> BezierCurve @ Rev @ hookPoints
  }
}];

(**************************************************************************************************)

SetListable[makeRotated]
makeRotated[name_ -> $bounded$[curve_, b_, bt_]] := List[
  name -> $bounded$[curve, b],
  StrRep[name, "Right" -> "Left"] -> $bounded$[ScalePrimitives[curve, {-1, 1}], boundsFlipX @ b],
  StrRep[name, "Right" -> "Up"]   -> $bounded$[RotatePrimitives[curve, Pi/2],   boundsRot[b, bt]],
  StrRep[name, "Right" -> "Down"] -> $bounded$[RotatePrimitives[curve, -Pi/2],  boundsRot[boundsFlipX @ b, bt]]
];

flip[{x_, y_}] := -{y, x};
boundsRot[{bx_, by_, _}, bt_] := {by, bx, bt};
boundsFlipX[b_] := flip /@ b;

$hline = Line @ {{-1, 0}, {1, 0}};
$leftBar = {{-1, -1/2}, {-1, 1/2}};
$rightBar = {{1, -1/2}, {1, 1/2}};
$leftArrowheadPoints = {{-1/2, 1/2}, {-1, 0}, {-1/2, -1/2}};
$rightArrowheadPoints = {{1/2, 1/2}, {1, 0}, {1/2, -1/2}};
$leftRightPrim = Line @ {P1 @ $hline, $leftArrowheadPoints, $rightArrowheadPoints};

$rbaH = {-0.634471, 0.635029};

$rotatedIcons := makeRotated @ {
  "RightArrow"         -> bound[$u, $h, $u, $z] @ Line @ {P1 @ $hline, $rightArrowheadPoints},
  "BarRightArrow"      -> bound[$u, $h, $u, $z] @ Line @ {$leftBar, P1 @ $hline, $rightArrowheadPoints},
  "RightHalfCircle"    -> bound[$r, $u, $r, $u] @ HalfCircle[{0,0}, {1, 0}],
  "RightHalfDisk"      -> bound[$r, $u, $r, $u] @ HalfDisk[{0,0}, {1, 0}],
  "BoldRightArrow"     -> bound[$u, $rbaH, $u, $z] @ Polygon @ $boldRightArrow
};

$boldRightArrow := $boldRightArrow = Uncompress @ "
1:eJxTTMoPSmViYGDQB2IQDQEf9vfa9LTc5pu1/6D791dphe/3C/Vt+Pv34Kb9x+/Kn0tPfbdfwui4js
jePfstL9lsDP3zZv8Cha9tqQsO7v+7PqZ+b+Lr/YJg9Yf2p24JPXye8YE9jN90s9GB//AF+8BrjK79C2
/s73abt73v5Fn7mQXq/XN9bu8vnfn02mvJM/a72La68hfd2x88tWaV6+oz9tUvthjH33iwP2Rvg4pIzn
n76SVrz9oEPdpvP68wYhXzJftEkWMfeCof7y/bltST+v2KfaKce7+D5pP9mxrE14mY3bCPvKvDMt33yf
7LYktT2qJv2wtt+ue97uHj/VIbT62bwnbP/mvY0siuT4/2/7jVd7bs1Vt72Yfcaie27ttfX7jSY0/4O3
tJqH+rbjek7k5+D/XPpv27JndWhXx6bz8BGl7Q8LOvyVE/F6bRZQ+Tl9zwamXoySlw/iyIenuDq95tmb
fe22tntc2+obTQ/m734mjtPe/tdygzyTWvXGnPOmtf5LLq9/a8GQITPbZssEez316+KG/ffI/39v0mq6
8VHNlsz2E+802p9nt76zmXaua1bLXvNms7dXXnO/sXM8PdTovutEfzj31tiWvTIZ139l8/JhpNbt5r/3
CBzlS1nLv2gexrmr0tH9sf7UyLZpp1y94s80L8Ic4n9n6uy75fTLluP9mlvlYj6In9dYZnkxfqX4HL73
kpH1UbexGuv1v7kcO/rnP2F41O7n3x86F9l9f+NzMDzth/bjK/JV/0wL4Tyg+pu+ucsuiufSdU/Uy5XT
WXJt2yNzJNWWklBk8/9mjpyx4t/dnD0ifMf7D0KwANL1j6ngYNfwCjAY0W";

(**************************************************************************************************)

$cs = 1 / 1.5;
$csx = {-$cs, 1}; $csy = {-$cs, $cs};
$rightCup = With[{b = 0.552284749831, r = 1 / $cs},
  {points = {{r, 1}, {r, 1}, {0, 1}, {0,1}, {-b, 1}, {-1, b}, {-1, 0}, {-1, -b}, {-b, -1}, {0, -1}, {0, -1}, {r, -1}}},
  ToPackedReals @ points * $cs
];

$setIcons := {
  "Element"     -> bound[$csx, $csy, $csx] @ {BezierCurve @ $rightCup, Line @ {{1, 0}, {-$cs, 0}}},
  "Subset"      -> bound[$csx, $csy, $csx] @ BezierCurve @ $rightCup,
  "SubsetEqual" -> bound[$csx, {-1, $cs}, $csx] @ {BezierCurve @ $rightCup, Line @ {{-$cs, -1.}, {1, -1.}}}
};

(**************************************************************************************************)

(* primitives are understood to live in bounding box of range -1 to 1, where 0 is treated as the origin point *)
(* iconNameOrder[a_] := StringDelete[a, {"Long","Short","Large","Small", ; *)

$u = {-1, 1};
$h = {-1, 1}/2;
$r = {0, 1};
$l = {-1, 0};
$z = {0, 0};

$sq2 = Sqrt[1/2.];
$plusPoints = {{{0, -1}, {0, 1}}, {{-1, 0}, {1, 0}}};
$crossPoints = $sq2 * {{{-1, -1}, {1, 1}}, {{-1, 1}, {1, -1}}};

$tilde = BSplineCurve @ {{-1,0}, {-.5,.33}, {.0,0},{0.5,-.33},{1,0}};

repeatV[head_[curve_], ys_] := Map[head[Threaded[{0, #}] + curve]&, ys];

$mathIcons := {
  "Dot"                -> bound[$u*.2, $u*.2, $z] @ Disk[{0, 0}, .2],
  "Times"              -> bound[$u*$sq2, $u*$sq2, $z] @ Line @ $crossPoints,
  "Plus"               -> Line @ $plusPoints,
  "CircleTimes"        -> {Line @ $crossPoints, Circle[{0, 0}, 1]},
  "CirclePlus"         -> {Line @ $plusPoints, Circle[{0, 0}, 1]},
  "CircleMinus"        -> {Line[{{-1, 0}, {1, 0}}], Circle[{0, 0}, 1]},
  "CircleDot"          -> {Disk[{0, 0}, .2], Circle[{0, 0}, 1]},

  "Minus"              -> $hline,
  "Equal"              -> repeatV[$hline, {-1, 1}/3],
  "TripleEqual"        -> repeatV[$hline, {-1, 0, 1}/2],

  "Tilde"              -> $tilde,
  "TildeTilde"         -> repeatV[$tilde, {-1, 1}/4]
};

(**************************************************************************************************)

$shapeIcons := {
  makeClosed @ makeArrowHead @ {
  "Square"             -> Line @ {{-1, 0}, {-1, 1}, {1, 1}, {1, 0}}
  },
  "TopLeftSquare"      -> Line @ {{-1, -1}, {-1,  1}, { 1, 1}},
  "TopRightSquare"     -> Line @ {{ 1, -1}, { 1,  1}, {-1, 1}},
  "BottomLeftSquare"   -> Line @ {{ 1, -1}, {-1, -1}, {-1, 1}},
  "BottomRightSquare"  -> Line @ {{-1, -1}, { 1, -1}, { 1, 1}},
  "BottomCup"          -> Line @ {{-1,  1}, {-1, -1}, { 1, -1}, { 1, 1}},
  "TopCup"             -> Line @ {{-1, -1}, {-1,  1}, { 1,  1}, { 1, -1}},
  "LeftCup"            -> Line @ {{ 1,  1}, {-1,  1}, {-1, -1}, { 1, -1}},
  "RightCup"           -> Line @ {{-1,  1}, { 1,  1}, { 1, -1}, {-1, -1}},
  "FilledSquare"       -> Rectangle[{-1, -1}, {1, 1}],
  "FilledUpperSquare"  -> Rectangle[{-1, 0}, {1, 1}],
  "FilledLowerSquare"  -> Rectangle[{-1, -1}, {1, 0}],
  "Disk"               -> Disk[{0, 0}, 1],
  "Circle"             -> Circle[{0, 0}, 1]
};

(**************************************************************************************************)

bound[h_, v_, b_][prims_]      := $bounded$[prims, {h, v, b}];
bound[h_, v_, b_, b2_][prims_] := $bounded$[prims, {h, v, b}, b2];

SetListable[setRightAligned];

setRightAligned[name_ -> $bounded$[curve_, {bx_, by_, {_, bb2_}}]] :=
  name -> $bounded$[curve, {bx, by, {bb2, bb2}}];

(* NOTE: make sure to update $knownIconNames with the keys from this lazy table so that autocomplete is correct *)
$namedIconData = MapP[makeIconData, DelCases[None | $bounded$[None, ___]] @ Dict[

  $arrowIcons,

  $shapeIcons,

  "Bar"                -> bound[$z, $u, $z] @ Line @ {{0, -1}, {0, 1}},
  "UpperBar"           -> bound[$z, $r, $z] @ Line @ {{0, 1}, {0, 0}},
  "LowerBar"           -> bound[$z, $l, $z] @ Line @ {{0, 0}, {0, -1}},
  "SkewBar"            -> bound[$h, $u, $z] @ Line @ {{-1/2, -1}, {1/2, 1}},
  "Tee"                -> bound[$r, $u, $r] @ Line @ {{{-0, 1}, {0, -1}}, {{0, 0}, {1, 0}}},

  $mathIcons,

  "Ellipsis"           -> bound[$u, $u*.25, $u] @ Disk[{{-1, 0}, {0, 0}, {1, 0}}*0.77, .25],
  "VerticalEllipsis"   -> bound[$u*.25, $u, $u*.25] @ Disk[{{0, -1}, {0, 0}, {0, 1}}*0.77, .25],

  $rotatedIcons,

  $setIcons,

  "LeftRightArrow"     -> bound[$u, $h, $u] @ $leftRightPrim,
  "UpDownArrow"        -> bound[$h, $u, $z] @ RotatePrimitives[$leftRightPrim, Pi/2]

]];


$namedIconData