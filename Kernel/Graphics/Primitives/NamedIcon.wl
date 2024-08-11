SystemExports[
  "Function",          NamedIconData,
  "GraphicsPrimitive", NamedIcon,
  "OptionSymbol",      IconThickness, IconColor, IconScaling, DebugBounds, GraphicsScale,
  "SymbolicHead",      Sized, Reversed
];

(**************************************************************************************************)

NamedIcon::usage =
"NamedIcon[pos$, dir$, 'name$'] represents a named curve at pos$ with direction dir$.
NamedIcon['name$'] typesets in %StandardForm as a right-pointing icon.
* the boxified form uses an InsetBox, unless GraphicsScale is set, in which case it produces primitives directly.
* the following options are supported:
| %AlignmentPoint | ranging from 0 to 1, where 0 is the back and 1 is the front (default 0.5) |
| %GraphicsScale | how to emit primitives directly to achieve a given pixel size (default None) |
| %ImageSize | size of icon in pixels (default 20) |
| %IconColor | color of the icon (default None) |
| %IconThickness | thickness of the icon (default 1) |
* for %AlignmentPoint, the back and front are not the bounding box, but rather the part of the icon that overlaps the x-axis.
* %AlignmentPoint -> None leaves the icon centered.
* %DebugBounds -> True will show the icon with bounding box highlighted."

Options[NamedIcon] = {
  AlignmentPoint -> None,
  GraphicsScale  -> None,
  IconThickness  -> 1,
  IconColor      -> Auto,
  IconScaling    -> 1,
  ImageSize      -> Auto,
  DebugBounds    -> False
};

(* AssociateTo[$MakeBoxesStyleData, KTake[Options @ NamedIcon, {IconThickness, IconScaling}]]; *)

DefineGPrim[NamedIcon, "Pos,PosDelta", namedIconGBoxes];

(**************************************************************************************************)

(* TODO: update me when icons are added *)
$knownIconNames = Uncompress @
"1:eJx9ldtOwzAMhidx3AkYCG644lEY4yAxJEQH94VmEC1rStKx1yexm8RpCjco/mz/+d0k4+pdvizX
vV5P75o/c67rbGgWNxv1w4prpeQ2m9hYSM2KNr3jQsT0xNDXqmKKwgsv0JlClSRlpeZy+5dUZwqlkp
SVumdlLRjQtlRnCqWS1LkbMMlcxlN25smoncrg/B/lv/Nk8iR/aPILxfPyU7DsyItFCPs9GrsxPTmL
x4s4GStSADsdCikn9j3fMxwHGPrOEGMHxgPnNhw3sRog8RkaYdN2YwsSewgPDJzxfC3LAibFAkdGzo
8Dp7Ejiokn2g57pe0JJs4ctg/5kdcMpsM8hH3nCqLj2JJnxI/vAvlWV8yIDWDWw4OUq7Cpj6AIon0T
Zd+bXDE4UShr4knsjlRBe1JF6cj7IWVkrITS5l04WI3mbrj6MBdxxyynuYJ3BAougEYb2OuQrdjWrm
31gmHXTNZwjRd8zTRoP4uNxvsM2pgY+LiVfuKlifs+dnrI7erW2BbQYd5NJRjGuKco8A7ACkPr+lYI
Xmmu4Wa/MVXzj1x4aBte+OdXjRcdTowt63DtX6uQmcltidEYvxHpHCEJzQMErr/JB4ljt/NDLpbNpz
9qticIH1erxqoQNKZScJwjIgRg4GV83okAsKpTKQoy0bhBYaRhQ9xMriIMdQAfnK3NbzLe9827Zvg/
Fpd4YG5Oshs+Ca/0C1HJWNE=";

(* DefineFunctionAutocomplete[NamedIcon, {$knownIconNames}];
DefineFunctionAutocomplete[NamedIconData, {$knownIconNames}];
 *)
(**************************************************************************************************)

DefinePatternRules[
  namedIconP -> _Str | _Sized | _Reversed | _Repeating
];

CoreBoxes[ni:NamedIcon[namedIconP, ___Rule]] := namedIconBoxes @ ni;

namedIconBoxes[_] := $Failed;
namedIconBoxes[NamedIcon[name:namedIconP, opts___Rule]] := Locals @ CatchError[NamedIcon,
  UnpackOptionsAs[
    NamedIcon, {opts},
    imageSize, iconScaling,
    iconColor, iconThickness, alignmentPoint
  ];
  SetAuto[imageSize, 10];
  inset = rawNamedIconBoxes[
    {0, 0}, {1, 0}, name,
    None, imageSize, iconScaling,
    iconColor, iconThickness,
    alignmentPoint
  ];
  If[!MatchQ[inset, _InsetBox], Return @ StyleBox["?", Red]];
  Append[P1 @ inset, BaselinePosition -> (Scaled[0.5] -> Axis)]
]

(**************************************************************************************************)

General::unknownIcon = "`` is not a known icon. Known icons include ``.";

namedIconGBoxes[NamedIcon[pos:PosAP|_Offset, dir:PosAP, name:namedIconP, opts:OptionsPattern[]]] := CatchError[NamedIcon,
  UnpackOptionsAs[
    NamedIcon, {opts},
    graphicsScale, imageSize, iconScaling,
    iconColor, iconThickness, alignmentPoint,
    $debugBounds
  ];
  SetAuto[imageSize, 20];
  rawNamedIconBoxes[
    pos, dir, name,
    graphicsScale, imageSize, iconScaling,
    iconColor, iconThickness, alignmentPoint
  ]
];

$debugBounds    = False;
$alignmentRules = {Left -> 0, Center -> 0.5, Right -> 1};
$scalingRules   = {Huge -> 1.75, Large -> 1.5, MediumLarge -> 1.25, Medium -> 1, MediumSmall -> .75, Small -> .5, Tiny -> .25};

(**************************************************************************************************)

rawNamedIconBoxes[pos_, dir2_, name2_,
                  gScale_, imgSize_, iscaling2_,
                  color_, thickness_, align2_] := Locals[
  dir = dir2; name = name2; iscaling = iscaling2; align = align2;
  If[Head[name] === Reversed,
    name //= P1;
    dir *= -1;
    If[NumberQ @ align, align //= OneMinus]
  ];
  If[Head[name] === Repeating,
    {name, reps} = List @@ name,
    reps = None
  ];
  If[Head[name] === Sized,
    {name, iscaling} = List @@ name
  ];
  iscaling = iscaling /. $scalingRules;
  align = align /. $alignmentRules;
  $imageSize = EnsurePair[imgSize * iscaling];
  iconData = LookupOrThrow[$namedIconData, name, "unknownIcon", LiteralCommaStringForm @ Keys @ $namedIconData];
  {prims, boxes, boxes3D, {{x1, x2}, {y1, y2}, {b1, b2}}, solid} = iconData;
  $styler = SolidEmptyStyleBoxOp[solid, color, None, thickness];
  $originx = If[NumberQ @ align, Lerp[b1, b2, align], 0];
  $origin = {$originx, 0};
  makeIcon[
    ResolveOffsets[pos, gScale],
    Normalize @ ToPackedReals @ dir,
    applyRep[boxes, reps],
    gScale
  ]
];

applyRep[b_, None] := b;
applyRep[b_, xs_] := Construct[GeometricTransformationBox, b, {{#, 0.}}& /@ xs];

(**************************************************************************************************)

"
SolidEmptyStyleBoxOp[isSolid$, color$, opacity$, thickness$] produces a box operator that will appropriately color solid or empty primitives.
"

SolidEmptyStyleBoxOp[True, args___]  := SolidStyleBoxOp[args];
SolidEmptyStyleBoxOp[False, args___] := EmptyStyleBoxOp[args];

(**************************************************************************************************)

"
EmptyStyleBoxOp[color$, opacity$, thickness$] produces a StyleBoxOp that will appropriately style thin primitives like %LineBox, %CircleBox, and %PointBox.
* color$ can be %SolidEdgeForm[$$], in which case the edge color will be used.
* if thickness$ is 0 or color$ is None, the operator will delete boxes.
"

EmptyStyleBoxOp = CaseOf[
  Seq[s_SolidEdgeForm, o_, t_] := $[PN @ solidEdgeColors @ s, o,  t];
  Seq[None, _, _]              := InvisibleOp;
  Seq[_, 0, _]                 := InvisibleOp;
  Seq[_, _, 0]                 := InvisibleOp;
  Seq[c_, o_, t_]              := StyleBoxOp[toThick @ t, toOpacity @ o, toColor @ c]
];

toThick[Auto | None] := Seq[];
toThick[n_] := AbsoluteThickness[n];

toColor[Auto] := Seq[];
toColor[None] := Opacity[0];
toColor[col_] := col;

toOpacity[Auto | None] := Seq[];
toOpacity[o_] := Opacity[o];

(**************************************************************************************************)

InvisibleOp[___] := {};

(**************************************************************************************************)

"
SolidStyleBoxOp[color$, opacity$, thickness$] produces a StyleBoxOp that will appropriately style thick primitives like %PolygonBox and %DiskBox.
* color$ can be %SolidEdgeForm[$$], in which case the face and edge colors will be used.
* if thickness$ is 0 or color$ is None, the operator will delete boxes.
"

toFaceForm[o_, {c_, _} | c_] := FaceForm[{toOpacity @ o, toColor @ c}];

(* Subtle issue: we must ignore opacity for edges because they are extend both within and without
the underlying polygon, and so produce a weird stripe if they are semiopaque *)
toEdgeForm = CaseOf[
  Seq[0|None, _]        := EdgeForm @ None;
  Seq[_, None]          := EdgeForm @ None;
  Seq[t_, {_, c_} | c_] := EdgeForm @ {toThick @ t, toColor @ c};
];

SolidStyleBoxOp = CaseOf[
  Seq[s_SolidEdgeForm, o_, t_] := $[t, o, solidEdgeColors @ s];
  Seq[c_, o_, t_]              := StyleBoxOp[toFaceForm[o, c], toEdgeForm[t, c]];
];

(**************************************************************************************************)

makeIcon[pos_, dir_, DiskBox[pos2:PosAP, r_], scale_] :=
  $styler @ Construct[DiskBox,
    PointPlus[pos, pos2],
    ResolveOffsets[Offset[r * $imageSize], scale]
  ];

makeIcon[pos_, dir_, prims_, scale_] :=
  If[NumberQ[scale],
    makeIconInline[pos, dir, prims, scale],
    makeIconInset[pos, dir, prims]
  ];

(* this optimization doesn't handle multilines!
makeIconInset[pos_, dirx_, LineBox[path:{_, _}]] := Locals[
  If[$originx =!= 0, path //= TranslateVector[-$origin]];
  path = RotateVectorTo[path, dirx * $imageSize/2];
  offsets = Offset[#, pos]& /@ path;
  $styler @ Construct[LineBox, SimplifyOffsets @ offsets]
];
*)

$boundsRect = {FaceForm[None], EdgeForm[Red], RectangleBox[{-1, -1}, {1, 1}]};
makeIconInset[pos_, dir_, prims_] := Locals[
  boxes = $styler @ prims;
  Construct[
    InsetBox,
    Construct[GraphicsBox,
      If[$debugBounds, addDebugBounds @ boxes, boxes],
      ImageSize -> $imageSize * {1.3, 1.3},
      PlotRangeClipping -> False,
      PlotRange -> {{-1, 1}, {-1, 1}} * 1.3,
      PlotRangePadding -> None,
      AspectRatio -> (PN[$imageSize] / P1[$imageSize]),
      ImagePadding -> None
    ],
    pos, $origin, Auto, dir
  ]
];

addDebugBounds[boxes_] := List[
  {EdgeForm @ {AbsoluteThickness[2], RGBColor[1, 0, 0, .5]}, FaceForm @ None,
   Construct[RectangleBox, {x1, y1}, {x2, y2}]},
  boxes,
  {RGBColor[0, 0, 1, .5], AbsoluteThickness[2], AbsolutePointSize[5],
    Construct[LineBox, {{b1, 0}, {b2, 0}}],
    Construct[PointBox, {{b1, 0}, {b2, 0}}]}
];

(* this optimization doesn't handle multilines!
makeIconInline[pos_, dirx_, LineBox[path:{_, _}], graphicsScale_] := Locals[
  If[$originx =!= 0, path //= TranslateVector[-$origin]];
  path = RotateVectorTo[path, dirx * $imageSize / graphicsScale / 2] + Threaded[pos];
  $styler @ Construct[LineBox, path]
];
*)

makeIconInline[pos_, dirx_, prims_, graphicsScale_] := Locals[
  If[$debugBounds, prims //= addDebugBounds];
  If[$originx != 0,
    prims = Construct[GeometricTransformationBox, prims, -$origin]];
  rotMatrix = RotateToMatrix[dirx * 0.5 / graphicsScale, $imageSize];
  transform = {rotMatrix, pos};
  $styler @ Construct[GeometricTransformationBox, prims, transform]
];

(**************************************************************************************************)

SetStrict @ makeIconData;

makeIconData[bounded[prims_, a_, ___], key_] :=
  makeIconData[prims, key, a];

makeIconData[prims_, key_] :=
  makeIconData[prims, key, {$u, $u, $u}];

makeIconData[prims2_, name_, align_] := Locals[
  prims = N @ prims2;
  boxes = ToGraphicsBoxes @ prims;
  boxes3D = ToGraphics3DBoxes @ prims;
  solid = solidPrimitiveQ @ prims;
  {prims, boxes, boxes3D, align, solid}
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
  upperPath = TranslateVector[{dx, 0}, upperPath];
  lowerPath = VectorReflectVertical @ upperPath;
  symmetricCurve = toJoinedCurve[head @ upperPath, head @ Rev @ Most @ lowerPath];
  {x, {y1, y2}} = CoordinateBounds @ upperPath;
  l = If[Part[upperPath, 1, 2] == 0, P11[upperPath], P1 @ x];
  r = If[Part[upperPath, -1, 2] == 0, PN1[upperPath], PN @ x];
  lr = {l, r};
  List[
    name                    -> bounded[symmetricCurve, {x, {-y2, y2}, lr}],
    StrJoin["Upper", name]  -> bounded[head[upperPath], {x, {0, y2}, lr}],
    StrJoin["Lower", name]  -> bounded[head[lowerPath], {x, {-y2, 0}, lr}]
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

makeClosed[name_ -> bounded[curve_, b_]] := List[
  name                    -> bounded[curve, ReplacePart[b, {3, 1} -> Part[b, 3, 2]]],
  StrJoin["Closed", name] -> bounded[toClosedCurve[curve], b]
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
  bounded[c_, a_] := bounded[toFilledCurve @ c, a];
  l_Line          := Polygon @@ simplifyLine @ l;
  p_Polygon       := p;
  e_EmptyPolygon  := Polygon @@ e;
  b_BezierCurve   := FilledCurve @ List @ b;
  j_JoinedCurve   := FilledCurve @@ j;
];

(**************************************************************************************************)

isClosedQ = CaseOf[
  bounded[c_, _]                      := $ @ c;
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
makeRotated[name_ -> bounded[curve_, b_, bt_]] := List[
  name -> bounded[curve, b],
  StrRep[name, "Right" -> "Left"] -> bounded[ScalePrimitives[curve, {-1, 1}], boundsFlipX @ b],
  StrRep[name, "Right" -> "Up"]   -> bounded[RotatePrimitives[curve, Pi/2], boundsRot[b, bt]],
  StrRep[name, "Right" -> "Down"] -> bounded[RotatePrimitives[curve, -Pi/2], boundsRot[boundsFlipX @ b, bt]]
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

bound[h_, v_, b_][prims_] := bounded[prims, {h, v, b}];
bound[h_, v_, b_, b2_][prims_] := bounded[prims, {h, v, b}, b2];

SetListable[setRightAligned];

setRightAligned[name_ -> bounded[curve_, {bx_, by_, {_, bb2_}}]] :=
  name -> bounded[curve, {bx, by, {bb2, bb2}}];

SetCached[$namedIconData, createIconData[]];

(* NOTE: make sure to update $knownIconNames with the keys from this lazy table so that autocomplete is correct *)
createIconData[] := MapP[makeIconData, DelCases[None | bounded[None, ___]] @ Assoc[

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

(**************************************************************************************************)

NamedIconData::usage =
"NamedIconData['name'] looks up a given icon and returns a tuple of icon data.
NamedIconData[All] returns tuples for all icons.
NamedIconData[] returns a list of icon names.
* tuples take the form {prims$, boxes$, boxes3D$, {{x$1, x$2}, {y$1, y$2}, {b$1, b$2}}, solid$}."

NamedIconData[] := Keys @ $namedIconData;
NamedIconData[All] := $namedIconData;

NamedIconData[name_Str] /; StrHasQ[name, "*"] := Select[NamedIconData[], StrMatchQ[name]];
NamedIconData[name_Str] := CatchError @ LookupOrThrow["unknownIcon", $namedIconData, name, Keys @ $namedIconData];