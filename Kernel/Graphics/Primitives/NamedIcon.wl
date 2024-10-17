SystemExports[
  "Function",          NamedIconData,
  "GraphicsPrimitive", NamedIcon,
  "BoxFn",             NamedIconCBox,
  "GraphicsBoxFn",     NamedIconGBox,
  "GraphicsOption",    IconThickness, IconColor, IconScaling, DebugBounds, GraphicsScale,
  "Symbol",            Huge, MediumLarge, MediumSmall,
  "SymbolicHead",      Sized, Reversed
];

PackageExports[
  "SymbolicHead",      GIconData
];

PrivateExports[
  "GraphicsBoxFn",     NamedIconBoxFast
];

SessionExports[
  "CacheVariable",     $NamedIconData
];

(**************************************************************************************************)

NamedIconData::usage =
"NamedIconData['name'] looks up a given icon and returns a tuple of icon data.
NamedIconData[All] returns tuples for all icons.
NamedIconData[] returns a list of icon names.
* tuples take the form {prims$, boxes$, boxes3D$, {{x$1, x$2}, {y$1, y$2}, {b$1, b$2}}, solid$}."

NamedIconData[]    := Keys @ $NamedIconData;
NamedIconData[All] := $NamedIconData;

NamedIconData[name_Str] /; StrHasQ[name, "*"] := Select[NamedIconData[], StrMatchQ[name]];
NamedIconData[name_Str] := getIconData @ name;

(**************************************************************************************************)

SetCached[$NamedIconData, LoadSystemData @ "NamedIconData.mx"];

(* DefineFunctionAutocomplete[NamedIcon, {Keys @ $NamedIconData}];
DefineFunctionAutocomplete[NamedIconData, {$knownIconNames}];
 *)

(**************************************************************************************************)

DefinePatternRules[
  namedIconP -> _Str | _Sized | _Reversed | _Repeating
];

DeclaredHere[NamedIcon]

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
  ImageSize      -> Auto
};

DefineGPrim[NamedIcon, "Pos,PosDelta", NamedIconGBox];

(**************************************************************************************************)

Options[NamedIconGBox] = Options @ NamedIcon;

NamedIconGBox[pos:PosAP|_Offset, dir:PosAP, name:namedIconP, opts___Rule] := CatchMessages[NamedIcon,
  UnpackSymbolsAs[NamedIconGBox, List @ opts, graphicsScale, imageSize, iconScaling, iconColor, iconThickness, alignmentPoint];
  SetAuto[imageSize, 20];
  NamedIconBoxFast[
    pos, dir, name,
    graphicsScale, imageSize, iconScaling,
    iconColor, iconThickness, alignmentPoint
  ]
];

$alignmentRules = {Left -> 0, Center -> 0.5, Right -> 1};
$scalingRules   = {Huge -> 1.75, Large -> 1.5, MediumLarge -> 1.25, Medium -> 1, MediumSmall -> .75, Small -> .5, Tiny -> .25};

(**************************************************************************************************)

Options[NamedIconCBox] = OptionValueRules[
  NamedIcon -> {IconScaling, IconColor, IconThickness, AlignmentPoint},
  ImageSize -> 10
];

CoreBox[NamedIcon[n:namedIconP, opts___Rule]] := NamedIconCBox[n, opts];

SetBoxFn @ NamedIconCBox;

NamedIconCBox[name:namedIconP, opts___Rule] := Locals @ CatchMessages[NamedIcon,
  UnpackSymbolsAs[
    NamedIconCBox, List @ opts,
    imageSize, iconScaling, iconColor, iconThickness, alignmentPoint
  ];
  inset = NamedIconBoxFast[
    {0, 0}, {1, 0}, name,
    None, imageSize, iconScaling,
    iconColor, iconThickness,
    alignmentPoint
  ];
  If[!MatchQ[inset, _InsetBox], Return @ StyleBox["?", Red]];
  Append[P1 @ inset, BLinePos -> Scaled[.3/2.6]]
]

(**************************************************************************************************)

NamedIconBoxFast[pos_, dir2_, name2_, gScale_, imgSize_, iscaling2_, color_, thickness_, align2_] := Locals[
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
  iconData = getIconData @ name;
  If[!MatchQ[iconData, _GIconData], Return @ $Failed];
  {prims, boxes, boxes3D, {{x1, x2}, {y1, y2}, {b1, b2}}, solid} = List @@ iconData;
  $styler = solidEmptyStyleBoxOp[solid, color, None, thickness];
  $originx = If[NumberQ @ align, Lerp[b1, b2, align], 0];
  $origin = {$originx, 0};
  makeIcon[
    ResolveOffsets[pos, gScale],
    Normalize @ ToPackedReals @ dir,
    applyRep[boxes, reps],
    gScale
  ]
];

$alignmentRules = {Left -> 0, Center -> 0.5, Right -> 1};

$scalingRules = {Huge -> 1.75, Large -> 1.5, MediumLarge -> 1.25, Medium -> 1, MediumSmall -> .75, Small -> .5, Tiny -> .25};

(**************************************************************************************************)

getIconData[name_Str] :=
  Lookup[$NamedIconData, name,
  Message[NamedIcon::unknownIcon, name, LitStrRow @ Keys @ $NamedIconData]; $Failed
];

NamedIcon::unknownIcon = "`` is not a known icon. Known icons include ``.";

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
  If[$originx =!= 0, path //= VecTrans[-$origin]];
  path = VecRotTo[path, dirx * $imageSize/2];
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
      boxes,
      ImageSize -> $imageSize * {1.3, 1.3},
      PClip -> False,
      PRange -> {{-1, 1}, {-1, 1}} * 1.3,
      PMargin -> None,
      AspectRatio -> (PN[$imageSize] / P1[$imageSize]),
      ImagePadding -> None
    ],
    pos, $origin, Auto, dir
  ]
];

(* this optimization doesn't handle multilines!
makeIconInline[pos_, dirx_, LineBox[path:{_, _}], graphicsScale_] := Locals[
  If[$originx =!= 0, path //= VecTrans[-$origin]];
  path = VecRotTo[path, dirx * $imageSize / graphicsScale / 2] + Threaded[pos];
  $styler @ Construct[LineBox, path]
];
*)

makeIconInline[pos_, dirx_, prims_, graphicsScale_] := Locals[
  If[$originx != 0,
    prims = Construct[GeometricTransformationBox, prims, -$origin]];
  rotMatrix = RotToMatrix[dirx * 0.5 / graphicsScale, $imageSize];
  transform = {rotMatrix, pos};
  $styler @ Construct[GeometricTransformationBox, prims, transform]
];

(**************************************************************************************************)

applyRep[b_, None] := b;
applyRep[b_, xs_] := Construct[GeometricTransformationBox, b, {{#, 0.}}& /@ xs];

(**************************************************************************************************)

(* "solidEmptyStyleBoxOp[isSolid$, color$, opacity$, thickness$] produces a box operator that will appropriately color solid or empty primitives." *)

solidEmptyStyleBoxOp[True, args___]  := solidStyleBoxOp[args];
solidEmptyStyleBoxOp[False, args___] := emptyStyleBoxOp[args];

(**************************************************************************************************)

(* "
emptyStyleBoxOp[color$, opacity$, thickness$] produces a StyleBoxOp that will appropriately style thin primitives like %LineBox, %CircleBox, and %PointBox.
* color$ can be %SolidEdgeForm[$$], in which case the edge color will be used.
* if thickness$ is 0 or color$ is None, the operator will delete boxes.
"
 *)
emptyStyleBoxOp = CaseOf[
  Seq[s_solidEdgeForm, o_, t_] := $[PN @ solidEdgeColors @ s, o,  t];
  Seq[None, _, _]              := invisibleOp;
  Seq[_, 0, _]                 := invisibleOp;
  Seq[_, _, 0]                 := invisibleOp;
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

invisibleOp[___] := {};

(**************************************************************************************************)

(* "
solidStyleBoxOp[color$, opacity$, thickness$] produces a StyleBoxOp that will appropriately style thick primitives like %PolygonBox and %DiskBox.
* color$ can be %SolidEdgeForm[$$], in which case the face and edge colors will be used.
* if thickness$ is 0 or color$ is None, the operator will delete boxes.
"
 *)
toFaceForm[o_, {c_, _} | c_] := FaceForm[{toOpacity @ o, toColor @ c}];

(* Subtle issue: we must ignore opacity for edges because they are extend both within and without
the underlying polygon, and so produce a weird stripe if they are semiopaque *)
toEdgeForm = CaseOf[
  Seq[0|None, _]        := EdgeForm @ None;
  Seq[_, None]          := EdgeForm @ None;
  Seq[t_, {_, c_} | c_] := EdgeForm @ {toThick @ t, toColor @ c};
];

solidStyleBoxOp = CaseOf[
  Seq[s_solidEdgeForm, o_, t_] := $[t, o, solidEdgeColors @ s];
  Seq[c_, o_, t_]              := StyleBoxOp[toFaceForm[o, c], toEdgeForm[t, c]];
];

