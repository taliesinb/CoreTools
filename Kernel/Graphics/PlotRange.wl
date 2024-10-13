SystemExports[
  "GraphicsFunction", EnlargeRange
];

PackageExports[
  "GraphicsBoxFunction", GBoxRange, GRow
];

(**************************************************************************************************)

GBoxRange::usage =
"GBoxRange[boxes$] returns the range {{x$min, x$max}, {y$min, y$max}} of primitive boxes.
GBoxRange[boxes$, scale$] assumes that one plot range unit corresponds to scale$ pixels.
* if no scale is provided:
* %Offset expressions are ignored
* %TextBox and %InsetBox contents are treated as a single point at their placement position."

GBoxRange[boxes_, gs_:None] := Locals[
  setupGPrimBoundDefs[];
  $aps = 5/2; $gs = gs; $fs = $ff = $fw = $fsl = $ft = Inherited;
  gboxRange @ ResolveOffsets[boxes, gs]
];

(* used internally to compute the center for Rotate *)
gboxRange[boxes_] := Locals[
  $p = Bag[]; $t = Id;
  scanRange @ boxes;
  $p = BagPart[$p, All];
  ToPackedReals @ If[$p === {}, $emptyBounds, fatten /@ CoordinateBounds[$p]]
];

$eps = 10.^-6;
fatten[{z_, z_}] := {z - $eps, z + $eps};
fatten[other_] := other;

$emptyBounds = {{-$eps, $eps}, {-$eps, $eps}};

(**************************************************************************************************)

(* This would be better called EnlargeRange, becuase it expects a PRange spec.
the scaled variant matches what CoordinateBounds[..., Scaled[...]] would do
*)
EnlargeRange = CaseOf[
  $[range_, Scaled[p_]]       := $[range, p * (Dist @@@ range)];
  $[range_, pad_ ? NumMatQ]   := range + {{-1, 1}, {-1, 1}} * pad;
  $[range_, {h:NumP, v:NumP}] := range + {{-1, 1} * h, {-1, 1} * v};
  $[range_, n:NumP]           := range + {{-1, 1}, {-1, 1}} * n;
];

(**************************************************************************************************)

(* scanRange is set up lazily so that the primitive box registery will have been populated *)

boxPattern[str_] := ToAltP @ GSigToGBoxes[str];

(* TODO: make this work in 3D, handle radii that are specified as Offset *)
setupGPrimBoundDefs[] := With[{
  prims       = boxPattern["Primitives"],
  pos         = boxPattern["Pos!Radius"],
  posPos      = boxPattern["Pos,Pos"],
  posRad      = boxPattern["Pos,Radius"],
  posList     = boxPattern["PosList!Radius"],
  posListRad  = boxPattern["PosList,Radius"],
  posLists    = boxPattern["PosLists!Radius"],
  posListsRad = boxPattern["PosLists,Radius"],
  directive   = _Directive | _APointSize | Rule[FontSize | FontWeight | FontFamily | FontSlant | FontTracking, _],
  curves      = JoinedCurveBox|FilledCurveBox,
  inset       = _TextBox | _Text3DBox | _InsetBox | _Inset3DBox,
  $ = scanRange},

  Clear[scanRange];

  $[PointBox[p_]]   /; $gs =!= None        := $ @ Make[DiskBox, p, $aps / $gs];
  $[Point3DBox[p_]] /; $gs =!= None        := $ @ Make[SphereBox, p, $aps / $gs];
  $[TagBox[_, "PlaneInset"|"NoBounds"]]    := Null; (* PlaneInset is meant to appear as a billboard *)
  $[ib:inset]                              := insetBounds @ ib;
  $[prims[p_, ___]]                        := $ @ p;
  $[pos[v:Pos2P]]                          := StuffBag[$p, $t @ v];
  $[posPos[v:Pos2P, w:Pos2P, ___]]         := StuffBag[$p, $t /@ {v, w}, 1];
  $[posRad[v:Pos2P, r_:1, ___]]            := StuffBag[$p, $t @ vecBall[v, r]];
  $[PolygonBox[Rule[m:Pos2ListP, _], ___]] := StuffBag[$p, $t @ m, 1];
  $[posList[m:Pos2ListP, ___]]             := StuffBag[$p, $t @ m, 1];
  $[posListRad [m:Pos2ListP,   r_:1, ___]] := StuffBag[$p, $t @ matBall[m, r], 1];
  $[posLists   [ms:Pos2ListsP, ___]]       := StuffBag[$p, $t /@ ms, 2];
  $[posListsRad[ms:Pos2ListsP, r_:1, ___]] := StuffBag[$p, $t[matBall[#, r]& /@ ms], 2];
  $[list_List]                             := styleBlock @ Scan[$, list];
  $[d:directive]                           := applyDir @ d;
  $[StyleBox[p_, opts___]]                 := styleBlock[Scan[applyDir, {opts}]; $ @ p];
  $[curves[c_List, ___]]                   := multiCurveBound @ c;
  $[GeometricTransformationBox[p_, t_]]    := transBoxBound[p, t];
  $[e_]                                    := Message[GBoxRange::unrecogBox, e];
  $[_]                                     := Null;

  Clear[setupGPrimBoundDefs];
];

GBoxRange::unrecogBox = "Unrecognized element ``.";

(**************************************************************************************************)

Clear[transBoxBound, multiCurveBound, applyDir, composeTransform, applyTrans];

applyDir = CaseOf[
  AbsolutePointSize[p:NumP]   := Set[$aps, p/2];
  AbsolutePointSize[s_Symbol] := % @ APointSize  @ Lookup[$SymbolicPointSizes, s];
  e_List                      := Scan[%, e];
  d_Directive                 := Scan[%, d];
  FontSize -> fs_             := Set[$fs, fs];
  FontFamily -> ff_           := Set[$ff, ff];
  FontWeight -> fw_           := Set[$fw, fw];
  FontSlant -> fs_            := Set[$fsl, fs];
  FontTracking -> ft_         := Set[$ft, ft];
  _                           := Null
];

(**************************************************************************************************)

(* TODO: join endpoints, which can effect bounds of BezierCurve *)
multiCurveBound[curves_] := scanRange @ ToGraphicsBoxes @ curves;

(**************************************************************************************************)

transBoxBound = CaseOf[
  Seq[p_, v:Pos2P]                := applyTrans[p, ThreadPlusOp[v]];
  Seq[p_, m:Pos2ListP]            := applyTrans[p, AffineOp[m]];
  Seq[p_, {m:Pos2ListP, v:Pos2P}] := applyTrans[p, AffineOp[m, v]];
  Seq[p_, {m:Pos2ListP, Center}]  := With[
    {v = N[Mean /@ gBoxesRange[p]]},
    applyTrans[p, ThreadPlusOp[-v] /* AffineOp[m] /* ThreadPlusOp[v]]
  ];
  Seq[_, t_] := Message[GBoxRange::unrecogTrans, t]
];

GBoxRange::unrecogTrans = "Unrecognized geometric transform spec ``.";

(**************************************************************************************************)

SetHoldF @ styleBlock;

styleBlock[e_] := InheritedBlock[{$aps, $fs, $ff, $fw, $fsl, $ft}, e];

SetCached[$circ8, ToPackedReals @ N @ ClockwisePoints[8]];
SetCached[$sphere26, Normalize /@ DelCases[{0.,0.,0.}] @ Tuples[N @ {-1, 0, 1}, 3]];

vecBall[v:{_, _, _}, r_] := Threaded[v] + r * $sphere26;
vecBall[v:{_, _}, r_]    := Threaded[v] + r * $circ8;
matBall[m_, r_]          := vecBall[#, r]& /@ m;

(**************************************************************************************************)

applyTrans[p_, new_] := Block[{$t = composeTransform[$t, new]}, scanRange @ p];

(* TODO: this won't simplify cases that are already involving Composition *)
composeTransform[Id, new_] := new;
composeTransform[old_, new_] := Composition[old, new];
composeTransform[DotRightOp[old_], DotRightOp[new_]] := DotRightOp[Dot[new, old]];
composeTransform[ThreadPlusOp[old_], ThreadPlusOp[new_]] := ThreadPlusOp[old + new];

(**************************************************************************************************)

$baseStyle := {FontSize -> $fs, FontFamily -> $ff, FontWeight -> $fw, FontSlant -> $fsl, FontTracking -> $ft};

GBoxRange::unsuppInset = "Unsupported InsetBox ``."

insetBounds[e_] := If[NumberQ[$gs], properInsetBounds, pointInsetBounds][e];

pointInsetBounds = CaseOf[
  (TextBox|Text3DBox)[_, v:Pos2P, ___]    := StuffBag[$p, $t @ v];
  (InsetBox|Inset3DBox)[_, v:Pos2P, ___]  := StuffBag[$p, $t @ v];
  TextBox[_] | InsetBox[_]                := StuffBag[$p, {0, 0}];
  Text3DBox[_] | Inset3DBox[_]            := StuffBag[$p, {0, 0, 0}];
  o_                                      := Null;
];

properInsetBounds = CaseOf[

  i:InsetBox[_GraphicsBox, ___] :=
    scanRange @ embedInsetBoxWithScale[i, $gs];

  InsetBox[FormBox[txt_, _] | txt_, pos_, offset:Except[_Rule]:ImageScaled[{0.5,0.5}], size_:Auto, dirx:Except[_Rule]:{1,0}, opts___Rule] := Locals[
    pos = ResolveOffsets[pos, $gs];
    offset //= Rep[ImageScaled[s_] :> (s - 0.5) * 2];
    (* TODO: Maybe use DefaultBaseStyle here? *)
    If[FreeQ[{opts}, BaseStyle], opts = Sequence[opts, BaseStyle -> $baseStyle]];
    {w, h} = ToTextImageSize @ Text[RawBoxes @ txt, pos, opts] + 1;
    dirx = Normalize[dirx] / $gs;
    diry = VecRot90[dirx];
    dirx = dirx * w/2.;
    diry = diry * h/2.;
    off = Mean[{dirx, diry} * offset] * -2.;
    points = {dirx -diry, -dirx +diry, dirx -diry, dirx + diry};
    points += Threaded[off + pos];
    StuffBag[$p, points, 1]
  ];

  other_ := Message[GBoxRange::unsuppInset, other];
];


