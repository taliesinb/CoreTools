PackageExports[
  "GraphicsFunction",
    AffineTransformPrimitives,
    GeometricTransformPrimitives,
    TranslatePrimitives, TranslatePrimitiveBoxes,
    ScalePrimitives, ScalePrimitiveBoxes,
    RotatePrimitives,
  "GraphicsFunction",
    ExtractPrimitiveCoordinates,
    ExtractPrimitiveBoxCoordinates
];

(**************************************************************************************************)

AffineTransformPrimitives::usage =
"AffineTransformPrimitives[g$, mx$, b$] transforms all graphics primitive coordinates by Dot[x$, mx$] + b$."

AffineTransformPrimitives[g_, None | {{1., 0.}, {0., 1.}} | {{1, 0}, {0, 1}}, t_] :=
  MapGPrimCoords[ThreadPlusOp[t], g];

AffineTransformPrimitives[g_, m_, None | {0., 0.} | {0, 0}] :=
  MapGPrimCoords[AffineOp[m], g];

AffineTransformPrimitives[g_, m_, t_] :=
  MapGPrimCoords[AffineOp[m, t], g];

(**************************************************************************************************)

GeometricTransformPrimitives::usage =
"GeometricTransformPrimitives[t$, spec$] transforms all graphics primitive coordinates by symbolic transformation spec$.
* spec$ is a spec that matches one of the forms supported by GeometricTransformation."

GeometricTransformPrimitives[g_, {m:PosAListP, t:PosAP}] :=
  AffineTransformPrimitives[g, m, t];

GeometricTransformPrimitives[g_, m:PosAListP] :=
  AffineTransformPrimitives[g, m, None];

GeometricTransformPrimitives[g_, fn_ ? MaybeFunctionQ] :=
  MapGPrimCoords[onCoords[fn], g];

onCoords[fn_][e_ ? PosAListsQ] := Map[fn, e];
onCoords[fn_][e_] := fn[e];

(**************************************************************************************************)

TranslatePrimitives::usage = "TranslatePrimitives[g$, vec$] transates all primitive coordinates by vec$."
TranslatePrimitiveBoxes::usage = "TranslatePrimitiveBoxes[g$, vec$] transates all primitive box coordinates by vec$.";

TranslatePrimitives[prims_, t_] :=
  MapGPrimCoords[ThreadPlusOp[t], prims];

TranslatePrimitiveBoxes[prims_, t_] :=
  MapGBoxCoords[ThreadPlusOp[t], prims];

(**************************************************************************************************)

ScalePrimitives::usage ="ScalePrimitives[g$, {s$x, s$y}] scales all primitive coordinates."
ScalePrimitiveBoxes::usage = "ScalePrimitiveBoxes[g$, {s$x, s$y}] scales all primitive box coordinates."

(* TODO: fix the fact that this doesn't rescale Disk[{0,0}], since the radius is implicitly 1 *)
ScalePrimitives[prims_, s_] :=
  MapGPrimCoords[ThreadTimesOp[s], prims];

ScalePrimitiveBoxes[prims_, s_] :=
  MapGBoxCoords[ThreadTimesOp[s], prims];

(**************************************************************************************************)

RotatePrimitives::usage = "RotatePrimitives[g$, theta$] rotates all primitive coordinates by theta$."

RotatePrimitives[prims_, theta_] :=
  AffineTransformPrimitives[prims, ToPacked @ RotationMatrix @ N @ theta, None];

