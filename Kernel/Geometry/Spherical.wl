PackageExports[
  "Function",
    FromSpherical, ToSpherical,
    SphericalVecRot
];

(*************************************************************************************************)

FromSpherical[{r_, a_, b_}] := {r * Cos[b] * Sin[a], r * Sin[a] * Sin[b], r * Cos[a]};

ToSpherical[{x_, y_, z_}]   := {Sqrt[x^2 + y^2 + z^2], ArcTan[z, Sqrt[x^2 + y^2]], ArcTan[x, y]};

(**************************************************************************************************)

SphericalVecRot[vecs:{___List}, t_] :=
  Map[SphericalVecRot[#, t]&, vecs];

SphericalVecRot[vec_List, t_] :=
  Dot[{{Cos[t], -Sin[t], 0}, {Sin[t], Cos[t], 0}, {0, 0, 1}}, vec];

SphericalVecRot[t_][vec_] := SphericalVecRot[vec, t];
