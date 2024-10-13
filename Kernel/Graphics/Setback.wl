PackageExports[
  "Function",
    SetbackLine
];

(**************************************************************************************************)

"SetbackLine[path$, d$] truncates the endpoints of a path by distance d$ towards its interior.
SetbackLine[path$, {d$1, d$2}] truncates by distance d$1 from start and d$2 from end.
SetbackLine[{path$1, path$2}, $$] truncates several paths at once.
* Line segments will be dropped if the truncated distance exceeds the length of that segment.
* The entire line will become a zero length line if the truncated distance exceeds the entire length.
* Specifications can be a single spec or a pair of:
| d$ | a distance in regular coordinates |
| Scaled[f$] | a fraction of the line length |
| Offset[p$] | a setback in absolute points |
| Offset[p$, d$] | setback distance d$ then absolute points p$ |
| Rectangular[{x$, y$}] | separate radii in the x$ and y$ directions |
* Using Offset[$$] only works for straight lines."

SetCurry2 @ SetbackLine;

$zeroVP = {ZeroP, ZeroP};
$zeroRP = Rectangular[{ZeroP,ZeroP}];
$zeroFP = ZeroP | $zeroRP;
$nullSBSpecP = None | $zeroFP | Offset[ZeroP] | Offset[ZeroP, $zeroFP];

$rectP = Rectangular[Pos2P];
$numRectP = NumP | $rectP;
$sbSpecP = $numRectP | Offset[NumP] | Offset[NumP, $numRectP];

$zeroSpecP = Flatten[$nullSBSpecP, Inf, Alt];

SetbackLine[spec_, $zeroSpecP] :=
  spec;

SetbackLine[spec_ ? Pos2ListsQ, d_] :=
  SetbackLine[#, d]& /@ spec;

SetbackLine[spec_, Scaled[s_ ? NumberQ]] :=
  SetbackLine[spec, LineLength[spec] * s];

SetbackLine[spec_, d:$sbSpecP] :=
  SetbackLine[spec, {d, d}];

(* we can preserve absolute offsets here for simple lines, but in general we can't walk
a complex line in points since we don't know how it will resolve later when a scale is chosen *)
SetbackLine[{a_, b_}, Offset[d_]] := SetbackLine[{a, b}, {Offset[d], Offset[d]}]

SetbackLine[{a2_, b2_}, spec:{Repeated[$sbSpecP, 2]} /; ContainsQ[spec, Offset]] := Locals[
  a = a2; b = b2;
  dx = PointTangent[a, b];
  {pos, off} = Transpose @ Map[FromOffsetNum, spec];
  {a, b} = SetbackLine[{a, b}, pos];
  {d1, d2} = N @ off;
  {Offset[dx * d1, a], Offset[-dx * d2, b]} // SimplifyOffsets
];

SetbackLine[{a_, b_}, {d1_ ? NumberQ, d2_ ? NumberQ}] := Locals[
  If[Dist[a, b] < d1 + d2, Return @ emptyLine[a, b]];
  dx = N @ Normalize[b - a];
  {a + dx * d1, b - dx * d2}
];

SetbackLine[coords_, {d1:$numRectP, d2:$numRectP}] :=
  setbackHalf[setbackHalf[coords, d1], -d2]

SetbackLine::invalidspec = "Invalid setback specification ``."
SetbackLine[coords_, other_] := (
  Message[SetbackLine::invalidspec, other];
  coords
);

$emptyLineD = 0.0001;
emptyLine[a_, b_] := Locals[
  mid = Avg[a, b];
  d = (b-a) * $emptyLineD;
  {mid - d, mid + d}
];

(**************************************************************************************************)

setbackHalf[{}, _] := {};
setbackHalf[p:{_}, _] := p;
setbackHalf[coords_, 0|0.] := coords;
setbackHalf[coords_, d_ ? Negative] := Rev @ setbackHalf[Rev @ coords, Abs[d]];
setbackHalf[coords_, -r_Rectangular] := Rev @ setbackHalf[Rev @ coords, r];

setbackHalf[coords_, d_] := takeLine[coords, d];

takeLine[{a_, b_}, Rectangular[{d1:NumP, d2:NumP}]] := Locals[
  sz = {d1,d2}/2;
  c = LineRectInter[{b, a}, {a - sz, a + sz}];
  If[Norm[c - b] == 0., c = Lerp[b, a, $emptyLineD]];
  {c, b}
];

takeLine[path_, Rectangular[{d1:NumP, d2:NumP}]] := Locals[
  a = First[path];
  sz = {d1,d2}/2; rect = {a-sz, a+sz};
  c = LineRectInter[path, rect];
  n = LengthWhile[path, RegionMember[Rectangle @@ rect]];
  DelDups @ Prepend[Drop[path, n], c]
];

takeLine[{a_, b_}, d:NumP] := (
  If[Dist[a, b] < d, Return @ emptyLine[a, b]];
  dx = N @ Normalize[b - a];
  {a + dx * d, b}
);

takeLine[coords_List, d:NumP] := Locals[
  prev = First @ coords; total = 0;
  n = LengthWhile[coords, curr |-> (total += Dist[curr, prev]; prev = curr; total < d)];
  If[n == Len[coords], Return @ Last @ coords];
  rem = total - d;
  newCoords = Drop[coords, n];
  If[rem == 0,
    newCoords,
    Prepend[newCoords, PointAlongLine[Part[coords, {n + 1, n}], rem]]
  ]
];