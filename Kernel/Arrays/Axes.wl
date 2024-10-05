PackageExports[
  "Function",
    MapAxis, MapAxisP, ScanAxisP, ApplyAxis,
    MapAxisN, ApplyAxisN,
    MoveAxis, DotAxis,
    MoveToPermutation
];

(**************************************************************************************************)

SetCurry1[MapAxisN, ApplyAxisN]

MapAxisN[f_, arr_]   := Map[f, arr, {-2}];
ApplyAxisN[f_, arr_] := Apply[f, arr, {-2}];

(**************************************************************************************************)

SetCurry12[MapAxis, MapAxisP, ScanAxisP, ApplyAxis]

MapAxis[f_, n_ ? Negative, arr_] := Map[f, arr, {n}];
MapAxis[f_, n_, arr_]            := toAxis[Map[f, #, {-2}]&, n, arr];
MapAxisP[f_, n_, arr_]           := toAxis[MapIndexed[prep /* f, #, {-2}]&, n, arr];
ScanAxisP[f_, n_, arr_]          := toAxis[ScanIndexed[prep /* f, #, {-2}]&, n, arr];
ApplyAxis[f_, n_, arr_]          := toAxis[Apply[f, #, {-2}]&, n, arr];

prep[e_, p_] := Prepend[p, e];

(* TODO: check if depth reduced, message if so.
also, the old code was broken, so double check this now *)
toAxis[fn_, n_, arr_] := Locals[
  d = ArrayDepth[arr];
  t = MoveToPermutation[n -> d, d];
  Transpose[fn @ Transpose[arr, t], Ordering @ t]
];

(*************************************************************************************************)

"`MoveAxis[c, i -> j]` sends axis `i` to axis `j`."

SetCurry2[MoveAxis]

MoveAxis[arr_, s_ -> s_] := arr;
MoveAxis[arr_, 1 -> -1]  := OutermostToInnermost @ arr;
MoveAxis[arr_, -1 -> 1]  := InnermostToOutermost @ arr;
MoveAxis[arr_, spec_]    := Transpose[arr, MoveToPermutation[spec, ArrayDepth @ spec]];

(*************************************************************************************************)

"`DotAxis[arr_1, i, j, arr_2]` constracts axis `i` of `arr_1` with axis `j` of `arr_2`."

DotAxis[a_, axisA_Int, axisB_Int, b_] := Dot[MoveAxis[a, axisA -> -1], MoveAxis[b, axisB -> 1]];

(*************************************************************************************************)

MoveToPermutation[spec_, n_Int] := moveToPerm[n, spec];

moveToPerm[n_, spec_] := moveToPerm0[n, spec /. i_Negative :> (d - i + 1)];

moveToPerm0 = CaseOf[
  $[n_, s_Int -> t_Int] := moveToPerm1[n, {{s}, {t}}];
  $[n_, s_List -> t_List]       := moveToPerm1[n, {s, t}];
  $[n_, st:{__Rule}]            := moveToPerm1[n, {Keys @ st, Values @ st}];
  $[_, e_]                      := Then[Message[MoveAxis::invalidAxisMove, e], {}];
];

moveToPerm1[n_, spec_] := moveToPerm1[n, spec] =
  moveToPerm2[n, spec /. i_ ? Negative :> (n + 1 - i)];

moveToPerm2 = CaseOf[
  $[n_, {{s_}, {s_}}]           := {};
  $[n_, {{s_}, {1 }}] /; s <= n := RotateLeft @ Range @ s;
  $[n_, {{1 }, {s_}}] /; s <= n := RotateRight @ Range @ s;
  $[n_, {{s_}, {t_}}] /; Max[s, t] <= n := Locals[
    inds = Range @ Max[s, t, n];
    If[s < t,
      Part[inds, s ;; t] //= RotateLeft,
      Part[inds, t ;; s] //= RotateRight
    ];
    inds
  ];
  $[n_, {s_, t_}] /; n > 0 := Locals[
    max = Max[s, t, n];
    inds = Delete[Range @ max, List /@ s];
    inds = PadRight[inds, max];
    inds = Insert[inds, 0, List /@ t];
    Part[inds, t] = s;
    Ordering @ Take[inds, max]
  ]
];

General::invalidAxisMove = "`` is not a valid axis move specification.";

