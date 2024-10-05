PackageExports[
  "Function", DistMatrix, NDistMatrix, SqDistMatrix, NSqDistMatrix, MinDist
];

(**************************************************************************************************)

General::distMatrixBadArray = "Input was not a vector or matrix or a pair of these of matching rank.";

(**************************************************************************************************)

DistMatrix = CaseOf[
  $[{}]                           := {};
  $[{}, _]                        := {};
  $[_, {}]                        := {};
  $[a_ ? RealMatQ]                := $distMatrixFn1[a, $distCodeSqrEuc, False];
  $[a_ ? RealMatQ, b_ ? RealMatQ] := $distMatrixFn2[a, b, $distCodeEuc, False];
  $[a_ ? VecQ]                    := Block[{Get}, DistanceMatrix[a]];
  $[a_ ? VecQ, b_ ? VecQ]         := DistanceMatrix[a, b];
  _                               := ErrorMessage["distMatrixBadArray"];
];
(* ^ unfortunately system one is faster in this case but loads a bunch of junk *)

(**************************************************************************************************)

SqDistMatrix = CaseOf[
  $[{}]                           := {};
  $[{}, _]                        := {};
  $[_, {}]                        := {};
  $[a_ ? RealMatQ]                := $distMatrixFn1[a, $distCodeSqrEuc, False];
  $[a_ ? RealMatQ, b_ ? RealMatQ] := $distMatrixFn2[a, b, $distCodeSqrEuc, False];
  $[a_ ? VecQ]                    := DistanceMatrix[a, DistFn -> SqrDist];
  $[a_ ? VecQ, b_ ? VecQ]         := DistanceMatrix[a, b, DistFn -> SqrDist];
  _                               := ErrorMessage["distMatrixBadArray"];
];

(**************************************************************************************************)

SetStrict[NDistMatrix, NSqDistMatrix];

NDistMatrix[a_]       := DistMatrix[EnsurePackedReals[a, $Failed]];
NDistMatrix[a_, b_]   := DistMatrix[EnsurePackedReals[a, $Failed], EnsurePackedReals[b, $Failed]];

NSqDistMatrix[a_]     := SqDistMatrix[EnsurePackedReals[a, $Failed]];
NSqDistMatrix[a_, b_] := SqDistMatrix[EnsurePackedReals[a, $Failed], EnsurePackedReals[b, $Failed]];

(**************************************************************************************************)

SetStrict @ MinDist;

MinDist[{}] := 0;
MinDist[coords_] := Locals[
  dists = DistMatrix[N @ coords];
  Min @ DeleteCases[Flatten @ dists, ZeroP]
];

(**************************************************************************************************)

$distMatrixFn1 := ($loadNumericArrayFns; $distMatrixFn1)
$distMatrixFn2 := ($loadNumericArrayFns; $distMatrixFn2)

$loadNumericArrayFns := (
  Clear[$loadNumericArrayFns];
  {getDistCode, $distMatrixFn1, $distMatrixFn2} = GetPackageSymbol[
    "NumericArrayUtilities`DistanceMatrix`PackagePrivate`",
    {"$extractLLDMMethod", "mTensorDistanceMatrix1Arg", "mTensorDistanceMatrix2Arg"}
  ];
  (* the final bool is whether to use OpenMP. maybe worth trying? *)
  $distCodeSqrEuc = getDistCode["SquaredEuclideanDistance"];
  $distCodeEuc    = getDistCode["EuclideanDistance"];
);
