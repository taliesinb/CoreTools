SystemExports[
  "Function",
    Ones, Zeros, Eye,
    RangeArray,
    Zip, Flip,
    ToRowVector, ToColumnVector,
  "ControlFlow",
    ArrayTable
];

PackageExports[
  "Function",
    ConstArr,
    SumNormalize, AbsNormalize, ToStochasticArray,
    EnsurePacked, EnsurePackedReals, EnsurePackedInts,
    FormalSymbolArray,
  "MessageFunction",
    ThrowRealArray
];

(**************************************************************************************************)

SetStrict[Eye, Ones, Zeros];

Eye[d_Int]             := IdentityMatrix @ d;
Eye[r_Int, c_Int]      := IdentityMatrix @ {r, c};
Ones[d___]             := ConstArr[1, d];
Zeros[d___]            := ConstArr[0, d];
ConstArr[c_, d___]     := ConstantArray[c, ToList @ d];

(*************************************************************************************************)

"Zip[list$1, list$2, $$] returns a matrix whose columns are the list$i.";
"Flip[array$] transposes an array.";

SetStrict[Zip, Flip];

Zip[seq___List] := Transpose[{seq}];
Flip[e_List]    := Transpose[e];

(**************************************************************************************************)

SetHoldF[ArrayTable]

ArrayTable[body_]                 := body;
ArrayTable[body_, shape__Int]     := Array[ThenOp[body], shape];
ArrayTable[body_, shape:{___Int}] := Array[ThenOp[body], shape];

(**************************************************************************************************)

SumNormalize[e_] := e / Total[e];
AbsNormalize[e_] := e / Total[Abs @ e];

(*************************************************************************************************)

SetStrict @ ToStochasticArray;

ToStochasticArray[e_ ? NumberArrayQ] := ToPackedReals[e / Total[e, {-1}]];

(**************************************************************************************************)

SetStrict @ RangeArray;

RangeArray[n_Int]      := Range[n];
RangeArray[ns:{__Int}] := Array[List, ns];

(**************************************************************************************************)

SetStrict[ToRowVector, ToColumnVector]

ToRowVector[e_List]    := List @ e;
ToColumnVector[e_List] := Map[List, e];

(**************************************************************************************************)

SetExcepting @ SetHoldR[EnsurePackedReals, EnsurePackedInts, EnsurePacked]

     EnsurePacked[arr_, else_] := Ensure[ToPacked[arr],           PackedQ, else];
 EnsurePackedInts[arr_, else_] := Ensure[ToPacked[arr, Int],      PackedQ, else];
EnsurePackedReals[arr_, else_] := Ensure[ToPacked[N @ arr, Real], PackedQ, else];

(**************************************************************************************************)

SetStrict[ThrowRealArray]

ThrowRealArray[array_, d_Int] := ThrowRealArray[array, {d, d}];

ThrowRealArray[array_, {minD_Int, maxD_Int}] := Module[
  {depth, depthStr, badPos, badValue},
  depth = ArrayDepth[array];
  depthStr = If[minD == maxD, minD, StringForm["`` to ``", minD, maxD]];
  If[!TrueQ[minD <= depth <= maxD], ThrowMsg["realArrayDepthBad", depth, depthStr]];
  badPos = FirstPosition[array, Except[_Real | _Int | _Rational], {d}, Heads -> False];
  If[badPos === None, ThrowMsg["realArray", depthStr]];
  badValue = Extract[array, badPos];
  ThrowMsg["realArrayValue", depthStr, badValue, badPos]
];

General::realArray = "Expected a real-valued array of depth ``."
General::realArrayValue = "Expected a numeric array of depth ``, but non-numeric value `` occurred at position ``."
General::realArrayDepthBad = "Expected a real-valued array of depth ``, but got an array of depth ``.";

(*************************************************************************************************)

SetStrict @ FormalSymbolArray;

$formalsRoman = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

FormalSymbolArray[dims_, str_Str] := FormalSymbolArray[dims, P11 @ StringPosition[$formalsRoman, str] - 1];

FormalSymbolArray[dims_, offset_:0] := Block[{n = 0 + offset}, Array[Symbol @ FromCharCode[63488 + n++]&, dims]];
