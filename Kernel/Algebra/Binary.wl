PackageExports[
  "Function",
    BitFn, BitXorFn,
    LFSR, LFSRWidth, LFSRSequence,
    BitTable,
    BitFnArity, BoolFnArity, BoolFnIndex
];

(**************************************************************************************************)

SetStrict[BoolFnArity, BoolFnIndex];

BoolFnArity[FalseFn]  := 1;
BoolFnArity[TrueFn]   := 1;
BoolFnArity[f_BoolFn] := DiscreteMath`DecisionDiagram`BDDArity[f];

BoolFnIndex[FalseFn]  := 0;
BoolFnIndex[TrueFn]   := 1;
BoolFnIndex[f_BoolFn] := FromDigits[Boole @ MapApply[f, Tuples[{True, False}, BoolFnArity @ f]], 2]

(**************************************************************************************************)

ToBoolFn[0] := FalseFn;
ToBoolFn[1] := TrueFn;
ToBoolFn[n_Int ? Positive] := BoolFn[n];

(**************************************************************************************************)

SetStrict[BitFnArity, BitFnIndex];

BitFnArity[BitXorFn[_, n_Int]] := n
BitFnArity[BitFn[_, n_Int]]    := n
BitFnArity[LFSR[f_, _]]        := BitFnArity @ f;

BitFnIndex[BitFn[f_Int, _]] := n
BitFnIndex[BitXorFn[_, _]]  := Unimplemented[]; (* TODO: convert to BoolFn index *)
BitFnIndex[LFSR[f_, _]]     := BitFnIndex @ f;

(**************************************************************************************************)

BitFn[f_Int] := With[{bf = ToBoolFn @ f}, BitFn[bf, BoolFnArity @ bf]];
BitFn[f_, n_Int][i_ ? IntOrVecQ] := bitFnE[f, n, i];

bitFnE[f_, n_, i_Int] := Boole[f @@ IntegerDigits[i, 2, n]];
bitFnE[f_, n_, i_]    := Boole[f @@@ IntegerDigits[i, 2, n]];

CoreBox[BitFn[f_BoolFn, w_Int]] := RowBox[{"BitFn", RowBox[{"[", IntStr @ BoolFnIndex @ f, ",", IntStr @ w, "]"}]}];

(**************************************************************************************************)

BitXorFn[f_Int] := BitXorFn[f, Max[BitLength @ f, 1]];
BitXorFn[f_Int, _Int][i_ ? IntOrVecQ] := bitXorE[f, i];

bitXorE[f_, i_] := BitAnd[BitPopCount[BitAnd[f, i]], 1];

(**************************************************************************************************)

SetStrict[LFSRWidth];

LFSRWidth[LFSR[_, n_Int]] := n;

(**************************************************************************************************)

LFSR[f_Int, w_Int][i_ ? IntOrVecQ] := lfsrE[f, w, i];
lfsrE[f_, w_, i_] := BitOr[BitClear[BitShiftRight @ i, w], BitShiftLeft[bitXorE[f, i], w]];

(**************************************************************************************************)

SetStrict @ LFSRSequence;

LFSRSequence[{f_Int, w_Int} | LFSR[f_Int, w_Int], i_Int, steps_Int] := lsfrSeqE[f, w, i, steps];

lsfrSeqE[f_, w_, i_, s_] := ToPackedInts @ NestList[lfsrE[f, w, #]&, i, s]

(**************************************************************************************************)

BitTable[f_] := Locals[
  a = BitFnArity[f];
  i = Range[0, 2^a-1];
  o = ToPackedInts @ f @ i;
  If[!IntVecQ[o], ReturnFailed[]];
  o
];

(*
BitIntTruthTable[f_, n_Int, k_] := Locals[
  inputs = Range[0, 2^n-1];
  outputs = Map[f, inputs];
  Print[outputs];
  outputs = Which[
    IntVecQ[outputs], BitAnd[outputs, 1],
    BoolVecQ[outputs], Boole @ outputs,
    True, ReturnFailed["notBoolFn", f];
  ];
  table = ZipMap[Append, IntegerDigits[inputs, 2, n], outputs];
  NumericArray[table]
];

BitTruthTable::notBoolFn = "Function `` did not return an integer or boolean."

BitVectorFnTable[f_, n_Int] := Locals[
  values = Table[f[i], {i, 0, 2^n-1}];
  values = Which[
    IntVecQ[values], BitAnd[values, 1],
    BoolVecQ[values], Boole @ values,
    True, ReturnFailed["notBoolFn", f];
  ];
  NumericArray[values]
];
*)