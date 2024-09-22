SystemExports[
  "Function",
    SequenceNothing,
    SequenceFirst, SequenceSecond, SequenceThird, SequenceLast, SequenceMost, SequenceRest, SequenceReverse, SequenceFirstSecond, SequenceSecondFirst,
  "Head",
    Unsequence
];

PackageExports[
  "Function",    Seq0, Seq1, Seq2, Seq3, SeqN, SeqMost, SeqRest, SeqReverse, Seq12, Seq21,
  "Function",    SeqLen, HSeqLen, HoldSeqLen,
  "Function",    SeqCol1, SeqCol2, SeqCol3,
  "ControlFlow", ThenNull, ThenPart, ThenFail, ThenFailEval, Then1, Then2, Then3
];

(*************************************************************************************************)

DefineAliasRules[
  SeqLen           -> SequenceLength,
  HSeqLen          -> HoldSequenceLength,
  HoldSeqLen       -> HoldSequenceLength
];

DefineAliasRules[
  Seq0             -> SequenceNothing,
  Seq1             -> SequenceFirst,
  Seq2             -> SequenceSecond,
  Seq3             -> SequenceThird,
  SeqN             -> SequenceLast,
  SeqMost          -> SequenceMost,
  SeqRest          -> SequenceRest,
  SeqReverse       -> SequenceReverse,
  Seq12            -> SequenceFirstSecond,
  Seq21            -> SequenceSecondFirst
];

(*************************************************************************************************)

SetHoldSeq[SeqCol1, SeqCol2, SeqCol3];

SeqCol1[a___] := Part[NoEval @ a, All, 1];
SeqCol2[a___] := Part[NoEval @ a, All, 2];
SeqCol3[a___] := Part[NoEval @ a, All, 3];

_SequenceNothing := Seq[];
SequenceFirst[e_, ___]       := e;
SequenceSecond[_, e_, ___]   := e;
SequenceThird[_, _, e_, ___] := e;
SequenceLast[___, e_]        := e;
SequenceMost[e___, _]        := e;
SequenceRest[_, e___]        := e;
SequenceReverse[e___]        := Seq @@ Reverse[{e}];

SequenceFirstSecond[e1_, e2_, ___] := Seq[e1, e2];
SequenceSecondFirst[e1_, e2_, ___] := Seq[e2, e1];

(*************************************************************************************************)

SetHoldSeq[Unsequence]

SetHoldC[ThenNull, ThenPart, ThenFail, ThenFailEval, Then1, Then2, Then3]

(* NOTE: Unevaluated[1, 2] turns into Sequence[1, 2] when substituted, which is good *)

ThenNull[e___]        := Then[e, Null];
ThenFail[e___]        := Then[e, $Fail];
ThenFailEval[e___]    := Then[e, FailEval];
ThenPart[n_Int, e___] := Part[Unsequence[e], n];

Then1[e___] := P1 @ Unsequence[e];
Then2[e___] := P2 @ Unsequence[e];
Then3[e___] := P3 @ Unsequence[e];

