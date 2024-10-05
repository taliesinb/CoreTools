SystemExports[
  "SeqFn",
    SequenceNothing,
    SequenceFirst, SequenceSecond, SequenceThird, SequenceLast,
    SequenceFirstSecond, SequenceSecondFirst,
    SequenceMost, SequenceRest, SequenceReverse,
  "Head",
    Unsequence
];

PackageExports[
  "SeqFn",
    Seq0, Seq1, Seq2, Seq3, SeqN, Seq12, Seq21,
    SeqCol1, SeqCol2, SeqCol3,
    SeqMost, SeqRest, SeqRev,
    SeqLen,
  "HoldFn",
    HMapSeq, HSeqLen,
  "ControlFlow",
    ThenNull, ThenPart, ThenFail, ThenFailEval,
    Then1, Then2, Then3
];

(*************************************************************************************************)

DefineAliasRules[
  HMapSeq          -> HoldMapSequence,
  HSeqLen          -> HoldSequenceLength
];

DefineAliasRules[
  Seq0             -> SequenceNothing,
  Seq1             -> SequenceFirst,
  Seq2             -> SequenceSecond,
  Seq3             -> SequenceThird,
  SeqN             -> SequenceLast,
  Seq12            -> SequenceFirstSecond,
  Seq21            -> SequenceSecondFirst,
  SeqMost          -> SequenceMost,
  SeqRest          -> SequenceRest,
  SeqRev           -> SequenceReverse,
  SeqLen           -> SequenceLength
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

