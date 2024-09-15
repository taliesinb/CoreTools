PrivateExports[
  "Function",   BinToCounts, BinToLists,
  "IOFunction", setupDBinData, setupBinFractions,
  "Predicate",  HasMissingDataQ
];

(**************************************************************************************************)

HasMissingDataQ[data_ ? PackedArrayQ] := False;
HasMissingDataQ[data_] := MemberQ[data, _Missing | None | _Failure | $Failed];

(**************************************************************************************************)

BinToLists[ids_, n_] := Lookup[PositionIndex @ ids, Range @ n, {}];
BinToCounts[ids_, n_] := Lookup[Counts @ ids, Range @ n, 0];

(**************************************************************************************************)

setupDBinData[data_, n_] := Scope[
  ids = DGetInitialValue @ data;
  binLists = BinToLists[ids, n];
  binCounts = ToPacked[Len /@ binLists];
  dBinData = DValue[binLists, "BinData"];
  dData = DValue[data, "Data"];
  {dData, dBinData, binCounts}
];

(**************************************************************************************************)

setupBinFractions[binCounts_, gapFraction_, finalGap_] := Locals[

  gapF = gapFraction;
  totalCount = Total @ binCounts;

  n = Len @ binCounts;
  gapN = If[finalGap, n, n - 1];
  totalGapF = gapF * gapN;
  (* prevent gaps from taking more than 25% of space *)
  If[totalGapF > 0.25,
    gapF *= 0.25 / totalGapF;
    totalGapF = 0.25;
  ];

  availF = 1.0 - totalGapF;
  countToFrac = availF / totalCount;
  binF = ToPackedReals[binCounts * (availF / totalCount)];

  gapsF = Repeat[gapF, n]; If[!finalGap, Part[gapsF, -1] = 0];

  binFGapped = Riffle[binF, gapsF];
  binFAcc = Accumulate @ Prepend[0] @ binFGapped;

  binFL = Part[binFAcc, 1 ;; -2;; 2];
  binFR = Part[binFAcc, 2 ;; ;; 2];
  binM = Avg[binFL, binFR];

  dBinL = DValue[binFL, "BinL"];
  dBinR = DValue[binFR, "BinR"];
  {dBinL, dBinR, binM, countToFrac}
];
