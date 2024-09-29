SystemExports[
  "Function",
    P1, P2, P3, P4, P5, PN, P11, P1N, PNN, PN1, Col1, Col2, Col12, Col21, Col3, Col4, ColN
];

(**************************************************************************************************)

SimpleMacroDefs[
  P1[e_]   := Part[e, 1],
  P2[e_]   := Part[e, 2],
  P3[e_]   := Part[e, 3],
  P4[e_]   := Part[e, 4],
  P4[e_]   := Part[e, 5],
  PN[e_]   := Part[e, -1],
  P11[e_]  := Part[e, 1, 1],
  P1N[e_]  := Part[e, 1, -1],
  PNN[e_]  := Part[e, -1, -1],
  PN1[e_]  := Part[e, -1, 1],
  Col1[e_] := Part[e, All, 1],
  Col2[e_] := Part[e, All, 2],
  Col3[e_] := Part[e, All, 3],
  Col4[e_] := Part[e, All, 4],
  ColN[e_] := Part[e, All, -1]
];

Col12[e_] := {Col1 @ e, Col2 @ e};
Col21[e_] := {Col1 @ e, Col2 @ e};
