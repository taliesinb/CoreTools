SystemExports[
  "Function",
    P1, P2, P3, P4, P5, PN, P11, P1N, PNN, PN1, Col1, Col2, Col12, Col21, Col3, Col4, ColN
];

(**************************************************************************************************)

DefineSimpleMacro[P1,   P1[e_] :> Part[e, 1]];
DefineSimpleMacro[P2,   P2[e_] :> Part[e, 2]];
DefineSimpleMacro[P3,   P3[e_] :> Part[e, 3]];
DefineSimpleMacro[P4,   P4[e_] :> Part[e, 4]];
DefineSimpleMacro[P5,   P4[e_] :> Part[e, 5]];
DefineSimpleMacro[PN,   PN[e_] :> Part[e, -1]];
DefineSimpleMacro[P11,  P11[e_] :> Part[e, 1, 1]];
DefineSimpleMacro[P1N,  P1N[e_] :> Part[e, 1, -1]];
DefineSimpleMacro[PNN,  PNN[e_] :> Part[e, -1, -1]];
DefineSimpleMacro[PN1,  PN1[e_] :> Part[e, -1, 1]];
DefineSimpleMacro[Col1, Col1[e_] :> Part[e, All, 1]];
DefineSimpleMacro[Col2, Col2[e_] :> Part[e, All, 2]];
DefineSimpleMacro[Col3, Col3[e_] :> Part[e, All, 3]];
DefineSimpleMacro[Col4, Col4[e_] :> Part[e, All, 4]];
DefineSimpleMacro[ColN, ColN[e_] :> Part[e, All, -1]];

Col12[e_] := {Col1 @ e, Col2 @ e};
Col21[e_] := {Col1 @ e, Col2 @ e};
