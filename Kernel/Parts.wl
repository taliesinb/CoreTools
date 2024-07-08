SystemExports[
  "Function",
    Ends, Second, Third, P1, P2, P3, P4, P11, P1N, PNN, PN1, Col1, Col2, Col3, ColL, MaybePart, PartOr
];

(**************************************************************************************************)

DeclareHoldRest[Second, Third]

Second[e_] /; Len[e] < 2 := None;
Second[e_] := Part[e, 2];
Second[e_, else_] /; Len[e] < 2 := else;
Second[e_, _] := Part[e, 2];

Third[e_] /; Len[e] < 3 := None;
Third[e_] := Part[e, 3];
Third[e_, else_] /; Len[e] < 3 := else;
Third[e_, _] := Part[e, 3];

(**************************************************************************************************)

P1[e_] := Part[e, 1];
P2[e_] := Part[e, 2];
P3[e_] := Part[e, 3];
P4[e_] := Part[e, 4];

P11[e_] := Part[e, 1, 1];
P1N[e_] := Part[e, 1, -1];
PNN[e_] := Part[e, -1, -1];
PN1[e_] := Part[e, -1, 1];

Col1[e_] := Part[e, All, 1];
Col2[e_] := Part[e, All, 2];
Col3[e_] := Part[e, All, 3];
ColL[e_] := Part[e, All, -1];

(**************************************************************************************************)

Ends[e_] /; Length[e] == 0 := {None, None};
Ends[e_] := {First[e], Last[e]};

(**************************************************************************************************)

DeclareCurry2[MaybePart]

MaybePart[e_, p_] := FastQuietCheck[Part[e, p]];

(**************************************************************************************************)

DeclareCurry23[PartOr]
DeclareHoldRest[PartOr]

PartOr[e_, p_, else_] := FastQuietCheck[Part[e, p], else];

