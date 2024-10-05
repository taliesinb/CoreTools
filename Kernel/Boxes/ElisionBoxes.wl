SystemExports[
  "FormHead",   ElidedForm, LDotsForm, CDotsForm,
  "BoxSymbol",  LDots, CDots, VDots
];

PackageExports[
  "FormHead",   HoldElidedForm,
  "BoxFn",      LDotsBox, CDotsBox, LenBox, HLenBox,
                ElidedBox, HoldElidedBox
];

(**************************************************************************************************)

SystemBox[LDots] := LDotsS;
SystemBox[CDots] := CDotsS;
SystemBox[VDots] := VDotsS;

CoreBox[LDotsForm[expr_, pos_:Below]] := LDotsBox[MakeBoxes @ expr, pos];
CoreBox[CDotsForm[expr_, pos_:Below]] := CDotsBox[MakeBoxes @ expr, pos];

LDotsBox[]               := LDotsS;
LDotsBox[None]           := LDotsS;
LDotsBox[num_Int]        := LDotsBox[num, True];
LDotsBox[num_Int, False] := SubscriptBox[VDotsS, IntStr @ num];
LDotsBox[num_Int, True]  := SubscriptBox[LDotsS, RaiseBox[IntStr @ num, 1]];

CDotsBox[]               := CDotsS;
CDotsBox[None]           := CDotsS;
CDotsBox[num_Int]        := CDotsBox[num, True];
CDotsBox[num_Int, False] := SubscriptBox[VDotsS, IntStr @ num];
CDotsBox[num_Int, True]  := SubscriptBox[CDotsS, RaiseBox[IntStr @ num, 1]];

(**************************************************************************************************)

LenBox[e_] := HLenBox @ e;

SetHoldC[HLenBox];

HLenBox[AtomP] := CDotsS;
HLenBox[e_]    := LDotsBox @ HLen @ e;

(**************************************************************************************************)

DeclaredHere[ElidedForm, HoldElidedForm];

SetHoldC[HoldElidedForm, HoldElidedBox];

CoreBox[ElidedForm[expr_]]     := HoldElidedBox @ expr;
CoreBox[HoldElidedForm[expr_]] := HoldElidedBox @ expr;

ElidedBox[expr_] := HoldElidedBox @ expr;

HoldElidedBox = CaseOf[
  l_List           := elidedListBox @ l;
  s:StrP           := elidedStrBox @ s;
  d:DictP          := elidedDictBox @ d;
  s:ASetP          := elidedSetBox @ s;
  (s_Sym)[]        := RBox[AliasSymName @ s, "[", "]"];
  e:(s_Sym)[___]   := RBox[AliasSymName @ s, "[", HLenBox @ e, "]"];
  e_               := HLenBox @ e;
];

SetHoldC[elidedSetBox, elidedListBox, elidedStrBox, elidedDictBox];

elidedSetBox[s_[a_]]       := RBox[AliasSymName @ s, "[", If[HEmptyQ @ a, Nothing, HLenBox @ a], "]"];

elidedListBox[{}]          := RBox[LBrace, RBrace];
elidedListBox[l_List]      := RBox[LBrace, HLenBox @ l, RBrace];

elidedStrBox[""]           := RBox[DQ, DQ];
elidedStrBox[s_Str]        := RBox["\[OpenCurlyDoubleQuote]", LDotsBox @ StrLen @ s, "\[CloseCurlyDoubleQuote]"];

elidedDictBox[EmptyDict]   := RBox[LAssoc, RAssoc];
elidedDictBox[EmptyUDict]  := RBox["UDict", "[", "]"];
elidedDictBox[d_ ? UDictQ] := RBox["UDict", "[", HLenBox @ d, "]"];
elidedDictBox[d_]          := RBox[LAssoc, HLenBox @ d, RAssoc];
