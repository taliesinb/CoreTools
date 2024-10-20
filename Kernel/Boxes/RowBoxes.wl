SystemExports[
  "FormHead",
    DelimitedRow, RiffledRow, RawRow, FullRow,
    BraceForm, AngleForm, ParenForm, BracketForm, DBracketForm,
    BraceSeq, AngleSeq, ParenSeq, BracketSeq, DBracketSeq,
    BraceRow, AngleRow, ParenRow, BracketRow, DBracketRow,
    SpaceSeq, QuadSeq, CommaSeq, ColonSeq, SColonSeq, ArrowSeq,
    SpaceRow, QuadRow, CommaRow, ColonRow, SColonRow, ArrowRow
];

PackageExports[

  "FormHead",
    DelimRow, RiffRow,
    DelimSeq, RiffSeq,

  "BoxFunction",
    DelimSeqBox, RiffSeqBox,
    DelimRowBox, RiffRowBox, RiffBox,
    BraceBox, AngleBox, ParenBox, BracketBox, DBracketBox,
    BraceSeqBox, AngleSeqBox, ParenSeqBox, BracketSeqBox, DBracketSeqBox,
    BraceRowBox, AngleRowBox, ParenRowBox, BracketRowBox, DBracketRowBox,
    SpaceSeqBox, QuadSeqBox, CommaSeqBox, ColonSeqBox, SColonSeqBox, ArrowSeqBox,
    SpaceRowBox, QuadRowBox, CommaRowBox, ColonRowBox, SColonRowBox, ArrowRowBox
];

(**************************************************************************************************)

DefineAliasRules[
  FullCol  -> FullColumn,
  DelimRow -> DelimitedRow,
  RiffRow  -> RiffledRow,
  RiffBox  -> RiffRowBox
];

(**************************************************************************************************)

SetForm0[FullRow];
SetFormR[RawRow];

SystemBox[FullRow[list_List]]        := MakeBox @ RawRow @ list;
SystemBox[FullRow[list_List, riff_]] := MakeBox @ RawRow[list, riff];

SystemBox[RawRow[list_List]]         := RiffRowBox[MapMakeBox @ list, Spc0];
SystemBox[RawRow[list_List, riff_]]  := RiffRowBox[MapMakeBox @ list, MakeBox @ riff];

(**************************************************************************************************)

SetFormR[RiffRow];
SetCurry2 @ RiffRow;

SystemBox[RiffRow[list_List, riff_]] := RiffRowBox[MapMakeBox @ list, MakeBox @ riff];

RiffRowBox[boxes_List, riff_] := riffle1[boxes, riff];

SetCurry2BoxFn @ RiffRowBox;

(**************************************************************************************************)

SystemBox[RiffSeq[r_][a___]] := RiffRowBox[MapMakeBox @ {a}, MakeBox @ r];

RiffSeqBox[r_][b_]    := b;
RiffSeqBox[r_][bs___] := RiffRowBox[{bs}, r];

r:RiffSeqBox[BlankSeq2] := BoxFnErrBox[RiffSeqBox, r];

(**************************************************************************************************)

riffle1[{},      m_]             := "";
riffle1[{b_},    m_]             := b;
riffle1[bs_List, ""]             := RowBox @ bs;
riffle1[bs_List, m_]             := RowBox @ Riffle[bs, m];

riffle3[{},      l_, m_, r_]     := RowBox @ List[l, r];
riffle3[{b_},    l_, m_, r_]     := RowBox @ List[l, b, r];
riffle3[bs_List, l_, "", r_]     := RowBox @ List[l, RowBox @ bs, r];
riffle3[bs_List, l_, m_, r_]     := RowBox @ List[l, RowBox @ Riffle[bs, m], r];

riffle4[bs_,     l_, m_, r_,  0] := riffle3[bs, l, m, r];
riffle4[{},      l_, m_, r_, n_] := RowBox @ List[l, SpacerBox[s * 2], r];
riffle4[{b_},    l_, m_, r_, n_] := RowBox @ List[l, SpacerBox @ s, b, SpacerBox @ s, r];
riffle4[bs_List, l_, "", r_, n_] := RowBox @ List[l, SpacerBox @ s, RowBox @ bs, SpacerBox @ s, r];
riffle4[bs_List, l_, m_, r_, n_] := RowBox @ List[l, SpacerBox @ s, RowBox @ Riffle[bs, m], SpacerBox @ s, r];

(**************************************************************************************************)

SetFormO[DelimRow];

SystemBoxDefs[
  DelimRow[d_][es_List]               := riffle1[MapMakeBox @ es, MakeBox @ d];
  DelimRow[d:Blank3][es_List]         := riffle3[MapMakeBox @ es, MapMakeBox];
  DelimRow[d:Blank3, n:NumP][es_List] := riffle4[MapMakeBox @ es, n];
  DelimSeq[d_][es___]                 := riffle3[MapMakeBox @ List @ es];
  DelimSeq[d:Blank3][es___]           := riffle3[MapMakeBox @ List @ es];
  DelimSeq[d:Blank3, n:NumP][es___]   := riffle4[MapMakeBox @ List @ es, n];
];

DelimSeqBox[d:Blank3][boxes___]           := riffle3[List @ boxes, d];
DelimSeqBox[d:Blank3, n:NumP][boxes___]   := riffle4[List @ boxes, d, n];

DelimRowBox[d:Blank3][boxes_List]         := riffle3[boxes, d];
DelimRowBox[d:Blank3, n:NumP][boxes_List] := riffle4[boxes, d, n];

SetCurryBoxFn[DelimRowBox, DelimSeqBox];

(**************************************************************************************************)

DeclareThenScan[DefineSeqRowFormBox]

DefineSeqRowFormBox[Then[uniForm_, seqForm_, rowForm_, uniBox_, seqBox_, rowBox_, l_, c_, r_]] := Then[
  SystemBoxDefs[
    uniForm[e_]              := uniBox @ MakeBox @ e;
    a_seqForm                := DelimRowBox[l, c, r] @ MapMakeBox @ a;
    rowForm[e_List]          := DelimRowBox[l, c, r] @ MapMakeBox @ e;
    rowForm[e_List, m_]      := DelimRowBox[l, MakeBox @ m, r] @ MapMakeBox @ e;
    rowForm[e_List, m_, s__] := DelimRowBox[l, MakeBox @ Style[m, s], r] @ MapMakeBox @ e;
  ],
  uniBox[b_]              := RowBox[{l, b, r}];
  seqBox[a___]            := DelimSeqBox[l, c, r] @ a,
  rowBox[e_List]          := DelimRowBox[l, c, r] @ e,
  rowBox[e_List, m_]      := DelimRowBox[l, m, r] @ e,
  rowBox[e_List, m_, s__] := DelimRowBox[l, StyleBox[m, s], r] @ e
];

DefineSeqRowFormBox[Then[seqForm_, rowForm_, seqBox_, rowBox_, r_]] := Then[
  SystemBoxDefs[
    a_seqForm            := RiffRowBox[r] @ MapMakeBox @ a;
    rowForm[e_List]      := RiffRowBox[r] @ MapMakeBox @ e;
    rowForm[e_List, s__] := RiffRowBox[StyleBox[r, s]] @ MapMakeBox @ e;
  ],
  seqBox[a___]          := RiffSeqBox[r] @ a,
  rowBox[e_List]        := RiffRowBox[r] @ e,
  rowBox[e_List, s_]    := RiffRowBox[StyleBox[r, s]] @ e
];

(**************************************************************************************************)

SetForm1[BraceForm, AngleForm, ParenForm, BracketForm, DBracketForm]
SetFormR[BraceRow, AngleRow, ParenRow, BracketRow, DBracketRow];
SetFormA[BraceSeq, AngleSeq, ParenSeq, BracketSeq, DBracketSeq];

SetBoxFn[BraceSeqBox, AngleSeqBox, ParenSeqBox, BracketSeqBox, DBracketSeqBox];
SetBoxFn[BraceRowBox, AngleRowBox, ParenRowBox, BracketRowBox, DBracketRowBox];
SetBoxFn[BraceBox, AngleBox, ParenBox, BracketBox, DBracketBox]

DefineSeqRowFormBox[
  BraceForm    ; BraceSeq    ; BraceRow    ; BraceBox    ; BraceSeqBox    ; BraceRowBox    ; LBrace    ; Com ; RBrace,
  AngleForm    ; AngleSeq    ; AngleRow    ; AngleBox    ; AngleSeqBox    ; AngleRowBox    ; LAngle    ; Com ; RAngle,
  ParenForm    ; ParenSeq    ; ParenRow    ; ParenBox    ; ParenSeqBox    ; ParenRowBox    ; LParen    ; Com ; RParen,
  BracketForm  ; BracketSeq  ; BracketRow  ; BracketBox  ; BracketSeqBox  ; BracketRowBox  ; LBracket  ; Com ; RBracket,
  DBracketForm ; DBracketSeq ; DBracketRow ; DBracketBox ; DBracketSeqBox ; DBracketRowBox ; LDBracket ; Com ; RDBracket
];

(**************************************************************************************************)

SetFormR[QuadRow, SpaceRow, CommaRow, ColonRow, SColonRow, ArrowRow];
SetFormA[QuadSeq, SpaceSeq, CommaSeq, ColonSeq, SColonSeq, ArrowSeq];

SetBoxFn[QuadRowBox, SpaceSeqBox, CommaSeqBox, ColonSeqBox, SColonSeqBox, ArrowSeqBox];
SetBoxFn[QuadSeqBox, SpaceRowBox, CommaRowBox, ColonRowBox, SColonRowBox, ArrowRowBox];

DefineSeqRowFormBox[
  SpaceSeq  ; SpaceRow  ; SpaceSeqBox  ; SpaceRowBox  ; Spc <> Spc0,
  QuadSeq   ; QuadRow   ; QuadSeqBox   ; QuadRowBox   ; "    " <> Spc0,
  CommaSeq  ; CommaRow  ; CommaSeqBox  ; CommaRowBox  ; Com,
  ColonSeq  ; ColonRow  ; ColonSeqBox  ; ColonRowBox  ; ColonS,
  SColonSeq ; SColonRow ; SColonSeqBox ; SColonRowBox ; SColonS,
  ArrowSeq  ; ArrowRow  ; ArrowSeqBox  ; ArrowRowBox  ; RuleS
];

