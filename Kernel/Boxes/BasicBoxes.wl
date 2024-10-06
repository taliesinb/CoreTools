SystemExports[
  "FormHead",
    LiteralStringRow, NullForm
];

PackageExports[
  "FormHead",
    StrForm,
  "BoxFn",
    RBox, SeqBox, NullBox,
    FnBox, FnRowBox,
    StrFormBox, BoxMsgBox,
    SpacerBox, SkelBox,
    LitStrBox, LitStrRowBox
];

(**************************************************************************************************)

RBox[args___]   := RowBox[{args}];
SeqBox[args___] := RowBox[{args}];

(**************************************************************************************************)

SetHoldA[NullForm, NullBox];
SetForm0 @ NullForm;

MakeBox[NullForm, _] := "";
MakeBox[_NullForm, _] := "";

_NullBox := "";

(**************************************************************************************************)

FnRowBox[f_, {}]         := RowBox[{f, "[", "]"}];
FnRowBox[f_, {box_}]     := RowBox[{f, "[", box, "]"}];
FnRowBox[f_, row_List]   := RowBox[{f, "[", RowBox @ Riffle[row, ","], "]"}];
FnRowBox[f_, row_RowBox] := RowBox[{f, "[", row, "]"}];

FnBox[f_Str, args___]   := FnRowBox[f, {args}];

SetCurry1BoxFn[FnRowBox, FnBox]

(**************************************************************************************************)

SetBoxFn @ SpacerBox;

SpacerBox[s:NumP]         := TemplateBox[{s}, "Spacer1"];
SpacerBox[w:NumP, h:NumP] := TemplateBox[{w, h}, "Spacer2"];

(**************************************************************************************************)

SetBoxFn @ SkelBox;

SkelBox[b_] := RowBox[{LSkel, b, RSkel}];

(**************************************************************************************************)

SystemBox[StrForm[args___]] := StrFormBox[args];

StrFormBox = CaseOf[
  msg_Str            := MakeBoxes @ msg;
  $[msg_Str, args__] := First @ ToBoxes @ StringForm[msg, args];
  $[args___]         := ToBoxes @ RowBox @ Riffle[{LSkel, args, RSkel}, ";"];
];

(*************************************************************************************************)

BoxMsgBox = CaseOf[
  $[e_Str, args__]          := $ @ StringForm[e, args];
  $[e:(_Str | _StringForm)] := ErrBox @ MakeBox @ e;
  _                         := ErrBox @ "InvalidBoxMsgBox";
];

(**************************************************************************************************)

SetBoxFn @ LitStrBox;

LitStrBox[s_Str] := ToBoxes @ LitStr @ s; (* does this do anything ? *)

(**************************************************************************************************)

SystemBox[LitStrRow[s:{StrP...}, r:StrP:","]] := LitStrRowBox[s, r];

(**************************************************************************************************)

SetBoxFn @ LitStrRowBox;

LitStrRowBox[{}, ___]              := LitStrBox @ "";
LitStrRowBox[s:{__Str}, r_Str:","] := LitStrBox @ StrJoin @ Riffle[s, r];

