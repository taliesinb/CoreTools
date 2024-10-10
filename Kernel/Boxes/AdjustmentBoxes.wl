PackageExports[
  "FormHead",
    TightForm, SpanSizeForm,
    RaiseForm, LowerForm, MarginForm,
    PadForm, HPadForm, VPadForm, LRPadForm, BTPadForm,
  "BoxFunction",
    TightBox, SpanSizeBox, NoLineBreakBox,
    RaiseBox, LowerBox, MarginBox,
    PadBox, LPadBox, RPadBox, BPadBox, TPadBox, HPadBox, VPadBox, LRPadBox, BTPadBox
];

(**************************************************************************************************)

SetForm1 @ TightForm;
SetBoxFn @ TightBox;

SystemBox[TightForm[e_]] := TightBox @ MakeBoxes @ e;

TightBox[e_] := StyleBox[e, AutoSpacing -> False, AutoIndent -> False, LineBreakWithin -> False];

(**************************************************************************************************)

SetForm1 @ SpanSizeForm;
SetCurry23BoxFn @ SpanSizeBox;

SystemBox[SpanSizeForm[e_, min_, max_]] := TightBox @ MakeBoxes @ e;

SpanSizeBox[box_, min_, max_] := StyleBox[box, SpanMinSize -> min, SpanMaxSize -> max];

(**************************************************************************************************)

NoLineBreakBox[box_] := StyleBox[box, LineBreakWithin -> False];

(**************************************************************************************************)

SetForm1[RaiseForm, LowerForm, MarginForm];
SetCurry2BoxFn[RaiseBox, LowerBox];
SetBoxFn[MarginBox];

SystemBoxDefs[
  RaiseForm[e_, a___]  := RaiseBox[MakeBoxes @ e, a];
  LowerForm[e_, a___]  := LowerBox[MakeBoxes @ e, a];
  MarginForm[e_, m___] := MarginBox[MakeBoxes @ e, m];
];

RaiseBox[e_, n:NumP] := AdjBox[e, BoxBaselineShift -> -n];
LowerBox[e_, n:NumP] := AdjBox[e, BoxBaselineShift -> n];

MarginBox[boxes_, lr:Num2P, bt:Num2P:{0,0}]          := AdjustmentBox[boxes, BoxMargins -> {lr, bt}];
MarginBox[boxes_, lr:Num2P, bt:Num2P:{0,0}, bl:NumP] := AdjustmentBox[boxes, BoxMargins -> {lr, bt}, BoxBaselineShift -> bl];

(**************************************************************************************************)

SetForm1[PadForm, HPadForm, VPadForm, LRPadForm, BTPadForm];

SystemBoxDefs[
    PadForm[e_, a___] := PadBox[MakeBox @ e, a];
   HPadForm[e_, a___] := HPadBox[MakeBox @ e, a];
   VPadForm[e_, a___] := VPadBox[MakeBox @ e, a];
  LRPadForm[e_, a___] := HPadBox[MakeBox @ e, a];
  BTPadForm[e_, a___] := HPadBox[MakeBox @ e, a];
];

(**************************************************************************************************)

padBox4[boxes_, l_, r_, b_, t_] := AdjBox[boxes, BoxMargins -> {{l, r}, {b, t}}];

(**************************************************************************************************)

LPadBox[boxes_, l:NumP] := padBox4[boxes, l, 0, 0, 0];
RPadBox[boxes_, r:NumP] := padBox4[boxes, 0, r, 0, 0];
BPadBox[boxes_, b:NumP] := padBox4[boxes, 0, 0, b, 0];
TPadBox[boxes_, t:NumP] := padBox4[boxes, 0, 0, 0, t];
HPadBox[boxes_, p:NumP] := padBox4[boxes, p, p, 0, 0];
VPadBox[boxes_, p:NumP] := padBox4[boxes, 0, 0, p, p];

SetCurry2BoxFn[LPadBox, RPadBox, BPadBox, TPadBox, HPadBox, VPadBox];

(**************************************************************************************************)

LRPadBox[boxes_, l:NumP, r:NumP]  := padBox4[boxes, l, r, 0, 0];
BTPadBox[boxes_, b:NumP, t:NumP]  := padBox4[boxes, 0, 0, b, t];

SetCurry23BoxFn[LRPadBox, BTPadBox];

(**************************************************************************************************)

PadBox[boxes_, l:NumP, r:NumP, b:NumP, t:NumP] := padBox4[boxes, l, r, b, t];
PadBox[l:NumP, r:NumP, b:NumP, t:NumP][boxes_] := padBox4[boxes, l, r, b, t];

p:PadBox[___][___] := BoxFnErrBox[PadBox, p];
p:PadBox[Blank03]  := BoxFnErrBox[PadBox, p];


