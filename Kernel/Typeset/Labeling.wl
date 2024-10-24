PackageExports[
  "FormHead", LabelTopL, LabelTopC, LabelTopR, LabelBotL, LabelBotC, LabelBotR, MapsToForm, HMapsToForm, VMapsToForm,
  "BoxFn",    LabelTopLBox, LabelTopCBox, LabelTopRBox, LabelBotLBox, LabelBotCBox, LabelBotRBox, MapsToBox, HMapsToBox, VMapsToBox
  "Variable", $LabelStyle
];

(*************************************************************************************************)

CoreBox[MapsToForm[lhs_, rhs_]]      := MapsToBox[MakeBox @ lhs, MakeBox @ rhs];
CoreBox[MapsToForm[lhs_, rhs_, sz_]] := MapsToBox[MakeBox @ lhs, MakeBox @ rhs, sz];

CoreBox[HMapsToForm[lhs_List, rhs_List]]      := HMapsToBox[MapMakeBox @ lhs, MapMakeBox @ rhs];
CoreBox[HMapsToForm[lhs_List, rhs_List, sz_]] := HMapsToBox[MapMakeBox @ lhs, MapMakeBox @ rhs, sz];
CoreBox[HMapsToForm[lhs_, rhs_]]              := HMapsToBox[MakeBox @ lhs, MakeBox @ rhs];
CoreBox[HMapsToForm[lhs_, rhs_, sz_]]         := HMapsToBox[MakeBox @ lhs, MakeBox @ rhs, sz];

CoreBox[VMapsToForm[lhs_List, rhs_List]]      := VMapsToBox[MapMakeBox @ lhs, MapMakeBox @ rhs];
CoreBox[VMapsToForm[lhs_List, rhs_List, sz_]] := VMapsToBox[MapMakeBox @ lhs, MapMakeBox @ rhs, sz];
CoreBox[VMapsToForm[lhs_, rhs_]]              := VMapsToBox[MakeBox @ lhs, MakeBox @ rhs];
CoreBox[VMapsToForm[lhs_, rhs_, sz_]]         := VMapsToBox[MakeBox @ lhs, MakeBox @ rhs, sz];

MapsToBox[l_GraphicsBox, r_GraphicsBox] := RowGridBox[{l, FontSizeBox[30] @ BoldBox @ "\[LongRightArrow]", r}, 4, RowJust -> Center];
MapsToBox[l_, r_, size_:Inherit]        := RowBox[{l, FontSizeBox[size] @ BoldBox @ "\[LongRightArrow]", r}];

HMapsToBox[l_List, r_List, size_:Inherit] := GridBox[Thread @ {l, FontSizeBox[size] @ BoldBox @ "\[LongRightArrow]", r}, ColGaps -> 1, RowJust -> Center, ColJust -> Center];
HMapsToBox[l_, r_, size_:Inherit]        := RowGridBox[{l, FontSizeBox[size] @ BoldBox @ "\[LongRightArrow]", r}, 4, RowJust -> Center];

VMapsToBox[l_List, r_List, size_:Inherit] := GridBox[Flip @ Thread @ {l, FontSizeBox[size] @ BoldBox @ "\[DownArrow]", r}, RowGaps -> 2, RowJust -> Center, ColJust -> Center];
VMapsToBox[l_, r_, size_:Inherit]         := ColGridBox[{l, FontSizeBox[size] @ BoldBox @ "\[DownArrow]", r}, 3, ColJust -> Center];

(*************************************************************************************************)

SetInitial[$LabelStyle, {FontFamily -> "Source Code Sans", FontWeight -> Bold}];

SetCurry1[LabelTopL, LabelTopC, LabelTopR, LabelBotL, LabelBotC, LabelBotR];

CoreBox[LabelTopL[label_, expr_, spc_:0.2]] := LabelTopLBox[MakeBox @ label, MakeBox @ expr, spc];
CoreBox[LabelTopC[label_, expr_, spc_:0.2]] := LabelTopCBox[MakeBox @ label, MakeBox @ expr, spc];
CoreBox[LabelTopR[label_, expr_, spc_:0.2]] := LabelTopRBox[MakeBox @ label, MakeBox @ expr, spc];
CoreBox[LabelBotL[label_, expr_, spc_:0.2]] := LabelBotLBox[MakeBox @ label, MakeBox @ expr, spc];
CoreBox[LabelBotC[label_, expr_, spc_:0.2]] := LabelBotCBox[MakeBox @ label, MakeBox @ expr, spc];
CoreBox[LabelBotR[label_, expr_, spc_:0.2]] := LabelBotRBox[MakeBox @ label, MakeBox @ expr, spc];

LabelTopLBox[label_, expr_, spc_:0.2] := vLabelBox[label, expr, Id,  Lef, spc];
LabelTopCBox[label_, expr_, spc_:0.2] := vLabelBox[label, expr, Id,  Cen, spc];
LabelTopRBox[label_, expr_, spc_:0.2] := vLabelBox[label, expr, Id,  Rig, spc];
LabelBotLBox[label_, expr_, spc_:0.2] := vLabelBox[label, expr, Rev, Lef, spc];
LabelBotCBox[label_, expr_, spc_:0.2] := vLabelBox[label, expr, Rev, Cen, spc];
LabelBotRBox[label_, expr_, spc_:0.2] := vLabelBox[label, expr, Rev, Rig, spc];

vLabelBox[labelBox_, exprBox_, fn_, pos_, spc_] := ColumnBox[
  fn @ List[
    StyleBox[labelBox, ToSeq @ $LabelStyle],
    exprBox
  ],
  pos, spc
];

SetCurry1BoxFn[LabelTopLBox, LabelTopCBox, LabelTopRBox, LabelBotLBox, LabelBotCBox, LabelBotRBox];
