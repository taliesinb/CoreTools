PackageExports[
  "SpecFn", SystemBox, MapMakeBox, KVMapMakeBox, MapSeqMakeBox, MapMakeBox2, MapMakeBox1, ClearFormRegistry,
  "Pred",   FormHeadQ, AtomFormHeadQ, CompoundFormHeadQ, OperatorFormHeadQ,
  "BoxFn",  BoxFnErrBox, ErrBox,
  "Fn",     CompoundFormArity,
  "MetaFn", SystemBoxDefs,
            SetForm0, SetForm1, SetForm2, SetForm3, SetFormA,
            SetFormR, SetFormC, SetFormG, SetFormO,
            SetBoxFn, SetCurryBoxFn, SetCurry1BoxFn, SetCurry2BoxFn, SetCurry23BoxFn
];

SessionExports[
  "CacheVariable",
    $AtomFormStore,
    $CompoundFormStore,
    $OperatorFormStore
];

(**************************************************************************************************)

SetHoldA[MapMakeBox, KVMapMakeBox, MapSeqMakeBox]

MapMakeBox = CaseOf[
  items_Dict := KeyMapValueMap[MakeBoxes, MakeBoxes, items]; (* KMVM preserves holdness *)
  items_List := HoldMap[MakeBoxes, items];
  _[args___] := HoldMap[MakeBoxes, List @ args];
];

KVMapMakeBox = CaseOf[
  items_Dict := KeyValueMap[Fn[{k, v}, {MakeBox[k], MakeBox[v]}, HoldAllComplete], items];
];

MapSeqMakeBox[items___] := Map[MakeBoxes, NoEval @ items]

(**************************************************************************************************)

SetHoldA[MapMakeBox2, MapMakeBox1];

MapMakeBox2[items_List] := Map[MapMakeBox1, NoEval @ items];
e_MapMakeBox2           := List @ List @ BoxFnErrBox[MapMakeBox2, e];

MapMakeBox1[items_List] := Map[MakeBox, NoEval @ items];
e_MapMakeBox1           := List @ BoxFnErrBox[MapMakeBox1, e];

(**************************************************************************************************)

SystemBox /: SetDelayed[SystemBox[lhs_], rhs_] := (
  MakeBoxes[lhs, StandardForm | TraditionalForm] := rhs;
);

Protect[SystemBox];

(**************************************************************************************************)

DeclareThenScan[SystemBoxDefs]

SystemBoxDefs[SetDelayed[lhs_, rhs_]] := (
  MakeBoxes[lhs, StandardForm | TraditionalForm] := rhs;
);

(*************************************************************************************************)

SetHoldC[BoxFnErrBox, dynCodeBox];

BoxFnErrBox[h_Sym, e_] := ErrBox[StyleBox[AliasSymName @ h, FontSize -> 10], dynCodeBox @ e];
BoxFnErrBox[_, e_]     := ErrBox[None, dynCodeBox @ e];
BoxFnErrBox[e_]        := ErrBox[None, dynCodeBox @ e];
BoxFnErrBox[]          := ErrBox[None];

dynCodeBox[expr_] := CreateCachedBox[CodeStyleBox @ MakeCodeBoxes[expr, 2, 4]]

(*************************************************************************************************)

ErrBox[boxes_, tooltip_] := TooltipBox[ErrBox @ boxes, tooltip];

ErrBox[None] := Make[GraphicsBox, {},
  Background -> $LightRed,
  ImageSize -> {10,10}, BaselinePosition -> Scaled[0.1]
];

$redFrameOpts = Seq[
  FrameMargins -> {{3, 3}, {-1, -1}},
  BLinePos   -> Baseline,
  AutoPad    -> True,
  Just       -> Center,
  Background -> Lighter[$LightRed],
  FrameStyle -> $DarkRed
];

ErrBox[boxes_] := TagBox[FrameBox[boxes, $redFrameOpts], ErrBox];
_ErrBox        := StyleBox["BadErrBox", $Red, FontSize -> 10];

(* ensure errors propogate *)
BlockUnprotect[TagBox,
  ebox:TagBox[_, ErrBox][___] := ebox;
];

(**************************************************************************************************)

DeclaredHere[SetBoxFn, SetCurryBoxFn, SetCurry1BoxFn, SetCurry2BoxFn, SetCurry23BoxFn];

setCurryBoxFnUnary[sym_Sym] := Then[
  SetD[e:sym[BlankSeq2], BoxFnErrBox[sym, e]],
  SetD[sym[],            BoxFnErrBox[sym, sym[]]]
];

setCurryBoxFnBinary[sym_Sym] := Then[
  SetD[e:sym[BlankSeq3], BoxFnErrBox[sym, e]],
  SetD[sym[],            BoxFnErrBox[sym, sym[]]]
];

DeclarationDefs[
  SetBoxFn[sym_Sym]        := SetD[e_sym,           BoxFnErrBox[sym, e]],
  SetCurryBoxFn[sym_Sym]   := SetD[e:sym[___][___], BoxFnErrBox[sym, e]],
  SetCurry1BoxFn[sym_Sym]  := Then[SetCurryBoxFn @ SetCurry1[sym],  setCurryBoxFnUnary[sym]],
  SetCurry2BoxFn[sym_Sym]  := Then[SetCurryBoxFn @ SetCurry2[sym],  setCurryBoxFnUnary[sym]],
  SetCurry23BoxFn[sym_Sym] := Then[SetCurryBoxFn @ SetCurry23[sym], setCurryBoxFnBinary[sym]]
]

(**************************************************************************************************)

ClearFormRegistry[] := Then[
  $AtomFormStore = KeyStoreNew[{Image, AnimatedImage, Graph, Graphics, SrcLoc, Spacer}],
  $OperatorFormStore = KeyStoreNew[{}];
  $CompoundFormStore = StoreNew[LoadSystemData["SystemForms.mx"]];
];

If[!HasIValueQ[$AtomFormStore], ClearFormRegistry[]];

(**************************************************************************************************)

SetHoldC @ SetPred1[FormHeadQ, AtomFormHeadQ, CompoundFormHeadQ, OperatorFormHeadQ]

FormHeadQ[s_Sym]         := AtomFormHeadQ[s] || CompoundFormHeadQ[s] || OperatorFormHeadQ[s];
AtomFormHeadQ[s_Sym]     := StoreKeyQ[$AtomFormStore,     NoEval @ s];
CompoundFormHeadQ[s_Sym] := StoreKeyQ[$CompoundFormStore, NoEval @ s];
OperatorFormHeadQ[s_Sym] := StoreKeyQ[$OperatorFormStore, NoEval @ s];
CompoundFormArity[s_Sym] := StoreGet[$CompoundFormStore,  NoEval @ s];

(**************************************************************************************************)

DeclaredHere[SetForm0, SetForm1, SetForm2, SetForm3, SetFormA];
DeclaredHere[SetFormR, SetFormC, SetFormG, SetFormO];

DeclarationDefs[
  SetForm0[sym_Sym] := KeyStoreAdd[$AtomFormStore, NoEval @ sym],
  SetFormO[sym_Sym] := KeyStoreAdd[$OperatorFormStore, NoEval @ sym],
  SetForm1[sym_Sym] := StoreSet[$CompoundFormStore, NoEval @ sym, 1],
  SetForm2[sym_Sym] := StoreSet[$CompoundFormStore, NoEval @ sym, 2],
  SetForm3[sym_Sym] := StoreSet[$CompoundFormStore, NoEval @ sym, 3],
  SetFormA[sym_Sym] := StoreSet[$CompoundFormStore, NoEval @ sym, All],
  SetFormR[sym_Sym] := StoreSet[$CompoundFormStore, NoEval @ sym, Row],
  SetFormC[sym_Sym] := StoreSet[$CompoundFormStore, NoEval @ sym, Column],
  SetFormG[sym_Sym] := StoreSet[$CompoundFormStore, NoEval @ sym, Grid]
];

