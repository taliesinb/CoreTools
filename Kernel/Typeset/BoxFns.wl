PackageExports[
  "BoxFunction",   DepthTruncateBoxes,
  "FormHead",      DepthTruncateForm,
  "Predicate",     MaybeBoxesQ, ValidBoxesQ, BoxSymQ,
  "PatternSymbol", MaybeBoxesP, BoxSymP
];

PrivateExports[
  "PatternSymbol",
    Box0SymP, Box1SymP, Box1VecSymP, Box1MatSymP, Box1RVecSymP, Box2SymP, Box2RVecSymP, BoxDValSymP, BoxDSymP,
    BoxTruncSymP, NestRowBoxListP
];

(*************************************************************************************************)

DefinePatternRules[
  Box0SymP      -> Alt[GraphicsBox, Graphics3DBox, TabViewBox, TemplateBox],
  Box1SymP      -> Alt[StyleBox, TagBox, TooltipBox, AdjBox, FrameBox, ButtonBox, FormBox, ItemBox, PaneBox, PanelBox, SqrtBox],
  Box1VecSymP   -> Alt[RowBox, OverlayBox],
  Box1MatSymP   -> Alt[GridBox, TableViewBox],
  Box1RVecSymP  -> Alt[PaneSelectorBox],
  Box2SymP      -> Alt[OverBox, UnderBox, SuperBox, SubBox, SubsuperBox, FractionBox, RadicalBox],
  Box2RVecSymP  -> Alt[ActionMenuBox, PopupMenuBox],
  BoxDValSymP   -> Alt[ValueBox, OptionValueBox, InputFieldBox, OpenerBox, TogglerBox, RadioButtonBox, SetterBox, ColorSetterBox, CounterBox, ProgressIndicatorBox, CheckboxBox],
  BoxDSymP      -> Alt[DynamicBox, DynamicModuleBox, DynamicWrapperBox]
];

DefineLiteralRules[
  BoxTruncSymP  -> Evaluate @ Join[Alt[InterpretationBox, TemplateBox, RowBox], Box1MatSymP, Box1VecSymP, Box2SymP],
  BoxSymP       -> Evaluate @ Join[Box1MatSymP, Box1VecSymP, Box2SymP, Box0SymP, Box1RVecSymP, Box2RVecSymP, BoxDValSymP, BoxDSymP]
];

DefinePatternRules[
  MaybeBoxesP      -> Alt[_BoxSymP[___], _Str],
  NestRowBoxListP  -> Alt[{_, "[", ___, "]"}, {"{", ___, "}"}, {LAssoc, ___, RAssoc}]
];

(*************************************************************************************************)

SystemBox[DepthTruncateForm[expr_, n_Int]] := At[DepthTruncateBoxes, MakeBoxes @ expr, n];

(*************************************************************************************************)

SetHoldC @ DepthTruncateBoxes;

DepthTruncateBoxes[boxes_, n_Int] := If[n <= 0, CDots, Block[{$d = n}, truncBox @ boxes]];

SetHoldC[truncBox, incDepth1, incDepth2];

truncBox = CaseOf[
  s_Str                             := s;
  InterpBox[b_, ___]                := $ @ b;
  TBox[args_List, name_]            := TBox[truncTBox[name, args], name];
  RowBox[b:NestRowBoxListP]            := RowBox @ HoldMap[$, b] // incDepth1;
  RowBox[b_List]                    := RowBox @ HoldMap[$, b];
  (h:Box1SymP)[a_, r___]            := With[{a1 = $ @ a},             h[a1, r]];
  (h:Box1VecSymP)[a_List, r___]     := With[{a1 = HoldMap[$, a]},    h[a1, r]];
  (h:Box1MatSymP)[a:{__List}, r___] := With[{a1 = HoldMap2[$, a]},     h[a1, r]] // incDepth2;
  (h:Box2SymP)[a_, b_, r___]        := With[{a1 = $ @ a, b1 = $ @ b}, h[a1, b1, r]];
  e:otherSymP[___]                  := e;
  e:(h:SymP)[___]                   := BoxFnErrorBox[h, e];
  e_                                := BoxFnErrorBox[e];
,
  {otherSymP -> Join[Box1RVecSymP, Box2RVecSymP, BoxDValSymP, BoxDSymP]}
];

incDepth1[body_] := If[$d <= 1, CDots, Block[{$d = $d - 1}, body]];
incDepth2[body_] := If[$d <= 2, CDots, Block[{$d = $d - 2}, body]];

SetHoldC[truncTBox, maybeTrunc];

truncTBox = CaseOf[
  $[_, {}]                           := {};
  $["RowDefault", args_]             := HoldMap[truncBox, args];
  $["RowWithSeparators", {a_, r___}] := Prepend[a] @ HoldMap[truncBox, {r}];
  $[_, args_]                        := HoldMap[maybeTrunc, args];
];

maybeTrunc = CaseOf[
  b:BoxTruncSymP[___] := incDepth1 @ truncBox @ b;
  b_                  := b;
];