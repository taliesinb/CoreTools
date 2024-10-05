PrivateExports[
  "PatternSymbol",
    MaybeBoxesP, BoxSymP,
    Box0SymP, Box1SymP, Box1VecSymP, Box1MatSymP, Box1RVecSymP, Box2SymP, Box2RVecSymP, BoxDValSymP, BoxDSymP,
    BoxTruncSymP, NestRowBoxListP,
  "Predicate",
    MaybeBoxesQ
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

SetPred1 @ SetHoldC[BoxSymQ, MaybeBoxesQ];

BoxSymQ[BoxSymP]         := True;
MaybeBoxesQ[MaybeBoxesP] := True;