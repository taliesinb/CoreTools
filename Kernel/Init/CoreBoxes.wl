SystemExports[
  "FormHead",          FormBlock, Themed, Unformatted, Uninteractive, ElidedForm, HoldElidedForm,
  "BoxSymbol",         Ellipsis
];

PackageExports[
  "IOFunction",        ThemeValue, MakeCoreBox, CoreBox, SystemBox, MapMakeBox, MapSeqMakeBox,
  "SpecialVariable",   $ThemeStack, $CoreFormatting, $CoreInteractive,
  "ScopingFunction",   BlockFormatting, BlockInteractive,
  "FormHead",          DotsForm, CDotsForm, MsgForm, StrForm,
  "MetaFunction",      DeclareCoreBox, DeclareCoreSubBoxes, MakeBoxDefinitions,
                       SetForm0, SetForm1,
                       SetBoxFn, SetCurryBoxFn, SetCurry1BoxFn, SetCurry2BoxFn, SetCurry23BoxFn,
  "Function",          CompoundFormArity,
  "BoxFunction",       DotsBox,  CDotsBox, HoldLenBox, LenBox, RedMsgFormBox, RedErrorBox, MsgFormBox, BoxFnErrorBox, StrFormBox,
  "BoxFunction",       UnformattedBoxes, ElidedBox, HoldElidedBox,
  "Predicate",         HasCoreBoxQ, HasCoreSubBoxQ, CoreBoxSubHeadQ, CoreBoxHeadQ, AtomFormHeadQ, CompoundFormHeadQ
];

PrivateExports[
  "CacheVariable",     $CoreBoxHeadDict, $CoreBoxStore, $CoreBoxSubStore, $AtomFormStore, $CompoundFormStore
];

(**************************************************************************************************)

SystemBox /: SetDelayed[SystemBox[lhs_], rhs_] := (
  MakeBoxes[lhs, StandardForm | TraditionalForm] := rhs;
);

Protect[SystemBox];

(**************************************************************************************************)

DeclaredHere @ MsgForm;
SetHoldA @ MsgFormBox;

SystemBox[MsgForm[args___]] := MsgFormBox[args];

MsgFormBox = CaseOf[
  msg_Str            := MakeBoxes @ msg;
  $[msg_Str, args__] := StrFormBox[msg, HoldMap[MsgArgForm, args]];
  $[args___]         := StrFormBox[HoldMap[MsgArgForm, args]];
];

(**************************************************************************************************)

DeclaredHere @ StrForm;

SystemBox[StrForm[args___]] := StrFormBox[args];

StrFormBox = CaseOf[
  msg_Str            := MakeBoxes @ msg;
  $[msg_Str, args__] := First @ ToBoxes @ StringForm[msg, args];
  $[args___]         := ToBoxes @ RawRow[{LGAngle, args, RGAngle}, ";"]
];

(**************************************************************************************************)

Initially[
  $CoreBoxHeadDict   = UDict[];
  $CoreBoxStore      = KeyStoreNew[{}];
  $CoreBoxSubStore   = KeyStoreNew[{}];
  $AtomFormStore     = KeyStoreNew[{Image, Graph, SourceLocation}];
  $CompoundFormStore = StoreNew[LoadSystemData["SystemForms.mx"]];
  $ThemeStack        = List[];
  $CoreFormatting    = True;
  $CoreInteractive   = True;
];

(**************************************************************************************************)

(* approach:
have an option called Theme, which is our version of PlotTheme.
RegisterTheme[head, name, opts] will set $ThemeData[head, name] = UDict[opts].
look through theme stack:
  if we see a single string, look up $ThemeData[head, name], and recurse through IT as a stack if present.
  if we see a rule head -> spec, look through it.
  if we see a rule prop -> value, our answer is value.
  if we see Theme -> 'name', look through $ThemeDa
any recursion can return DefaultValue, which will cause us to keep looking.

*)

(* ThemeLookup[head_Symbol, prop_Symbol] :=  *)

(**************************************************************************************************)

SetHoldA[MapMakeBox, MapSeqMakeBox]

MapMakeBox = CaseOf[
  items_Dict := KeyMapValueMap[MakeBoxes, MakeBoxes, items]; (* KMVM preserves holdness *)
  items_List := HoldMap[MakeBoxes, items];
];

MapSeqMakeBox[items___] := Seq @@ HoldMap[MakeBoxes, {items}]

(**************************************************************************************************)

SetPred1 @ SetHoldC[HasCoreBoxQ, HasCoreSubBoxQ]

(* TODO: switch to using $CoreBoxStore *)
HasCoreBoxQ[s_Sym]    := Lookup[$CoreBoxHeadDict, Hold @ s, False];
HasCoreSubBoxQ[s_Sym] := Lookup[$CoreBoxHeadDict, Hold @ s, False];

(**************************************************************************************************)

SetHoldC[MakeCoreBox];

MakeCoreBox[_] := $Failed;

(**************************************************************************************************)

MakeBoxes[  Unformatted[lhs_], form_] /; $CoreFormatting :=  BlockFormatting @ MakeBoxes[lhs, form];
MakeBoxes[Uninteractive[lhs_], form_] /; $CoreFormatting := BlockInteractive @ MakeBoxes[lhs, form];

SetHoldC[BlockFormatting, BlockInteractive];

 BlockFormatting[body_] := Block[{ $CoreFormatting = False}, body];
BlockInteractive[body_] := Block[{$CoreInteractive = False}, body];

(**************************************************************************************************)

SetHoldA @ DeclareCoreBox;
DeclareDeclare @ DeclareCoreBox;

DeclareCoreBox[sym_Symbol] := With[{name = SymName[sym]},

  $CoreBoxHeadDict[Hold[sym]] = True;
  KeyStoreAdd[$CoreBoxStore, NoEval @ sym];

  MakeBoxes[$LHS:sym | _sym, _] /; $CoreFormatting :=
    With[{res = MakeCoreBox @ $LHS}, res /; res =!= $Failed];
];

(**************************************************************************************************)

SetHoldA @ DeclareCoreSubBoxes;
DeclareDeclare @ DeclareCoreSubBoxes;

DeclareCoreSubBoxes[sym_Symbol] := With[{name = SymbolName[sym]},

  $CoreBoxHeadDict[Hold[sym]] = True;

  KeyStoreAdd[$CoreBoxStore,    NoEval @ sym];
  KeyStoreAdd[$CoreBoxSubStore, NoEval @ sym];

  MakeBoxes[$LHS:(_sym[___]), _] /; $CoreFormatting :=
    With[{res = MakeCoreBox @ $LHS}, res /; res =!= $Failed];
];

(**************************************************************************************************)

CoreBox::unknownHead = "Don't know which symbol CoreBox rules are associated with."

setupCoreBox[_]             := Message[CoreBox::unknownHead];
setupCoreBox[Hold[sym_Sym]] := (DeclareCoreBox[sym]; setupCoreBox[sym] := Null);

CoreBox /: SetDelayed[CoreBox[lhs_], rhs_] := (
  setupCoreBox @ PatHead @ lhs;
  MakeCoreBox[lhs] := rhs;
);

Protect[CoreBox];

(**************************************************************************************************)

SetHoldF @ FormBlock;

FormBlock::usage =
"FormBlock[var$ = val$, expr$] typesets expr$ with the given override, applied at boxification.
FormBlock[{var$1 = val$1, $$}, expr$] applies multiple settings."

CoreBox[FormBlock[settings_, body_]] := formBlockBoxes[settings, body];

SetHoldA @ formBlockBoxes;

formBlockBoxes = CaseOf[
  $[set_Set,    expr_] := formBlockBoxes[{set}, expr];
  $[{set__Set}, expr_] := Block[{sym}, MakeBoxes @ expr];
  $[bad_,       expr_] := ErrorTooltipBox[MakeBoxes @ expr, FormBlock::badSettingArg, HoldForm @ bad];
  _                    := $Failed
];

FormBlock::badSettingArg = "Not a Set or list of Sets: ``"

(**************************************************************************************************)

CoreBox[themed_Themed] := themedBoxes[themed];

SetHoldC @ themedBoxes;

themedBoxes = CaseOf[
  Themed[expr_, theme_Str] := MakeBoxes @ expr;
  Themed[expr_]            := MakeBoxes @ expr;
  _                        := $Failed;
];

(*************************************************************************************************)

RedMsgFormBox = CaseOf[
  $[e_Str, args__]          := RedMsgFormBox[StringForm[e, args]];
  $[e:(_Str | _StringForm)] := RedErrorBox[MakeBox @ e];
  $[___]                    := RedErrorBox["InvalidRedMsgFormBox"];
];

(*************************************************************************************************)

RedErrorBox[boxes_, tooltip_] :=
  NiceTooltipBox[RedErrorBox @ boxes, tooltip];

RedErrorBox[None] := Make[GraphicsBox, {},
  Background -> $LightRed,
  ImageSize -> {10,10}, BaselinePosition -> Scaled[0.1]
];

RedErrorBox[boxes_] := TagBox[FrameBox[boxes,
  FrameMargins -> {{3, 3}, {-1, -1}}, BaselinePosition -> Baseline,
  ContentPadding -> True,
  Alignment -> Center,
  Background -> Lighter[$LightRed],
  FrameStyle -> $DarkRed
], RedErrorBox];

_RedErrorBox := StyleBox["InvalidRedErrorBox", $Red];

(* ensure errors propogate *)
BlockUnprotect[TagBox,
  ebox:TagBox[_, RedErrorBox][___] := ebox;
];

(*************************************************************************************************)

SetHoldC[BoxFnErrorBox, dynCodeBox];

BoxFnErrorBox[h_Sym, e_] := RedErrorBox[StyleBox[AliasSymName @ h, FontSize -> 10], dynCodeBox @ e];
BoxFnErrorBox[_, e_]     := RedErrorBox[None, dynCodeBox @ e];
BoxFnErrorBox[e_]        := RedErrorBox[None, dynCodeBox @ e];
BoxFnErrorBox[]          := RedErrorBox[None];

dynCodeBox[expr_] := CreateCachedBox[CodeStyleBox @ MakeCodeBoxes[expr, 2, 4]]

(**************************************************************************************************)

DeclaredHere[SetBoxFn, SetCurryBoxFn, SetCurry1BoxFn, SetCurry2BoxFn, SetCurry23BoxFn]

setCurryBoxFnUnary[sym_Sym] := Then[
  SetD[e:sym[BlankSeq2], BoxFnErrorBox[sym, e]],
  SetD[sym[],            BoxFnErrorBox[sym, sym[]]]
];

setCurryBoxFnBinary[sym_Sym] := Then[
  SetD[e:sym[BlankSeq3], BoxFnErrorBox[sym, e]],
  SetD[sym[],          BoxFnErrorBox[sym, sym[]]]
];

DeclarationDefs[
  SetBoxFn[sym_Sym]        := SetD[e_sym,           BoxFnErrorBox[sym, e]],
  SetCurryBoxFn[sym_Sym]   := SetD[e:sym[___][___], BoxFnErrorBox[sym, e]],
  SetCurry1BoxFn[sym_Sym]  := Then[SetCurryBoxFn @ SetCurry1[sym],  setCurryBoxFnUnary[sym]],
  SetCurry2BoxFn[sym_Sym]  := Then[SetCurryBoxFn @ SetCurry2[sym],  setCurryBoxFnUnary[sym]],
  SetCurry23BoxFn[sym_Sym] := Then[SetCurryBoxFn @ SetCurry23[sym], setCurryBoxFnBinary[sym]]
]

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
  e:(s_Sym)[___]   := RBox[AliasSymName @ s, "[", HoldLenBox @ e, "]"];
  e_               := HoldLenBox @ e;
];

SetHoldC[elidedSetBox, elidedListBox, elidedStrBox, elidedDictBox];

elidedSetBox[s_[a_]]       := RBox[AliasSymName @ s, "[", If[HEmptyQ @ a, Nothing, HoldLenBox @ a], "]"];

elidedListBox[{}]          := RBox[LBrace, RBrace];
elidedListBox[l_List]      := RBox[LBrace, HoldLenBox @ l, RBrace];

elidedStrBox[""]           := RBox[DQuote, DQuote];
elidedStrBox[s_Str]        := RBox[DQuote, DotsBox @ StrLen @ s, DQuote];

elidedDictBox[EmptyDict]   := RBox[LAssoc, RAssoc];
elidedDictBox[EmptyUDict]  := RBox["UDict", "[", "]"];
elidedDictBox[d_ ? UDictQ] := RBox["UDict", "[", HoldLenBox @ d, "]"];
elidedDictBox[d_]          := RBox[LAssoc, HoldLenBox @ d, RAssoc];

(**************************************************************************************************)

SetHoldC[HoldLenBox];

LenBox[e_] := HoldLenBox @ e;

HoldLenBox[AtomP] := CDots;
HoldLenBox[e_] := DotsBox @ HoldLen @ e;

(**************************************************************************************************)

CoreBox[Ellipsis]                     := Dots;
CoreBox[DotsForm[expr_, pos_:Below]]  := DotsBox[MakeBoxes @ expr, pos];
CoreBox[CDotsForm[expr_, pos_:Below]] := CDotsBox[MakeBoxes @ expr, pos];

DotsBox[]               := Dots;
DotsBox[None]           := Dots;
DotsBox[num_Int]        := DotsBox[num, True];
DotsBox[num_Int, False] := SubscriptBox[VDots, IntStr @ num];
DotsBox[num_Int, True]  := SubscriptBox[Dots, RaiseBox[IntStr @ num, 1]];

CDotsBox[]               := CDots;
CDotsBox[None]           := CDots;
CDotsBox[num_Int]        := CDotsBox[num, True];
CDotsBox[num_Int, False] := SubscriptBox[VDots, IntStr @ num];
CDotsBox[num_Int, True]  := SubscriptBox[CDots, RaiseBox[IntStr @ num, 1]];

(**************************************************************************************************)

DeclareThenScan[MakeBoxDefinitions]

(* TODO: why not use CoreBox for this? *)
MakeBoxDefinitions[SetDelayed[lhs_, rhs_]] := (
  MakeBoxes[lhs, StandardForm | TraditionalForm] := rhs;
);

(**************************************************************************************************)

DeclaredHere[SetForm0, SetForm1];

DeclarationDefs[
  SetForm0[sym_Sym] := KeyStoreAdd[$AtomFormStore, NoEval @ sym],
  SetForm1[sym_Sym] := StoreSet[$CompoundFormStore, NoEval @ sym, 1]
];

(**************************************************************************************************)

SetHoldC @ SetPred1[CoreBoxHeadQ, CoreBoxSubHeadQ, AtomFormHeadQ, CompoundFormHeadQ]

CoreBoxHeadQ[s_Sym]      := StoreKeyQ[$CoreBoxStore,      NoEval @ s];
CoreBoxSubHeadQ[s_Sym]   := StoreKeyQ[$CoreBoxSubStore,   NoEval @ s];
AtomFormHeadQ[s_Sym]     := StoreKeyQ[$AtomFormStore,     NoEval @ s];
CompoundFormHeadQ[s_Sym] := StoreKeyQ[$CompoundFormStore, NoEval @ s];
CompoundFormArity[s_Sym] := StoreGet[$CompoundFormStore,  NoEval @ s];

