SystemExports[
  "FormHead",          FormBlock, Themed, Unformatted, Uninteractive, ExpanderForm, OutlineForm, ElidedForm, HoldElidedForm,
  "BoxSymbol",         Ellipsis
];

PackageExports[
  "IOFunction",        ThemeValue, MakeCoreBoxes, CoreBoxes, SystemBoxes, MapMakeBoxes, MapMakeBoxesSeq,
  "DebuggingFunction", MakeCoreBoxesTraditional, MakeCoreBoxesModified,
  "SpecialVariable",   $ThemeStack, $UseCoreBoxFormatting, $UseCoreBoxInteractivity, $CurrentCoreBoxModifiers, $UseTraditionalForm,
  "ScopingFunction",   DisableCoreBoxFormatting, DisableCoreBoxInteractivity,
  "FormHead",          DotsForm, CDotsForm, MsgForm, StrForm,
  "MetaFunction",      DeclareCoreBoxes, DeclareCoreSubBoxes, MakeBoxDefinitions, DeclareOutlineBoxes, DeclareExpanderBoxes, SetAtomFormHead, SetCompoundFormHead,
  "Function",          CompoundFormArity,
  "BoxFunction",       DotsBox,  CDotsBox, HoldLenBox, LenBox, NiceErrorBox, MsgFormBoxes, StrFormBoxes,
  "BoxFunction",       UnformattedBoxes, OutlineBoxes, ExpanderBoxes, HoldExpanderBoxes, ElidedBox, HoldElidedBox,
  "Predicate",         HasCoreBoxesQ, CoreBoxHeadQ, AtomFormHeadQ, CompoundFormHeadQ
];

PrivateExports[
  "CacheVariable",     $CoreBoxHeadDict, $OutlineBoxHeadDict,
  "CacheVariable",     $CoreBoxStore, $OutlineBoxStore, $AtomFormStore, $CompoundFormStore
];

(**************************************************************************************************)

DeclaredHere[MsgForm]

SetHoldF[MsgForm];

MakeBoxes[MsgForm[args___], StandardForm | TraditionalForm] := MsgFormBoxes[args];

MsgFormBoxes = CaseOf[
  msg_Str            := MakeBoxes @ msg;
  $[msg_Str, args__] := StrFormBoxes[msg, MapSeq[MsgPrePrint, args]];
  $[args___]         := StrFormBoxes[MapSeq[MsgPrePrint, args]];
];

(**************************************************************************************************)

DeclaredHere[StrForm]

MakeBoxes[StrForm[args___], StandardForm | TraditionalForm] := StrFormBoxes[args];

StrFormBoxes = CaseOf[
  msg_Str            := MakeBoxes @ msg;
  $[msg_Str, args__] := First @ ToBoxes @ StringForm[msg, args];
  $[args___]         := ToBoxes @ RawRow[{LGAngle, args, RGAngle}, ";"]
];

(**************************************************************************************************)

Initially[
  $CoreBoxHeadDict = UDict[];
  $OutlineBoxHeadDict = UDict[];
  $CoreBoxStore      = KeyStoreNew[{}];
  $OutlineBoxStore   = KeyStoreNew[{}];
  $AtomFormStore     = KeyStoreNew[{Image, Graph}];
  $CompoundFormStore = StoreNew[{
    Row -> 1, Column -> 1, Grid -> 1, Pane -> 1, Framed -> 1, Labeled -> 2, EventHandler -> 1,
    TableForm -> 1, MatrixForm -> 1,
    Annotation -> 1, Tooltip -> 2, Style -> 1, DirectedEdge -> All, UndirectedEdge -> All
  }];
  $ThemeStack = List[];
  $UseTraditionalForm = False;
  $CurrentCoreBoxModifiers = UDict[TraditionalForm :> $UseTraditionalForm];
  $UseCoreBoxFormatting = True;
  $UseCoreBoxInteractivity = True;
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

SetHoldA[MapMakeBoxes, MapMakeBoxesSeq]

MapMakeBoxes = CaseOf[
  items_Dict := KeyMapValueMap[MakeBoxes, MakeBoxes, items]; (* KMVM preserves holdness *)
  items_List := HoldMap[MakeBoxes, items];
];

MapMakeBoxesSeq[items___] := Seq @@ HoldMap[MakeBoxes, {items}]

(**************************************************************************************************)

SetHoldC[HasCoreBoxesQ]
SetPred1[HasCoreBoxesQ]

HasCoreBoxesQ[s_] = Lookup[$CoreBoxHeadDict, Hold @ s, False];

(**************************************************************************************************)

SetHoldC[MakeCoreBoxes, DisableCoreBoxFormatting, DisableCoreBoxInteractivity]

(* we'll add to these definitions *)
MakeCoreBoxes[_] := FailEval;

DisableCoreBoxFormatting[body_]    := Block[{$UseCoreBoxFormatting = False}, body];
DisableCoreBoxInteractivity[body_] := Block[{$UseCoreBoxInteractivity = False}, body];

(* TODO: make interactivity just a modifier *)

(**************************************************************************************************)

SetHoldC[MakeCoreBoxesModified, MakeCoreBoxesTraditional]

MakeCoreBoxesModified[rules_, expr_] :=
  BlockAssociate[$CurrentCoreBoxModifiers, rules, MakeCoreBoxes @ expr];

MakeCoreBoxesTraditional[expr_] :=
  BlockTrue[$UseTraditionalForm, MakeCoreBoxes @ expr];

(**************************************************************************************************)

SetHoldA[DeclareCoreBoxes, DeclareCoreSubBoxes]
DeclareDeclare[DeclareCoreBoxes, DeclareCoreSubBoxes]


DeclareCoreBoxes[sym_Symbol] := With[
  {name = SymbolName[sym]},

  $CoreBoxHeadDict[Hold[sym]] = True;
  KeyStoreAdd[$CoreBoxStore, NoEval @ sym];

  MakeBoxes[$LHS:sym | _sym, StandardForm] /; $UseCoreBoxFormatting :=
    MaybeEval @ MakeCoreBoxes @ $LHS;

(*   MakeBoxes[$LHS:sym | _sym, StandardForm] /; $UseCoreBoxFormatting :=
    MaybeEval @ StatusAreaBox[MakeCoreBoxes @ $LHS, name];
 *)
  MakeBoxes[$LHS:sym | _sym, TraditionalForm] /; $UseCoreBoxFormatting :=
    MaybeEval @ MakeCoreBoxesTraditional @ $LHS;
];

DeclareCoreSubBoxes[sym_Symbol] := With[
  {name = SymbolName[sym]},

  $CoreBoxHeadDict[Hold[sym]] = True;
  KeyStoreAdd[$CoreBoxStore, NoEval @ sym];

  MakeBoxes[$LHS:(_sym[___]), StandardForm] /; $UseCoreBoxFormatting :=
    MaybeEval @ StatusAreaBox[MakeCoreBoxes @ $LHS, name];

  MakeBoxes[$LHS:(_sym[___]), TraditionalForm] /; $UseCoreBoxFormatting :=
    MaybeEval @ MakeCoreBoxesTraditional @ $LHS;
];

(**************************************************************************************************)

CoreBoxes::unknownHead = "Don't know which symbol CoreBox rules are associated with."
setupCoreBoxes[_] := Message[CoreBoxes::unknownHead];
setupCoreBoxes[Hold[sym_Symbol]] := (DeclareCoreBoxes[sym]; setupCoreBoxes[sym] := Null);

CoreBoxes /: SetDelayed[CoreBoxes[lhs_], rhs_] := (
  setupCoreBoxes @ PatHead @ lhs;
  MakeCoreBoxes[lhs] := rhs;
);

Protect[CoreBoxes];

(**************************************************************************************************)

SystemBoxes /: SetDelayed[SystemBoxes[lhs_], rhs_] := (
  MakeBoxes[lhs, StandardForm | TraditionalForm] := rhs;
);

Protect[SystemBoxes];

(**************************************************************************************************)

SetHoldF[FormBlock]

FormBlock::usage =
"FormBlock[var$ = val$, expr$] typesets expr$ with the given override, applied at boxification.
FormBlock[{var$1 = val$1, $$}, expr$] applies multiple settings."

CoreBoxes[FormBlock[settings_, body_]] := formBlockBoxes[settings, body];

SetHoldA[formBlockBoxes];

formBlockBoxes[set_Set, expr_]    := formBlockBoxes[{set}, expr];
formBlockBoxes[{set__Set}, expr_] := Block[{sym}, MakeBoxes @ expr];
formBlockBoxes[bad_, expr_] := ErrorTooltipBox[MakeBoxes @ expr, FormBlock::badSettingArg, HoldForm @ bad];

FormBlock::badSettingArg = "Not a Set or list of Sets: ``"

(**************************************************************************************************)

MakeBoxes[Unformatted[lhs_], form:StandardForm | TraditionalForm] /; $UseCoreBoxFormatting :=
  UnformattedBoxes[lhs, form];

SetHoldC[UnformattedBoxes];

UnformattedBoxes[lhs_, form_:StandardForm] :=
  DisableCoreBoxFormatting @ MakeBoxes[lhs, form];

(**************************************************************************************************)

MakeBoxes[Uninteractive[lhs_], form:StandardForm | TraditionalForm] /; $UseCoreBoxFormatting := Block[
  {$UseTraditionalForm = form === TraditionalForm, $UseCoreBoxInteractivity = False},
  MakeBoxes[lhs, form]
];

(**************************************************************************************************)

MakeBoxes[themed_Themed, form:StandardForm | TraditionalForm] /; $UseCoreBoxFormatting :=
  themedBoxes[themed];

SetHoldC[themedBoxes];

themedBoxes = CaseOf[
  Themed[expr_, theme_Str] := MakeBoxes @ expr;
  Themed[expr_]            := MakeBoxes @ expr;
  Themed[]                 := RBox["Themed", "[", "]"];
];

(*************************************************************************************************)

NiceErrorBox[e_String, args__] := NiceErrorBox[StringForm[e, args]];

NiceErrorBox[e:(_String | _StringForm)] := FrameBox[
  MakeBoxes @ e,
  Alignment -> Center,
  Background -> $LightRed,
  FrameStyle -> $DarkRed
];

(**************************************************************************************************)

CoreBoxes[OutlineForm[expr_]]           := OutlineBoxes[expr];

(**************************************************************************************************)

SetHoldC[OutlineBoxes, outlineBoxesSmart, outlineBoxesFull, outlineBoxesArgs, outlineBoxesLeaf, outlineQ];

OutlineBoxes[HoldForm[expr_]] := OutlineBoxes[expr];
OutlineBoxes[expr_] := If[HoldLen[expr] <= 4, outlineBoxesFull @ expr, outlineBoxesSmart @ expr];

outlineQ[head_] := TrueQ[$OutlineBoxHeadDict @ Hold @ head];

outlineBoxesSmart = CaseOf[
  expr:((head_Sym ? outlineQ)[___]) := outlineBoxesFull[expr];
  expr:(head_Sym[___])              := HoldElidedBox @ expr;
  expr_                             := outlineBoxesLeaf @ expr;
];

outlineBoxesFull = CaseOf[
  expr:(head_Sym[args___])          := outlineHeadArgs[head, outlineBoxesArgs @ List[args]];
  expr_                             := outlineBoxesLeaf @ expr;
];

outlineBoxesArgs = CaseOf[
  {some:Repeated[_, {6}], rest__}   := CommaRowBox @ Append[DotsBox @ HoldSeqLen @ rest] @ HoldMap[outlineBoxesSmart, {some}];
  args_List                         := CommaRowBox @ HoldMap[outlineBoxesSmart, args];
];

(* SetHoldF @ outlineHeadArgs;
outlineHeadArgs = CaseOf[
  $[_, argBoxes_]        := RBox[Dots, "[", GrayBox @ argBoxes, "]"];
  $[sym_Sym, argBoxes_]  := RBox[AliasSymName @ sym, "[", GrayBox @ argBoxes, "]"];
  $[List, argBoxes_]     := RBox[LBrace,    GrayBox @ argBoxes, RBrace];
  $[Dict, argBoxes_]     := RBox[LAssoc, GrayBox @ argBoxes, RAssoc];
];
 *)

(* outlineBoxesLeaf = CaseOf[
  i_Int                           := IntStr @ i;
  s_Sym                           := AliasSymName @ s;
  s_Str ? HAtomQ /; StrLen[s] < 4 := MakeBoxes @ s;
  e_                              := HoldElidedBox @ e;
];
 *)
(**************************************************************************************************)

SetHoldC[HoldElidedForm, HoldElidedBox];

CoreBoxes[ElidedForm[expr_]]     := HoldElidedBox @ expr;
CoreBoxes[HoldElidedForm[expr_]] := HoldElidedBox @ expr;

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

LenBox[e_]     := HoldLenBox @ e;

HoldLenBox[AtomP] := Dots;
HoldLenBox[e_] := DotsBox @ HoldLen @ e;

(**************************************************************************************************)

CoreBoxes[Ellipsis]                     := Dots;
CoreBoxes[DotsForm[expr_, pos_:Below]]  := DotsBox[MakeBoxes @ expr, pos];
CoreBoxes[CDotsForm[expr_, pos_:Below]] := CDotsBox[MakeBoxes @ expr, pos];

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

DeclareSeqScan[DeclareOutlineBoxes]

DeclareOutlineBoxes[sym_Symbol] := Then[
  $OutlineBoxHeadDict[Hold[sym]] = True,
  zStoreAdd[$OutlineBoxStore, NoEval @ sym];
  CoreBoxes[s_sym] := outlineBoxesFull[s]
];

DeclareOutlineBoxes[InternalData];

(**************************************************************************************************)

DeclareSeqScan[DeclareExpanderBoxes]

DeclareExpanderBoxes[sym_Sym] := CoreBoxes[sym[args___]] := HoldExpanderBoxes[sym, args];

(**************************************************************************************************)

$remExpansions = Inf;

CoreBoxes[ExpanderForm[head_Sym[args___]]] :=
  StyleBox[HoldExpanderBoxes[head, args], ShowStringCharacters -> True];

CoreBoxes[ExpanderForm[head_Sym[args___], level_]] := StyleBox[
  BlockSet[$remExpansions, level, HoldExpanderBoxes[head, args]],
  StyleBox[HoldExpanderBoxes[head, args], ShowStringCharacters -> True]
];

SetHoldC[HoldExpanderBoxes, makeExpanderBoxes1, makeExpanderBoxes2, openHead, closeHead];

ExpanderBoxes[args___] := HoldExpanderBoxes[args];
HoldExpanderBoxes[head_Sym, args___] := makeExpanderBoxes2[head, {args}];

makeExpanderBoxes1[expr_] := MakeBoxes[expr];
makeExpanderBoxes1[head_Sym[args___]] := makeExpanderBoxes2[head, {args}];

makeExpanderBoxes2[Rule, {d:DatumP, rhs_}] :=
  joinFirstRow[RowBox[{MakeBoxes @ d, "\[Rule]", makeExpanderBoxes1 @ rhs}]];

joinFirstRow[boxes_] := boxes;
joinFirstRow[RowBox[{a_, b_, GridBox[{{f1_, fr___}, rest___}, opts___]}]] :=
  GridBox[{{RowBox[{a, b, f1}], fr}, rest}, opts];


makeExpanderBoxes2[head_Sym, {}] := RBox[openHead @ head, closeHead @ head];
makeExpanderBoxes2[head_Sym, args_List] := ColumnBox[
  FlatList[
    openHead @ head,
    MapMostLast[
      addTabComma, RBox["\t", #]&,
      If[$remExpansions > 0,
        BlockDecrement[$remExpansions, HoldMap[makeExpanderBoxes1, args]],
        MapMakeBoxes @ args
      ]
    ],
    closeHead @ head
  ],
  Left,
  RowAlignments -> Baseline
];

openHead[head_] := RBox[MakeBoxes @ head, "["];
openHead[List]  := "{";
openHead[Dict]  := LAssoc;

closeHead[_]    := "]";
closeHead[List] := "}";
closeHead[Dict] := RAssoc;

addTabComma[boxes_] := RBox["\t", boxes, ","];
addTabComma[GridBox[grid_, opts___]] := RBox["\t", Make[GridBox, MapAt[addComma, grid, {-1, -1}], opts]];
addComma[box_] := RBox[box, ","];

(**************************************************************************************************)

DeclareThenScan[MakeBoxDefinitions]

(* TODO: why not use CoreBoxes for this? *)
MakeBoxDefinitions[SetDelayed[lhs_, rhs_]] := (
  MakeBoxes[lhs, StandardForm | TraditionalForm] := rhs;
);

(**************************************************************************************************)

DeclarationDefs[
  SetAtomFormHead[sym_Sym]     := KeyStoreAdd[$AtomFormStore, NoEval @ sym],
  SetCompoundFormHead[sym_Sym] := StoreSet[$CompoundFormStore, NoEval @ sym, 1]
];

(**************************************************************************************************)

SetHoldC @ SetPred1[CoreBoxHeadQ, AtomFormHeadQ, CompoundFormHeadQ]

CoreBoxHeadQ[s_Sym]      := StoreKeyQ[$CoreBoxStore, NoEval @ s];
AtomFormHeadQ[s_Sym]     := StoreKeyQ[$AtomFormStore, NoEval @ s];
CompoundFormHeadQ[s_Sym] := StoreKeyQ[$CompoundFormStore, NoEval @ s];
CompoundFormArity[s_Sym] := StoreGet[$CompoundFormStore, NoEval @ s];

