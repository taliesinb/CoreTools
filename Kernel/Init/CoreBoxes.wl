SystemExports[
  "FormHead",          Unformatted, Uninteractive, ExpanderForm, OutlineForm, EllipsisForm,
  "BoxFunction",       OutlineBoxes, ExpanderBoxes, HoldExpanderBoxes, LenDotsBox, EllipsisBox,
  "IOFunction",        MakeCoreBoxes, CoreBoxes, SystemBoxes, MapMakeBoxes, MapMakeBoxesSeq, MakeBoxes1, MakeBoxes12, MakeBoxes21, MapMakeBoxesLimited,
  "DebuggingFunction", MakeCoreBoxesTraditional, MakeCoreBoxesModified,
  "SpecialVariable",   $UseCoreBoxFormatting, $UseCoreBoxInteractivity, $CurrentCoreBoxModifiers, $UseTraditionalForm,
  "Predicate",         HasCoreBoxesQ,
  "MetaFunction",      DeclareCoreBoxes, DeclareCoreSubBoxes, MakeBoxDefinitions, DeclareOutlineBoxes, DeclareExpanderBoxes,
  "ScopingFunction",   DisableCoreBoxFormatting, DisableCoreBoxInteractivity
];

PrivateExports[
  "SpecialVariable", $CoreBoxHeadDict, $OutlineBoxHeadDict
];

(**************************************************************************************************)

DeclareHoldAll[MapMakeBoxes, MapMakeBoxesSeq]

MapMakeBoxes = CaseOf[
  items_Dict := KeyMapValueMap[MakeBoxes, MakeBoxes, items]; (* KMVM preserves holdness *)
  items_List := HoldMap[MakeBoxes, items];
];

MapMakeBoxesSeq[items___] := Seq @@ HoldMap[MakeBoxes, {items}]

(**************************************************************************************************)

SetHoldC[MakeBoxes1, MakeBoxes12, MakeBoxes21]
SetHoldF[MapMakeBoxesLimited]

MakeBoxes1[e_, _] := MakeBoxes[e];
MakeBoxes12[e_, p_] := {MakeBoxes[e], MakeBoxes[p]};
MakeBoxes21[e_, p_] := {MakeBoxes[p], MakeBoxes[e]};

MapMakeBoxesLimited[expr_, axis_, maxSize_, maxCount_, fn_:MakeBoxes1] := Locals[
  totalSize = 0; heldItems = HoldArgsP[expr];
  len = Len @ heldItems;
  boxes = {}; i = 0; num = Min[len, maxCount];
  While[++i <= num && totalSize <= maxSize,
    itemBox = First[fn @@@ Extract[heldItems, i, HoldC]];
    itemSize = ToImageSize @ RawBoxes @ itemBox;
    totalSize += Part[itemSize, axis];
    AppendTo[boxes, itemBox];
  ];
  If[i < len, AppendTo[boxes, EllipsisBox[len - i]]];
  boxes
];

(**************************************************************************************************)

DeclareHoldAllComplete[HasCoreBoxesQ]
DeclarePredicate1[HasCoreBoxesQ]

SetInitial[$CoreBoxHeadDict, UDict[]]

HasCoreBoxesQ[s_] = Lookup[$CoreBoxHeadDict, Hold @ s, False];

(**************************************************************************************************)

DeclareHoldAllComplete[MakeCoreBoxes, DisableCoreBoxFormatting, DisableCoreBoxInteractivity]

(* we'll add to these definitions *)
MakeCoreBoxes[_] := FailEval;

DisableCoreBoxFormatting[body_]    := Block[{$UseCoreBoxFormatting = False}, body];
DisableCoreBoxInteractivity[body_] := Block[{$UseCoreBoxInteractivity = False}, body];

(* TODO: make interactivity just a modifier *)

MakeBoxes[Unformatted[lhs_], form:StandardForm | TraditionalForm] := Block[
  {$UseTraditionalForm = form === TraditionalForm, $UseCoreBoxFormatting = False},
  MakeBoxes[lhs, form]
];

MakeBoxes[Uninteractive[lhs_], form:StandardForm | TraditionalForm] := Block[
  {$UseTraditionalForm = form === TraditionalForm, $UseCoreBoxInteractivity = False},
  MakeBoxes[lhs, form]
];

(**************************************************************************************************)

DeclareHoldAllComplete[MakeCoreBoxesModified, MakeCoreBoxesTraditional]

SetInitial[$UseTraditionalForm, False];
SetInitial[$CurrentCoreBoxModifiers, UDict[TraditionalForm :> $UseTraditionalForm]]

MakeCoreBoxesModified[rules_, expr_] :=
  BlockAssociate[$CurrentCoreBoxModifiers, rules, MakeCoreBoxes @ expr];

MakeCoreBoxesTraditional[expr_] :=
  BlockTrue[$UseTraditionalForm, MakeCoreBoxes @ expr];

(**************************************************************************************************)

DeclareHoldAll[DeclareCoreBoxes, DeclareCoreSubBoxes]
DeclareDeclare[DeclareCoreBoxes, DeclareCoreSubBoxes]

SetInitial[$UseCoreBoxFormatting, True]
SetInitial[$UseCoreBoxInteractivity, True]

DeclareCoreBoxes[sym_Symbol] := With[
  {name = SymbolName[sym]},

  $CoreBoxHeadDict[Hold[sym]] = True;

  MakeBoxes[$LHS:sym | _sym, StandardForm] /; $UseCoreBoxFormatting :=
    MaybeEval @ StatusAreaBox[MakeCoreBoxes @ $LHS, name];

  MakeBoxes[$LHS:sym | _sym, TraditionalForm] /; $UseCoreBoxFormatting :=
    MaybeEval @ MakeCoreBoxesTraditional @ $LHS;
];

DeclareCoreSubBoxes[sym_Symbol] := With[
  {name = SymbolName[sym]},

  $CoreBoxHeadDict[Hold[sym]] = True;

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
  setupCoreBoxes @ PatHeadSym @ lhs;
  MakeCoreBoxes[lhs] := rhs;
);

Protect[CoreBoxes];

(**************************************************************************************************)

SystemBoxes /: SetDelayed[SystemBoxes[lhs_], rhs_] := (
  MakeBoxes[lhs, StandardForm | TraditionalForm] := rhs;
);

Protect[SystemBoxes];

(**************************************************************************************************)

CoreBoxes[OutlineForm[expr_]]           := OutlineBoxes[expr];

(**************************************************************************************************)

SetHoldC[OutlineBoxes, outlineBoxesSmart, outlineBoxesFull, outlineBoxesArgs, outlineBoxesLeaf];

OutlineBoxes[HoldForm[expr_]] := OutlineBoxes[expr];
OutlineBoxes[expr_] := If[HoldLen[expr] <= 4, outlineBoxesFull @ expr, outlineBoxesSmart @ expr];

outlineBoxesSmart[expr:(head_Sym[___])] /; TrueQ[$OutlineBoxHeadDict @ Hold @ head] := outlineBoxesFull[expr];
outlineBoxesSmart[expr:(head_Sym[___])] := outlineHeadArgs[head, LenDotsBox @ HoldLen @ expr];
outlineBoxesSmart[expr_] := outlineBoxesLeaf @ expr;

outlineBoxesFull[expr:(head_Sym[args___])] := outlineHeadArgs[head, outlineBoxesArgs @ List[args]];
outlineBoxesFull[expr_] := outlineBoxesLeaf @ expr;

outlineBoxesArgs[{some:Repeated[_, {6}], rest__}] := CommaRowBox @ Append[LenDotsBox @ HoldSeqLen @ rest] @ HoldMap[outlineBoxesSmart, {some}];
outlineBoxesArgs[args_List] := CommaRowBox @ HoldMap[outlineBoxesSmart, args];

SetHoldF @ outlineHeadArgs;
outlineHeadArgs[_, argBoxes_]        := RBox[Dots, "[", grayBox @ argBoxes, "]"]
outlineHeadArgs[Function, argBoxes_] := RBox["Fn", "[", grayBox @ argBoxes, "]"]
outlineHeadArgs[head_Sym, argBoxes_] := RBox[HoldSymbolName @ head, "[", grayBox @ argBoxes, "]"]
outlineHeadArgs[List, argBoxes_]     := RBox["{",    grayBox @ argBoxes, "}"];
outlineHeadArgs[Dict, argBoxes_]     := RBox[LAssoc, grayBox @ argBoxes, RAssoc];

grayBox[e_] := StyleBox[e, FontColor -> GrayLevel[0.5]];
outlineBoxesLeaf[_List] := "{\[Ellipsis]}";
outlineBoxesLeaf[_Dict] := "\[LeftAssociation]\[Ellipsis]}\[RightAssociation]";
outlineBoxesLeaf[i_Int] := IntStr @ i;
outlineBoxesLeaf[s_Sym] := HoldSymbolName @ s;
outlineBoxesLeaf[_]     := "\[Ellipsis]";

LenDotsBox[n_Int] := EllipsisBox[NatStr @ n];

(**************************************************************************************************)

CoreBoxes[EllipsisForm[expr_]] := EllipsisBox @ MakeBoxes @ expr;

EllipsisBox[nBoxes_] := OverscriptBox["\"\[Ellipsis]\"", LowerBox[nBoxes, 1]];
EllipsisBox[] := EllipsisBox[None] := "\"\[Ellipsis]\"";

(**************************************************************************************************)

DeclareSeqScan[DeclareOutlineBoxes]

SetInitial[$OutlineBoxHeadDict, UDict[]];

DeclareOutlineBoxes[sym_Symbol] := Then[
  $OutlineBoxHeadDict[Hold[sym]] = True,
  CoreBoxes[s_sym] := outlineBoxesFull[s]
];

DeclareOutlineBoxes[InternalData];

(**************************************************************************************************)

DeclareSeqScan[DeclareExpanderBoxes]

DeclareExpanderBoxes[sym_Sym] := CoreBoxes[sym[args___]] := HoldExpanderBoxes[sym, args];

(**************************************************************************************************)

$remExpansions = Inf;

CoreBoxes[ExpanderForm[head_Sym[args___]]] :=
  HoldExpanderBoxes[head, args];

CoreBoxes[ExpanderForm[head_Sym[args___], level_]] :=
  BlockSet[$remExpansions, level, HoldExpanderBoxes[head, args]];

SetHoldC[HoldExpanderBoxes, makeExpanderBoxes1, makeExpanderBoxes2];

ExpanderBoxes[args___] := HoldExpanderBoxes[args];
HoldExpanderBoxes[head_Sym, args___] := makeExpanderBoxes2[head, {args}];

makeExpanderBoxes1[expr_] := MakeBoxes[expr];
makeExpanderBoxes1[head_Sym[args___]] := makeExpanderBoxes2[head, {args}];

makeExpanderBoxes2[head_Sym, args_List] := ColumnBox[
  FlatList[
    RBox[MakeBoxes @ head, "["],
    MapMostLast[
      addTabComma, RBox["\t", #]&,
      If[$remExpansions > 0,
        BlockDecrement[$remExpansions, HoldMap[makeExpanderBoxes1, args]],
        MapMakeBoxes @ List @ args
      ]
    ],
    "]"
  ],
  Left
];

addTabComma[boxes_] := RBox["\t", boxes, ","];
addTabComma[GridBox[grid_, opts___]] := RBox["\t", Make[GridBox, MapAt[addComma, grid, {-1, -1}], opts]];
addComma[box_] := RBox[box, ","];

(**************************************************************************************************)

DeclareThenScan[MakeBoxDefinitions]

(* TODO: why not use CoreBoxes for this? *)
MakeBoxDefinitions[SetDelayed[lhs_, rhs_]] := (
  MakeBoxes[lhs, StandardForm | TraditionalForm] := rhs;
);

