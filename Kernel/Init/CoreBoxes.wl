SystemExports[
  "FormHead",          Unformatted, Uninteractive, OutlineForm,
  "BoxFunction",       OutlineBoxes, LenDotsBox,
  "IOFunction",        MakeCoreBoxes, CoreBoxes, SystemBoxes, MapMakeBoxes, MapMakeBoxesSeq,
  "DebuggingFunction", MakeCoreBoxesTraditional, MakeCoreBoxesModified,
  "SpecialVariable",   $UseCoreBoxFormatting, $UseCoreBoxInteractivity, $CurrentCoreBoxModifiers, $UseTraditionalForm,
  "Predicate",         HasCoreBoxesQ,
  "MetaFunction",      DeclareCoreBoxes, DeclareCoreSubBoxes, MakeBoxDefinitions, DeclareOutlineBoxes,
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

LenDotsBox[n_Int] := SubBox[Dots, NatStr @ n];

(**************************************************************************************************)

DeclareSeqScan[DeclareOutlineBoxes]

SetInitial[$OutlineBoxHeadDict, UDict[]];

DeclareOutlineBoxes[sym_Symbol] := Then[
  $OutlineBoxHeadDict[Hold[sym]] = True,
  CoreBoxes[s_sym] := outlineBoxesFull[s]
];

DeclareOutlineBoxes[InternalData];

(**************************************************************************************************)

DeclareThenScan[MakeBoxDefinitions]

(* TODO: why not use CoreBoxes for this? *)
MakeBoxDefinitions[SetDelayed[lhs_, rhs_]] := (
  MakeBoxes[lhs, StandardForm | TraditionalForm] := rhs;
);
