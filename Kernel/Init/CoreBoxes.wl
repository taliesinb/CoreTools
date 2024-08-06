SystemExports[
  "FormHead",          Unformatted, Uninteractive,
  "IOFunction",        MakeCoreBoxes, CoreBoxes, SystemBoxes, MapMakeBoxes, MapMakeBoxesSeq,
  "DebuggingFunction", MakeCoreBoxesTraditional, MakeCoreBoxesModified,
  "SpecialVariable",   $UseCoreBoxFormatting, $UseCoreBoxInteractivity, $CurrentCoreBoxModifiers, $UseTraditionalForm,
  "Predicate",         HasCoreBoxesQ,
  "MetaFunction",      DeclareCoreBoxes, DeclareCoreSubBoxes, MakeBoxDefinitions,
  "ScopingFunction",   DisableCoreBoxFormatting, DisableCoreBoxInteractivity
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

SetInitial[$coreBoxHead, UAssoc[]]

HasCoreBoxesQ[s_] = Lookup[$coreBoxHead, Hold @ s, False];

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
SetInitial[$CurrentCoreBoxModifiers, UAssoc[TraditionalForm :> $UseTraditionalForm]]

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

  $coreBoxHead[Hold[sym]] = True;

  MakeBoxes[$LHS:sym | _sym, StandardForm] /; $UseCoreBoxFormatting :=
    MaybeEval @ StatusAreaBox[MakeCoreBoxes @ $LHS, name];

  MakeBoxes[$LHS:sym | _sym, TraditionalForm] /; $UseCoreBoxFormatting :=
    MaybeEval @ MakeCoreBoxesTraditional @ $LHS;
];

DeclareCoreSubBoxes[sym_Symbol] := With[
  {name = SymbolName[sym]},

  $coreBoxHead[Hold[sym]] = True;

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
  setupCoreBoxes @ PatternHeadSymbol @ lhs;
  MakeCoreBoxes[lhs] := rhs;
);

Protect[CoreBoxes];

(**************************************************************************************************)

SystemBoxes /: SetDelayed[SystemBoxes[lhs_], rhs_] := (
  MakeBoxes[lhs, StandardForm | TraditionalForm] := rhs;
);

Protect[SystemBoxes];

(**************************************************************************************************)

DeclareThenScan[MakeBoxDefinitions]

(* TODO: why not use CoreBoxes for this? *)
MakeBoxDefinitions[SetDelayed[lhs_, rhs_]] := (
  MakeBoxes[lhs, StandardForm | TraditionalForm] := rhs;
);
