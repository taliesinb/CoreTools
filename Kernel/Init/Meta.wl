PackageExports[
  "MetaFunction",
    DeclareDeclare,
    DeclareSeqScan, DeclareThenScan,
    StrListableDefs, DeclarationDefs,

    DefineOperator1Rules, DefineOperator2Rules,

    SetCurry1, SetCurry2, SetCurry12, SetCurry23, SetCurry13,
    SetCurry123, SetCurry234,
    SetPred1, SetPred2, SetPred3, SetNPred1, SetNPred2, SetNPred3,
    SetHoldF, SetHoldR, SetHoldA, SetHoldC, SetHoldSeq,
    SetFlat, SetListable, SetListable1, SetListableOp,
    SetStrict, SetStrictOp, SetExcepting,
    SetBoolVar, SetSwitchVar,

    DefinePseudoMacro,

  "Function",         DelDupsBy, CatenateSymbolLists, JoinSymbolLists,
  "ScopingFunction",  SubWith,
  "ControlFlow",      Initially,
  "IOFunction",       ToImplementationSymbol,
  "DataHead",         SymbolList
];

PrivateExports[
  "MessageFunction",  StrictMsg,
  "SpecialVariable",  $ExceptingSyms, $ExceptingSymP,
  "SpecialFunction",  BoolLValFn, SwitchLValFn
];

SessionExports[
  "RegistryVariable", $SwitchValues,
  "CacheVariable",    $InitializationHashes
];

(*************************************************************************************************)

DelDupsBy[list_List, fn_] := Map[First, GatherBy[list, fn]];
DelDupsBy[expr_, fn_]     := DeleteDuplicatesBy[expr, fn];
DelDupsBy[fn_][expr_]     := DelDupsBy[expr, fn];

(*************************************************************************************************)

SetAttributes[Initially, HoldAllComplete];

If[!HasIValueQ[$InitializationHashes], $InitializationHashes = UDict[]];

Initially[body___] := Which[
  Lookup[$InitializationHashes, $CurrentPackageFile] === $CurrentPackageFileHash,
    "InitializationNotNeeded",
  Check[Then[body], $Failed] === $Failed,
    Lookup[$InitializationHashes, $CurrentPackageFile] = None;
    "InitializationFailed",
  True,
    $InitializationHashes[$CurrentPackageFile] = $CurrentPackageFileHash;
    "Initialized"
];

(*************************************************************************************************)

DeclaredHere[SymbolList];

SetAttributes[SymbolList, {HoldAll, Flat}]

CatenateSymbolLists[{}] := SymbolList[];
CatenateSymbolLists[list:{__SymbolList}] := DelDups @ Apply[Join, list];

JoinSymbolLists[] := SymbolList[];
JoinSymbolLists[list_SymbolList] := list;
JoinSymbolLists[lists__SymbolList] := DelDups @ Join[lists];

(*************************************************************************************************)

ToImplementationSymbol[sym_] := ToImplementationSymbol[sym] =
  Symbol @ StrJoin[$Context, "i", SymbolName @ sym];

(*************************************************************************************************)

General::expectedDeclareSymbol = "Expected symbol in metafunction call ``.";
SetAttributes[declareSymbolQ, {HoldAllComplete}];
declareSymbolQ[_] := False;

DeclareDeclare[head__Symbol] := Scan[DeclareDeclare, {head}];
DeclareDeclare[head_Symbol] := (
  declareSymbolQ[head] := True;
  head[syms__Symbol]       := Scan[head, Hold[syms]];
  e_head                   := (Message[General::expectedDeclareSymbol, HoldForm @ e]; $Failed)
);

(*************************************************************************************************)

General::declarationExpectedSetD = "Expected SetD instead of ``.";

declareDeclarationDefinitions[sym_] := (
  SetAttributes[sym, HoldAllComplete];
  sym[defs:BlankSeq2] := Scan[sym, Hold[defs]];
  sym[expr_]          := (Message[sym::declarationExpectedSetD, HoldForm @ expr]; $Failed)
);

(*************************************************************************************************)

SetAttributes[declareFnScan, HoldAllComplete];

declareDeclarationDefinitions[DeclarationDefs];

DeclarationDefs[HoldP[SetD][(fn_Symbol)[VPattern[var_Symbol, VBlank[Symbol]]], rhs_]] := With[
  {lhs = Make[Pattern, var, Blank @ Symbol]},
  If[!declareSymbolQ[fn], setupDeclareSym[fn]];
  fn[lhs] := Then[rhs, SymbolList[var]];
];

DeclarationDefs[HoldP[SetD][(fn_Symbol)[VPattern[var_Symbol, VBlankSeq[Symbol]]], rhs_]] := With[
  {lhs = Make[Pattern, var, BlankSeq @ Symbol]},
  If[!declareSymbolQ[fn], setupDeclareSym[fn]];
  fn[lhs] := Then[rhs, SymbolList[var]];
];

setupDeclareSym[fn_] := (
  declareSymbolQ[fn]  := True;
  fn[args___]         := declareFnScan[fn, args];
  fn[args_SymbolList] := Then[Scan[fn, args], args];
  fn[other:(_Symbol ? declareSymbolQ)[___]] := Construct[fn, other];
  e_fn           := (Message[General::expectedDeclareSymbol, HoldForm @ e]; $Failed)
);

declareFnScan[fn_, syms___Symbol] := Then[Scan[fn, Hold @ syms], SymbolList @ syms];
declareFnScan[fn_, args___]       := Apply[SymbolList, Map[fn, NoEval @ {args}]];

(*************************************************************************************************)

DeclaredHere[SetListableOp, SetListable1];

DeclarationDefs[
  SetListableOp[sym_Sym] := Set[ListableFunctionQ[_sym], True],
  SetListable1[sym_Sym]  := SetD[FmL:sym[_List, _], Thread[NoEval @ FmL, List, 1]]
];

(*************************************************************************************************)

DeclaredHere[SetCurry1, SetCurry2, SetCurry12, SetCurry23, SetCurry13, SetCurry123, SetCurry234];

(* TODO: auto making of held operator forms via appropriate AttributeFn *)
DeclarationDefs[
    SetCurry1[sym_Sym] := SetD[sym[FmA_][FmR___],           sym[FmA, FmR]],
    SetCurry2[sym_Sym] := SetD[sym[FmB_][FmA_],             sym[FmA, FmB]],
   SetCurry12[sym_Sym] := SetD[sym[FmA_, FmB_][FmC_],       sym[FmA, FmB, FmC]],
   SetCurry23[sym_Sym] := SetD[sym[FmB_, FmC_][FmA_],       sym[FmA, FmB, FmC]],
   SetCurry13[sym_Sym] := SetD[sym[FmA_, FmC_][FmB_],       sym[FmA, FmB, FmC]],
  SetCurry123[sym_Sym] := SetD[sym[FmA_, FmB_, FmC_][FmD_], sym[FmA, FmB, FmC, FmD]],
  SetCurry234[sym_Sym] := SetD[sym[FmB_, FmC_, FmD_][FmA_], sym[FmA, FmB, FmC, FmD]]
];

(*************************************************************************************************)

DeclaredHere[SetPred1, SetPred2, SetPred3, SetNPred1, SetNPred2, SetNPred3];

DeclarationDefs[
  SetPred1[sym_Sym]  := Set[sym[_],       False],
  SetPred2[sym_Sym]  := Set[sym[_, _],    False],
  SetPred3[sym_Sym]  := Set[sym[_, _, _], False],
  SetNPred1[sym_Sym] := Set[sym[_],       True],
  SetNPred2[sym_Sym] := Set[sym[_, _],    True],
  SetNPred3[sym_Sym] := Set[sym[_, _, _], True]
];

(*************************************************************************************************)

DeclaredHere[SetHoldF, SetHoldR, SetHoldA, SetFlat, SetHoldC, SetListable, SetHoldSeq];

DeclarationDefs[
  SetHoldF[syms__Sym]    := SetAttributes[{syms}, HoldFirst],
  SetHoldR[syms__Sym]    := SetAttributes[{syms}, HoldRest],
  SetHoldA[syms__Sym]    := SetAttributes[{syms}, HoldAll],
  SetFlat[syms__Sym]     := SetAttributes[{syms}, Flat],
  SetHoldC[syms__Sym]    := SetAttributes[{syms}, HoldAllComplete],
  SetListable[syms__Sym] := SetAttributes[{syms}, Listable],
  SetHoldSeq[syms__Sym]  := SetAttributes[{syms}, SequenceHold]
];

(*************************************************************************************************)

If[!ListQ[$ExceptingSyms],
  $ExceptingSyms = {};
  $ExceptingSymP = Alt[];
];

DeclaredHere[SetExcepting];

DeclarationDefs[
  SetExcepting[syms__Sym] := Then[
    $ExceptingSyms = Join[$ExceptingSyms, {syms}],
    $ExceptingSymP = Join[$ExceptingSymP, Alt[syms]]
  ]
];

(*************************************************************************************************)

SetStrict::usage = "SetStrict[sym$] declares that sym$[___] should throw an error if it doesn't match.";

DeclaredHere[SetStrict, SetStrictOp, DeclareSeqScan, DeclareThenScan];

SetHoldC[SetStrict, SetStrictOp, DeclareSeqScan, DeclareThenScan]

General::badArguments = "Bad arguments: ``.";
General::badSeqScanArg = "Bad argument to ``: ``.";

StrictMsg[head_, sloc_, lhs_] := If[HasDownDefsQ[IssueMessage],
  IssueMessage[head -> sloc, "badArguments", lhs],
  Message[head::badArguments, lhs]; $Failed
];

DeclarationDefs[

  SetStrict[head_Sym] := With[{sloc = SourceLocation[]},
    SetD @@ Hold[$LHS_head, StrictMsg[head, sloc, HoldForm @ $LHS]]
  ],

  SetStrictOp[head_Sym] := With[{sloc = SourceLocation[]},
    SetD @@ Hold[$LHS:(_head[___]), StrictMsg[head, sloc, HoldForm @ $LHS]]
  ],

  DeclareSeqScan[head_Sym] := (
    SetStrict[head];
    SetD[e_head, Message[General::badSeqScanArg, head, HoldForm @ e]; $Failed];
    SetD[head[seq:BlankSeq2], Scan[head, Hold[seq]]];
  ),

  DeclareThenScan[head_Sym] := (
    SetHoldC[head];
    SetD[head[$LHS_], Message[MessageName[head, "badArguments"], HoldForm @ $LHS]; $Failed];
    SetD[head[Null], Null];
    SetD[head[seq:BlankSeq2], Scan[head, Hold[seq]]];
    SetD[head[Then[args___]], head[args]];
  )
];

SetStrict[DefineAliasRules]

(**************************************************************************************************)

SetHoldF @ DeclareSeqScan @ SetBoolVar;

SetBoolVar[sym_Sym] := Then[
  sym::usage = StrJoin[SymName @ sym, " can be set to True or False."],
  SetLValFn[sym, BoolLValFn];
]

SetHoldC @ BoolLValFn;

BoolLValFn[Set[_, BoolP]]     := LValOk;
BoolLValFn[Set[sym_, value_]] := msgVarValue[sym, "boolean", value, {False, True}];
BoolLValFn[e_]                := msgVarChange[e, "boolean"];

(**************************************************************************************************)

SetStrict @ SetHoldF @ SetListable1 @ SetSwitchVar;

If[!HasIValueQ[$SwitchValues], $SwitchValues = UDict[]];

SetSwitchVar[sym_Sym, vals_List] := Then[
  $SwitchValues[Hold[sym]] = vals,
  sym::usage = StrJoin[SymName @ sym, " can be set to one of ", StrJoin @ Riffle[ToInputStr /@ vals, ", "], "."],
  SetLValFn[sym, SwitchLValFn];
];

SetHoldC @ SwitchLValFn;

SwitchLValFn[Set[sym_Sym, value_]] := LValOk /; ElementQ[value, $SwitchValues @ Hold @ sym];
SwitchLValFn[Set[sym_Sym, value_]] := msgVarValue[sym, "switch", value, $SwitchValues @ Hold @ sym];
SwitchLValFn[e_]                   := msgVarChange[e, "switch"];

(**************************************************************************************************)

msgSwitchVal[Hold[s_], e_] := Message[s::invalidFlagValue, HoldForm[s], e];

SetHoldF @ msgVarValue;
msgVarValue[sym_, kind_, value_, allowed_] :=
  (Message[Set::invalidVariableValue, kind, HoldForm[sym], HoldForm[value], FullRow[allowed, ", "]]; $Failed);

SetHoldF @ msgVarChange;
msgVarChange[expr_, kind_] := msgVarChange[expr, kind, LValHead @ expr];
msgVarChange[expr_, kind_, Hold[sym_]] :=
  (Message[sym::invalidVariableChange, kind, HoldForm @ expr]; $Failed);

General::invalidVariableValue = "Setting for `` variable `` not valid: ``. Allowed values are ``.";
General::invalidVariableChange = "Invalid change for `` variable: ``.";

(*************************************************************************************************)

General::notStringOrStrings = "First argument `` should be a string, or nested container of such.";

strImplNeedsSetupQ[_] := True;

declareDeclarationDefinitions[StrListableDefs];

(* TODO: use this technique more widely *)
StrListableDefs[sd:SetD[$LHS_, $RHS_]] := With[
  {head = First @ PatHead @ $LHS},
  {impl = ToImplementationSymbol @ head},
  ReleaseHold @ ReplaceAll[Hold[sd] /. {mn_MessageName :> mn, head -> impl}];
  If[strImplNeedsSetupQ[head],
    strImplNeedsSetupQ[head] = False;
    head[expr_ ? StrOrVecQ, args___] := impl[expr, args];
    head[expr:ListDictP, args___]    := Map[elem |-> head[elem, args], expr];
    head[expr_, ___]                 := ErrorMsg[head::notStringOrStrings, expr];
    expr_impl                        := badImplArgs[head, expr];
  ];
];

SetHoldR[badImplArgs];
badImplArgs[sym_, _[args___]] := ErrorMsg[sym::badArguments, HoldForm[sym[args]]];

(*************************************************************************************************)

DeclareSeqScan[DefineOperator1Rules, DefineOperator2Rules]

DefineOperator1Rules[opSym_Symbol -> fn_Symbol] := SetD[opSym[FmA_][FmB_], fn[FmA, FmB]];
DefineOperator2Rules[opSym_Symbol -> fn_Symbol] := SetD[opSym[FmB_][FmA_], fn[FmA, FmB]];

(**************************************************************************************************)

SetStrict @ DefinePseudoMacro;

DefinePseudoMacro[sym_Sym, rule:RuleD[VHoldP[lhs_], rhs_]] := Then[
  UpValues[sym] = Prepend[UpValues[sym], pseudoMacroUpValue[sym, lhs, rhs]];
  DefinePartialMacro[sym, rule]
];

SetHoldC @ pseudoMacroUpValue;

pseudoMacroUpValue[sym_Sym, lhs_, rhs_] := Apply[RuleD, HoldC[
  HoldPattern @ $setd$[$LHS_, lhs],
  $with$[
    {$macroHead$ = First @ PatHead @ $LHS, $macroSrcLoc$ = SourceLocation[]},
    $setd$[$LHS, rhs]
  ]
]] // subTags;

subTags[e_] := e /. {
  $with$ -> With, $setd$ -> SetD,
  HoldP[$MacroHead] -> $macroHead$,
  HoldP[$MacroSrcLoc] -> $macroSrcLoc$
};

(**************************************************************************************************)

SetStrict @ SetHoldC @ SubWith;

SubWith[{}, body_] := body;

SubWith[v_Symbol, body_] :=
  With[{v = v}, body];

SubWith[{v_Symbol}, body_] :=
  With[{v = v}, body];

SubWith[{v1_Symbol, v2_Symbol}, body_] :=
  With[{v1 = v1, v2 = v2}, body];

SubWith[{v1_Symbol, v2_Symbol, v3_Symbol}, body_] :=
  With[{v1 = v1, v2 = v2, v3 = v3}, body];

SubWith[{v1_Symbol, v2_Symbol, v3_Symbol, v4_Symbol, rest___}, body_] :=
  SubWith[{rest}, With[{v1 = v1, v2 = v2, v3 = v3, v4 = v4}, body]];



