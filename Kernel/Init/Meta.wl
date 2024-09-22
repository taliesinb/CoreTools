PackageExports[
  "MetaFunction",
    DeclaredHere,
    DeclareDeclare,
    DeclareSeqScan, DeclareThenScan,
    StrListableDefs, DeclarationDefs,

    DefineOperator1Rules, DefineOperator2Rules,

    SetCurry1, SetCurry2, SetCurry12, SetCurry23, SetCurry13,
    SetPred1, SetPred2, SetPred3, SetNPred1, SetNPred2, SetNPred3,
    SetHoldF, SetHoldR, SetHoldA, SetHoldC, SetHoldSeq,
    SetFlat, SetListable, SetListable1, SetListableOp,
    SetStrict, SetExcepting,

    DefinePseudoMacro,

  "Function",         CatenateSymbolLists, JoinSymbolLists,
  "ScopingFunction",  SubWith,
  "ControlFlow",      Initially,
  "IOFunction",       ToImplementationSymbol,
  "Head",             SymbolList
];

PrivateExports[
  "SpecialVariable",  $ExceptingSymbols, $ExceptingSymbolP
  "CacheVariable",    $InitializationHashes
];

(*************************************************************************************************)

SetAttributes[Initially, HoldAllComplete];

If[!HasIValueQ[$InitializationHashes], $InitializationHashes = UDict[]];

Initially[body___] := Which[
  Lookup[$InitializationHashes, $CurrentPackageFile] === Prelude`Packages`$CurrentPackageFileHash,
    "InitializationNotNeeded",
  Check[Then[body], $Failed] === $Failed,
    Lookup[$InitializationHashes, $CurrentPackageFile] = None;
    "InitializationFailed",
  True,
    $InitializationHashes[$CurrentPackageFile] = Prelude`Packages`$CurrentPackageFileHash;
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

General::declarationExpectedSetDelayed = "Expected SetDelayed instead of ``.";

declareDeclarationDefinitions[sym_] := (
  SetAttributes[sym, HoldAllComplete];
  sym[defs:BlankSeq2] := Scan[sym, Hold[defs]];
  sym[expr_]          := (Message[sym::declarationExpectedSetDelayed, HoldForm @ expr]; $Failed)
);

(*************************************************************************************************)

SetAttributes[declareFnScan, HoldAllComplete];

declareDeclarationDefinitions[DeclarationDefs];

DeclarationDefs[HoldP[SetDelayed][(fn_Symbol)[VPattern[var_Symbol, VBlank[Symbol]]], rhs_]] := With[
  {lhs = Make[Pattern, var, Blank @ Symbol]},
  If[!declareSymbolQ[fn], setupDeclareSym[fn]];
  fn[lhs] := Then[rhs, SymbolList[var]];
];

DeclarationDefs[HoldP[SetDelayed][(fn_Symbol)[VPattern[var_Symbol, VBlankSeq[Symbol]]], rhs_]] := With[
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

DeclarationDefs[
  SetListableOp[sym_Sym] := Set[ListableFunctionQ[_sym], True],
  SetListable1[sym_Sym]  := SetDelayed[sym[arg1_List, arg2_], Map[Function[a1, sym[a1, arg2]], arg1]]
];

(*************************************************************************************************)

(* TODO: auto making of held operator forms via appropriate AttributeFn *)
DeclarationDefs[
   SetCurry1[sym_Sym] := SetDelayed[sym[arg1_][rest___],      sym[arg1, rest]],
   SetCurry2[sym_Sym] := SetDelayed[sym[arg2_][arg1_],        sym[arg1, arg2]],
  SetCurry12[sym_Sym] := SetDelayed[sym[arg1_, arg2_][arg3_], sym[arg1, arg2, arg3]],
  SetCurry23[sym_Sym] := SetDelayed[sym[arg2_, arg3_][arg1_], sym[arg1, arg2, arg3]],
  SetCurry13[sym_Sym] := SetDelayed[sym[arg1_, arg3_][arg2_], sym[arg1, arg2, arg3]]
];

(*************************************************************************************************)

DeclarationDefs[
  SetPred1[sym_Sym]  := Set[sym[_],       False],
  SetPred2[sym_Sym]  := Set[sym[_, _],    False],
  SetPred3[sym_Sym]  := Set[sym[_, _, _], False],
  SetNPred1[sym_Sym] := Set[sym[_],       True],
  SetNPred2[sym_Sym] := Set[sym[_, _],    True],
  SetNPred3[sym_Sym] := Set[sym[_, _, _], True]
];

(*************************************************************************************************)

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

If[!ListQ[$ExceptingSymbols],
  $ExceptingSymbols = {};
  $ExceptingSymbolP = Alt[];
];

DeclarationDefs[
  SetExcepting[syms__Sym] := Then[
    $ExceptingSymbols = Join[$ExceptingSymbols, {syms}],
    $ExceptingSymbolP = Join[$ExceptingSymbolP, Alt[syms]]
  ]
];

(*************************************************************************************************)

SetStrict::usage = "SetStrict[sym$] declares that sym$[___] should throw an error if it doesn't match.";

SetHoldC[SetStrict, DeclareSeqScan, DeclareThenScan]

General::badArguments = "Bad arguments: ``.";
General::badSeqScanArg = "Bad argument to ``: ``.";

DeclarationDefs[
  SetStrict[head_Sym] := With[{sloc = SourceLocation[]}, Apply[
    SetDelayed,
    Hold[$LHS_head, IssueMessage[head -> sloc, "badArguments", HoldForm @ $LHS]]
  ]],

  DeclareSeqScan[head_Sym] := (
    SetStrict[head];
    SetDelayed[e_head, Message[General::badSeqScanArg, head, HoldForm @ e]; $Failed];
    SetDelayed[head[seq:BlankSeq2], Scan[head, Hold[seq]]];
  ),

  DeclareThenScan[head_Sym] := (
    SetHoldC[head];
    SetDelayed[head[$LHS_], Message[MessageName[head, "badArguments"], HoldForm @ $LHS]; $Failed];
    SetDelayed[head[Null], Null];
    SetDelayed[head[seq:BlankSeq2], Scan[head, Hold[seq]]];
    SetDelayed[head[Then[args___]], head[args]];
  )
];

SetStrict[DefineAliasRules]

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

DefineOperator1Rules[opSym_Symbol -> fn_Symbol] := SetDelayed[opSym[arg1_][arg2_], fn[arg1, arg2]];
DefineOperator2Rules[opSym_Symbol -> fn_Symbol] := SetDelayed[opSym[arg2_][arg1_], fn[arg1, arg2]];

(**************************************************************************************************)

SetStrict @ DefinePseudoMacro;

DefinePseudoMacro[sym_Sym, rule:RuleD[lhs_, rhs_]] := Then[
  TagSetDelayed @@ iDefinePseudoMacro[sym, lhs, rhs],
  DefinePartialMacro[sym, rule]
];

SetHoldC @ iDefinePseudoMacro;

iDefinePseudoMacro[sym_Sym, lhs_, rhs_] :=
  HoldC[
    sym,
    setdDummy[$LHS_, lhs],
    withDummy[
      {lhsHead = First @ PatHead @ $LHS},
      SetDelayed @@ Hold[$LHS, rhs]
    ]
  ] /. $pseudoMacroRules;

$pseudoMacroRules = {
  HoldP[$MacroParentSymbol]   :> lhsHead,
  HoldP[$MacroSourceLocation] :> RuleEval[SourceLocation[]],
  withDummy                   -> With,
  setdDummy                   -> SetDelayed
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



