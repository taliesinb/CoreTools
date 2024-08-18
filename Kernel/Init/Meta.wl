SystemExports[
  "Function",
    HoldSymbolName, Ensure, AssertThat,
  "ControlFlowFunction",
    HoldMap, EvaluateMap, HoldScan,
  "MutatingFunction",
    BlockSet, BlockAssociate, BlockJoin, BlockAppend,
    BlockIncrement, BlockDecrement,
    BlockTrue, BlockFalse,
    BlockContext,
    BlockUnprotect
];

PackageExports[
  "MetaFunction",
    DeclaredHere,
    DeclareDeclare,

    DeclareCurry1, DeclareCurry2, DeclareCurry12, DeclareCurry23, DeclareCurry13,
    DeclarePredicate1, DeclarePredicate2, DeclarePredicate3,
    DeclareNotPredicate1, DeclareNotPredicate2, DeclareNotPredicate3,
    DeclareFlat, DeclareListableOperator, DeclareListable1,
    DefineOperator1Rules, DefineOperator2Rules,
    StringListableFunctionDefs,
    DeclareHoldFirst, DeclareHoldRest, DeclareHoldAll, DeclareHoldAllComplete, DeclareSequenceHold, DeclareListable,

    DeclareStrict, DeclareSeqScan, DeclareThenScan,
    DeclarationFunctionDefinitions,

    SetCurry1, SetCurry2, SetCurry12, SetCurry23, SetCurry13,
    SetPred1, SetPred2, SetPred3, SetNPred1, SetNPred2, SetNPred3,
    SetHoldF, SetHoldR, SetHoldA, SetHoldC,
    SetStrict, SetFlat, SetListable, SetListable1,
  "Function",
    Unmake, Remake,
  "ControlFlowFunction",
    HoldMake, HoldApply, HoldUnmake, HoldCompUnmake,
  "IOFunction",
    ToImplementationSymbol,
  "Head",
    SymbolList
];

(*************************************************************************************************)

SetAttributes[SymbolList, {HoldAll, Flat}]

(*************************************************************************************************)

DefineAliasRules[
  SetCurry1    -> DeclareCurry1,
  SetCurry2    -> DeclareCurry1,
  SetCurry12   -> DeclareCurry1,
  SetCurry23   -> DeclareCurry1,
  SetCurry13   -> DeclareCurry1,
  SetPred1     -> DeclarePredicate1,
  SetPred2     -> DeclarePredicate2,
  SetPred3     -> DeclarePredicate3,
  SetNPred1    -> DeclareNotPredicate1,
  SetNPred2    -> DeclareNotPredicate2,
  SetNPred3    -> DeclareNotPredicate3,
  SetHoldF     -> DeclareHoldFirst,
  SetHoldR     -> DeclareHoldRest,
  SetHoldA     -> DeclareHoldAll,
  SetFlat      -> DeclareFlat,
  SetHoldC     -> DeclareHoldAllComplete,
  SetStrict    -> DeclareStrict,
  SetListable  -> DeclareListable,
  SetListable1 -> DeclareListable1
];

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

declareDeclarationDefinitions[DeclarationFunctionDefinitions];

DeclarationFunctionDefinitions[HoldP[SetDelayed][(fn_Symbol)[VPattern[var_Symbol, VBlank[Symbol]]], rhs_]] := With[
  {lhs = Make[Pattern, var, Blank @ Symbol]},
  If[!declareSymbolQ[fn], setupDeclareSym[fn]];
  fn[lhs] := Then[rhs, SymbolList[var]];
];

DeclarationFunctionDefinitions[HoldP[SetDelayed][(fn_Symbol)[VPattern[var_Symbol, VBlankSeq[Symbol]]], rhs_]] := With[
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

DeclarationFunctionDefinitions[
  DeclareListableOperator[sym_Sym] := Set[ListableFunctionQ[_sym], True],
  DeclareListable1[sym_Sym]        := SetDelayed[sym[arg1_List, arg2_], Map[Function[a1, sym[a1, arg2]], arg1]]
];

(*************************************************************************************************)

(* TODO: auto making of held operator forms via appropriate AttributeFn *)
DeclarationFunctionDefinitions[
   DeclareCurry1[sym_Sym] := SetDelayed[sym[arg1_][rest___],      sym[arg1, rest]],
   DeclareCurry2[sym_Sym] := SetDelayed[sym[arg2_][arg1_],        sym[arg1, arg2]],
  DeclareCurry12[sym_Sym] := SetDelayed[sym[arg1_, arg2_][arg3_], sym[arg1, arg2, arg3]],
  DeclareCurry23[sym_Sym] := SetDelayed[sym[arg2_, arg3_][arg1_], sym[arg1, arg2, arg3]],
  DeclareCurry13[sym_Sym] := SetDelayed[sym[arg1_, arg3_][arg2_], sym[arg1, arg2, arg3]]
];

(*************************************************************************************************)

DeclarationFunctionDefinitions[
  DeclarePredicate1[sym_Sym]    := Set[sym[_],       False],
  DeclarePredicate2[sym_Sym]    := Set[sym[_, _],    False],
  DeclarePredicate3[sym_Sym]    := Set[sym[_, _, _], False],
  DeclareNotPredicate1[sym_Sym] := Set[sym[_],       True],
  DeclareNotPredicate2[sym_Sym] := Set[sym[_, _],    True],
  DeclareNotPredicate3[sym_Sym] := Set[sym[_, _, _], True]
];

(*************************************************************************************************)

DeclarationFunctionDefinitions[
  DeclareHoldFirst[syms__Sym]       := SetAttributes[{syms}, HoldFirst],
  DeclareHoldFirst[syms__Sym]       := SetAttributes[{syms}, HoldFirst],
  DeclareHoldRest[syms__Sym]        := SetAttributes[{syms}, HoldRest],
  DeclareHoldAll[syms__Sym]         := SetAttributes[{syms}, HoldAll],
  DeclareFlat[syms__Sym]            := SetAttributes[{syms}, Flat],
  DeclareHoldAllComplete[syms__Sym] := SetAttributes[{syms}, HoldAllComplete],
  DeclareListable[syms__Sym]        := SetAttributes[{syms}, Listable],
  DeclareSequenceHold[syms__Sym]    := SetAttributes[{syms}, SequenceHold]
];

(*************************************************************************************************)

DeclareStrict[BlockSet, BlockAssociate, BlockJoin, BlockAppend, BlockIncrement, BlockDecrement, BlockTrue, BlockFalse, BlockContext, BlockUnprotect];
DeclareHoldAll[BlockSet, BlockAssociate, BlockJoin, BlockAppend, BlockIncrement, BlockDecrement, BlockTrue, BlockFalse, BlockContext, BlockUnprotect]

BlockSet[var_Sym, val_, body_]         := Block[{var = val}, body];
BlockSet[{v1_, v2_}, val_, body_]      := Block[{v1 = val, v2 = val}, body];
BlockSet[{v1_, v2_, v3_}, val_, body_] := Block[{v1 = val, v2 = val, v3 = val}, body];

BlockAssociate[var_, rules_, body_] := InheritedBlock[{var}, AssociateTo[var, rules]; body];
BlockJoin[var_, item_, body_]       := Block[{var = Join[var, item]}, body];
BlockAppend[var_, item_, body_]     := Block[{var = Append[var, item]}, body];

BlockIncrement[var_, body_]       := Block[{var = var + 1}, body];
BlockDecrement[var_, body_]       := Block[{var = var - 1}, body];

BlockTrue[var_, body_]             := Block[{var = True}, body];
BlockTrue[{v1_, v2_}, body_]       := Block[{v1 = True, v2 = True}, body];
BlockTrue[{v1_, v2_, v3_}, body_]  := Block[{v1 = True, v2 = True, v3 = True}, body];

BlockFalse[var_, body_]            := Block[{var = False}, body];
BlockFalse[{v1_, v2_}, body_]      := Block[{v1 = False, v2 = False}, body];
BlockFalse[{v1_, v2_, v3_}, body_] := Block[{v1 = False, v2 = False, v3 = False}, body];

BlockContext[context_, body_]               := Block[{$Context = context, $ContextPath = {"System`"}}, body];
BlockContext[context_, contextPath_, body_] := Block[{$Context = context, $ContextPath = contextPath}, body];

BlockUnprotect[var_Sym, body_]     := WithLocalSettings[Unprotect[var], body, Protect[var]];
BlockUnprotect[{vars__Sym}, body_] := WithLocalSettings[Unprotect[vars], body, Protect[vars]];

(*************************************************************************************************)

DeclareStrict::usage = "DeclareStrict[sym$] declares that sym$[___] should throw an error if it doesn't match.";

DeclareHoldAllComplete[DeclareStrict, DeclareSeqScan, DeclareThenScan]

General::badArguments = "Bad arguments: ``.";
General::badSeqScanArg = "Bad argument to ``: ``.";

DeclarationFunctionDefinitions[
  DeclareStrict[head_Sym] := Apply[
    SetDelayed,
    Hold[$LHS_head, Message[MessageName[head, "badArguments"], HoldForm @ $LHS]; $Failed]
  ],

  DeclareSeqScan[head_Sym] := (
    DeclareStrict[head];
    SetDelayed[e_head, Message[General::badSeqScanArg, head, HoldForm @ e]; $Failed];
    SetDelayed[head[seq:BlankSeq2], Scan[head, Hold[seq]]];
  ),

  DeclareThenScan[head_Sym] := (
    DeclareHoldAllComplete[head];
    SetDelayed[head[$LHS_], Message[MessageName[head, "badArguments"], HoldForm @ $LHS]; $Failed];
    SetDelayed[head[Null], Null];
    SetDelayed[head[seq:BlankSeq2], Scan[head, Hold[seq]]];
    SetDelayed[head[Then[args___]], head[args]];
  )
];

DeclareStrict[DefineAliasRules]

(*************************************************************************************************)

HoldSymbolName::usage = "HoldSymbolName[sym$] gives the name of sym$ without evaluating sym$.";

DeclareHoldAllComplete[HoldSymbolName]
HoldSymbolName[sym_Symbol ? Developer`HoldAtomQ] := SymbolName @ Unevaluated @ sym;
HoldSymbolName[_] := $Failed;

(*************************************************************************************************)

DeclareHoldAll[HoldMake, HoldApply, HoldUnmake];
DeclareHoldAllComplete[HoldCompUnmake];

HoldMake[f_, a___]     := f[a];
HoldApply[f_, _[a___]] := f[a];

Unmake[h_[a___], w_:List]              := w[h, a];
HoldUnmake[h_[a___], w_:Hold]          := w[h, a];
HoldCompUnmake[h_[a___], w_:HoldComplete] := w[h, a];

Remake[w_[h_, a___]] := w[h[a]];

(**************************************************************************************************)

HoldMap::usage = "HoldMap[fn$, args$] maps fn$ over args$ without evaluating args$.";
HoldScan::usage = "HoldScan[fn$, args$] maps fn$ over args$ without evaluating args$.";

DeclareStrict @ DeclareHoldAllComplete[HoldMap, HoldScan]

HoldMap[f_, args_]                     := Map[f, Unevaluated[args]];
HoldMap[Function[body_], args_]        := Map[Function[Null, body, HoldAllComplete], Unevaluated[args]];
HoldMap[Function[args_, body_], args_] := Map[Function[args, body, HoldAllComplete], Unevaluated[args]];

HoldScan[f_, args_]                     := Scan[f, Unevaluated[args]];
HoldScan[Function[body_], args_]        := Scan[Function[Null, body, HoldAllComplete], Unevaluated[args]];
HoldScan[Function[args_, body_], args_] := Scan[Function[args, body, HoldAllComplete], Unevaluated[args]];

(* unlike Map, EvaluateMap[f, Hold[1,2,3]] actually does something *)
EvaluateMap[f_, args:ListDictP] := Map[f, args];
EvaluateMap[f_, h_[args___]]    := Apply[h, HoldMap[f, {args}]];

(*************************************************************************************************)

General::notStringOrStrings = "First argument `` should be a string, or nested container of such.";

strImplNeedsSetupQ[_] := True;

declareDeclarationDefinitions[StringListableFunctionDefs];

(* TODO: use this technique more widely *)
StringListableFunctionDefs[sd:SetD[$LHS_, $RHS_]] := With[
  {head = First @ PatternHeadSymbol @ $LHS},
  {impl = ToImplementationSymbol @ head},
  ReleaseHold @ ReplaceAll[Hold[sd] /. {mn_MessageName :> mn, head -> impl}];
  If[strImplNeedsSetupQ[head],
    strImplNeedsSetupQ[head] = False;
    head[expr_ ? StrOrStrVecQ, args___] := impl[expr, args];
    head[expr:ListDictP, args___]                := Map[elem |-> head[elem, args], expr];
    head[expr_, ___]                             := ErrorMsg[head::notStringOrStrings, expr];
    expr_impl                                    := badImplArgs[head, expr];
  ];
];

DeclareHoldRest[badImplArgs];
badImplArgs[sym_, _[args___]] := ErrorMsg[sym::badArguments, HoldForm[sym[args]]];

(*************************************************************************************************)

DeclareSeqScan[DefineOperator1Rules, DefineOperator2Rules]

DefineOperator1Rules[opSym_Symbol -> fn_Symbol] := SetDelayed[opSym[arg1_][arg2_], fn[arg1, arg2]];
DefineOperator2Rules[opSym_Symbol -> fn_Symbol] := SetDelayed[opSym[arg2_][arg1_], fn[arg1, arg2]];

(**************************************************************************************************)

DeclareHoldRest[Ensure]

Ensure[expr_, testFn_, else_] := If[TrueQ @ testFn @ expr, expr, else];
Ensure[testFn_, else_][expr_] := If[TrueQ @ testFn @ expr, expr, else];

