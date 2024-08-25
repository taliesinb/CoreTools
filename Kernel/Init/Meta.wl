SystemExports[
  "Function",
    Ensure, AssertThat,
    HoldLength, HoldSequenceLength,
  "ControlFlowFunction",
    HoldHead, HoldMap, EvaluateMap, HoldScan,
  "MutatingFunction",
    SetSequence,
    BlockSet, BlockAssociate, BlockJoin, BlockAppend,
    BlockIncrement, BlockDecrement,
    BlockTrue, BlockFalse,
    BlockContext,
    BlockUnprotect,
  "Head",
    Unsequence
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
    SetExcepting,

    DefinePseudoMacro,
  "Function",
    Unmake, Remake,
    CatenateSymbolLists, JoinSymbolLists,
  "ScopingFunction",
    SubWith,
  "ControlFlowFunction",
    HoldMake, HoldApply, HoldUnmake, HoldCompUnmake, Initially,
    ThenNull, ThenPart, ThenFail, ThenFailEval, Then1, Then2, Then3,
    SeqCol1, SeqCol2, SeqCol3,
    AllowRHSPatterns,
  "IOFunction",
    ToImplementationSymbol,
  "Head",
    SymbolList
];

PrivateExports[
  "SpecialVariable",
    $ExceptingSymbols, $ExceptingSymbolP
  "CacheVariable",
    $InitializationHashes
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

DefineAliasRules[
  SetCurry1    -> DeclareCurry1,
  SetCurry2    -> DeclareCurry2,
  SetCurry12   -> DeclareCurry12,
  SetCurry23   -> DeclareCurry23,
  SetCurry13   -> DeclareCurry13,
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

If[!ListQ[$ExceptingSymbols],
  $ExceptingSymbols = {};
  $ExceptingSymbolP = Alt[];
];

DeclarationFunctionDefinitions[
  SetExcepting[syms__Sym] := Then[
    $ExceptingSymbols = Join[$ExceptingSymbols, {syms}],
    $ExceptingSymbolP = Join[$ExceptingSymbolP, Alt[syms]]
  ]
];

(*************************************************************************************************)

SetSequence[lhs1_, lhs2__, rhs_] := Set[lhs1, SetSequence[lhs2, rhs]];
SetSequence[lhs1_, rhs_]         := Set[lhs1, rhs];

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
  DeclareStrict[head_Sym] := With[{sloc = SourceLocation[]}, Apply[
    SetDelayed,
    Hold[$LHS_head, IssueMessage[head -> sloc, "badArguments", HoldForm @ $LHS]]
  ]],

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

SetStrict @ SetHoldC[HoldHead, HoldMap, HoldScan]

HoldMap[f_, args_]                     := Map[f, Unevaluated[args]];
HoldMap[Function[body_], args_]        := Map[Function[Null, body, HoldAllComplete], Unevaluated[args]];
HoldMap[Function[args_, body_], args_] := Map[Function[args, body, HoldAllComplete], Unevaluated[args]];

HoldScan[f_, args_]                     := Scan[f, Unevaluated[args]];
HoldScan[Function[body_], args_]        := Scan[Function[Null, body, HoldAllComplete], Unevaluated[args]];
HoldScan[Function[args_, body_], args_] := Scan[Function[args, body, HoldAllComplete], Unevaluated[args]];

HoldHead[e_]      := Head @ NoEval @ e;
HoldHead[e_, fn_] := Head[NoEval @ e, fn];

(* unlike Map, EvaluateMap[f, Hold[1,2,3]] actually does something *)
EvaluateMap[f_, args:ListDictP] := Map[f, args];
EvaluateMap[f_, h_[args___]]    := Apply[h, HoldMap[f, {args}]];

(*************************************************************************************************)

General::notStringOrStrings = "First argument `` should be a string, or nested container of such.";

strImplNeedsSetupQ[_] := True;

declareDeclarationDefinitions[StringListableFunctionDefs];

(* TODO: use this technique more widely *)
StringListableFunctionDefs[sd:SetD[$LHS_, $RHS_]] := With[
  {head = First @ PatHeadSym @ $LHS},
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

(**************************************************************************************************)

DeclareStrict @ DefinePseudoMacro;

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
      {lhsHead = First @ PatHeadSym @ $LHS},
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

(*************************************************************************************************)

SetHoldC[HoldLength, HoldSequenceLength]

HoldLength[e_]       := Length @ NoEval @ e;
e_HoldSequenceLength := Length @ NoEval @ e;

(*************************************************************************************************)

SetHoldC[ThenNull, ThenPart, ThenFail, ThenFailEval, Then1, Then2, Then3]

DeclareSequenceHold[Unsequence]

(* NOTE: Unevaluated[1, 2] turns into Sequence[1, 2] when substituted, which is good *)

ThenNull[e___] := Then[e, Null];
ThenFail[e___] := Then[e, $Fail];
ThenFailEval[e___] := Then[e, FailEval];
ThenPart[n_Int, e___] := Part[Unsequence[e], n];

Then1[e___] := P1 @ Unsequence[e];
Then2[e___] := P2 @ Unsequence[e];
Then3[e___] := P3 @ Unsequence[e];

(*************************************************************************************************)

DeclareSequenceHold[SeqCol1, SeqCol2, SeqCol3];

SeqCol1[a___] := Part[NoEval @ a, All, 1];
SeqCol2[a___] := Part[NoEval @ a, All, 2];
SeqCol3[a___] := Part[NoEval @ a, All, 3];

_SequenceNothing := Seq[];
SequenceFirst[e_, ___]       := e;
SequenceSecond[_, e_, ___]   := e;
SequenceThird[_, _, e_, ___] := e;
SequenceLast[___, e_]        := e;
SequenceMost[e___, _]        := e;
SequenceRest[_, e___]        := e;
SequenceReverse[e___]        := Seq @@ Reverse[{e}];

SequenceFirstSecond[e1_, e2_, ___] := Seq[e1, e2];
SequenceSecondFirst[e1_, e2_, ___] := Seq[e2, e1];

(*************************************************************************************************)

SequenceLength[]        := 0;
SequenceLength[_]       := 1;
SequenceLength[_, _]    := 2;
SequenceLength[_, _, _] := 3;
s_SequenceLength        := Length[Unevaluated @ s];

(*************************************************************************************************)

SetHoldC @ AllowRHSPatterns;

AllowRHSPatterns[body_] := Quiet[body, RuleDelayed::rhs];

