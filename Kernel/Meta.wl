SystemExports[
  "Function",
    HoldSymbolName, MapHold, Ensure,
  "MetaFunction",
    DeclareDeclare,
    DeclareCurry1, DeclareCurry2, DeclareCurry12, DeclareCurry23,
    DeclarePredicate1, DeclarePredicate2, DeclarePredicate3,
    DeclareListableOperator, DeclareListable1,
    DeclareHoldFirst, DeclareHoldRest, DeclareHoldAll, DeclareHoldAllComplete, DeclareSequenceHold, DeclareListable,
    DeclareStrict, DeclareSeqScan, DeclareThenScan,
    DefineOperator1Rules, DefineOperator2Rules,
    DeclarationFunctionDefinitions,
  "ScopingFunction",
    BlockIncrement, BlockDecrement, BlockAppendTo, BlockTrue, BlockFalse, BlockContext,
  "Head",
    SymbolList
];

(*************************************************************************************************)

SetAttributes[SymbolList, {HoldAll, Flat}]

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

SetAttributes[{DeclarationFunctionDefinitions, declareFnScan}, HoldAllComplete];

DeclarationFunctionDefinitions::expectedSetDelayed = "Expected SetDelayed instead of ``.";
DeclarationFunctionDefinitions[defs__] := Scan[DeclarationFunctionDefinitions, Hold[defs]];
DeclarationFunctionDefinitions[expr_] := (Message[DeclarationFunctionDefinitions::expectedSetDelayed, HoldForm @ expr]; $Failed)

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

DeclarationFunctionDefinitions[
   DeclareCurry1[sym_Sym] := SetDelayed[sym[arg1_][rest___],      sym[arg1, rest]],
   DeclareCurry2[sym_Sym] := SetDelayed[sym[arg2_][arg1_],        sym[arg1, arg2]],
  DeclareCurry12[sym_Sym] := SetDelayed[sym[arg1_, arg2_][arg3_], sym[arg1, arg2, arg3]],
  DeclareCurry23[sym_Sym] := SetDelayed[sym[arg2_, arg3_][arg1_], sym[arg1, arg2, arg3]]
];

(*************************************************************************************************)

DeclarationFunctionDefinitions[
  DeclarePredicate1[sym_Sym] := Set[sym[_],       False],
  DeclarePredicate2[sym_Sym] := Set[sym[_, _],    False],
  DeclarePredicate3[sym_Sym] := Set[sym[_, _, _], False]
];

(*************************************************************************************************)

DeclarationFunctionDefinitions[
  DeclareHoldFirst[syms__Sym]       := SetAttributes[{syms}, HoldFirst],
  DeclareHoldFirst[syms__Sym]       := SetAttributes[{syms}, HoldFirst],
  DeclareHoldRest[syms__Sym]        := SetAttributes[{syms}, HoldRest],
  DeclareHoldAll[syms__Sym]         := SetAttributes[{syms}, HoldAll],
  DeclareHoldAllComplete[syms__Sym] := SetAttributes[{syms}, HoldAllComplete],
  DeclareListable[syms__Sym]        := SetAttributes[{syms}, Listable],
  DeclareSequenceHold[syms__Sym]    := SetAttributes[{syms}, SequenceHold]
];

(*************************************************************************************************)

DeclareStrict[BlockIncrement, BlockDecrement, BlockAppendTo, BlockTrue, BlockFalse, BlockContext]
DeclareHoldAll[BlockIncrement, BlockDecrement, BlockAppendTo, BlockTrue, BlockFalse, BlockContext]

BlockIncrement[var_, body_]       := Block[{var = var + 1}, body];
BlockDecrement[var_, body_]       := Block[{var = var - 1}, body];
BlockAppendTo[var_, item_, body_] := Block[{var = Append[var, item]}, body];
BlockTrue[var_, body_]            := Block[{var = True}, body];
BlockFalse[var_, body_]           := Block[{var = False}, body];

BlockContext[context_, body_]               := Block[{$Context = context, $ContextPath = {"System`"}}, body];
BlockContext[context_, contextPath_, body_] := Block[{$Context = context, $ContextPath = contextPath}, body];

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
    SetDelayed[head[seq__], Scan[head, Hold[seq]]];
  ),

  DeclareThenScan[head_Sym] := (
    DeclareHoldAllComplete[head];
    SetDelayed[head[$LHS_], Message[MessageName[head, "badArguments"], HoldForm @ $LHS]; $Failed];
    SetDelayed[head[Null], Null];
    SetDelayed[head[seq__], Scan[head, Hold[seq]]];
    SetDelayed[head[Then[args___]], head[args]];
  )
];

DeclareStrict[DefineAliasRules]

(*************************************************************************************************)

HoldSymbolName::usage = "HoldSymbolName[sym$] gives the name of sym$ without evaluating sym$.";

DeclareHoldAllComplete[HoldSymbolName]
HoldSymbolName[sym_Symbol ? Developer`HoldAtomQ] := SymbolName @ Unevaluated @ sym;
HoldSymbolName[_] := $Failed;

(**************************************************************************************************)

MapHold::usage = "MapHold[fn$, args$] maps fn$ over args$ without evaluating args$.";

DeclareHoldAllComplete[MapHold]
MapHold[f_, args_]                     := Map[f, Unevaluated[args]];
MapHold[Function[body_], args_]        := Map[Function[Null, body, HoldAllComplete], Unevaluated[args]];
MapHold[Function[args_, body_], args_] := Map[Function[args, body, HoldAllComplete], Unevaluated[args]];

(*************************************************************************************************)

DeclareSeqScan[DefineOperator1Rules, DefineOperator2Rules]

DefineOperator1Rules[opSym_Symbol -> fn_Symbol] := SetDelayed[opSym[arg1_][arg2_], fn[arg1, arg2]];
DefineOperator2Rules[opSym_Symbol -> fn_Symbol] := SetDelayed[opSym[arg2_][arg1_], fn[arg1, arg2]];

(**************************************************************************************************)

DeclareHoldRest[Ensure]

Ensure[expr_, testFn_, else_] := If[TrueQ @ testFn @ expr, expr, else];
Ensure[testFn_, else_][expr_] := If[TrueQ @ testFn @ expr, expr, else];

