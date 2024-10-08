SystemExports[
  "ScopingFunction",
    Locals, MLocals, BLocals, ILocals,
    GlobalVar, LocalVar, ModuleVar, BlockVar, InheritVar
];

PackageExports[
  "Function",         FindMutatedSymbols,
  "SpecialVariable",  $MutatingSymbols
];

(*************************************************************************************************)

SetHoldC[Locals, BLocals, MLocals, ILocals]

ComplexMacroDefs[
   Locals[args___] := mLocals[Block,  args],
  MLocals[args___] := mLocals[Module, args],
  BLocals[args___] := mLocals[Block,  args],
  ILocals[args___] := mLocals[IBlock, args]
];

(*************************************************************************************************)

SetHoldC[mLocals]

mLocals[_]             := HoldM @ Unimplemented;
mLocals[head_, held__] := mLocals[head, Then @ held];
mLocals[head_, held_]  := Block[
  {$ReturnTarget = head, expanded},
  expanded = ExpandMacros @ HoldC @ held;
  If[VFreeQ[expanded, $varH],
    toLocalResult1[head, FindMutatedSymbols @ expanded, fixThen @ expanded],
    toLocalResult2[head, expanded]
  ]
];

DeclaredHere[GlobalVar, LocalVar, BlockVar, InheritVar, ModuleVar];

SetHoldA[GlobalVar, LocalVar, BlockVar, InheritVar, ModuleVar]

$varH = {GlobalVar, LocalVar, BlockVar, InheritVar, ModuleVar}

fixThen[HoldC[]]         := HoldC[Null];
fixThen[HoldC[Then[]]]   := HoldC[Null];
fixThen[HoldC[Then[a_]]] := HoldC[a];
fixThen[HoldC[a_]]       := HoldC[a];
fixThen[HoldC[a__]]      := HoldC[Then[a]];

(*************************************************************************************************)

toLocalResult1[head_, SymbolList[vars___], HoldC[expanded_]] :=
  MacroHold @ head[{vars}, expanded];

(*************************************************************************************************)

toLocalResult2[head_, expanded_] := Module[{rules, groups, mutated, ignored, result},
  rules = Cases[
    expanded /. _Locals -> Null,
    (varH:$varHeadP)[vars__Symbol | {vars__Symbol}] :> Rule[$toScopeHead @ varH, SymbolList[vars]],
    {0, Infinity}
  ];
  groups = Merge[rules, CatenateSymbolLists];
  ignored = CatenateSymbolLists @ Values @ groups;
  mutated = FindMutatedSymbols[expanded, ignored];
  result = DelCases[expanded /. $varHeadSubPat, $varP, {0, Infinity}];
  Unset[groups[None]];
  headSymbols = Lookup[groups, head, SymbolList[]];
  AppendTo[groups, head -> JoinSymbolLists[mutated, headSymbols]];
  attachBlock[fixThen @ result, Seq @@ Normal @ groups]
];

attachBlock[HoldC[body_], head_ -> SymbolList[vars___], rest___] := attachBlock[HoldC[head[{vars}, body]], rest];
attachBlock[HoldC[body_]] := MacroHold[body];

$varHeadP = Alt @@ $varH;
$varP = Alt @@ Map[Blank, $varH];
$varHeadSubPat = With[{vh = $varHeadP}, HoldP[Set[vh[a_], b_]] :> Set[a, b]];

$toScopeHead = UDict[
  GlobalVar  -> None,
  LocalVar   :> $ReturnTarget,
  BlockVar   -> Block,
  ModuleVar  -> Module,
  InheritVar -> InheritedBlock
];

(*************************************************************************************************)

$MutatingSymbols = {
  Set, SetDelayed, AppendTo, PrependTo, AssociateTo, ApplyTo, AddTo, SubtractFrom,
  JoinTo, UnionTo, ReplaceAllIn, ReplaceRepeatedIn,
  KeyApplyTo, KeyIncrement, KeyDecrement, KeyAddTo, KeySubFrom, KeyTimesBy, KeyDivideBy, KeyUnionTo, KeyJoinTo, KeyAppendTo, KeyPrependTo, KeyBindTo, KeyUBindTo,
  TimesBy, DivideBy, KeyDropFrom, Increment, Decrement, PreIncrement, PreDecrement
};

(*************************************************************************************************)

SetStrict @ FindMutatedSymbols;

FindMutatedSymbols[expr_, ignoredVars_:SymbolList[], mutatingSyms_:Auto] := Block[
  {$mutatedVars = SymbolList[], $ignoredVars = ignoredVars, $mutDispatch = toMutDispatch @ mutatingSyms},
  collectMutatedVars @ expr;
  $mutatedVars
];

(*************************************************************************************************)

SetHoldC[collectMutatedVars, visitScoping, visitMutating]

collectMutatedVars[e_] := Then[ReplaceAll[HoldC @ e, $mutDispatch], True];

visitScoping[_[vars_, body___]] := Block[
  {$ignoredVars = JoinSymbolLists[$ignoredVars, procVarList @ vars]},
  collectMutatedVars @ body
];

visitMutating[_[lhs_, args___]] := Then[
  $mutatedVars = JoinSymbolLists[$mutatedVars, Compl[procVarList @ lhs, $ignoredVars]],
  collectMutatedVars @ args
];

(*************************************************************************************************)

SetHoldC[procVarList];

procVarList[var_Sym]            := SymbolList[var];
procVarList[{vars___Sym}]       := SymbolList[vars];
procVarList[Set[lhs_, rhs_Set]] := Make[SymbolList, procVarList @ lhs, procVarList @ rhs];
procVarList[Set[lhs_, _]]       := Make[SymbolList, procVarList @ lhs];
procVarList[SetD[lhs_, _]]      := Make[SymbolList, procVarList @ lhs];
procVarList[list_List]          := Apply[SymbolList, Map[procVarList, NoEval @ list]];
procVarList[_]                  := SymbolList[];

(*************************************************************************************************)

$shieldingHeads = {Locals, ILocals, MLocals, BLocals, GeneralUtilities`Scope, GeneralUtilities`ModuleScope};
$scopingHeads   = {Function, Block, With, SubWith, Module};

makeMutDispatch[mutatingHeads_] := Dispatch @ Join[
  Map[head |-> Rule[_head,  Null],                $shieldingHeads],
  Map[head |-> Rule[_head ? visitScoping,  Null], $scopingHeads],
  Map[head |-> Rule[_head ? visitMutating, Null], mutatingHeads]
];

toMutDispatch[All]    := toMutDispatch[All]   = makeMutDispatch[$MutatingSymbols];
toMutDispatch[Auto]   := toMutDispatch[Auto]  = makeMutDispatch[{Set, SetDelayed}];
toMutDispatch[heads_] := toMutDispatch[heads] = makeMutDispatch[heads];

