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

DefineComplexMacro[Locals,   Locals[args___] :> mLocals[Block,  args]]
DefineComplexMacro[MLocals, MLocals[args___] :> mLocals[Module, args]]
DefineComplexMacro[BLocals, BLocals[args___] :> mLocals[Block,  args]]
DefineComplexMacro[ILocals, ILocals[args___] :> mLocals[IBlock, args]]

(*************************************************************************************************)

SetHoldC[mLocals]

mLocals[_]             := Null
mLocals[head_, held__] := mLocals[head, Then @ held];
mLocals[head_, held_]  := Block[
  {$FunctionReturnTarget = head, expanded},
  expanded = ExpandMacros @ HoldComplete @ held;
  If[VFreeQ[expanded, $varH],
    toLocalResult1[head, FindMutatedSymbols @ expanded, fixThen @ expanded],
    toLocalResult2[head, expanded]
  ]
];

DeclaredHere[GlobalVar, LocalVar, BlockVar, InheritVar, ModuleVar];

SetHoldA[GlobalVar, LocalVar, BlockVar, InheritVar, ModuleVar]

$varH = {GlobalVar, LocalVar, BlockVar, InheritVar, ModuleVar}

fixThen[HoldComp[]]         := HoldComp[Null];
fixThen[HoldComp[Then[]]]   := HoldComp[Null];
fixThen[HoldComp[Then[a_]]] := HoldComp[a];
fixThen[HoldComp[a_]]       := HoldComp[a];
fixThen[HoldComp[a__]]      := HoldComp[Then[a]];

(*************************************************************************************************)

toLocalResult1[head_, SymbolList[vars___], HoldComplete[expanded_]] :=
  MacroHold @ head[{vars}, expanded];

(*************************************************************************************************)

toLocalResult2[head_, expanded_] := Module[{rules, groups, mutated, ignored, result},
  rules = Cases[
    expanded /. _Locals -> Null,
    (varH:$varHeadP)[vars___Symbol] :> Rule[$toScopeHead @ varH, SymbolList[vars]],
    {0, Infinity}
  ];
  groups = Merge[rules, CatenateSymbolLists];
  ignored = CatenateSymbolLists @ Values @ groups;
  mutated = FindMutatedSymbols[expanded, ignored];
  result = DelCases[expanded, $varP, {0, Infinity}];
  Unset[groups[None]];
  headSymbols = Lookup[groups, head, SymbolList[]];
  AppendTo[groups, head -> JoinSymbolLists[mutated, headSymbols]];
  attachBlock[fixThen @ result, Seq @@ Normal @ groups]
];

attachBlock[HoldComp[body_], head_ -> SymbolList[vars___], rest___] := attachBlock[HoldComp[head[{vars}, body]], rest];
attachBlock[HoldComp[body_]] := MacroHold[body];

$varHeadP = Alt @@ $varH;
$varP = Alt @@ Map[Blank, $varH];

$toScopeHead = UDict[
  GlobalVar  -> None,
  LocalVar   :> $FunctionReturnTarget,
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

collectMutatedVars[e_] := Then[ReplaceAll[HoldComp @ e, $mutDispatch], True];

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

