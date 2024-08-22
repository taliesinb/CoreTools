SystemExports[
  "Function",
    P1, P2, P3, P4, P5, PN, P11, P1N, PNN, PN1, Col1, Col2, Col12, Col21, Col3, Col4, ColN,
  "ControlFlowFunction",
    SubAll, SubNone, SubAuto, SubInherited, SubMissing, SubFailed,
  "MutatingFunction",
    SetAll, SetNone, SetAuto, SetFailed, SetMissing, SetInherited, SetScaledFactor,
    SetCached, SetInitial, SetDelayedInitial, SetSequence,
    PackAssociation, UnpackAssociation, UnpackTuple,
  "ScopingFunction",
    IsMatchOf, IsNotMatchOf,
    CaseOf, ExtendCaseOf, CaseFn, Locals, GlobalVar, LocalVar, InheritVar,
    CollectBegin, CollectEnd, Collecting,
  "MessageFunction",
    ThrowUnmatchedError
];

PackageExports[
  "SpecialVariable",  $MutatingSymbols,
  "MessageFunction",  ReturnFailed, ReturnMessage,
  "MutatingFunction", UnpackOptions, UnpackOptionsAs, UnpackAnnotations,
  "PatternSymbol",    StrMatchP, StrStartsP, StrContainsP, DeepStrContainsP
];

(**************************************************************************************************)

DefineSimpleMacro[P1,   P1[e_] :> Part[e, 1]];
DefineSimpleMacro[P2,   P2[e_] :> Part[e, 2]];
DefineSimpleMacro[P3,   P3[e_] :> Part[e, 3]];
DefineSimpleMacro[P4,   P4[e_] :> Part[e, 4]];
DefineSimpleMacro[P5,   P4[e_] :> Part[e, 5]];
DefineSimpleMacro[PN,   PN[e_] :> Part[e, -1]];
DefineSimpleMacro[P11,  P11[e_] :> Part[e, 1, 1]];
DefineSimpleMacro[P1N,  P1N[e_] :> Part[e, 1, -1]];
DefineSimpleMacro[PNN,  PNN[e_] :> Part[e, -1, -1]];
DefineSimpleMacro[PN1,  PN1[e_] :> Part[e, -1, 1]];
DefineSimpleMacro[Col1, Col1[e_] :> Part[e, All, 1]];
DefineSimpleMacro[Col2, Col2[e_] :> Part[e, All, 2]];
DefineSimpleMacro[Col3, Col3[e_] :> Part[e, All, 3]];
DefineSimpleMacro[Col4, Col4[e_] :> Part[e, All, 4]];
DefineSimpleMacro[ColN, ColN[e_] :> Part[e, All, -1]];

Col12[e_] := {Col1 @ e, Col2 @ e};
Col21[e_] := {Col1 @ e, Col2 @ e};

(**************************************************************************************************)

DefinePatternMacro[StrMatchP,
  StrMatchP[patt_] :> (_String ? (StringMatchQ[patt]))
]

DefinePatternMacro[StrStartsP,
  StrStartsP[patt_] :> (_String ? (StringStartsQ[patt]))
]

DefinePatternMacro[StrContainsP,
  StrContainsP[patt_] :> (_String ? (StringContainsQ[patt]))
]

DefinePatternMacro[DeepStrContainsP,
  DeepStrContainsP[patt_] :> (_ ? (ContainsQ[_String ? (StringContainsQ[patt])]))
]

(*************************************************************************************************)

DeclareHoldAll[SetAll, SetNone, SetAuto, SetFailed, SetMissing, SetInherited, SetScaledFactor]

DefineSimpleMacro[SetAll,                   SetAll[lhs_, rhs_] :> If[lhs === All,       lhs = rhs, lhs]];
DefineSimpleMacro[SetNone,                 SetNone[lhs_, rhs_] :> If[lhs === None,      lhs = rhs, lhs]];
DefineSimpleMacro[SetAuto,                 SetAuto[lhs_, rhs_] :> If[lhs === Automatic, lhs = rhs, lhs]];
DefineSimpleMacro[SetFailed,             SetFailed[lhs_, rhs_] :> If[FailureQ[lhs],     lhs = rhs, lhs]];
DefineSimpleMacro[SetMissing,           SetMissing[lhs_, rhs_] :> If[MissingQ[lhs],     lhs = rhs, lhs]];
DefineSimpleMacro[SetInherited,       SetInherited[lhs_, rhs_] :> If[lhs === Inherited, lhs = rhs, lhs]];
DefineSimpleMacro[SetScaledFactor, SetScaledFactor[lhs_, rhs_] :> If[MatchQ[lhs, Scaled[_ ? NumericQ]], lhs //= First /* N; lhs *= rhs]];

(*************************************************************************************************)

(* TODO: rename these IfAll, IfNone, etc *)
DeclareHoldAll[SubAll, SubNone, SubAuto, SubInherited, SubMissing, SubFailed]

DefineSimpleMacro[SubAll,          {SubAll      [rhs_] :> Replace[All       :> rhs], SubAll      [lhs_, rhs_] :> Replace[lhs, All        :> rhs]}];
DefineSimpleMacro[SubNone,         {SubNone     [rhs_] :> Replace[None      :> rhs], SubNone     [lhs_, rhs_] :> Replace[lhs, None       :> rhs]}];
DefineSimpleMacro[SubAuto,         {SubAuto     [rhs_] :> Replace[Auto      :> rhs], SubAuto     [lhs_, rhs_] :> Replace[lhs, Auto       :> rhs]}];
DefineSimpleMacro[SubInherited,    {SubInherited[rhs_] :> Replace[Inherited :> rhs], SubInherited[lhs_, rhs_] :> Replace[lhs, Inherited  :> rhs]}];
DefineSimpleMacro[SubMissing,      {SubMissing  [rhs_] :> Replace[_Missing  :> rhs], SubMissing  [lhs_, rhs_] :> Replace[lhs, _Missing   :> rhs]}];
DefineSimpleMacro[SubFailed,       {SubFailed   [rhs_] :> Replace[$Failed   :> rhs], SubFailed   [lhs_, rhs_] :> Replace[lhs, $Failed    :> rhs]}];

(*************************************************************************************************)

DeclareHoldAll[SetCached, SetInitial, SetDelayedInitial, SetSequence]

DefineSimpleMacro[SetCached,                  SetCached[lhs_, rhs_] :> SetDelayed[lhs, Set[lhs, rhs]]]
DefineSimpleMacro[SetDelayedInitial,  SetDelayedInitial[lhs_, rhs_] :> If[System`Private`HasNoEvaluationsQ[lhs], SetDelayed[lhs, rhs]]]
DefineSimpleMacro[SetInitial, {
  SetInitial[lhs_Sym, rhs_]     :> If[System`Private`HasNoEvaluationsQ[lhs], Set[lhs, rhs]],
  SetInitial[lhs__Sym, rhs_]    :> setMultiInitial[Hold[lhs], rhs]
}];

DeclareHoldAll[setMultiInitial];

setMultiInitial[lhs_Hold, rhs2_] := Module[{rhs := (rhs = rhs2)},
  Scan[
    Function[sym, If[System`Private`HasNoEvaluationsQ[sym], Set[sym, rhs2]], HoldFirst],
    lhs
  ]
];

SetSequence[lhs1_, lhs2__, rhs_] := Set[lhs1, SetSequence[lhs2, rhs]];
SetSequence[lhs1_, rhs_]         := Set[lhs1, rhs];

(*************************************************************************************************)

DeclareHoldAll[CollectBegin, CollectEnd, Collecting]

DefineSimpleMacro[CollectBegin,   {
  CollectBegin[var_Symbol]   :> Set[var, NewCollector[]],
  CollectBegin[vars__Symbol] :> Set[{vars}, NewCollector @ HoldLen @ {vars}]
}];
DefineSimpleMacro[CollectEnd, {
  CollectEnd[var_Symbol]    :> ThenNull[Set[var, FromCollector @ var]],
  CollectEnd[vars__Symbol]  :> ThenNull[Set[{vars}, FromCollector @ {vars}]]
}];

DefineSimpleMacro[Collecting,     {
  Collecting[s_Symbol, body_]    :> Then2[CollectBegin[s], body, CollectEnd[s]],
  Collecting[{s__Symbol}, body_] :> Then2[CollectBegin[s], body, CollectEnd[s]]
}];

(*************************************************************************************************)

SetAttributes[ReturnMessage, HoldFirst];

DefinePartialMacro[ReturnMessage,
  ReturnMessage[args___] :> FunctionReturn[ErrorMessage[args]]
];

ReturnMessage /: SetDelayed[$LHS_, ReturnMessage[args___]] :=
  SetDelayed @@ Hold[$LHS, ErrorMessage[args]];

(*************************************************************************************************)

DefineSimpleMacro[ReturnFailed, {
  ReturnFailed[]                    :> Return[$Failed, Block],
  ReturnFailed[msg_String, args___] :> (issueMessage[$MacroParentSymbol, msg, args]; Return[$Failed, Block])
}]

issueMessage[$MacroParentSymbol, msgName_String, args___] := issueMessage[General, msgName, args];
issueMessage[msgSym_, msgName_String, args___] := Message[MessageName[msgSym, msgName], args];

(*************************************************************************************************)

Locals::usage = "Locals foo.";

SetHoldC[Locals, mLocals]

DefineComplexMacro[Locals, Locals[args___] :> mLocals[args]]

mLocals[]       := Null
mLocals[held__] := mLocals @ CompoundExpression[held];
mLocals[held_]  := Module[{expanded, locals, globals, res},
  Block[{$FunctionReturnTarget = Block}, expanded = ExpandMacros @ HoldComplete @ held];
  locals = MacroHold @@@ findLocalVariables @ expanded;
  If[VContainsQ[expanded, {GlobalVar, LocalVar, InheritVar}],
    globals = findOtherVariables[GlobalVar, expanded];
    inherit = findOtherVariables[InheritVar, expanded];
    expanded = DeleteCases[expanded, _GlobalVar | _LocalVar | _InheritVar, {0, Infinity}];
    If[inherit =!= {}, expanded = toInheritBlock[inherit, expanded]];
    locals = Complement[locals, globals, inherit];
  ];
  res = toLocalBlock[locals, expanded];
  res
];

toInheritBlock[vars_, HoldComplete[arg_]]  := HoldComplete @ InheritedBlock[vars, arg];
toLocalBlock[vars_, HoldComplete[arg_]]    := MacroHold @ Block[vars, arg];
toLocalBlock[vars_, HoldComplete[args___]] := MacroHold @ Block[vars, CompoundExpression[args]];

findOtherVariables[head_, expr_] := MacroHold @@@ Flatten @ Cases[
  expr /. _Locals -> Null,
  head[s___Symbol] :> lhsSymbols[{s}],
  {0, Infinity}
];

(*************************************************************************************************)

(* Internal`GetUnboundSymbols? *)

findLocalVariables[expr_] := Block[{$lvBag = Internal`Bag[]},
  collectLVs @ expr;
  DeleteDuplicates @ Flatten @ Internal`BagPart[$lvBag, All]
];

SetHoldC[collectLVs]

collectLVs[e_] := (ReplaceAll[e, $lvRules]; Null);

(* TODO: SetDelayed seems not to work at the moment?
should I have AddTo, Increment, etc automatically instantiate a zero-ed local?
and similarly for AssociateTo and the like making associations, and JoinTo / UnionTo etc making lists?
TODO: local defintions as comma-separated things within the Locals?
*)

(* for now, i'm disabling these, because you should really set a value initially
before the other things would make sense. the only disadvantage is you can't
as easily change a bound symbol, but that's complex code to implement in macros
and its maybe clearer if you do it yourself anyway *)

$lvMutatingHeads = {
  Set, SetDelayed, LocalVar
};

$MutatingSymbols = {
  Set, SetDelayed, AppendTo, PrependTo, AssociateTo, ApplyTo, AddTo, SubtractFrom,
  JoinTo, UnionTo, ReplaceAllIn, ReplaceRepeatedIn,
  KeyApplyTo, KeyIncrement, KeyDecrement, KeyAddTo, KeySubFrom, KeyTimesBy, KeyDivideBy, KeyUnionTo, KeyJoinTo, KeyAppendTo, KeyPrependTo, KeyBindTo, KeyUBindTo,
  TimesBy, DivideBy, KeyDropFrom, Increment, Decrement, PreIncrement, PreDecrement
};

$scopingHeads = {
  Function, Block, With, SubWith, Module, Locals, GeneralUtilities`Scope, GeneralUtilities`ModuleScope
};

$lvRules = Dispatch @ Join[
  Map[head |-> RuleDelayed[s_head, RuleCondition @  procScopingExpr @ s], $scopingHeads],
  Map[head |-> RuleDelayed[m_head, RuleCondition @ procMutatingExpr @ m], $lvMutatingHeads]
];

SetHoldC[procScopingExpr, procMutatingExpr, lhsSymbols]

procScopingExpr[_Locals | _GeneralUtilities`Scope | _GeneralUtilities`ModuleScope] := Null;

procScopingExpr[_[vars_, body___]] := With[
  {locals = Union @ lhsSymbols @ vars},
  Construct[
    collectLVs,
    ReplaceAll[HoldComplete @ body, Apply[Alternatives, locals] -> 0]
  ]
];

procMutatingExpr[_[lhs_, args___]] := (
  Internal`StuffBag[$lvBag, lhsSymbols @ lhs];
  collectLVs @ HoldComplete[args]
);

lhsSymbols[s_Symbol] := HoldPattern @ s;
lhsSymbols[list_List] := HoldMap[lhsSymbols, list];
lhsSymbols[(Set | SetDelayed)[lhs_, rhs_]] := lhsSymbols @ lhs;
lhsSymbols[_] := {};

(*************************************************************************************************)

IsMatchOf::usage = "";
IsNotMatchOf::usage = "";

(*************************************************************************************************)

CaseOf::usage =
"Case[rules$$] is a macro for defining functions of one variable, specifying LHS and RHS rules for the argument.
Case[rules$$, {alias$1, alias$2, $$}] applies temporary aliases to the rules$ before evaluation.
* Use the form func$ = Case[$$] to attach the rules to the function func$.
* Each of the rules should be of the form patt$ :> body$, and should be seperated by semicolons.
* The aliases can be used to transform the rules before they attached used as definitions.
* Use \[Rule] in an alias to have the RHS of the alias evaluate, and \[RuleDelayed] to perform a literal replacement.
* Aliases can be thought of as 'local macros' that make a particular function definition cleaner or more concise.
"

$::usage = "$ stands for the function currently being defined."

CaseOf::badCaseDefinition = "Bad Case definition for ``.";

DeclareHoldAll[CaseOf, ExtendCaseOf]

DefineComplexMacro[CaseOf, {
  SetDelayed[sym_Symbol[pre___], CaseOf[args___]] :> attachedCaseOf[sym, True, Hold[pre], args],
  Set[sym_Symbol,                CaseOf[args___]] :> attachedCaseOf[sym, True, Hold[], args]
}];

DefineComplexMacro[ExtendCaseOf, {
  Set[sym_Symbol,                ExtendCaseOf[args___]] :> attachedCaseOf[sym, False, Hold[], args]
}];

(* TODO: IsMatchOf *)
(* TODO: IsNotMatchOf *)
(* TODO: Set OR SetDelayed *)
(* TODO: complain about /; *)

(*************************************************************************************************)

DeclareHoldAll[CaseFn]

DefineComplexMacro[CaseFn, {
CaseFn[CompoundExpression[args___SetDelayed, Null...]][arg_] :> appliedCaseOf[{args, _ :> ThrowUnmatchedError[]}, arg],
CaseFn[CompoundExpression[args___SetDelayed, Null...]]       :> standaloneCaseOf[{args, _ :> ThrowUnmatchedError[]}]
}];

SetHoldC[appliedCaseOf, standaloneCaseOf];

appliedCaseOf[mh_, arg_]    := With[
  {rules = Apply[RuleDelayed, MacroHold @ mh, {2}]},
  MacroHold[Replace[arg, rules]]
];

standaloneCaseOf[mh_] := Replace @ Apply[RuleDelayed, MacroHold @ mh, {2}];

(*************************************************************************************************)

(* TODO: fix this not working: foo = Case[Seq[a_, b_] /; cond[a] := ...] *)

SetHoldC[attachedCaseOf, procRewrites]

attachedCaseOf[sym_, excl_, pre_, arg_SetDelayed]                := attachedCaseOf[sym, excl, pre, CompoundExpression[arg], {}];
attachedCaseOf[sym_, excl_, pre_, arg_SetDelayed, rewrites_List] := attachedCaseOf[sym, excl, pre, CompoundExpression[arg], rewrites];

attachedCaseOf[sym_, excl_, pre_, CompoundExpression[args__SetDelayed, rewrites_List]] :=
  attachedCaseOf[sym, excl, pre, CompoundExpression[args], rewrites];

attachedCaseOf[sym_Symbol, excl_, pre_, CompoundExpression[args__SetDelayed, Null...], rewrites_:{}] := Module[
  {holds},
  holds = Hold @@@ Hold[args];
  If[pre =!= Hold[], holds //= Map[Join[pre, #]&]];
  holds = ReplaceRepeated[holds, procRewrites @ rewrites];
  If[excl, PrependTo[holds, With[{sloc = SourceLocation[]},
    Hold[$LHS___, WithSourceLocation[sloc, ThrowUnmatchedError[sym, $LHS]]]]]]; (* TODO: why not _sym ? *)
  (* holds = ReplaceAll[holds, HoldPattern[$$|$] :> sym]; *)
  Replace[List @@ holds, {
    Hold[lhs_$, rhs_]                     :> MacroHold @ SetDelayed[lhs, rhs],
    Hold[lhs:VPatternTest[_$, _], rhs_]   :> MacroHold @ SetDelayed[lhs, rhs],
    Hold[lhs:VCondition[_$, _],   rhs_]   :> MacroHold @ SetDelayed[lhs, rhs],
    Hold[VPatternTest[lhs_, test_], rhs_] :> MacroHold @ SetDelayed[$ @ PatternTest[lhs, test], rhs],
    Hold[  VCondition[lhs_, cond_], rhs_] :> MacroHold @ SetDelayed[VCondition[$ @ lhs, cond], rhs],
    Hold[lhs___, rhs_]                    :> MacroHold @ SetDelayed[$[lhs], rhs] (* <- remove the ___ *)
  }, {1}] /. HoldPattern[$$|$] :> sym
];

procRewrites[s_Symbol ? System`Private`HasImmediateValueQ] := HoldPattern[s] -> s;
procRewrites[l_List] := Map[procRewrites, Unevaluated @ l];
procRewrites[a_ -> b_] := HoldPattern[a] -> b;
procRewrites[a_ :> b_] := HoldPattern[a] :> b;

attachedCaseOf[sym_, ___] := (Message[CaseOf::badCaseDefinition, HoldForm @ sym]; $Failed);

(*************************************************************************************************)

General::unmatchedCase = "Case unmatched: ``";
ThrowUnmatchedError[head_Symbol, $LHS_] := ThrowMsg["unmatchedCase", HoldForm @ $LHS];

CaseOf::unmatched = "Unmatched case in a CaseOf application.";
ThrowUnmatchedError[] := ThrowMsg["unmatched"];

(**************************************************************************************************)

UnpackOptions::usage = "UnpackOptions[sym$1, sym$2, $$] looks up options associated with capitalized versions of sym$i.";

DefineComplexMacro[UnpackOptions, UnpackOptions[syms__Symbol] :> mUnpackOptions[{syms}]]
SetHoldC[mUnpackOptions];

mUnpackOptions[syms_] := With[
  {names = symsToCapStrings @ syms},
  MacroHold[syms = OptionValue[names]]
];

(**************************************************************************************************)

UnpackOptionsAs::usage =
"UnpackOptionsAs[head$, options$, sym$1, sym$2, $$] unpacks option from options$ with matching names to sym$i, \
using head$ for default value."

DefineComplexMacro[UnpackOptionsAs, UnpackOptionsAs[head_Symbol, opts_, syms__Symbol] :> mUnpackOptions[head, opts, {syms}]]
SetHoldC[mUnpackOptionsAs]

mUnpackOptions[head_, opts_, syms_] := With[
  {names = symsToCapStrings @ syms},
  MacroHold[syms = OptionValue[head, {opts}, names]]
];

(**************************************************************************************************)

DefineComplexMacro[UnpackAnnotations, UnpackAnnotations[obj_, syms__Symbol] :> mUnpackAnnos[obj, {syms}]]
SetHoldC[mUnpackOptionsAs]

mUnpackAnnos[obj_, syms_] := With[
  {capSymbols = symsToCapSymbols @ syms},
  MacroHold[syms = VectorReplace[AnnotationValue[obj, capSymbols], $Failed :> None]]
];

(**************************************************************************************************)

PackAssociation::usage = "PackAssociation[sym$1, sym$2, $$] creates an association whose keys are the title-cased names of sym_i and values are their values.";
UnpackAssociation::usage = "UnpackAssociation[assoc$, sym$1, sym$2, $$] takes association whose string keys are capitalized versions of sym$i and sets the corresponding symbols to their values.";

DefineComplexMacro[PackAssociation, PackAssociation[syms__Symbol] :> mPackAssociation[{syms}]]
DefineComplexMacro[UnpackAssociation, UnpackAssociation[assoc_, syms__Symbol] :> mUnpackAssociation[assoc, {syms}]]
SetHoldC[mPackAssociation, packAssocRule, mUnpackAssociation];

mPackAssociation[syms_] := With[
  {rules = HoldMap[packAssocRule, syms]},
  MacroHold[Association[rules]]
];

packAssocRule[s_Symbol] := ToUpperCase1[HoldSymbolName[s]] -> MacroHold[s];

(**************************************************************************************************)

General::badAssociation = "One or more fields in `` were missing from the association.";

mUnpackAssociation[assoc_, syms_] := With[
  {names = symsToCapStrings @ syms},
  MacroHold[syms = Lookup[assoc, names, ThrowMsg["badAssociation", names]]]
];

(**************************************************************************************************)

General::badTuple = "Argument `` should be a single value or a list of `` values."

DefineComplexMacro[UnpackTuple, UnpackTuple[val_, syms__Symbol] :> mUnpackTuple[val, syms]]
SetHoldC[mUnpackTuple];

mUnpackTuple[val_, s1_Symbol, s2_Symbol] :=
  MacroHold @ If[ListQ[val],
    If[Length[val] != 2, ThrowMsg["badTuple", val, 2]]; {s1, s2} = val,
    s1 = s2 = val
  ];

mUnpackTuple[val_, s1_Symbol, s2_Symbol, s3_Symbol] :=
  MacroHold @ If[ListQ[val],
    If[Length[val] != 3, ThrowMsg["badTuple", val, 3]]; {s1, s2, s3} = val,
    s1 = s2 = s3 = val
  ];

mUnpackTuple[val_, s1_Symbol, s2_Symbol, s3_Symbol, s4_Symbol] :=
  MacroHold @ If[ListQ[val],
    If[Length[val] != 4, ThrowMsg["badTuple", val, 4]]; {s1, s2, s3, s4} = val,
    s1 = s2 = s3 = s4 = val
  ];

mUnpackTuple[val_, s1_Symbol, s2_Symbol, s3_Symbol, s4_Symbol, s5_Symbol] :=
  MacroHold @ If[ListQ[val],
    If[Length[val] != 5, ThrowMsg["badTuple", val, 5]]; {s1, s2, s3, s4, s5} = val,
    s1 = s2 = s3 = s4 = s5 = val
  ];

(**************************************************************************************************)

SetHoldC[symsToCapStrings, symsToCapSymbols]

symsToCapStrings[syms_] := Map[
  Function[sym, toOptionNameStr @ HoldSymbolName @ sym, HoldAllComplete],
  Unevaluated @ syms
];

symsToCapSymbols[syms_] := Map[
  Function[sym, toCapSymbol @ HoldSymbolName @ sym, HoldAllComplete],
  Unevaluated @ syms
];

General::noCorrespondingSymbol = "No symbol found corresponding to ``.";
toCapSymbol[str_String] := toCapSymbol[str] = Module[
  {str2 = toOptionNameStr[str]},
  Which[
    NameQ[str2], Symbol[str2],
    NameQ[str2 //= StringReplace["Fn" -> "Function"]], Symbol[str2],
    True, ThrowMsg["noCorrespondingSymbol", str]
  ]
];

toOptionNameStr[str_String] := toOptionNameStr[str] =
  makeOptionNameStr @ StrTrimL[str, "$"];

makeOptionNameStr[str_] := Which[
  StrStartsQ[str, "json"], "JSON" <> StringDrop[str, 4],
  True,                    ToUpperCase1 @ str
];


