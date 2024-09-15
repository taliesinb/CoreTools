SystemExports[
  "ScopingFunction",
    CaseOf, ExtendCaseOf, CaseFn,
    IsMatchOf, IsNotMatchOf
];

PackageExports[
  "MessageFunction", ThrowUnmatchedError
];

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

SetHoldA @ ThrowUnmatchedError;

General::unmatchedCase = "Case unmatched: ``[``]";
ThrowUnmatchedError[head_Symbol, $LHS___] := ThrowMsg[head -> "unmatchedCase", HoldForm @ head, HoldForm @ SequenceForm @ $LHS];

CaseOf::unmatched = "Unmatched case in a CaseOf application.";
ThrowUnmatchedError[] := ThrowMsg["unmatched"];
