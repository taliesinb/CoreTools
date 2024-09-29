SystemExports[
  "ScopingFunction",
    CaseOf, ExtendCaseOf, CaseFn,
    IsMatchOf, IsNotMatchOf
];

PackageExports[
  "MessageFunction", ThrowUnmatchedError
];

(*************************************************************************************************)

(* TODO: IsMatchOf *)
(* TODO: IsNotMatchOf *)
(* TODO: Set OR SetD *)
(* TODO: complain about /; *)

DeclaredHere[CaseOf, ExtendCaseOf];
SetHoldC[CaseOf, ExtendCaseOf];

CaseOf::usage =
"Case[rules$$] is a macro for defining functions of one variable, specifying LHS and RHS rules for the argument.
Case[rules$$, {alias$1, alias$2, $$}] applies temporary aliases to the rules$ before evaluation.
* Use the form func$ = Case[$$] to attach the rules to the function func$.
* Each of the rules should be of the form patt$ :> body$, and should be seperated by semicolons.
* The aliases can be used to transform the rules before they attached used as definitions.
* Use \[Rule] in an alias to have the RHS of the alias evaluate, and \[RuleDelayed] to perform a literal replacement.
* Aliases can be thought of as 'local macros' that make a particular function definition cleaner or more concise."

$::usage = "$ stands for the function currently being defined."

ComplexMacroDefs[CaseOf,
  SetD[sym_Sym[pre___], CaseOf[args___]] := mCaseOf[sym, True, Hold[pre], args],
  Set [sym_Sym,         CaseOf[args___]] := mCaseOf[sym, True, Hold[],    args]
];

ComplexMacroDefs[ExtendCaseOf,
  Set[sym_Sym, ExtendCaseOf[args___]]    := mCaseOf[sym, False, Hold[],   args]
];

SetHoldC[mCaseOf]

mCaseOf[sym_, excl_, pre_, arg_SetD]                := mCaseOf[sym, excl, pre, Then[arg], {}];
mCaseOf[sym_, excl_, pre_, arg_SetD, rewrites_List] := mCaseOf[sym, excl, pre, Then[arg], rewrites];

mCaseOf[sym_, excl_, pre_, Then[args__SetD, rewrites_List]] :=
  mCaseOf[sym, excl, pre,  Then[args], rewrites];

mCaseOf[sym_Sym, excl_, pre_, Then[args__SetD, Null...], rewrites_:{}] := Module[
  {holds = Hold @@@ Hold[args]},
  If[pre =!= Hold[], holds //= Map[sig |-> Join[pre, sig]]];
  holds = ReplaceRepeated[holds, procRewrites @ rewrites];
  If[excl, PrependTo[holds, With[{sloc = SourceLocation[]},
    Hold[$LHS___, SetSrcLoc[sloc, ThrowUnmatchedError[sym, $LHS]]]]]]; (* TODO: why not _sym ? *)
  (* holds = ReplaceAll[holds, HoldPattern[$$|$] :> sym]; *)
  Replace[List @@ holds, {
    Hold[lhs_$, rhs_]                     :> HoldM @ SetD[lhs, rhs],
    Hold[lhs:VPatternTest[_$, _], rhs_]   :> HoldM @ SetD[lhs, rhs],
    Hold[lhs:VCondition[_$, _],   rhs_]   :> HoldM @ SetD[lhs, rhs],
    Hold[VPatternTest[lhs_, test_], rhs_] :> HoldM @ SetD[$ @ PatternTest[lhs, test], rhs],
    Hold[  VCondition[lhs_, cond_], rhs_] :> HoldM @ SetD[Condition[$ @ lhs, cond], rhs],
    Hold[lhs___, rhs_]                    :> HoldM @ SetD[$[lhs], rhs] (* <- remove the ___ *)
  }, {1}] /. HoldPattern[$$|$] :> sym
];

SetHoldC[procRewrites];

procRewrites[s_Sym ? HasIValueQ] := HoldPattern[s] -> s;
procRewrites[l_List] := Map[procRewrites, NoEval @ l];
procRewrites[a_ -> b_] := HoldPattern[a] -> b;
procRewrites[a_ :> b_] := HoldPattern[a] :> b;

mCaseOf[sym_, ___] := MacroError[CaseOf::badCaseDefinition, PrivHold @ sym];

CaseOf::badCaseDefinition = "Bad case definition for ``.";

(*************************************************************************************************)

SetHoldA[CaseFn]

ComplexMacroDefs[
  CaseFn[Then[args___SetD, Null...]][arg_] := mCaseFn[HoldC @ {defs}, HoldC @ arg],
  CaseFn[Then[args___SetD, Null...]]       := mCaseFn[HoldC @ {defs}, None]
];

mCaseFn0[defs_,  arg_]        := mCaseFn1[Append[$unmatchRule] @ SetDsToRuleDs @ defs, arg];
mCaseFn1[rules_, HoldC[arg_]] := HoldM @ Replace[arg, rules];
mCaseFn1[rules_, None]        := Replace[rule];

$unmatchRule = RuleD[_, ThrowUnmatchedError[]];

(*************************************************************************************************)

SetHoldA @ ThrowUnmatchedError;

General::unmatchedCase = "Case unmatched: ``.";
ThrowUnmatchedError[head_Sym, $LHS___] := ThrowMsg[head -> "unmatchedCase", PrivHold @ head[$LHS]];

CaseOf::unmatched = "Unmatched case in a CaseOf application.";
ThrowUnmatchedError[] := ThrowMsg["unmatched"];
