SystemExports[
  "SymbolicHead", Choice, HoldChoice, FunctionChoice,
  "Function",     EnumerateChoices
];

(**************************************************************************************************)

SetHoldF[EnumerateChoices, iEnumerateChoices];

EnumerateChoices[expr_] :=
  CatchMessages @ iEnumerateChoices[expr, {}];

EnumerateChoices[expr_, rules__] :=
  CatchMessages @ iEnumerateChoices[expr, enumRulesInContext[{}, Reverse @ Hold @ rules]];

iEnumerateChoices[expr_, context_] :=
  List @@ enumInContext[
    HoldChoice[expr] /. Choice -> HoldChoice,
    context
  ];

(**************************************************************************************************)

SetStrict @ enumRulesInContext;

enumRulesInContext = CaseOf[
  $[context_List, Hold[Rule[sym_Sym, rhs_], rest___]] :=
    $[Append[context, sym -> enumInContext[toHoldChoice @ rhs, context]], Hold[rest]];

  $[context_List, Hold[]] := context;

  $[_, spec_] := ThrowMsg["badRuleSpec", spec];
];

EnumerateChoices::badRuleSpec = "Expect a rule to Choice, a list, or an integer, not ``.";

SetHoldC[toHoldChoice, fnChoice, toChoice];

toChoice[e_] := toHoldChoice[e] /. HoldChoice -> Choice;

toHoldChoice = CaseOf[
  n_Int                  := HoldChoice @@ Range[n];
  (List|Choice)[c___]    := HoldChoice @ c;
  FunctionChoice[a_, b_] := Make[fnChoice, toHoldChoice @ a, toHoldChoice @ b];
  other_                 := HoldChoice @ other;
];

enumInContext[expr_HoldChoice, context_] :=
  FormalDistribute[expr /. context //. $choiceFinal, HoldChoice];

$choiceFinal = {
  FormalPower[cod_, dom_HoldChoice] :> RuleEval @ fnChoice[cod, dom]
};

fnChoice[cod_, HoldChoice[dom___]] := With[
  {len = Len @ Hold @ dom},
  {rep = ConstList[cod, len]},
  HoldChoice @ DictThread[List @ dom, rep]
];

(**************************************************************************************************)

Choice[n_Int] := Choice @@ Range[n];

f:FunctionChoice[a_, b_] := Choice @@ EnumerateChoices[toChoice[f]];

(**************************************************************************************************)

SetFlat @ Choice;
SetFlat @ SetHoldA @ HoldChoice;

