PackageExports[
  "ScopingFunction",
    Generator, Yield, YieldFrom, StepContinuable,
  "Function",
    ContinuableInitialState,
  "Head",
    Continuable,
    CModule, CSymbol, CSequential, CMutate, CLiteral, CApply,
    CModuleState, CSequentialState, CValue, CState, CStack,
  "SpecialVariable",
    $Exhausted
];

(*************************************************************************************************)

(*
GeneratorCompExpr[i, ...]
Generator

*)

DeclareHoldAllComplete[Continuable, CEval, freeze];

Continuable[e_] := freeze[e];

$moduleId = 0;
$mutP = ToAltPattern @ $MutatingSymbols;
freeze = CaseOf[

  Module[{vars__Symbol}, body_] := With[
    {sym = FormalSymbol @ Mod[$moduleId++, 52]},
    {ivars = HoldMap[HoldPattern, {vars}]},
    {cvars = Thread @ CSymbol[sym, LenRange @ ivars]},
    {rules = RuleThread[ivars, cvars]},
    {cbody = $ @@ ReplaceAll[HoldComplete[body], rules]},
    CModule[Len @ ivars, id, cbody]
  ];

  CompoundExpression[exprs___] := HoldMap[$, CSequential[exprs]];
  s:$mutP[lhs_CSymbol, rhs__]  := CMutate[s, rhs];
  c_CSymbol                    := c;
  c_Symbol                     := CLiteral[c];
  a_ ? HoldAtomQ               := CLiteral[a];
  list_List                    := CList @@ HoldMap[$, list];
  h_[]                         := CCall0[$ @ h];
  h_[a1_]                      := CCall1[$ @ h, $ @ a1];
  h_[a1_, a2_]                 := CCall2[$ @ h, $ @ a1, $ @ a2];
];

(*************************************************************************************************)

$stepVars = UDict[];

StepContinuable[e_] = step[e];
(*
step = CaseOf[

  Seq[CModule[_, sym_, body_], stack_] := Block[
    {sym = First[stack, ConstList[CValue[None], n]]},
    Prepend[sym] @ $[body, Rest @ stack]]
  ];

  Seq[c_CSequential, stack] := Module[{n},
    n = First[stack, 1];
    Part[c, 1]

  ];





]; *)