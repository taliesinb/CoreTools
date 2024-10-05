SystemExports[
  "Function",
    VectorReplace,
    FullReplaceAll, FullReplaceRepeated,
    ReplaceAllList,
    ReplaceAllWithin,
    ReplaceIndices, ConstantReplaceIndices,
    ExtractSpans, ReplaceSpans,
    LiteralSequenceReplace,
    LiteralSequenceReplaceRepeated,

  "SymbolicHead",
    ExprPath
];

PackageExports[
  "Function",
    FindExprPaths, AllExprPaths, LeafExprPaths,
    ToExprPaths, ExtractExprPaths,
    FnRule,
    MatchFn
];

(*************************************************************************************************)

SetCurry2[VectorReplace]

VectorReplace[vector_, rule_] := Replace[vector, rule, {1}];

(*************************************************************************************************)

"ConstantReplaceIndices[expr$, indices$, value$]` replaces the parts at various indices with a single value value$.
* indices can be All, Span[$$], or List[p$1, p$2, $$].";

(* TODO: support Broadcast *)
ConstantReplaceIndices[expr_, indices_, value_List] := ReplacePart[expr, Map[List, indices] -> value];
ConstantReplaceIndices[expr_, indices_, value_]     := setParts[expr, indices, value];

(*************************************************************************************************)

"ConstantReplaceIndices[expr$, indices$, value$]` replaces the parts at various indices with value$.
* indices can be All, Span[$$], or List[p$1, p$2, $$].";

SetStrict @ ReplaceIndices;

General::replaceLengthMismatch = "List of indices to replace has length `` but values have length ``."
ReplaceIndices[expr_, indices_List, values_List] /; SameLenQOrThrow[indices, values, "replaceLengthMismatch"] :=
  setParts[expr, indices, values];

setParts[expr_, parts_, value_] := Locals[expr2 = expr; Part[expr2, parts] = value; expr2];

(**************************************************************************************************)

SetCurry2[FullReplaceAll, FullReplaceRepeated]

FullReplaceAll[expr_, rules_]      :=      First[ReplaceAll[Hold[expr] /. Association -> assocDummy, rules] /. assocDummy -> Association];
FullReplaceRepeated[expr_, rules_] := First[ReplaceRepeated[Hold[expr] /. Association -> assocDummy, rules] /. assocDummy -> Association];

(*************************************************************************************************)

(* unlike ReplaceAll, this is bottom up, not top down *)
ReplaceAllWithin[expr_, rep_, n_:1] := Replace[expr, rep, {n,Infinity}, Heads -> True];

(*************************************************************************************************)

ReplaceAllList[expr_, rules_] := Locals[
  positions = Position[expr, toRepLHS @ rules];
  Switch[positions,
    {},   {},
    {_},  replaceListAt[expr, rules, P1 @ positions],
    _,    Catenate @ Map[pos |-> replaceListAt[expr, rules, pos], positions]
  ]
];

replaceListAt[expr_, rules_, {}] := ReplaceList[expr, rules];

replaceListAt[expr_, rules_, pos_] := Locals[
  Map[
    result |-> ReplacePart[expr, pos -> result],
    ReplaceList[Extract[expr, pos], rules]
  ]
];

ReplaceAllList[rules_][expr_] := ReplaceAllList[expr, rules];

toRepLHS = CaseOf[
  rules_List := Alternatives @@ Map[toRepLHS, rules];
  lhs_ -> _  := lhs;
  lhs_ :> _  := lhs;
];

(*************************************************************************************************)

SetStrict @ ExtractSpans;

ExtractSpans[expr_, {}] := {};

ExtractSpans[expr_, {{l_, r_}}] := Part[expr, l;;r];

ExtractSpans[expr_, list_List] := Part[expr, Span[#1, #2]]& @@@ list;

(*************************************************************************************************)

SetStrict @ ReplaceSpans;

ReplaceSpans[expr_, {}, new_] := expr;

ReplaceSpans[expr_, {{l_, r_}}, new_] :=
  Join[Take[expr, l-1], new, Drop[expr, r]];

ReplaceSpans[expr_, spans_List, new_] := Locals[
  $expr = expr;
  Scan[Fn[Part[$expr, #] = $dum[]], Span @@@ spans];
  Part[$expr, Part[spans, All, 1]] = $dum @@ new;
  Flatten[$expr, 1, $dum]
]

(*************************************************************************************************)

SetStrict @ LiteralSequenceReplace;

LiteralSequenceReplace[expr_, patt_, new_] :=
  ReplaceSpans[expr, SublistPosition[expr, patt, Infinity, False], new];

(*************************************************************************************************)

SetStrict @ LiteralSequenceReplaceRepeated;

LiteralSequenceReplaceRepeated[expr_, {}] := expr;
LiteralSequenceReplaceRepeated[expr2_, rules_List] := Locals[
  expr = expr2; n = 0;
  Label[restart];
  If[n > 8, Abort[]];
  Scan[rule |-> If[Len[expr] >= Len[lhs = P1 @ rule],
    spans = pos = SublistPosition[expr, P1 @ rule, Infinity, False];
    expr = ReplaceSpans[old = expr, spans, P2 @ rule];
    If[NotEmptyQ[pos] && expr =!= old, n++; Goto[restart]]],
    rules
  ];
  expr
];