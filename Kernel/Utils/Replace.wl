SystemExports[
  "Function",
    Occurences,         FirstOccurence,
    OccurencePositions, FirstOccurencePosition,
    ArgumentPositions,  FirstArgumentPosition,
    LeafPositions,
    LevelPositions,
    OccurencesWithin,

    FindExprPaths, AllExprPaths, LeafExprPaths,
    ToExprPaths, ExtractExprPaths,

    LevelAssociation, OccurenceAssociation, ArgumentAssociation, LeafAssociation,
    PartAssociation,

    FullReplaceAll, FullReplaceRepeated,
    ReplaceAllList,
    ReplaceAllWithin,
    FnRule,
    MatchFn,
    ExtractSpans, ReplaceSpans,
    LiteralSequenceReplace,
    LiteralSequenceReplaceRepeated,

  "Head",
    ExprPath
];

(**************************************************************************************************)

MatchFn[rules___] := Replace @ ToList[rules, _ :> InternalError];

(**************************************************************************************************)

FnRule[_, Id]      := Nothing;
FnRule[patt_, fn_] := RuleD[FmS:patt, fn[FmS]];

(**************************************************************************************************)

SetCurry2[Occurences, ArgumentPositions];
SetCurry2[FirstOccurence, FirstOccurencePosition];
SetHoldR[FirstOccurence, FirstOccurencePosition, FirstArgumentPosition];

    Occurences[expr_, patt_, level_:All]        := Cases[expr, patt, level, Heads -> True];
FirstOccurence[expr_, patt_, else_, level_:All] := FirstCase[expr, patt, else, level, Heads -> True];

(* OccurencePositions matches how Position normally works *)
DefineAliasRules[OccurencePositions -> Position];

(* unlike FirstPosition, FirstOccurencePosition defaults to None *)
FirstOccurencePosition[expr_, patt_, else_:None, level_:All] := FirstPosition[expr, patt, else, level];

(* unlike Position, ArgumentPositions avoids heads *)
    ArgumentPositions[expr_, patt_, level_:All]    :=      Position[expr, patt, level, Heads -> False];
FirstArgumentPosition[expr_, patt_, else_:None]    := FirstPosition[expr, patt, else, All, Heads -> False];
FirstArgumentPosition[expr_, patt_, else_, level_] := FirstPosition[expr, patt, else, level, Heads -> False];

 LeafPositions[expr_]            := Position[expr, _, {-1}, Heads -> False];
LevelPositions[expr_]            := Position[expr, _, All, Heads -> False];
LevelPositions[expr_, level_Int] := Position[expr, _, {level}, Heads -> False];
LevelPositions[expr_, level_]    := Position[expr, _, level, Heads -> False];

(*************************************************************************************************)

SetCurry2[OccurenceAssociation, ArgumentAssociation]

    LevelAssociation[expr_, level_:All, fn_:Id]        := partAssoc[expr, Position[expr,    _, level, Heads -> False], fn];
OccurenceAssociation[expr_, patt_, level_:All, fn_:Id] := partAssoc[expr, Position[expr, patt, level, Heads -> True], fn];
 ArgumentAssociation[expr_, patt_, level_:All, fn_:Id] := partAssoc[expr, Position[expr, patt, level, Heads -> False], fn];
     LeafAssociation[expr_, fn_:Id]                    := partAssoc[expr, Position[expr,    _, {-1},  Heads -> False], fn];

PartAssociation[expr_, parts_ ? ListVectorQ, fn_:Id] := partAssoc[expr, parts, fn];

partAssoc[expr_, parts_, Id]  := AssociationThread[parts, Extract[EnsureODict @ expr, parts]];
partAssoc[expr_, parts_, fn_] := AssociationThread[parts, Extract[EnsureODict @ expr, parts, fn]];

(**************************************************************************************************)

CoreBoxes[ExprPath[path___]] := exprPathBoxes[path];

$dimDot = DimmedBox @ ".";

exprPathBoxes[] := $dimDot;
exprPathBoxes[a_] := RowBox[{ToBoxes @ a, $dimDot}];
exprPathBoxes[a__] := RowBox @ FlatList @ Map[z |-> {ToBoxes[z], $dimDot}, {a}];

(*************************************************************************************************)

ToExprPaths[list_ ? ListVecQ] := Sort[ExprPath @@@ list];

FindExprPaths[expr_, spec:Blank12] := ToExprPaths @ Position[expr, spec, Heads -> False];

AllExprPaths[expr_]  := FindExprPaths[expr, _];

LeafExprPaths[expr_] := FindExprPaths[expr, _, {-1}];

(*************************************************************************************************)

ExtractExprPaths[expr_, paths_] := CatchMessages @ Block[{$expr = expr}, extractPaths @ paths];

extractPaths = CaseOf[
  {}                 := {};
  ExprPath[]         := $expr;
  paths:{__ExprPath} := Extract[$expr, List @@@ paths];
  path_ExprPath      := Extract[$expr, List @@ path];
  dict:ListDictP     := Map[$, dict];
  other_             := ThrowMsg["notExprPath", other];
];

ExtractExprPaths::notExprPath = "Expected a (list or association of) ExprPath, not ``."

(*************************************************************************************************)

(* SetCurry2[OccurenceMapP,  OccurenceScanP]
SetCurry2[ScanOccurences, ScanArguments, ScanLevel]
 *)
(*************************************************************************************************)

OccurencesWithin[expr_, patt_, up_Int] := Locals[
  pos = Position[expr, patt];
  pos //= Map[If[Len[#] >= up, Drop[#, -up], {}]&];
  pos //= SortBy[Length];
  pos = DeleteDuplicates[pos, PrefixListQ];
  Extract[expr, pos]
];

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