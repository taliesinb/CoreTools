SystemExports[

  "Function",
    UniqueOccurrences,
    Occurrences,         FirstOccurrence,
    OccurrencePositions, FirstOccurrencePosition,
    ArgumentPositions,  FirstArgumentPosition,
    LeafPositions,
    LevelPositions,
    OccurrencesWithin,
    LevelAssociation,
    PartAssociation,
    OccurrenceAssociation,
    ArgumentAssociation,
    LeafAssociation

];

PackageExports[

  "Function",
    DeepCount, DeepReplace, DeepCases, DeepFirstCase,
    DeepStrContainsQ, DeepStrMatchQ,
    DeepStrMatchQ, DeepStrHasQ, DeepStrFreeQ, DeepStrTestQ,
    FindExprPaths, AllExprPaths, LeafExprPaths,
    ToExprPaths, ExtractExprPaths,
    FnRule,
    MatchFn,

  "SymbolicHead",
    ExprPath
];

(**************************************************************************************************)

SetCurry2[DeepStrMatchQ, DeepStrHasQ, DeepStrFreeQ, DeepStrTestQ]

DeepStrMatchQ[expr_, patt_]                := deepStrTest[expr, StrMatchQ[patt]];
DeepStrMatchQ[expr_, patt_, IgCase -> ic_] := deepStrTest[expr, StrMatchQ[patt, IgCase -> ic]];

DeepStrHasQ[expr_, patt_]                  := deepStrTest[expr, StrHasQ[patt]];
DeepStrHasQ[expr_, patt_, IgCase -> ic_]   := deepStrTest[expr, StrHasQ[patt, IgCase -> ic]];

DeepStrFreeQ[expr_, patt_]                 := !deepStrTest[expr, StrHasQ[patt]];
DeepStrFreeQ[expr_, patt_, IgCase -> ic_]  := !deepStrTest[expr, StrHasQ[patt, IgCase -> ic]];

DeepStrTestQ[expr_, test_]                 := deepStrTest[expr, test];
deepStrTest[expr_, test_]                  := VContainsQ[expr, Str] && ContainsQ[expr, _Str ? test];

(**************************************************************************************************)

SetCurry2[DeepCount, DeepReplace, DeepCases, DeepFirstCase];

DeepCount[expr_, patt_]                    := deepCount[expr, patt, False];
DeepCount[expr_, patt_, Heads -> heads_]   := deepCount[expr, patt, heads];
deepCount[expr_, patt_, heads_]            := Count[expr, patt, {0, Infinity}, Heads -> heads];

DeepReplace[expr_, repl_]                  := deepReplace[expr, repl, False];
DeepReplace[expr_, repl_, Heads -> heads_] := deepReplace[expr, repl, heads];
deepReplace[expr_, repl_, heads_]          := Replace[expr, repl, {0,Infinity}, Heads -> heads];

DeepCases[expr_, repl_,            level:All] := Cases[expr, Infinity, level];
DeepFirstCase[expr_, repl_, else_, level:All] := FirstCase[expr, patt, else, level];

(**************************************************************************************************)

MatchFn[rules___] := Replace @ ToList[rules, _ :> InternalError];

(**************************************************************************************************)

FnRule[_, Id]      := Nothing;
FnRule[patt_, fn_] := Make[RuleD, Make[Pattern, FmS, patt], NoEval @ fn @ FmS];

(**************************************************************************************************)

SetCurry2[Occurrences, UniqueOccurrences];

      Occurrences[expr_, patt_, level_:All] := Cases[expr, patt, level, Heads -> True];
UniqueOccurrences[expr_, patt_, level_:All] := DelDups @ Occurrences[expr, patt, level];

(**************************************************************************************************)

(* OccurrencePositions matches how Position normally works *)
DefineAliasRules[OccurrencePositions -> Position];

SetCurry2 @ SetHoldR[FirstOccurrence, FirstOccurrencePosition];

        FirstOccurrence[expr_, patt_, else_:None, level_:All] := FirstCase[expr, patt, else, level, Heads -> True];
FirstOccurrencePosition[expr_, patt_, else_:None, level_:All] := FirstPosition[expr, patt, else, level];
(* unlike FirstPosition, FirstOccurrencePosition defaults to None *)

(**************************************************************************************************)

SetCurry2[ArgumentPositions, FirstArgumentPosition];
SetHoldR[FirstArgumentPosition];

(* unlike Position, ArgumentPositions avoids heads *)
    ArgumentPositions[expr_, patt_, level_:All]    :=      Position[expr, patt, level, Heads -> False];
FirstArgumentPosition[expr_, patt_, else_:None]    := FirstPosition[expr, patt, else, All, Heads -> False];
FirstArgumentPosition[expr_, patt_, else_, level_] := FirstPosition[expr, patt, else, level, Heads -> False];

 LeafPositions[expr_]            := Position[expr, _, {-1}, Heads -> False];
LevelPositions[expr_]            := Position[expr, _, All, Heads -> False];
LevelPositions[expr_, level_Int] := Position[expr, _, {level}, Heads -> False];
LevelPositions[expr_, level_]    := Position[expr, _, level, Heads -> False];

(*************************************************************************************************)

SetCurry2[OccurrenceAssociation, ArgumentAssociation]

    LevelAssociation[expr_, level_:All, fn_:Id]        := partAssoc[expr, Position[expr,    _, level, Heads -> False], fn];
OccurrenceAssociation[expr_, patt_, level_:All, fn_:Id] := partAssoc[expr, Position[expr, patt, level, Heads -> True], fn];
 ArgumentAssociation[expr_, patt_, level_:All, fn_:Id] := partAssoc[expr, Position[expr, patt, level, Heads -> False], fn];
     LeafAssociation[expr_, fn_:Id]                    := partAssoc[expr, Position[expr,    _, {-1},  Heads -> False], fn];

PartAssociation[expr_, parts_ ? ListVectorQ, fn_:Id] := partAssoc[expr, parts, fn];

partAssoc[expr_, parts_, Id]  := AssociationThread[parts, Extract[EnsureODict @ expr, parts]];
partAssoc[expr_, parts_, fn_] := AssociationThread[parts, Extract[EnsureODict @ expr, parts, fn]];

(**************************************************************************************************)

CoreBox[ExprPath[path___]] := exprPathBoxes[path];

$dimDot = DimBox @ ".";

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

(* SetCurry2[OccurrenceMapP,  OccurrenceScanP]
SetCurry2[ScanOccurrences, ScanArguments, ScanLevel]
 *)
(*************************************************************************************************)

OccurrencesWithin[expr_, patt_, up_Int] := Locals[
  pos = Position[expr, patt];
  pos //= Map[If[Len[#] >= up, Drop[#, -up], {}]&];
  pos //= SortBy[Length];
  pos = DeleteDuplicates[pos, PrefixListQ];
  Extract[expr, pos]
];

(*************************************************************************************************)

