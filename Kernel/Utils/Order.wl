SystemExports[
  "Function",
    Minimum,          Maximum,
    MinimumBy,        MaximumBy,
    MinimumIndex,     MaximumIndex,
    Minima,           Maxima,
    MinimaBy,         MaximaBy,
    MinimumIndexBy,   MaximumIndexBy,
    MinimaIndices,    MaximaIndices,
    MinimaIndicesBy,  MaximaIndicesBy,
    OrderSort,
    MostCommon
];

(*************************************************************************************************)

(* there are four abstract dimensions:
By:       X     vs   XBy
Sign:     MinX  vs   MaxX
Index:    X     vs   XIndex
Multi:    Xmum  vs   Xma
*)

(**************************************************************************************************)

SetStrict[Minimum, Maximum];

"Minimum[expr$] gives the minimum expr$i, as computed by Ordering."
"Maximum[expr$] gives the maximum expr$i, as computed by Ordering."
"MinimumBy[expr$, f$] gives the first expr$i for which f$[expr$i] is minimal, as computed by Ordering."
"MaximumBy[expr$, f$] gives the first expr$i for which f$[expr$i] is maximal, as computed by Ordering."

Minimum[expr_]  := Part[expr, First @ Ordering[expr,  1]];
Maximum[expr_]  := Part[expr, First @ Ordering[expr, -1]];
Minimum[e:_[_]] := First @ e;
Maximum[e:_[_]] := First @ e;
Minimum[EmptyP] := None
Maximum[EmptyP] := None

SetCurry2[MinimumBy, MaximumBy];

MinimumBy[expr_, f_] := Part[expr, First @ OrderingBy[expr, f,  1]];
MaximumBy[expr_, f_] := Part[expr, First @ OrderingBy[expr, f, -1]];
MinimumBy[e:_[_], _] := First @ e;
MaximumBy[e:_[_], _] := First @ e;
MaximumBy[EmptyP, _] := None;
MinimumBy[EmptyP, _] := None;

(**************************************************************************************************)

SetStrict[MinimumIndex, MaximumIndex];

"MinimumIndex[expr$] gives the first integer i$ for which expr$i is minimal, as computed by Ordering."
"MaximumIndex[expr$] gives the first integer i$ for which expr$i is maximal, as computed by Ordering."
"MinimumIndexBy[expr$, f$] gives the first integer i$ for which f$[expr$i] is minimal, as computed by Ordering. MinimumIndexBy[f$] is the operator form."
"MaximumIndexBy[expr$, f$] gives the first integer i$ for which f$[expr$i] is maximal, as computed by Ordering. MaximumIndexBy[f$] is the operator form."

MinimumIndex[expr_]  := First @ Ordering[expr,  1];
MaximumIndex[expr_]  := First @ Ordering[expr, -1];
MinimumIndex[EmptyP] := None;
MaximumIndex[EmptyP] := None;
MinimumIndex[_[_]]   := 1;
MaximumIndex[_[_]]   := 1;

SetCurry2[MinimumIndexBy, MaximumIndexBy];

MinimumIndexBy[expr_, f_] := First @ OrderingBy[expr, f,  1];
MaximumIndexBy[expr_, f_] := First @ OrderingBy[expr, f, -1];
MinimumIndexBy[EmptyP, _] := None;
MaximumIndexBy[EmptyP, _] := None;
MinimumIndexBy[_[_],   _] := 1;
MaximumIndexBy[_[_],   _] := 1;

(**************************************************************************************************)

SetStrict[Minima, Maxima];

"Minima[expr$] gives a list of the minimal expr$i, as computed by Ordering."
"Maxima[expr$] gives a list of the maximal expr$i, as computed by Ordering."
"MinimaBy[expr$, f$] gives a list of the expr$i for which f$[expr$i] is minimal, as computed by Ordering."
"MaximaBy[expr$, f$] gives a list of the expr$i for which f$[expr$i] is maximal, as computed by Ordering."

Minima[expr_] := MinimaBy[expr, Id];
Maxima[expr_] := MaximaBy[expr, Id];

SetCurry2[MinimaBy, MaximaBy];

MinimaBy[expr_, f_] := MinimalBy[Args @ expr, f];
MaximaBy[expr_, f_] := MaximalBy[Args @ expr, f];
MinimaBy[e:_[_], _] := List @ e;
MaximaBy[e:_[_], _] := List @ e;
MinimaBy[EmptyP, _] := None;
MaximaBy[EmptyP, _] := None;

(**************************************************************************************************)

SetStrict[MinimaIndices, MaximaIndices];

"MinimaIndices[expr$] gives the list of integer i$ for which expr$i is minimal, as computed by Ordering."
"MaximaIndices[expr$] gives the list of integer i$ for which expr$i is maximal, as computed by Ordering."
"MinimaIndicesBy[expr$] gives the list of integer i$ for which f$[expr$i] is minimal, as computed by Ordering."
"MaximaIndicesBy[expr$] gives the list of integer i$ for which $f[expr$i] is maximal, as computed by Ordering."

MinimaIndices[expr_]       := MinimalBy[Range @ Len @ expr, Part[expr, #]&];
MaximaIndices[expr_]       := MaximalBy[Range @ Len @ expr, Part[expr, #]&];
MinimaIndices[EmptyP]      := {};
MaximaIndices[EmptyP]      := {};
MinimaIndices[_[_]]        := {1};
MaximaIndices[_[_]]        := {1};

SetCurry2[MinimaIndicesBy, MaximaIndicesBy];

MinimaIndicesBy[expr_, f_] := MinimalBy[Range @ Len @ expr, f[Part[expr, #]]&];
MaximaIndicesBy[expr_, f_] := MaximalBy[Range @ Len @ expr, f[Part[expr, #]]&];
MinimaIndicesBy[EmptyP, _] := {};
MaximaIndicesBy[EmptyP, _] := {};
MinimaIndicesBy[_[_], _]   := {1};
MaximaIndicesBy[_[_], _]   := {1};

(**************************************************************************************************)

SetCurry2[OrderSort];

OrderSort[{}, _]             := {};
OrderSort[{e_}, _]           := {e};
OrderSort[list_List, None]   := Sort @ list;
OrderSort[list_List, order_] := Part[list, Ordering[
    FirstPosition[order, #, Null, {1}]& /@ list
  ]];

(**************************************************************************************************)

MostCommon[EmptyP] := None;
MostCommon[expr_]  := First @ Commonest @ expr;

(**************************************************************************************************)

MostCommonBy[EmptyP, Blank01] := None;
(* MostCommonBy[list_, f_:First] := P11 @ [Tally @ expr, 1, 1]; *)
