SystemExports[
  "Head",
    RelationData,
  "Function",
    RelationFromRows, RelationFromColumns,
  "Variable",
    $FormalSymbols
];

(*************************************************************************************************)

(* find positions matching a criterion *)

RelationData[arg_] := makeRelationIndex[arg];

RelationFromColumns::badColuments = "Argument `` should be a matrix or an association of columns.";
RelationFromColumns[arg_] := Locals[
  Which[
    AnyMatrix[arg], keys = LenRange @ args; values = args,
    AssocQ[arg],    keys = LenRange @ args; values = Values @ arg,
    True,           ReturnFailed["badColuments", arg]
  ];
  values = ToPackedInts[values];
  If[!PackedQ[values], values = ToPackedInts /@ values];
  ConstructNoEntryExpr[SparseRelation, 1]
];

