SystemExports[
  "FormHead", ExpanderForm
];

PackageExports[
  "BoxFn",    ExpanderBoxes, HoldExpanderBoxes,
  "MetaFn",   DeclareExpanderBoxes
];

(**************************************************************************************************)

DeclareSeqScan[DeclareExpanderBoxes]

DeclareExpanderBoxes[sym_Sym] := CoreBox[sym[args___]] := HoldExpanderBoxes[sym, args];

(**************************************************************************************************)

$remExpansions = Inf;

CoreBox[ExpanderForm[head_Sym[args___]]] :=
  StyleBox[HoldExpanderBoxes[head, args], ShowStringCharacters -> True];

CoreBox[ExpanderForm[head_Sym[args___], level_]] := StyleBox[
  BlockSet[$remExpansions, level, HoldExpanderBoxes[head, args]],
  StyleBox[HoldExpanderBoxes[head, args], ShowStringCharacters -> True]
];

(**************************************************************************************************)

SetHoldC[HoldExpanderBoxes, makeExpanderBoxes1, makeExpanderBoxes2, openHead, closeHead];

ExpanderBoxes[args___] := HoldExpanderBoxes[args];
HoldExpanderBoxes[head_Sym, args___] := makeExpanderBoxes2[head, {args}];

makeExpanderBoxes1[expr_] := MakeBoxes[expr];
makeExpanderBoxes1[head_Sym[args___]] := makeExpanderBoxes2[head, {args}];

makeExpanderBoxes2[Rule, {d:DatumP, rhs_}] :=
  joinFirstRow[RowBox[{MakeBoxes @ d, "\[Rule]", makeExpanderBoxes1 @ rhs}]];

joinFirstRow[boxes_] := boxes;
joinFirstRow[RowBox[{a_, b_, GridBox[{{f1_, fr___}, rest___}, opts___]}]] :=
  GridBox[{{RowBox[{a, b, f1}], fr}, rest}, opts];


makeExpanderBoxes2[head_Sym, {}] := RBox[openHead @ head, closeHead @ head];
makeExpanderBoxes2[head_Sym, args_List] := ColumnBox[
  FlatList[
    openHead @ head,
    MapMostLast[
      addTabComma, RBox["\t", #]&,
      If[$remExpansions > 0,
        BlockDecrement[$remExpansions, HoldMap[makeExpanderBoxes1, args]],
        MapMakeBox @ args
      ]
    ],
    closeHead @ head
  ],
  Left,
  RowAlignments -> Baseline
];

openHead[head_] := RBox[MakeBoxes @ head, "["];
openHead[List]  := "{";
openHead[Dict]  := LAssoc;

closeHead[_]    := "]";
closeHead[List] := "}";
closeHead[Dict] := RAssoc;

addTabComma[boxes_] := RBox["\t", boxes, ","];
addTabComma[GridBox[grid_, opts___]] := RBox["\t", Make[GridBox, MapAt[addComma, grid, {-1, -1}], opts]];
addComma[box_] := RBox[box, ","];
