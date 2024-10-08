SystemExports[
  "ScopingFunction",
    Collecting, CollectBegin, CollectEnd,
  "Function",
    NewCollector, FromCollector,
  "Operator",
    CollectorFn
];

(**************************************************************************************************)

SetHoldA[CollectBegin, CollectEnd, Collecting]

SimpleMacroDefs[
  CollectBegin[var_Symbol]       := Set[var, NewCollector[]],
  CollectBegin[vars__Symbol]     := Set[{vars}, NewCollector @ HLen @ {vars}],
  CollectEnd[var_Symbol]         := ThenNull[Set[var, FromCollector @ var]],
  CollectEnd[vars__Symbol]       := ThenNull[Set[{vars}, FromCollector @ {vars}]],
  Collecting[s_Symbol, body_]    := Then2[CollectBegin[s], body, CollectEnd[s]],
  Collecting[{s__Symbol}, body_] := Then2[CollectBegin[s], body, CollectEnd[s]]
];

(*************************************************************************************************)

SetStrict[NewCollector, FromCollector]
SetListable[FromCollector];

NewCollector[]       := CollectorFn[Bag[]];
NewCollector[n_Int]  := Table[NewCollector[], n];

FromCollector[CollectorFn[b_Bag]] := BagPart[b, All];

CollectorFn::badArguments = "`` is not valid."
e:(_CollectorFn[___])           := ReturnMsg["badArguments", HoldForm @ e];
CollectorFn[bag_][item_]        := StuffBag[bag, item];
CollectorFn[bag_][item_, n_Int] := StuffBag[bag, item, n];
