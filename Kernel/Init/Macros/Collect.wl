SystemExports[
  "ScopingFunction",
    CollectBegin, CollectEnd, Collecting
];

(**************************************************************************************************)

SetHoldA[CollectBegin, CollectEnd, Collecting]

DefineSimpleMacro[CollectBegin,   {
  CollectBegin[var_Symbol]   :> Set[var, NewCollector[]],
  CollectBegin[vars__Symbol] :> Set[{vars}, NewCollector @ HoldLen @ {vars}]
}];

DefineSimpleMacro[CollectEnd, {
  CollectEnd[var_Symbol]    :> ThenNull[Set[var, FromCollector @ var]],
  CollectEnd[vars__Symbol]  :> ThenNull[Set[{vars}, FromCollector @ {vars}]]
}];

DefineSimpleMacro[Collecting,     {
  Collecting[s_Symbol, body_]    :> Then2[CollectBegin[s], body, CollectEnd[s]],
  Collecting[{s__Symbol}, body_] :> Then2[CollectBegin[s], body, CollectEnd[s]]
}];

