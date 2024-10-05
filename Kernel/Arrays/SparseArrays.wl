PackageExports[
  "Function", SparseRules, SparseRows, SparseColumns
];

(**************************************************************************************************)

SetStrict @ SparseRules;

SparseRules[{} | <||>, sz_] := SparseArray[{}, sz];

SparseRules[assoc_Dict, sz_] := SparseArray[Normal @ assoc, sz];

SparseRules[list:{___Int} ? DuplicateFreeQ, sz_] := SparseArray[Thread[list -> 1], sz];

SparseRules[list:{___List} ? DuplicateFreeQ, sz_] := SparseArray[Thread[list -> 1], sz];

SparseRules[list:{___List}, sz_] := SparseArray[Normal @ Counts @ list, sz];

SparseRules[list:{___Rule}, sz_] := SparseArray[sumRules @ list, sz];

(**************************************************************************************************)

sumRules[rules_] := Normal @ Merge[rules, Total];

(**************************************************************************************************)

SetStrict @ SparseRows;

SparseRows[rowSpecs_List, n_Int] := SparseArray[
  Flatten @ MapP[rowSpecToFullSpec, rowSpecs],
  {Len @ rowSpecs, n}
];

SparseRows[rowSpecs_List] := SparseArray[
  Flatten @ MapP[rowSpecToFullSpec, rowSpecs]
];

rowSpecToFullSpec[{}, row_] := {};

rowSpecToFullSpec[cols:{__Rule}, row_] := MapApply[{row, #1} -> #2&, sumRules @ cols];

rowSpecToFullSpec[cols_List -> k_, row_] := sumRules @ Map[{row, #} -> k&, cols];
rowSpecToFullSpec[cols_List, row_] := sumRules @ Map[{row, #} -> 1&, cols];

rowSpecToFullSpec[col_Int -> k_, row_] := {{row, col} -> k};
rowSpecToFullSpec[col_Int, row_] := {{row, col} -> 1};

(**************************************************************************************************)

SetStrict @ SparseColumns;

SparseColumns[args___] := Transpose @ SparseRows[args];
