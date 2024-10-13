PrivateExports[
  "Function",
    MergeUSetData, MergeOSetData, MergeMSetData,
       ToUSetDict,    ToOSetDict,    ToMSetDict,
       ToOSetData,    ToUSetData,    ToMSetData,
  "SymbolicHead",
    ArgList,
  "MessageFunction",
    ThrowOrderedSet, AssertSetLike, AssertSet
];

(*************************************************************************************************)

$coerceSetElem = False;

(*************************************************************************************************)

ToUSetDict[e_]          := MergeUSetData @ ToUSetData @ e;
ToUSetDict[e_, coerce_] := Block[{$coerceSetElem = coerce}, ToUSetDict @ e];

MergeUSetData[d_Dict] := d; (* we know it doesn't need to be coerced to be UDict *)
MergeUSetData[e_]     := UDict @ e;

ToUSetData = CaseOf[
  USet[a_]  := a;
  OSet[a_]  := UDict @ a;
  MSet[a_]  := TrueRules @ Keys @ a;
  l_List    := TrueRules @ l;
  d_Dict    := TrueRules @ Keys @ d;
  a_ArgList := Map[$, List @@ a];
  e_        := If[$coerceSetElem, Rule[e, True], AssertSetLike[e, USet]];
];

(*************************************************************************************************)

ToOSetDict[e_]          := MergeOSetData @ ToOSetData @ e;
ToOSetDict[e_, coerce_] := Block[{$coerceSetElem = coerce}, ToOSetDict @ e];

MergeOSetData[d_Dict] := d; (* we know it doesn't need to be coerced to be Dict *)
MergeOSetData[e_]     := ODict @ e;

ToOSetData = CaseOf[
  OSet[a_]  := a;
  u_USet    := ThrowOrderedSet[u];
  m_MSet    := ThrowOrderedSet[m];
  l_List    := TrueRules @ l;
  d_Dict    := TrueRules @ Keys @ d;
  a_ArgList := Map[$, List @@ a];
  e_        := If[$coerceSetElem, Rule[e, True], AssertSetLike[e, OSet]];
];

(*************************************************************************************************)

ToMSetDict[e_]          := MergeMSetData @ ToMSetData @ e;
ToMSetDict[e_, coerce_] := Block[{$coerceSetElem = coerce}, ToMSetDict @ e];

MergeMSetData[d_Dict] := d; (* we know it doesn't need to be coerced to be UDict *)
MergeMSetData[e_]     := UDictSum @ e;

ToMSetData = CaseOf[
  MSet[a_]  := a;
  USet[a_]  := lCounts @ Keys @ a;
  OSet[a_]  := lCounts @ Keys @ a;
  dict_Dict := lCounts @ Keys @ dict;
  list_List := lCounts @ list;
  a_ArgList := Map[$, List @@ a];
  e_        := If[$coerceSetElem, Rule[e, 1], AssertSetLike[e, MSet]];
];

(*************************************************************************************************)

lCounts = CaseOf[
  {}        := {};
  {a_}      := a -> 1;
  list_List := UCounts @ list;
];

(*************************************************************************************************)

ThrowOrderedSet[uset_] := ThrowMsg["orderedSetFromUnorderedSet", uset];
General::orderedSetFromUnorderedSet = "Cannot create an OrderedSet from an UnorderedSet."

AssertSetLike[spec_, as_] := ThrowMsg["notSetLike", spec, as];
General::notSetLike = "Cannot interpret `` as a ``."

AssertSet[spec_] := ThrowMsg["notSet", spec, as];
General::notSet = "Cannot interpret `` as a set."
