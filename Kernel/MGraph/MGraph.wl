SystemExports[
  "Head",
    Multigraph, Multiedge, MultiedgeField,
    IndexedVertex, IndexedMultiedge,
    GraphPath,

  "PatternHead",
    MultiedgesTo, MultiedgesFrom,

  "Symbol",
    Ordered, Keyed,

  "Function",
    MultigraphVertexList,
    MultigraphVertexCount,
    MultigraphVertexIndex,

    MultigraphNullaryGroups,

    IntegerVertexList,
    IntegerMultiedgeList,
    IntegerMultiedgeInputLists,
    IntegerMultiedgeInputRecords,
    IntegerMultiedgeOutputs,
    IntegerMultiedgeFields,
    IntegerMultiedgeTypes,

    MultiedgeList,
    MultiedgeName,
    MultiedgeNameList,
    MultiedgeNameIndex,

    MultiedgeCount,
    MultiedgeInputs,
    MultiedgeInputLists,
    MultiedgeInputRecords,
    MultiedgeTypes,

    MultiedgeArity,
    MultiedgeOutputs,
    MultiedgeOutputGroups,
    MultiedgeFields,

    MultigraphFieldList,
    MultigraphFieldCount,
    MultigraphFieldIndex,

    MultiedgePaths, MultiedgePathGraph,
    MultiedgeLeafCount, MultiedgeDepth,

  "Predicate",
    MultiedgeQ
];

PackageExports[
  "Head",
    MGraph, MEdge, MEField,

  "SpecialFunction",
    MakeMultigraph,

  "Function",
    MVertexList, MEdgeList, MVertexCount, MVertexIndex,
    IVertexList, IEdgeList, MEdgeCount,
    OEdgeInputs, IEdgeTypes,
    MEdgeInputs, MEdgeRecords, MEdgeOutputs, MEdgeFields,
    IEdgeInputs, IEdgeRecords, IEdgeOutputs, IEdgeFields,
    MEdgeNames,  MEdgeNameIndex, MEdgeArities, MEdgeTypes,
    MFieldList,  MFieldCount, MFieldIndex,

  "PatternSymbol", MGraphP,
  "PatternHead",   MGraphDataP
];

PrivateExports[
  "Variable", $MGraphDataKeys
];

(**************************************************************************************************)

(* technically we could override VertexIndex so it works on multigraphs *)
(* again we could override EdgeList so it works on multigraphs *)
(* MultigraphMultiedgeArity / MultigraphEdgeArity *)

(**************************************************************************************************)

DefineAliasRules[
  MGraph           -> Multigraph,
  MEdge            -> Multiedge
];

DefineAliasRules[
  MVertexList       -> MultigraphVertexList,
  IVertexList       -> IntegerVertexList,
  MVertexCount      -> MultigraphVertexCount,
  MVertexIndex      -> MultigraphVertexIndex,
  MEdgeList         -> MultiedgeList,
  IEdgeList         -> IntegerMultiedgeList,
  MEdgeCount        -> MultiedgeCount,
  MEdgeNames        -> MultiedgeNameList,
  MEdgeNameIndex    -> MultiedgeNameIndex,
  OEdgeInputs       -> MultiedgeInputs,
  MEdgeInputs       -> MultiedgeInputLists,
  MEdgeTypes        -> MultiedgeTypes,
  IEdgeInputs       -> IntegerMultiedgeInputLists,
  MEdgeRecords      -> MultiedgeInputRecords,
  IEdgeRecords      -> IntegerMultiedgeInputRecords,
  MEdgeOutputs      -> MultiedgeOutputs,
  IEdgeOutputs      -> IntegerMultiedgeOutputs,
  MEdgeFields       -> MultiedgeFields,
  IEdgeFields       -> IntegerMultiedgeFields,
  IEdgeTypes        -> IntegerMultiedgeTypes,
  MEdgeArities      -> MultiedgeArity,
  MFieldList        -> MultigraphFieldList,
  MFieldCount       -> MultigraphFieldCount,
  MFieldIndex       -> MultigraphFieldIndex
];

DefinePatternRules[
  MGraphP -> HoldP[Multigraph[_InternalData] ? SealedQ]
];

PatternMacroDefs[
  MGraphDataP[sym_] := HoldP[Multigraph[InternalData[sym]] ? SealedQ]
];

(**************************************************************************************************)

$MGraphDataKeys = {
  MVertexList, MEdgeNames, MFieldList, MEdgeArities, MVertexIndex,
  MEdgeNameIndex, MFieldIndex, IEdgeInputs, IEdgeOutputs,
  IEdgeFields, IEdgeTypes
};

General::notMultigraph1 = "First argument should be a Multigraph: ``.";
setupSimpleMGraphFn[fn_] := Then[
  fn[other_]             := ErrorMsg[fn::notMultigraph1, other],
  fn[MGraphDataP[data_]] := Lookup[data, fn]

];
Scan[setupSimpleMGraphFn, $MGraphDataKeys];

selectMultiedges[mg_MGraph, pred_] := selectMultiedges[MEdgeList @ mg, pred];
selectMultiedges[edges_List, pred_] := Select[edges, toPredicate @ pred];

toPredicate = CaseOf[
  MultiedgeArity -> n_Int := MultiedgeArity /* EqualTo[n];
  m_Multiedge             := MatchQ[m];
  MultiedgesTo[v_]        := P3 /* SameAs[v];
  MultiedgesFrom[s_List]  := P1 /* Args /* Supply1[s] /* SameSetQ;
];

MEdgeNames[mg_MGraph, pred_] := Col3 @ selectMultiedges[mg, pred];

(**************************************************************************************************)

MultiedgeOutputGroups[mg_MGraph] := GroupBy[MEdgeList[mg], Second];

(**************************************************************************************************)

SetStrict @ MEdgeFields;

(* TODO: return Key as a wrapper *)
MEdgeFields[MGraphDataP[data_]] := getEdgeFields[data];

getEdgeFields[data_, v_] := toFieldList[data @ MFieldList] @ getVertex[data, v];
getEdgeFields[data_] := Map[toFieldList[data @ MFieldList], data @ IEdgeFields];
toFieldList[fs_][i_Int]  := Range @ i;
toFieldList[fs_][l_List] := Part[fs, l];

(**************************************************************************************************)

SetStrict[MVertexCount, MEdgeCount, MFieldCount];

MVertexCount[MGraphDataP[data_]]  := Len @ data[VertexList];
MEdgeCount[MGraphDataP[data_]]    := Len @ data[IEdgeInputs];
MFieldCount[MGraphDataP[data_]]   := Total @ data[MEdgeArities];

(**************************************************************************************************)

OEdgeInputs[MGraphDataP[data_], v_] := getVertex[v]
OEdgeInputs[MGraphDataP[data_]]     := getOrigInputs @ data;
MEdgeInputs[MGraphDataP[data_]]     := getInputs @ data;
MEdgeOutputs[MGraphDataP[data_]]    := getOutputs @ data;
MEdgeRecords[MGraphDataP[data_]]    := ZipMap[DictThread, getEdgeFields @ data, getInputs @ data];
MEdgeTypes[MGraphDataP[data_]]      := getTypes @ data;

getOrigInputs[data_] := ZipMap[toListOrDict[data @ MFieldList], data @ IEdgeFields, getInputs @ data];
getInputs[data_]     := Parts[data @ MVertexList, data @ IEdgeInputs];
getOutputs[data_]    := Part[data @ MVertexList, data @ IEdgeOutputs];
getTypes[data_]      := Part[{Ordered, Keyed}, data @ IEdgeTypes];

(**************************************************************************************************)

IEdgeList[MGraphDataP[data_]] :=
  ZipMap[Multiedge, data @ IEdgeInputs, data @ IEdgeOutputs, RangeLen @ data @ IEdgeInputs];

MultiedgeList[MGraphDataP[data_]] :=
  ZipMap[Multiedge, getOrigInputs @ data, getOutputs @ data, data @ MEdgeNames];

MultiedgeList[mg_MGraph, pred_] := selectMultiedges[mg, pred];

toListOrDict[_][_, ins_List] := ins;
toListOrDict[fields_][fieldIndices_List, ins_List] := DictThread[Part[fields, fieldIndices], ins];

MultigraphNullaryGroups[mg_MGraph] := Select[MEdgeList[mg], nullaryEdgeQ];
nullaryEdgeQ[Multiedge[EmptyP, _, _]] := True;
nullaryEdgeQ[_] := False;

(**************************************************************************************************)

Scan[fn |-> SetDelayed[fn[MGraphDataP[dict_]], dict[fn]], $multigraphPropertyFunctions];

(**************************************************************************************************)

SetHoldC[evalMultigraph];

m_Multigraph ? UnsealedQ := evalMultigraph[m];

evalMultigraph = CaseOf[
  m:Multigraph[_InternalData]        := HSetNoEntryFlag[m];
  Multigraph[multiedges_List]        := MakeMultigraph[Multigraph, Auto, multiedges];
  Multigraph[vertices_, multiedges_] := MakeMultigraph[Multigraph, vertices, multiedges];
  m_                                 := ErrorMsg[Multigraph::invalidMultigraphSpec, HoldForm[m]]
];

(**************************************************************************************************)

MultigraphEdgeColor = CaseOf[
  Multiedge[_, o_]    := $ @ o;
  Multiedge[_, _, n_] := $ @ n;
  e_                  := MultigraphVertexColor @ e;
];

MultigraphVertexColor = CaseOf[
  Style[_, c:ColorP] := c;
  Keyed[_, c:ColorP] := c;
  Keyed[_, _]        := $Gray;
  c:ColorP           := c;
  other_             := If[$MultiedgeAutoColoring, autoColor @ other, $Gray];
];

autoColor = CaseOf[
  False       := Black;
  True        := Gray;
  i_Int       := PartOr[$MediumColorPalette, i + 1, $Gray];
  e_          := HashToColor @ Hash @ e;
];

