SystemExports[
  "Function",        SyntaxMultigraph
];

PackageExports[
  "SpecialFunction", MakeMultigraph
];

(**************************************************************************************************)

General::badMultigraphVertexSpec = "First argument should be Automatic or a list of vertices.";
General::badMultiedgeSpec = "Second argument should be a list of multiedges.";
General::duplicateMultigraphVertices = "Vertex list contained duplicates.";
General::duplicateMultiedges = "Multiple multiedges used the name ``.";
General::invalidMultigraphSpec = "Not a valid Multigraph construction: ``.";

(**************************************************************************************************)

Options[MakeMultigraph] = {
  "EdgeParser" -> Auto
};

MakeMultigraph[head_Symbol, vertices2_, multiedges_, opts___Rule] := Locals @ CatchMessages[head,

  If[!ListQ[multiedges], ThrowMsg["badMultiedgeSpec"]];
  vertices = vertices2;
  SetAuto[vertices, {}];
  If[!ListQ[vertices], ThrowMsg["badMultigraphVertexSpec"]];

  UnpackOptionsAs[MakeMultigraph, opts, edgeParser];
  SetAuto[edgeParser, parseEdge];

  $vertexNameIndex = DictRange @ vertices; (* dict of vertex names to integer index *)
  If[Len[$vertexNameIndex] < Len[vertices], ThrowMsg["duplicateMultigraphVertices"]];
  $parseVertexName = KeyIndex[$vertexNameIndex];

  $fieldNameIndex = Dict[]; (* <- dict of non-unique field name to integer index *)
  $parseFieldName = KeyIndex[$fieldNameIndex];

  $edgeNameIndex = Dict[]; (* <- dict of unique edge names to integer index *)
  $parseEdgeName = KeyIndexUnique[$edgeNameIndex, ThrowMsg["duplicateMultiedges", #]&];

  Collecting[{
    $meInputs,  (* edge-ordered list of {vindex...} *)
    $meOutputs, (* edge-ordered list of vindex *)
    $meFields,  (* edge-ordered list of {findex..} or fcount (for lists) *)
    $meTypes    (* edge-ordered list of 1 (ordered) or 2 (keyed) *)
    },
    edgeParser /@ multiedges;
  ];

  {vertexNames, fieldNames, edgeNames} = Keys /@
  {$vertexNameIndex, $fieldNameIndex, $edgeNameIndex};

  data = Dict[
    MVertexList    -> vertexNames,
    MEdgeNames     -> edgeNames,
    MFieldList     -> fieldNames,
    MEdgeArities   -> Map[Len, $meInputs],
    MVertexIndex   -> $vertexNameIndex,
    MEdgeNameIndex -> $edgeNameIndex,
    MFieldIndex    -> $fieldNameIndex,
    IEdgeInputs    -> $meInputs,
    IEdgeOutputs   -> $meOutputs,
    IEdgeFields    -> $meFields,
    IEdgeTypes     -> $meTypes
  ];

  SameSetQOrThrow[Keys @ data, $MGraphDataKeys, "badMultigraphKeys"];
  ConstructNoEntryExpr[Multigraph, InternalData @ data]
];

General::badMultigraphKeys = "An internal error occurred. The following internal keys were unexpected `` and missing: ``.";

(**************************************************************************************************)

DeclareStrict[SyntaxMultigraph]

SyntaxMultigraph[medges_List] := MakeMultigraph[
  SyntaxMultigraph,
  Auto, medges,
  "EdgeParser" -> parseSyntaxEdge
];

toListOrDictInputs = CaseOf[
  rules__Rule              := Dict[rules];
  patts:Repeated[_Pattern] := Dict @ MapApply[Rule, {patts}];
  ins___                   := List[ins];
];

parseSyntaxEdge = CaseOf[
  Rule[head_Sym, out_]        := parseEdge @ Multiedge[{}, out, head];
  Rule[head_Sym[in___], out_] := parseEdge @ Multiedge[toListOrDictInputs[in], out, checkInert @ head];
  other_                      := ThrowMsg["invalidSyntaxEdge", other];
];

SyntaxMultigraph::invalidSyntaxEdge = "Expected an expression of the form head[inputs...] -> output, not ``.";

checkInert[sym_Sym] := If[
  InertUserSymbolQ[sym], sym,
  ThrowMsg["invalidSyntaxHead", sym]
];

SyntaxMultigraph::invalidSyntaxHead = "The symbol `` is not an inert user symbol.";

(**************************************************************************************************)

parseEdge = CaseOf[
  ins_List -> out_ := $ @ Multiedge[ins, out];
  Multiedge[inputs_, output_, name_:Auto] := Then[
    parseInputs @ inputs,
    parseOutput @ output,
    parseEdgeName @ name
  ];
  spec_ := ThrowMsg["badMultiedgeSpec", spec];
];
Multigraph::badMultiedgeSpec = "Not a valid multiedge: ``.";

parseInputs = CaseOf[
  ins_List := Then[$meTypes[1], $meFields @ Len @ ins,                         $meInputs @ Map[$parseVertexName] @ ins];
  ins_Dict := Then[$meTypes[2], $meFields @ Map[$parseFieldName] @ Keys @ ins, $meInputs @ Map[$parseVertexName] @ Vals @ ins];
  spec_    := ThrowMsg["badMultiedgeInputs", spec];
];

Multigraph::badMultiedgeInputs = "Not a valid multiedge input spec: ``.";

parseOutput[out_]     := $meOutputs    @ $parseVertexName @ out;
parseEdgeName[name_]  := $meEdgeNames  @ $parseEdgeName   @ name;
