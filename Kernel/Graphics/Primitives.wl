SystemExports[
  "GraphicsBoxFunction", ToGraphicsBoxes, ToGraphics3DBoxes
];

PrivateExports[
  "MetaFunction", DefineGPrim, DefineGPrimSig,
  "Predicate",    GPrimSymQ, GBoxSymQ,
  "IOFunction",   MakeCoreGBoxes, GSigToGPrims, GSigToGBoxes,
  "GraphicsBoxFunction", EmptyRectangleBox, EmptyPolygonBox,
  "MutatingFunction", BlockGStyle,
  "SpecialVariable",
    $GStyle, $GDim,
    $GPrimFns,
    $GPrimSyms, $GBoxSyms,
    $GPrimToGBoxes, $GPrimToGSigs, $GSigToGPrims, $GSig1ToGPrims,
    $GBoxToGPrim,   $GBoxToGSigs,  $GSigToGBoxes, $GSig1ToGBoxes,
  "TagSymbol",
    PrimPos, PrimPosPair, PrimPosDelta, PrimPosList, PrimPosLists, PrimRadius, PrimOpaque, PrimPrimitives, PrimCurve, PrimColor, PrimPosRules
];

(**************************************************************************************************)

ToGraphicsBoxes[{}]   := {};
ToGraphicsBoxes[e_]   := Typeset`MakeBoxes[e, StandardForm, Graphics] //. $gboxSimpRules;
ToGraphics3DBoxes[e_] := Typeset`MakeBoxes[e, StandardForm, Graphics3D] //. $gboxSimpRules;

$gboxSimpRules = {InterpretationBox[b_, _] :> b, Typeset`Hold[h_] :> h};

(**************************************************************************************************)

EmptyRectangleBox[{x1_, y1_}, {x2_, y2_}] := Make[JoinedCurveBox,
  List @ Line @ ToPacked @ {{x1, y1}, {x2, y1}, {x2, y2}, {x1, y2}},
  CurveClosed -> True
];

EmptyPolygonBox[p_] := Make[JoinedCurveBox,
  List @ Line @ ToPacked @ p,
  CurveClosed -> True
];

(**************************************************************************************************)

Initially[
  $GDim = 2;
  $GStyle = UDict[];
  $GPrimFns =  UDict[];
  $GPrimSyms = $GBoxSyms = {};
  $GPrimToGBoxes = $GPrimToGSigs = $GSigToGPrims = $GSig1ToGPrims = UDict[];
  $GBoxToGPrim = $GBoxToGSigs = $GSigToGBoxes = $GSig1ToGBoxes = UDict[];
];

SetStrict @ DefineGPrim;

DefineGPrim[head_Symbol, signature_, fn_, dims_:{2}] := Then[
  $GPrimFns[head] = fn,
  DefineGPrimSig[signature, head],
  (* declareOptionableHead[head]; *)
  If[MemberQ[dims, 2], Typeset`MakeBoxes[e_head, StandardForm | TraditionalForm, Graphics]   := MakeCoreGBoxes[e]];
  If[MemberQ[dims, 3], Typeset`MakeBoxes[e_head, StandardForm | TraditionalForm, Graphics3D] := Block[{$GDim = 3}, MakeCoreGBoxes[e]]];
];

SetPred1[GPrimSymQ, GBoxSymQ];

(**************************************************************************************************)

SetHoldR @ BlockGStyle;

BlockGStyle[{} | UDict[] | Dict[], body_] := body;
BlockGStyle[rules_, body_]                := BlockAssociate[$GStyle, rules, body];

(**************************************************************************************************)

GStyleValue[head_Sym, key_Sym]   := Lookup[$GStyle, key, OptionValue[head, key]];
GStyleValue[head_Sym, keys_List] := LookupKeys[$GStyle, keys, 1];

(**************************************************************************************************)

SetHoldF @ MakeCoreGBoxes;

General::unrecogprim = "Unrecognized usage of ``: ``.";
General::failprim = "Failed to boxify ``: ``.";
General::internalPrimEror = "Internal error while boxifying `` involving call ``.";

MakeCoreGBoxes[prim_] := With[
  {head = Head @ NoEval @ prim},
  {fn = Lookup[$GPrimFns, head, $Failed&]},
  {res = CatchMessages[head, fn @ prim]},
  Which[
    Head[res] === fn,               gprimErrorMsg[head, prim, "unrecogprim"],
    res === $Failed,                gprimErrorMsg[head, prim, "failprim"],
    True,                           res
  ]
];

SetStrict @ gprimErrorMsg;

gprimErrorMsg[head_, prim_, msg_] := (Message[MessageName[h, msg], prim, args]; {})

(**************************************************************************************************)

DefineGPrimSig::usage =
"DefineGPrimSig[sig$, symbol$] attaches a given signature$ to a graphics primitive symbol$.
DefineGPrimSig[sig$, s$1 | s$2 | $$] attaches a signature to multiple symbols.
* sig$ can be a string containing a single signature or multiple separated by '|'.
* each signature is a list of types seperated by ','.
* each type is one of the following:
| 'Pos' | a coordinate vector |
| 'PosPair' | a pair of coordinate vectors |
| 'PosDelta' | a relative offset vector |
| 'PosList' | a list of coordinate vectors |
| 'PosLists' | a list of matrices |
| 'Radius' | a numeric radius in coordinate space |
| 'Opaque' | an opaque value that will be ignored |
| 'Primitives' | a list of graphics primitives |
| 'Curve' | a symbolic curve |
| 'PosRules' | rules from coordinate vectors to opaque objects |
* lookups against the database of signatures can be achieved via %GSigToGPrims."

SetStrict @ DefineGPrimSig;

DefineGPrimSig[sig_, heads_List | heads_Alternatives] :=
  Scan[DefineGPrimSig[sig, #]&, List @@ heads];

$gprimSym = None;
DefineGPrimSig[sig_, primSym_Symbol] := CatchMessages[primSym, Block[
  {$gprimSym, $gboxSyms},
  $gprimSym = primSym;
  setupPrimSym @ primSym;
  $gboxSyms = getBoxSyms @ primSym;
  Map[procGSig, toGSigList @ sig]
]];

getBoxSyms[primSym_Symbol] := Module[
  {primName, box2DName, box3DName, boxSyms},
  primName = SymbolName @ primSym;
  box2DName = StrJoin["System`", primName, "Box"];
  box3DName = StrJoin["System`", primName, "3DBox"];
  boxSyms = Symbol /@ Select[{box2DName, box3DName}, NameQ];
  If[boxSyms =!= {}, $GPrimToGBoxes[primSym] = boxSyms];
  Scan[setupBoxSym, boxSyms];
  boxSyms
];

setupBoxSym[boxSym_] :=
  If[!GBoxSymQ[boxSym],
    AppendTo[$GBoxSyms, boxSym];
    GBoxSymQ[boxSym] = True;
    AssociateTo[$GBoxToGPrim, boxSym -> $gprimSym]
  ];

setupPrimSym[primSym_Symbol] :=
  If[!GPrimSymQ[primSym],
    AppendTo[$GPrimSyms, primSym];
    GPrimSymQ[primSym] = True
  ];

(**************************************************************************************************)

procGSig[str_Str] := Locals[
  sig = parseGSig @ str;
  registerGPrimSym[$gprimSym, sig];
  registerGBoxSyms[$gboxSyms, sig];
  sig
];

General::badprimsig = "Bad signature `` for graphics primitive ``."
procGSig[shape_] := ThrowMsg["badprimsig", shape, $gprimSym];

(**************************************************************************************************)

SetStrict[registerGPrimSym, registerGBoxSyms, registerSyms];

registerGPrimSym[sym_Sym, sig_]   := registerSyms[$GSigToGPrims, $GSig1ToGPrims, $GPrimToGSigs, {sym}, sig];
registerGBoxSyms[syms_List, sig_] := registerSyms[$GSigToGBoxes, $GSig1ToGBoxes, $GBoxToGSigs, syms, sig];
registerGBoxSyms[{}, _] := Null;

SetHoldA @ registerSyms;

registerSyms[sigToSyms_Sym, sig1ToSyms_Sym, symToGSigs_Sym, syms_List, sig_List] := Then[
  KeyUnionTo[sigToSyms, sig, syms],
  ScanP[{sig1, slot} |-> KeyUnionTo[sig1ToSyms, {slot, sig1}, syms]&, sig],
  Scan[sym |-> KeyUnionTo[symToGSigs, sym, {sig}], syms]
];

(**************************************************************************************************)

(* TagBox has no corresponding form, so we register it manually *)
registerGBoxSyms[{TagBox}, {PrimPrimitives}];

(**************************************************************************************************)

parseGSig[str_] := parseGSig[str] = Map[parseSigElem, StrSplit[str, ","]];

General::badprimsigelem = "Bad signature element `` for graphics primitive ``."

parseSigElem = CaseOf[
  "Pos"        := PrimPos;
  "PosPair"    := PrimPosPair;
  "PosDelta"   := PrimPosDelta;
  "PosList"    := PrimPosList;
  "PosLists"   := PrimPosLists;
  "Radius"     := PrimRadius;
  "Opaque"     := PrimOpaque;
  "Primitives" := PrimPrimitives;
  "Curve"      := PrimCurve;
  "Color"      := PrimColor;
  "PosRules"   := PrimPosRules;
  sym_Symbol   := sym;
  e_           := ThrowMsg["badprimsigelem", e, $gprimSym];
];

(**************************************************************************************************)

toGSigList = CaseOf[
  s_Str  := handleOpt /@ StrTrim[StrSplit[s, "|"]];
  other_ := ToList @ other;
]

handleOpt[s_] /; StrContainsQ[s, "?"] := Splice[{StrDelete[s, "?" ~~ ___], StrRep[s, "?"->","]}];
handleOpt[s_] /; StrContainsQ[s, "!"] := Except[StrDelete[s, "!" ~~ ___],  StrRep[s, "!"->","]];
handleOpt[s_] := s;

(**************************************************************************************************)

GSigToGPrims::usage =
"GSigToGPrims[slot$ -> type$1] returns a list of primitive symbols which accept argument type$ at argument position slot$.
GSigToGPrims[{rules$1, rule$2, $$}] applies multiple criteria simultaneously.
GSigToGPrims['sig$'] returns a list of primitives that have exactly the signature sig$ (which can contain multiple specs).
GSigToGPrims[sym$] returns a list of symbolic signatures for primitive sym$."

GSigToGPrims[spec_] := CatchMessages @ iGSigToGPrims @ spec;

iGSigToGPrims = CaseOf[

  pos_Int -> sig_Str :=
    Lookup[$GSig1ToGPrims, Key @ {pos, parseSigElem[sig]}, {}];

  rules:{__Rule} :=
    Inter @@ Map[$, rules];

  $[sig_Str /; StrContainsQ[sig, "|"|"?"|"!"]] :=
    Union @@ Map[$, toGSigList @ sig];

  Verbatim[Except][a_, b_] :=
    Compl[$ @ a, $ @ b];

  sig_Str :=
    Lookup[$GSigToGPrims, Key @ parseGSig @ sig, {}];

  sym_Symbol :=
    Lookup[$GPrimToGSigs, sym, Message[GSigToGPrims::nosym, sym]; {}];
];

GSigToGPrims::nosym = "`` is not a graphics primitive symbol.";

(**************************************************************************************************)

GSigToGBoxes::usage =
"GSigToGBoxes is like %GSigToGPrims but returns graphics primitive boxes."

GSigToGBoxes[spec_] := Block[
  {$GSigToGPrims = $GSigToGBoxes, $GSig1ToGPrims = $GSig1ToGBoxes, $GPrimToGSigs = $GBoxToGSigs},
  GSigToGPrims @ spec
];

