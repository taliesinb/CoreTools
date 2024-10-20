SystemExports[
  "Head",
    NArray,
    NShape,
    NArrayType,
    FormalSum,
    GradedSum,
  "Function",
    Multiproduct,
    GradedThread,
    GradedApply,
    NArrayPlus,
    NArrayData
];

PrivateExports[
  "Function",
    NArrayDims,
  "SpecialFunction",
    MakeNArray,
  "Head",
    NArrayT,
  "SpecialFunction",
    MakeNArray,
  "PatternSymbol",
    NArrayP, NArrayTypeP,
  "PatternHead",
    NArrayDataP
];

(**************************************************************************************************)

DefineAliasRules[
  NArrayT -> NArrayType
];

(**************************************************************************************************)

DefinePatternRules[
  NArrayP     -> HoldP[NArray[_, _, _] ? SealedQ],
  NArrayTypeP -> HoldP[NArrayType[_, _, _]]
];

PatternMacroDefs[
  NArrayDataP[dsym_, tsym_] := HoldP[NArray[InternalData[dsym, tsym]] ? SealedQ]
];

(**************************************************************************************************)

MakeNArray[head_Sym, data_, type_:Auto] := CatchMessages[head,
  MakeSealed[NArray, data, toNArrayType[type, data], None]
];

toNArrayType = CaseOf[
  $[type_NArrayTypeP, _]       := type;
  $[type:ScalarDomainP, data_] := NArrayType[Dims @ data, type, SemiringFor @ type];
  $[Auto, data_]               := procArrayType @ FindArrayType @ data;
];

procArrayType = CaseOf[
  (SparseArraysOf|PackedArraysOf|ArraysOf)[type_, dims_] := NArrayType[dims, type, SemiringFor @ type];
];

(**************************************************************************************************)

CoreBox[na_NArray ? SealedQ] := nArrayBoxes @ na;

nArrayBoxes[_] := FailEval;

nArrayBoxes[HoldP @ NArray[data_, nat:NArrayType[dims_List, type_, _], _]] := Locals[
  ClickBox[
    TooltipBox[ToBoxes @ ArraysOf[type, dims], ToBoxes @ nat],
    Print @ MatrixForm @ data
  ]
];

(**************************************************************************************************)

NArrayData[Broadcast[data_]] := data;
NArrayData[HoldP @ NArray[data_, _, _]] := data;
NArrayData[_] := $Failed;

Dimensions[na_NArray] ^:= NArrayDims @ na;
NArrayDims[HoldP @ NArray[_, NArrayType[dims_List, _, _], _]] := dims;
NArrayDims[_] := $Failed;

(**************************************************************************************************)

CoreBox[GradedSum[dict_Dict]] := formalSumBoxes @ MapValues[ToBoxes, dict];

formalSumBoxes[{}]    := NiceObjectBoxes["GradedSum", {}, .2];
formalSumBoxes[list_] := RiffRowBox[$formalPlus][list];

$formalPlus := $formalPlus = MarginBox[.5] @ StyleBox["+", FontFamily -> "Roboto", FontSize -> (2 + Inherited), FontColor -> Orange];

NArray /: FormalSum[a_NArray, b___] := NArrayPlus[a, b];

(**************************************************************************************************)

SetStrict @ SetHoldR[registerOps];

registerOps[ops:{__Sym}, defs__SetD] := Scan[op |-> registerOps[op, defs], ops];
registerOps[op_Sym, defs__SetD]      := HoldScan[def |-> registerOps[op, def], defs];
registerOps[op_Sym, SetD[lhs_, rhs_]] := With[
  {hold = MakeTagSetD[NArray, lhs, rhs, Hold] /. $ -> op},
  ReleaseHold @ hold;
  ReleaseHold @ ReplaceAll[hold, NArray -> GradedSum];
];

(**************************************************************************************************)

toArgsDict[args_List, casting_:Flat | True | False] := Locals[
  $casting = casting; $scalars = {};
  dict = Merge[toGradeRules /@ args, Id];
  If[$scalars === {}, dict,
    KeyValueMap[inputs |-> Join[inputs, $scalars], dict]]
];

(* broadcast must ??? *)
toGradeRules = CaseOf[
  d:DatumP         := $ @ Broadcast[r];
  a_NArray         := Rule[NArrayDims @ a, a];
  GradedSum[dict_] := Splice @ Normal @ dict;
  b_Broadcast      := Switch[$casting,
    Flat,  NArray @ First @ b; Nothing,
    True,  NArray @ First @ b,
    False, ThrowMsg["noBroadcasting"]
  ];
  spec_ := ThrowMsg["invalidOperand", spec];
];

General::noBroadcasting = "Broadcasting not allowed.";
General::invalidOperand = "Can't handle operand ``.";

fromGradeRules = CaseOf[
  d_ ? SingleQ := Part[d, 1, 2];
  d_           := GradedSum[Dict @ d];
];

checkRes[grade_, Nothing]      := Nothing;
checkRes[grade_, array_NArray] := Rule[grade, array];
checkRes[grade_, _]            := ThrowRawException[];

(**************************************************************************************************)

NArrayPlus[arg_]   := arg;
NArrayPlus[args__] := iNArrayPlus[{args}];

iNArrayPlus[args:{__NArray}] := MaybeEval @ Module[
  {grades = NArrayDims /@ args},
  If[!AllSameQ[grades], FailEval, NArray @ Total @ Map[NArrayData] @ args]
];

iNArrayPlus[args_] := GradedThread[Plus, args]

(**************************************************************************************************)

GradedThread[fn_, args_List] := CatchMessages[NArray,
  $numArgs = Len @ args;
  isFlat = ListableFunctionQ[fn];
  implFn = If[isFlat, listableImpl[fn], threadImpl[fn, Len @ args]];
  argsDict = toArgsDict[args, If[isFlat, Flat, True]];
  fromGradeRules @ KeyValueMap[
    {grade, arrays} |-> checkRes[grade, implFn[grade, arrays]],
    argsDict
  ]
];

$threadFunctions = {Plus, Times, And, Or, Join, Inter, Union, ThreadMin, ThreadMax};

registerOps[$threadFunctions, $[l_NArray, r__] := GradedThread[$, FlatList[l, r]]];

listableImpl[fn_][grade_, arrays_] := If[SingleQ[arrays], P1 @ arrays,
  NArray[Apply[fn, NArrayData /@ arrays], grade]
];

threadImpl[fn_, n_][grade_, arrays_] := If[Len[arrays] =!= n, Nothing,
  NArray[MapThread[fn, NArrayData /@ arrays, Len @ grade], grade]
];

(**************************************************************************************************)

(* GradedApply treats args as a list of arrays, *)
GradedApply::usage =
"GradedApply[fn$, arg$1, arg$2, $$] applies fn$ to a sequence of arguments.
* if any of the arguments are GradedSums, fn$ will be applied to all combinations of graded elements.
* such multiplicity of results will be combined again into another GradedSum.
* fn$ can return 0, such results will be discarded.
"

GradedApply[fn_, args__] := CatchMessages[NArray,
  impl = toImplFn[head, op];
  inputsDict = Merge[toGradeRules /@ args, Id];
  If[$bcast =!= {}, inputsDict //= Map[inputs |-> Join[inputs, $bcast]]];
  {inputsDict, impl}

  {inputsDict, prodFn} = toImplData[GradedApply, op, $multiargs$[args], False];
  If[PairQ[prodFn], {prodFn, sumFn} = prodFn, sumFn = NArrayPlus];
  fromGradeRules @ Outer[gradeApply[impl], inputsDict]
];

registerOps[
  {NonCommutativeMultiply},
  $[l___, m_NArray, r___] := GradedApply[$, {l, a, r}]
];

threadImplementation[Dot] := threadDot;

threadDot[{{l___, m_}, {m_, r___}}, {a1_, a2_}] := NArray[Dot[a1, a2], {l, r}];

(**************************************************************************************************)

Multiproduct[GradedSum[dict1_], GradedSum[dict2_]] :=
  FormalSum @ Outer[Multiproduct, Vals @ dict1, Vals @ dict2];

Multiproduct[a_GradedSum, b_NArray] := Multiproduct[a, GradedSum[<|{} -> b|>]];
Multiproduct[a_NArray, b_GradedSum] := Multiproduct[GradedSum[<|{} -> a|>], b];

Multiproduct[HoldP @ NArray[adata_, ashape_], HoldP @ NArray[bdata_, bshape_]] := Locals[
  aout = Last @ ashape;
  bins = Most @ bshape;
  matchingAxes = VectorIndicesOf[bins, aout];
  FormalSum @ Map[axis |-> NArray[DotAxis[adata, -1, axis, bdata]], matchingAxes]
];

(* TODO: ** NonCommutativeMultiply should pass through straight via DotAxis, becoming
word-forming or using the underlying monoid *)

(**************************************************************************************************)

SetHoldC[evalNArray];

m_NArray ? UnsealedQ := evalNArray[m];

evalNArray = CaseOf[
  m:NArray[_, NArrayTypeP, _]        := HSetNoEntryFlag[m];
  NArray[data_]                      := MakeNArray[NArray, ToPacked @ data];
  NArray[data_, type_]               := MakeNArray[NArray, ToPacked @ data, type];
  m_                                 := ErrorMsg[NArray::invalidNArraySpec, HoldForm[m]]
];

NArray::invalidNArraySpec = "Not a valid NArray construction: ``.";

