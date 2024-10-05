PackageExports[
  "Function",
    FindArrayType,
    FindExprType,
    RandomArray, RandomElement, RandomLength,
    SparseArrayDomain,

  "TypeSymbol",
    Strings, Symbols, Numbers, Probabilities, NegativeLogProbabilities, Expressions, Datums, Atoms,
    ExtendedReals, ExtendedNonNegativeReals,
    ExtendedIntegers, ExtendedNonNegativeIntegers,
    ExtendedNumbers, Lists, Trees,
  "TypeSymbol",
    Probs, NegLogProbs,
    Exprs, Nats, Nums, Ints, ExtNums, Bools,
    NegInts, NonPosInts, NonNegInts, PosInts,
    NegReals, NonPosReals, NonNegReals, PosReals,
    ExtReals, ExtInts, ExtNonNegReals, ExtNonNegInts,
  "TypeHead",
    ArraysOf, PackedArraysOf, SparseArraysOf, NumericArraysOf,
    ListsOf, TuplesOf, RecordsOf, DictsOf,
    RulesOf, OptionalOf, HeadsOf, AnyOf,
    InductiveTypeOf,
  "Predicate",
    DimVecQ,
    ScalarDomainQ, ArrayDomainQ, CompoundDomainQ, DomainQ,
  "PatternSymbol",
    ScalarDomainP, ArrayDomainP, CompoundDomainP, DomainP,
  "BoxFunction", DomainLetterBox,
  "Option", ListLength
];

PackageExports[
  "Function",
    RandLen, RandArr, Rand
];

PrivateExports[
  "PatternSymbol",
    ArrayObjP, DimListP,
    IntDomainP,     BoundedIntDomainP,    ExtIntDomainP,
    RealDomainP,    BoundedRealDomainP,   ExtRealDomainP,
    NumberDomainP,  BoundedNumberDomainP, ExtNumberDomainP,
    PackingDomainP,
    NonNumberDomainP,
  "SymbolicHead",
    Semiring,
  "Function",
    SemiringFor
];

(*************************************************************************************************)

(* TODO: Investigate use of DimsTree here *)

(*************************************************************************************************)

DefinePatternRules[
  ArrayObjP -> Alt[_NumericArray, _SparseArray]
];

DefineAliasRules[
  RandLen     -> RandomLength,
  RandArr     -> RandomArray,
  Rand        -> RandomElement,
  Exprs       -> Expressions,
  Probs       -> Probabilities,
  Bools       -> Booleans,
  NegLogProbs -> NegativeLogProbabilities
];

DefineAliasRules[
  Ints           -> Integers,
  Nums           -> Numbers,
  Nats           -> NonNegativeIntegers,
  NegInts        -> NegativeIntegers,
  NonPosInts     -> NonPositiveIntegers,
  NonNegInts     -> NonNegativeIntegers,
  PosInts        -> PositiveIntegers,
  NegReals       -> NegativeReals,
  NonPosReals    -> NonPositiveReals,
  NonNegReals    -> NonNegativeReals,
  PosReals       -> PositiveReals,
  ExtReals       -> ExtendedReals,
  ExtInts        -> ExtendedIntegers,
  ExtNonNegReals -> ExtendedNonNegativeReals,
  ExtNonNegInts  -> ExtendedNonNegativeIntegers,
  ExtNums        -> ExtendedNumbers
];

DefinePatternRules[

  BoundedIntDomainP    -> Alt[NegInts, NonPosInts, NonNegInts, PosInts],
  BoundedRealDomainP   -> Alt[NegReals, NonPosReals, NonNegReals, PosReals],
  BoundedNumberDomainP -> Alt[BoundedIntDomainP, BoundedRealDomainP],

  IntDomainP           -> Alt[Ints,  BoundedIntDomainP],
  RealDomainP          -> Alt[Reals, BoundedRealDomainP],

  ExtIntDomainP        -> Alt[ExtInts, ExtNonNegInts],
  ExtRealDomainP       -> Alt[ExtReals, ExtNonNegReals],
  ExtNumberDomainP     -> Alt[ExtNums, ExtRealDomainP, ExtIntDomainP],

  PackingDomainP       -> Alt[Integers, Reals, Complexes],

  NumberDomainP        -> Alt[Integers, Reals, Rationals, Complexes, Algebraics, Numbers, BoundedNumberDomainP, ExtNumberDomainP],
  NonNumberDomainP     -> Alt[Strings, Symbols, Booleans, Datums, Atoms, Trees],

  ScalarDomainP        -> Union @ Flatten @ Alt[NumberDomainP, NonNumberDomainP]
];

DefinePatternRules[
  DimListP            -> _List ? PositiveIntegerVectorQ,
  ArrayDomainP        -> ToBlankP[{ArraysOf, PackedArraysOf, SparseArraysOf, NumericArraysOf}] ? ArrayDomainQ,
  CompoundDomainP     -> ToBlankP[{ListsOf, TuplesOf, RecordsOf, DictsOf, RulesOf, OptionalOf, AnyOf, HeadsOf}] ? CompoundDomainQ,
  DomainP             -> _ ? DomainQ
];

(*************************************************************************************************)

SetPred1[DimVecQ, ScalarDomainQ, ArrayDomainQ, CompoundDomainQ, DomainQ]

DimVecQ[e_] := PosIntVecQ[e];

ScalarDomainQ[ScalarDomainP] = True;

ArrayDomainQ = ExtendCaseOf[
  NumericArraysOf[PackingDomainP, DimListP] := True;
  SparseArraysOf[PackingDomainP, DimListP]  := True;
  PackedArraysOf[PackingDomainP, DimListP]  := True;
  ArraysOf[DomainP, DimListP]               := True;
];

(* TODO: IsMatchOf *)
(* TODO: IsNotMatchOf *)

DomainQ = ExtendCaseOf[
  ScalarDomainP   := True;
  ArrayDomainP    := True;
  CompoundDomainP := True;
  Lists           := True;
];

CompoundDomainQ = ExtendCaseOf[
  ListsOf[DomainP]              := True;
  OptionalOf[DomainP]           := True;
  HeadsOf[_Symbol]              := True;
  TuplesOf[{DomainP..}]         := True;
  RecordsOf[_List, {DomainP..}] := True;
  RulesOf[DomainP, DomainP]     := True;
  DictsOf[DomainP, DomainP]     := True;
  AnyOf[{DomainP..}]            := True;
];

(*************************************************************************************************)

CoreBox[d_NumericArraysOf ? DomainQ] := arrayDomainBoxes @ d;
CoreBox[d_SparseArraysOf ? DomainQ]  := arrayDomainBoxes @ d;
CoreBox[d_PackedArraysOf ? DomainQ]  := arrayDomainBoxes @ d;
CoreBox[d_ArraysOf ? DomainQ]        := arrayDomainBoxes @ d;

(*************************************************************************************************)

$domainLetterDict = Dict[
  Nats                -> "\[DoubleStruckCapitalN]",
  Probs               -> "\[DoubleStruckCapitalP]",
  Rationals           -> "\[DoubleStruckCapitalQ]",
  Integers            -> "\[DoubleStruckCapitalZ]",
  Reals               -> "\[DoubleStruckCapitalR]",
  Strings             -> "\[DoubleStruckCapitalS]",
  Booleans            -> "\[DoubleStruckCapitalB]",
  Lists               -> "\[DoubleStruckCapitalL]",
  Trees               -> "\[DoubleStruckCapitalT]"
];

BlockUnprotect[{NonNegativeIntegers, Reals, Ints, Rationals, Booleans},
  FormatValues[NonNegativeIntegers] = {};
  FormatValues[Reals] = {};
  FormatValues[Integers] = {};
  FormatValues[Rationals] = {};
  FormatValues[Booleans] = {};
  KeyValueMap[{sym, letter} |->
    SetD[SystemBox[sym], MarginBox[DomainLetterBox[letter], {.1,.1},{.1,.1}]],
    $domainLetterDict
  ]
];

DomainLetterBox[letter_, opts___Rule] :=
  StyleBox[letter,
    opts,
    FontWeight -> "SemiBold", FontSize -> Inherited + 2, ShowSyntaxStyles -> False,
    FontFamily -> "Source Code Pro"
  ];

(*************************************************************************************************)

arrayDomainBoxes = CaseOf[
  expr:(_[s:ScalarDomainP, dims_]) := NiceTooltipBox[BlockFormatting @ ToBoxes @ expr] @ StyleBox[
    FontSizeDeltaBox[5] @ SuperscriptBox[ToBoxes @ s, dimsBox @ dims],
    ScriptSizeMultipliers -> 0.65,
    ScriptBaselineShifts -> {0.5, 0.5}
  ];
  _ := $Failed
];

dimsBox[dims_List] := CodeFontBox @ TightBox @ RiffRowBox[$dimTimesBox][NatStr /@ dims]

$dimTimesBox = FontColorBox[.5] @ "\[Times]";

(*************************************************************************************************)

SemiringFor = CaseOf[
  IntDomainP    | ExtIntDomainP     := Semiring[Times,   1,     Plus,     0];
  RealDomainP   | ExtRealDomainP    := Semiring[Times,   1.0,   Plus,     0.0];
  NumberDomainP | ExtNumberDomainP  := Semiring[Times,   1,     Plus,     0];
  Booleans                          := Semiring[And,     True,  Or,       False];
  Strings                           := Semiring[FormalFn[StrJoin], "", FPlus, FPlus[]];
  _ListsOf                          := Semiring[FormalFn[Join],    {}, FPlus, FPlus[]];
  _                                 := Semiring[FTimes, FTimes[], FPlus, FPlus[]];
];

(*************************************************************************************************)

SetStrict[RandomArray, RandomElement]

$randLen = 5.0;

Options[RandomArray] = Options[RandomElement] = {
  ListLength -> $randLen
};

RandomArray[type_, shape:NatP...] := RandomArray[type, {shape}];
RandomArray[type_, shape:NatVecP] := RandomArray[ArraysOf[type, shape]];
RandomArray[type_ ? ArrayDomainQ] := CatchMessages @ iRandArray @ type;

RandomElement[type_ ? DomainQ]           := CatchMessages @ iRandElem @ type;
RandomElement[type_ ? DomainQ, num:NatP] := CatchMessages @ iRandArray @ ArraysOf[type, num];

RandomArray[args___, ListLength -> len_]   := BlockSet[$randLen, len, RandomArray @ args];
RandomElement[args___, ListLength -> len_] := BlockSet[$randLen, len, RandomElement @ args];

(* TODO: allow for *asking* for unpacked arrays *)

iRandArray[e_] := iRandArray1[e /. PackedArraysOf -> ArraysOf];

iRandArray1 = CaseOf[
  NumericArraysOf[t_, d_]          := NumericArray @ $ @ ArraysOf[d, t];
  ArraysOf[ArraysOf[t_, d1_], d2_] := $ @ ArraysOf[t, Join[d2, d1]];
  ArraysOf[t:PackingDomainP, d_]   := ToPacked @ $typeToRandFn[t] @ d;
  ArraysOf[t:ScalarDomainP, d_]    := $typeToRandFn[t] @ d;
  ArraysOf[t_, d_]                 := ArrayTable[iRandElem @ t, d];
];

(* TODO: introduce clipping to ensure we hit zero for PositiveReals etc *)
$typeToRandFn = UDict[
  Exprs               -> RandomDatum,
  Reals               -> RandomUnitReal,
  Booleans            -> RandomBoolean,
  Probs               -> RandomUnitReal,
  NegLogProbs         -> RandomUnitReal /* Minus /* Log,
  Integers            -> Supply1[RandomInteger, {-9, 9}],
  NegativeIntegers    -> Supply1[RandomInteger, {-9,-1}],
  NonPositiveIntegers -> Supply1[RandomInteger, {-9, 0}],
  NonNegativeIntegers -> Supply1[RandomInteger, { 0, 9}],
  PositiveIntegers    -> Supply1[RandomInteger, { 1, 9}],
  NegativeReals       -> Supply1[RandomReal,    {-9,-1}],
  NonPositiveReals    -> Supply1[RandomReal,    {-9, 0}],
  NonNegativeReals    -> Supply1[RandomReal,    { 0, 9}],
  PositiveReals       -> Supply1[RandomReal,    { 1, 9}],
  Rationals           -> Function[shape, RandRange[-9,9, shape] / RandRange[1,100,shape]],
  Complexes           -> Function[shape, RandomComplex[1+I, shape]],
  Algebraics          -> Function[shape, Sqrt @ RandRange[3,99, shape]],
  Datums              -> RandomDatum,
  Atoms               -> RandomAtom,
  Numbers             -> RandomUnitNormal,
  Strings             -> Supply1[RandomLowercaseString, {3,5}],
  Symbols             -> RandomSymbol
];

RandLen = CaseOf[
  r:RealP    := RandomGeometric @ r;
  b:Nat2P    := RandomInteger @ b;
  i:NatP     := i;
  $[]        := $ @ $randLen
];

decLen[r:RealP] := Ceiling[r/2];
decLen[n_]      := n;

blockDecLen[body_] := Block[{$randLen = decLen @ $randLen}, body];

iRandElem = CaseOf[
  t:ArrayDomainP    := iRandArray @ t;
  ListsOf[t_]       := blockDecLen @ ArrayTable[$ @ t, RandLen[]];
  OptionalOf[t_]    := CoinToss[$ @ t, None];
  TuplesOf[ts_]     := Map[$, ts];
  RecordsOf[k_, v_] := DictThread[k, Map[$, v]];
  RulesOf[kt_, vt_] := Rule[$ @ kt, $ @ vt];
  DictsOf[kt_, vt_] := Locals[
    n = RandLen[];
    blockDecLen @ DictThread[ArrayTable[$ @ kt, n], ArrayTable[$ @ vt, n]]
  ];
  AnyOf[alts_List]  := $ @ RandomChoice @ alts;
  sym_Sym ? ScalarDomainQ := $typeToRandFn[sym][{}];
  _ := Unimplemented
];

(*************************************************************************************************)

FindArrayType[a_] := arrayType @ a;

FindExprType = CaseOf[
  arr:ArrayObjP    := arrayType @ arr;
  arr_List         := arrayType @ arr;
  expr_            := singleType @ expr;
];

(*************************************************************************************************)

arrayType[arr_]  := iArrayType0 @ ToPacked @ arr;

iArrayType0[arr_] := Locals[
  $arrDims = Dims @ arr;
  Append[$arrDims] @ iArrayType1 @ arr
];

iArrayType1 = CaseOf[
  arr_SparseArray    := SparseArraysOf @ SparseArrayDomain @ arr;
  arr_NumericArray   := NumericArraysOf @ $fromNAType @ NumericArrayType @ arr;
  arr_List ? PackedQ := PackedArraysOf @ $fromPAType @ PackedType @ arr;
  arr_List           := ArraysOf @ arrayElemType[arr, Len @ $arrDims];
  e_                 := ArraysOf @ singleType @ e;
];

$naTypesDict = UDict[
  Integers  -> {"Integer8", "UnsignedInteger8", "Integer16", "UnsignedInteger16", "Integer32", "UnsignedInteger32", "Integer64", "UnsignedInteger64"},
  Reals     -> {"Real32", "Real64"},
  Complexes -> {"ComplexReal32", "ComplexReal64"}
];
$fromNAType = UDict @ KeyValueMap[ConstantRules[#2, #1]&, $naTypesDict];
$fromPAType = UDict[Real -> Reals, Complex -> Complexes, Int -> Integers];

(*************************************************************************************************)

SparseArrayDomain[arr_SparseArray] :=
  scalarMeet[commonType @ arr["ExplicitValues"], singleType @ arr["ImplicitValue"]];

(* TODO: LatticeGraph *)
$scalarLattice := $scalarLattice = Graph[{
  Integers -> Numbers, Reals -> Numbers, Rationals -> Numbers,
  NegativeIntegers -> NonPositiveIntegers, NonPositiveIntegers -> Integers,
  PositiveIntegers -> NonNegativeIntegers, NonNegativeIntegers -> Integers,
  NegativeReals -> NonPositiveReals, NonPositiveReals -> Reals,
  PositiveReals -> NonNegativeReals, NonNegativeReals -> Reals,
  Booleans -> Symbols, Numbers -> Datums, Strings -> Datums,
  NegLogProbs -> ExtendedNonNegativeReals, Probs -> NonNegativeReals,
  Reals -> ExtendedReals, Integers -> ExtendedIntegers,
  ExtendedNonNegativeReals -> ExtendedReals, ExtendedNonNegativeIntegers -> ExtendedIntegers,
  ExtendedReals -> ExtendedNumbers, ExtendedIntegers -> ExtendedNumbers, Numbers -> ExtendedNumbers, ExtendedNumbers -> Datums,
  Datums -> Atoms, Symbols -> Atoms
}];

latticeUpset[a_] := latticeUpset[a] = VertexOutComponent[$scalarLattice, a];

scalarMeet[a_, a_] := a;
scalarMeet[a_, b_] := latticeJoin[a, b] = Locals[
  {upA, upB} = latticeUpset /@ {a, b};
  upAB = Inter[upA, upB];
  FirstCase[upA, Alt @@ upAB, Exprs]
];

(*************************************************************************************************)

arrayElemType = CaseOf[
  $[{},      _] := Exprs;
  $[v_List,  1] := commonType[v];
  $[a_List, d_] := commonArrType[a, d];
  $[{a_},   d_] := $[a, d - 1];
];

(*************************************************************************************************)

(* TODO: my VectorQ funcs are insufficiently precise to capture NumberQ vs RealValuedNumericQ vs RealValuedNumberQ *)

commonType = CaseOf[
  {}                 := Exprs;
  _ ? IntegerVectorQ := Integers;
  _ ? RealVectorQ    := Reals;
  _ ? NumberVectorQ  := Numbers;
  _ ? StringVectorQ  := Strings;
  _ ? BooleanVectorQ := Booleans;
  _ ? SymbolVectorQ  := Symbols;
  list_List          := commonCompoundType @ sampleVector @ list;
];

singleType = CaseOf[
  _Integer    := Integers;
  _Real       := Reals;
  _Rational   := Numbers;
  _Complex    := Complexes;
  _String     := Strings;
  BoolP       := Booleans;
  SymP        := Symbols;
  a:ArrayObjP := FindArrayType @ pa;
  l_List      := singleListType @ l;
  d_Dict      := singleDictType @ d;
  h_          := HeadsOf @ Head @ e;
];

(*************************************************************************************************)

commonArrType[array_List, depth_Int] := IfNone[
  commonArrScalarType @ array,
  commonCompoundType @ sampleArray[array, depth]
];

commonArrScalarType = CaseOf[
  {}                := Exprs;
  _ ? IntegerArrayQ := Integers;
  _ ? RealArrayQ    := Reals;
  _ ? NumberArrayQ  := Numbers;
  _ ? StringArrayQ  := Strings;
  _ ? BooleanArrayQ := Booleans;
  _ ? SymbolArrayQ  := Symbols;
  _                 := None;
];


(*************************************************************************************************)

$exprSampleSize = 20;
sampleVector[a_] /; Len[a] < $exprSampleSize := a;
sampleVector[a_] := RandomSample[a, $exprSampleSize];

sampleArray[a_, d_] := ByteCount[a] < 1000 := Level[a, {d}];
sampleArray[a_, d_] := Module[
  {dims = Dimensions[a, d], cells},
  cells = Times @@ dims;
  If[cells > $exprSampleSize,
    Extract[a, RandomPositiveInteger[dims, $exprSampleSize]],
    Level[a, {d}]
  ]
];

(* TODO: test the array against the resulting predicate after sampling, in case we
missed some stuff! *)

(*************************************************************************************************)

commonCompoundType = CaseOf[
  {}                := Exprs;
  vec_ ? RuleVecQ   := commonRuleType @ vec;
  vec_ ? DictVecQ   := commonDictType @ vec;
  vec_ ? ListVecQ   := commonListType @ vec;
  vec_              := commonTypeByHead @ vec;
];

(*************************************************************************************************)

commonTypeByHead[{}] := Exprs;
commonTypeByHead[e_] := Locals[
  heads = Head /@ e;
  If[AllSameQ[heads],
    Switch[First[heads],
      NumericArray, compoundMeet[arrayType /@ e],
      _,            headToType @ First @ heads
      (* TODO: call singleType on all the things, then unify *)
    ]
  ,
    If[AnyTrue[heads, SymbolQ], heads = Map[head2, e]];
    headSetToType @ Union @ heads
  ]
];

head2[True|False] := Booleans;
head2[s_Symbol] := $sym[s];
head2[e_]       := Head[e];

headToType = CaseOf[
  Integer  := Integers;
  Rational := Numbers;
  Real     := Reals;
  Complex  := Complexes;
  List     := ListsOf[Exprs];
  Rule     := RulesOf[Exprs, Exprs];
  Dict     := DictsOf[Exprs, Exprs];
  $sym[Booleans] := Booleans;
  $sym[_]  := Symbols;
  other_   := HeadsOf[other];
];

$realHeads = {Integer, Rational, Real};
$compHeads = {Complex, Integer, Rational, Real};
$datumHeads = {Complex, Integer, Rational, Real, String, $sym[Booleans]};
$atomHeads = {Complex, Integer, Rational, Real, Symbol, String, $sym[Booleans]};
$optHeadsP = $sym[$Failed | None | Null] | Missing | Failure;

headSetToType[heads_ /; ContainsQ[heads, $optHeadsP]] := Locals[
  type = headSetToType @ DelCases[$optHeadsP] @ heads;
  OptionalOf[type, Union @ Col1 @ Cases[$optHeadsP] @ heads]
];

headSetToType[heads2_] := Locals[
  heads = heads2;
  Which[
    SubsetOfQ[heads, $realHeads], Numbers,
    SubsetOfQ[heads, $compHeads], Complexes,
    heads = deSym[heads];
    SubsetOfQ[heads, $datumHeads], Datums,
    SubsetOfQ[heads, $atomHeads],  Atoms,
    Len[heads] == 1,               headToType @ First @ heads,
    1 <= Len[heads] <= 4,          HeadsOf[hset2],
    True,                          Exprs
  ]
];

deSym[e_] := Union @ ReplaceAll[e, _$sym -> Symbol];

(*************************************************************************************************)

catcomType[lists_List] := commonType @ Catenate @ lists;

(* note: this is the simplest thing we could do, but in theory we are trying to find the best permutation
of the axes that isolates the type dependency somehow (?). think about this! *)

(* we won't reach this through arrayElemType, since if they were all the same length
it would have sampled within this final axis. but we could get here from commonDictType, etc.
through commonType2.

the outer list won't be empty, but the inner lists here can be empty;
that's expected if we have e.g. a matrix of empty lists.
*)

commonListType[lists_List ? AllSameLengthQ] := commonTupleType @ lists;
commonListType[lists_List]   := ListsOf @ catcomType @ lists
singleListType[list_List]    := ListsOf @ commonType @ list // tryHeteroType[list];
commonTupleType[tuples_List] := TuplesOf @ MapFlip[commonType] @ lists;
singleTupleType[tuple_List]  := TuplesOf @ Map[singleType] @ tuple;

(*************************************************************************************************)

(* IDEA: Block Nothing and Splice so that they doesn't ruin our naive use of Keys etc *)

commonDictType[dicts_List ? AllSameOKeysQ] := commonRecordType @ dicts;
commonDictType[dicts_List]   := kvType[DictsOf, catcomType, dicts];
singleDictType[dict_Dict]    := kvType[DictsOf, commonType, dicts] // tryHeteroType[dict];
commonRecordType[dicts_List] := kvType[P1, MapFlip[commonType], dicts];
singleRecordType[dict_Dict]  := kvType[Id, Map[singleType], dict];

(*************************************************************************************************)

SetCurry1[tryHeteroType];
tryHeteroType[list_, ListsOf[Exprs]]                := singleTupleType @ list;
tryHeteroType[dict_, DictsOf[Strings|Exprs, Exprs]] := singleRecordType @ dict;
tryHeteroType[_, type_] := type;

(*************************************************************************************************)

commonRuleType[rules_List] := kvType[RulesOf, commonType, rules];
singleRuleType[rule_Rule]  := kvType[RulesOf, singleType, rule];

kvType[head_, __, EmptyP]          := head[Exprs, Exprs];
kvType[head_, fn_, object_]        := head[ fn @ Keys @ object, vfn @ Values @ object];
kvType[head_, kfn_, vfn_, object_] := head[Keys @ kfn @ object, vfn @ Values @ object];

(*************************************************************************************************)

(* TODO: unusued, what was it for? *)
longMatrixQ[e_] := Len[e] > Len[First @ e];
wideMatrixQ[e_] := Len[e] < Len[First @ e];
