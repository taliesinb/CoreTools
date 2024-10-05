SystemExports[
  "Function",
    RandomAtom, RandomDatum,
    RandomSymbol,
    RandomInt, RandomRange,
    RandomLetter, RandomLowercaseString, RandomBase36String,
    RandomBit, RandomBoolean,
    RandomDecimal,
    RandomGeometric, RandomSurvival,
    RandomUnitInteger, RandomUnitReal,
    RandomUnitVectorND, RandomBallVectorND,
    RandomUnitVector2D, RandomBallVector2D,
    RandomUnitVector3D, RandomBallVector3D,
    RandomNatural, RandomPositiveInteger, RandomPositiveReal, RandomSymmetricReal,
    RandomTuple,
    RandomUnitNormal, RandomUnitNormal2D, RandomUnitNormal3D, RandomUnitNormalND,
    RandomExprTree,
    RandomAssociation,
    RandomUAssociation,
    NthLowercase,
    Softmax, RuleVectorChoice, WeightedRandomChoice, RandomChoiceArray,
    RandomPart,
  "ControlFlow",
    CoinToss,
    BlockSeed
];

PackageExports[
  "Function",
    RandInt, RandDict, RandUDict, RandAtom, RandDatum, RandSym, RandLet,
    RandDec, RandBit, RandNat, RandBool, RandSign, RandRange, RandReal, RandNorm
];

(*************************************************************************************************)

DefineAliasRules[
  RandInt          -> RandomInt,
  RandDict         -> RandomAssociation,
  RandUDict        -> RandomUAssociation,
  RandAtom         -> RandomAtom,
  RandDatum        -> RandomDatum,
  RandSym          -> RandomSymbol,
  RandLet          -> RandomLetter,
  RandDec          -> RandomDecimal,
  RandBit          -> RandomBit,
  RandNat          -> RandomNatural,
  RandBool         -> RandomBoolean,
  RandSign         -> RandomUnitInteger,
  RandRange        -> RandomRange,
  RandReal         -> RandomUnitReal,
  RandNorm         -> RandomUnitNormal
];

(*************************************************************************************************)

SetHoldF[BlockSeed];

BlockSeed[body_] := BlockRandom[body, RandomSeeding -> 1];
BlockSeed[body_, seed_] := BlockRandom[body, RandomSeeding -> seed];

(*************************************************************************************************)

SetHoldA[CoinToss];

CoinToss[a_]     := a;
CoinToss[a_, b_] := If[RandomBoolean[], a, b];
CoinToss[a__]    := RandomPart @ Hold[a];

RandomPart[a_]   := Part[a, RandomInteger[{1, Len @ a}]];

(*************************************************************************************************)

RuleVectorChoice[rules_List]                     := RandomChoice @ RuleUnthread @ rules;
RuleVectorChoice[rule:Rule[_List, _List]]        := RandomChoice @ rule;
RuleVectorChoice[rules_List, n_Int]              := RandomChoice[RuleUnthread @ rules, n];
RuleVectorChoice[rule:Rule[_List, _List], n_Int] := RandomChoice[rule, n];

(*************************************************************************************************)

SetCurry2[WeightedRandomChoice]

WeightedRandomChoice[set_List, weights_ ? VectorQ] := RandomChoice[weights -> set];
WeightedRandomChoice[set_List, weights_ ? ArrayQ] := Map[w |-> RandomChoice[w -> set], weights, {-2}];
_WeightedRandomChoice := ReturnMsg[WeightedRandomChoice::invalidUsage];
WeightedRandomChoice::invalidUsage = "First argument should be set and second argument should be weights."

(*************************************************************************************************)

RandomChoiceArray[weights_List ? VectorQ] := FastQuietCheck[RandomChoice[weights], RandomChoiceArray[]];
RandomChoiceArray[weights_List ? ListVectorQ]  := MapAxisN[RandomChoiceArray, weights];
_RandomChoiceArray := ReturnMsg[RandomChoiceArray::invalidUsage];
RandomChoiceArray::invalidUsage = "Input was not an array of numbers.";

(*************************************************************************************************)

Softmax[array_List] := Normalize[Abs @ Exp @ ToPackedReals @ N @ array, Total /* Max];

(*************************************************************************************************)

declareSimpleRandFn[head_] := (
  SetStrict[head];
  head[dims___Integer]          := head @ {dims};
  head[dims_] /; !NatVecQ[dims] := (Message[head::invalidRandomShape, dims]; $Failed);
);

Scan[declareSimpleRandFn, {
  RandomAtom, RandomDatum, RandomSymbol, RandomLetter,
  RandomBit, RandomBoolean,
  RandomDecimal, RandomUnitInteger, RandomUnitReal,
  RandomUnitVector2D, RandomBallVector2D, RandomUnitVector3D, RandomBallVector3D,
  RandomUnitNormal, RandomUnitNormal2D, RandomUnitNormal3D
}];

$datumChoices := $datumChoices = FlatList[
  False, True, None, Null,
  CharRange["a", "f"],
  Range[-1., 1., .5], Range[0, 4],
  -1/2, -1/3, -1/4, 1/4, 1/3, 1/2
];

$atomChoices := $atomChoices = FlatList[$datumChoices, FormalSymbol @ Range[12]];

RandomAtom[dims_]         := RandomChoice[$atomChoices, dims];
RandomDatum[dims_]        := RandomChoice[$datumChoices, dims];
RandomSymbol[dims_]       := RandomChoice[$lowerFormal, dims];
RandomLetter[dims_]       := Map[FromCharacterCode, RandomInteger[{97, 122}, dims], {-1}];

RandomBit[dims_]          := RandomInteger[1, dims];
RandomBoolean[dims_]      := RandomChoice[{False, True}, dims];

RandomDecimal[dims_]      := RandomInteger[9, dims];

RandomUnitInteger[dims_]  := RandomInteger[1, dims]*2-1;
RandomUnitReal[dims_]     := RandomReal[{0, 1}, dims];
RandomUnitVector2D[dims_] := RandomUnitVectorND[2, dims];
RandomBallVector2D[dims_] := RandomBallVectorND[2, dims];
RandomUnitVector3D[dims_] := RandomUnitVectorND[3, dims];
RandomBallVector3D[dims_] := RandomBallVectorND[3, dims];
RandomUnitNormal[dims_]   := RandomVariate[$unitNormal, dims];
RandomUnitNormal2D[dims_] := RandomUnitNormalND[2, dims];
RandomUnitNormal3D[dims_] := RandomUnitNormalND[3, dims];


(*************************************************************************************************)

declareSpecRandFn[head_] := (
  SetStrict[head];
  head[spec_, dims___Integer] := head[spec, {dims}];
);

Scan[
  declareSpecRandFn,
  {RandomInt, RandomLowercaseString, RandomUnitVectorND, RandomGeometric, RandomSurvival, RandomBallVectorND, RandomUnitNormalND}
];

(**************************************************************************************************)

RandomRange[lo_, hi_, shape___Integer]                := RandomRange[lo, hi, {shape}];
RandomRange[lo_Int, hi_Int, shape_ ? PosIntVecQ]      := RandomInteger[{lo, hi}, shape];
RandomRange[lo_?NumQ, hi_?NumQ, shape_ ? PosIntVecQ]  := RandomReal[{lo, hi}, shape];
RandomRange[-Inf, Inf, shape_ ? PosIntVecQ]           := Unimplemented;
RandomRange[_, Inf, shape_ ? PosIntVecQ]              := Unimplemented;

RandomInt[n_Int, shape_ ? PosIntVecQ]                 := RandomInteger[n, shape];

RandomGeometric[mode:NumP, shape_ ? PosIntVecQ]       := RandomVariate[GeometricDistribution[1.0 / (1 + mode)], shape];
RandomSurvival[lamb:NumP, shape_ ? PosIntVecQ]        := RandomVariate[ExponentialDistribution[1.0 / lamb], shape];

(**************************************************************************************************)

RandomLowercaseString = ExtendCaseOf[
  $[n_Int, shape_ ? PosIntVecQ]          := codeToStrs @ randLowerCode[n, shape];
  $[{m_Int, n_Int}, shape_ ? PosIntVecQ] := codeToStrs @ Array[randLowerCode[RandomInteger[{m, n}], {}]&, shape];
];

RandomBase36String[n:PosIntP] := Base36String[RandomInteger[36^n - 1], n];

randLowerCode[len_, shape_] := RandomInteger[{97, 122}, Append[shape, len]];
codeToStrs[code_] := Map[FromCharacterCode, code, {-2}];

(**************************************************************************************************)

RandomUnitVectorND[d_Integer ? Positive, shape_List ? PosIntVecQ] :=
  RandomVariate[Random`Private`InternalStandardSphereUniformDistribution[d], shape];

RandomBallVectorND[d_Integer ? Positive, shape_List ? PosIntVecQ] :=
  RandomVariate[Random`Private`InternalStandardBallUniformDistribution[d], shape];

RandomUnitNormalND[d_Integer ? Positive, shape_List ? PosIntVecQ] :=
  RandomVariate[$unitNormal, Append[shape, d]];

RandomUnitNormalND[d_Integer ? Positive, {}] :=
  RandomVariate[$unitNormal, d];

$unitNormal := $unitNormal = NormalDistribution[0, 1];

(*************************************************************************************************)

General::invalidRandomSpec = "Invalid random specification: ``.";
General::invalidRandomShape = "Invalid random shape: ``.";

$rshape = {};
defineThreadingRandFn[head_, impl_] := (
  SetStrict[head];
  head[spec_, dims___Integer] := head[spec, {dims}];
  head[spec_List, shape_List] := rcheck[head, shape, OutermostToInnermost @ impl @ spec];
  head[spec_, shape_List]     := rcheck[head, shape, impl @ spec];

  SetListable[impl];
  impl[spec_] := Message[head::invalidRandomSpec, spec];
);

SetHoldA[rcheck];
rcheck[head_, shape_, body_] := If[!NatVecQ[shape],
  Message[head::invalidRandomShape, shape]; $Failed,
  Block[{$rshape = shape}, Check[body, $Failed]]
];

(*************************************************************************************************)

defineThreadingRandFn[RandomNatural, randNat];
randNat[n_Integer ? Positive] := RandomInteger[n, $rshape];

(*************************************************************************************************)

defineThreadingRandFn[RandomPositiveInteger, randPosInt];
randPosInt[n_Integer ? Positive] := RandomInteger[{1, n}, $rshape];

(*************************************************************************************************)

defineThreadingRandFn[RandomPositiveReal, randPosReal];
randPosReal[r_ ? Positive] := RandomReal[r, $rshape];

(*************************************************************************************************)

defineThreadingRandFn[RandomSymmetricReal, randSymReal];
randSymReal[r_ ? Positive] := RandomReal[{-r, r}, $rshape];

(*************************************************************************************************)

SetStrict[RandomTuple];

RandomTuple[sets_, dims__Integer]      := RandomTuple[sets, {dims}];
RandomTuple[sets:{__List}]             := RandomChoice /@ sets;
RandomTuple[sets:{__List}, shape_List] := OutermostToInnermost[RandomChoice[#, shape]& /@ sets];

(*************************************************************************************************)

(* TODO: use Groupings *)

RandomExprTree[n_Int, shape__Int] := RandomExprTree[n, {shape}];
RandomExprTree[n_Int, shape:{__Int}] := Array[RandomExprTree[n]&, shape];
RandomExprTree[n_Int] := Block[{$nh = 1, $nl = 1}, toRandExpr @ RandomTree[n]];

toRandExpr = CaseOf[
  _Symbol[_, children_List] := Apply[Part[$upperFormal, Mod[$nh++, 26, 1]], $$ /@ children];
  _Symbol[_, None]          := Part[$lowerFormal, Mod[$nl++, 26, 1]];
];

(*************************************************************************************************)

SetListable @ NthLowercase;

NthLowercase[n:PosIntP] := FromCharCode[97 + IntegerDigits[n-1, 26]]

Scan[NthLowercase[#] = NthLowercase[#]&, Range[26]];

(*************************************************************************************************)

RandomAssociation[n:NatP]  := DictThread[NthLowercase[Range[n]], RandomDecimal[n]];
RandomUAssociation[n:NatP] := UDictThread[NthLowercase[Range[n]], RandomDecimal[n]];


