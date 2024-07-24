SystemExports[
  "Function",
    RandomSymbol,
    RandomInt,
    RandomLetter, RandomLowercaseString, RandomBase36String,
    RandomBit, RandomBoolean, RandomUnitInteger, RandomUnitReal,
    RandomUnitVectorND, RandomBallVectorND,
    RandomUnitVector2D, RandomBallVector2D,
    RandomUnitVector3D, RandomBallVector3D,
    RandomNatural, RandomPositiveInteger, RandomPositiveReal, RandomSymmetricReal,
    RandomTuple,
    RandomUnitNormal, RandomUnitNormal2D, RandomUnitNormal3D, RandomUnitNormalND,
    RandomExprTree,
    FormalSymbol, FromFormalSymbol,
    FormalSymbolQ, HoldFormalSymbolQ,
    Softmax, RuleVectorChoice, WeightedRandomChoice, RandomChoiceArray,
    RandomPart,
  "ControlFlowFunction",
    CoinToss,
  "Variable",
    $FormalSymbols
];

(*************************************************************************************************)

DeclareHoldAll[CoinToss];
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

DeclareCurry2[WeightedRandomChoice]

WeightedRandomChoice[set_List, weights_ ? VectorQ] := RandomChoice[weights -> set];
WeightedRandomChoice[set_List, weights_ ? ArrayQ] := Map[w |-> RandomChoice[w -> set], weights, {-2}];
_WeightedRandomChoice := ReturnMsg[WeightedRandomChoice::badUsage];
WeightedRandomChoice::badUsage = "First argument should be set and second argument should be weights."

(*************************************************************************************************)

RandomChoiceArray[weights_List ? VectorQ] := FastQuietCheck[RandomChoice[weights], RandomChoiceArray[]];
RandomChoiceArray[weights_List ? ListVectorQ]  := MapLastAxis[RandomChoiceArray, weights];
_RandomChoiceArray := ReturnMsg[RandomChoiceArray::badUsage];
RandomChoiceArray::badUsage = "Input was not an array of numbers.";

(*************************************************************************************************)

Softmax[array_List] := Normalize[Abs @ Exp @ ToPackedReals @ N @ array, Total /* Max];

(*************************************************************************************************)

declareSimpleRandFn[head_] := (
  DeclareStrict[head];
  head[dims___Integer]                         := head @ {dims};
  head[dims_] /; !PositiveIntegerVectorQ[dims] := (Message[head::invalidRandomShape, dims]; $Failed);
);

Scan[declareSimpleRandFn, {
  RandomSymbol, RandomLetter,
  RandomBit, RandomBoolean, RandomUnitInteger, RandomUnitReal,
  RandomUnitVector2D, RandomBallVector2D, RandomUnitVector3D, RandomBallVector3D,
  RandomUnitNormal, RandomUnitNormal2D, RandomUnitNormal3D
}];

RandomSymbol[dims_]       := RandomChoice[$lowerFormal, dims];
RandomLetter[dims_]       := Map[FromCharacterCode, RandomInteger[{97, 122}, dims], {-1}];
RandomBit[dims_]          := RandomInteger[1, dims];
RandomBoolean[dims_]      := RandomChoice[{False, True}, dims];
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
  DeclareStrict[head];
  head[spec_, dims___Integer] := head[spec, {dims}];
);

Scan[declareSpecRandFn, {RandomInt, RandomLowercaseString, RandomUnitVectorND, RandomBallVectorND, RandomUnitNormalND}];

(**************************************************************************************************)

RandomInt[n_Int, shape_ ? PositiveIntegerVectorQ] := RandomInteger[n, shape];
RandomLowercaseString[n_Int, shape_ ? PositiveIntegerVectorQ] := Map[FromCharacterCode, RandomInteger[{97, 122}, Append[shape, n]], {-2}];
RandomBase36String[n_Integer] := Base36String[RandomInteger[36^n - 1], n];

(**************************************************************************************************)

RandomUnitVectorND[d_Integer ? Positive, shape_List ? PositiveIntegerVectorQ] :=
  RandomVariate[Random`Private`InternalStandardSphereUniformDistribution[d], shape];

RandomBallVectorND[d_Integer ? Positive, shape_List ? PositiveIntegerVectorQ] :=
  RandomVariate[Random`Private`InternalStandardBallUniformDistribution[d], shape];

RandomUnitNormalND[d_Integer ? Positive, shape_List ? PositiveIntegerVectorQ] :=
  RandomVariate[$unitNormal, Append[shape, d]];

RandomUnitNormalND[d_Integer ? Positive, {}] :=
  RandomVariate[$unitNormal, d];

$unitNormal := $unitNormal = NormalDistribution[0, 1];

(*************************************************************************************************)

General::invalidRandomSpec = "Invalid random specification: ``.";
General::invalidRandomShape = "Invalid random shape: ``.";

$rshape = {};
defineThreadingRandFn[head_, impl_] := (
  DeclareStrict[head];
  head[spec_, dims___Integer] := head[spec, {dims}];
  head[spec_List, shape_List] := rcheck[head, shape, OutermostToInnermost @ impl @ spec];
  head[spec_, shape_List]     := rcheck[head, shape, impl @ spec];

  DeclareListable[impl];
  impl[spec_] := Message[head::invalidRandomSpec, spec];
);

DeclareHoldAll[rcheck];
rcheck[head_, shape_, body_] := If[!PositiveIntegerVectorQ[shape],
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

DeclareStrict[RandomTuple];

RandomTuple[sets_, dims__Integer]      := RandomTuple[sets, {dims}];
RandomTuple[sets:{__List}]             := RandomChoice /@ sets;
RandomTuple[sets:{__List}, shape_List] := OutermostToInnermost[RandomChoice[#, shape]& /@ sets];

(*************************************************************************************************)

$lowerFormal := $lowerFormal = ToExpression[CharRange["\[FormalA]","\[FormalZ]"], InputForm];
$upperFormal := $upperFormal = ToExpression[CharRange["\[FormalCapitalA]","\[FormalCapitalZ]"], InputForm];
$FormalSymbols := $FormalSymbols = Join[$lowerFormal, $upperFormal];

DeclareListable[FormalSymbol];

FormalSymbol::notFormalSpec = "`` should be a integer or string containing a roman letter.";
FormalSymbol[e_] := ErrorMsg["notFormalSpec", e];
FormalSymbol[s_Str ? CharQ] := codeToFormal @ First @ ToCharCode @ s;
FormalSymbol[n_Int /; 1 <= n <= 52] := Part[$FormalSymbols, n];

codeToFormal[n_] := Which[
  65 <= n <= 90,  Part[$upperFormal, n - 64],
  97 <= n <= 122, Part[$lowerFormal, n - 96],
  True, ErrorMsg["notFormalSpec", FromCharCode @ n]
];

(*************************************************************************************************)

DeclareListable[FromFormalSymbol];

$fromFormalSymbols := $fromFormalSymbols = UAssocThread[
  $FormalSymbols, Join[CharRange["a", "z"], CharRange["A", "Z"]]
];

FromFormalSymbol[sym_Symbol] := Lookup[$fromFormalSymbols, sym, sym];

(*************************************************************************************************)

DeclareHoldFirst[HoldFormalSymbolQ]
DeclarePredicate1[HoldFormalSymbolQ, FormalSymbolQ]

HoldFormalSymbolQ[s_Symbol] := MemberQ[$FormalSymbols, Unevaluated @ s];
FormalSymbolQ[s_Symbol] := MemberQ[$FormalSymbols, Unevaluated @ s];

(*************************************************************************************************)

(* TODO: use Groupings *)

RandomExprTree[n_Int, shape__Int] := RandomExprTree[n, {shape}];
RandomExprTree[n_Int, shape:{__Int}] := Array[RandomExprTree[n]&, shape];
RandomExprTree[n_Int] := Block[{$nh = 1, $nl = 1}, toRandExpr @ RandomTree[n]];

toRandExpr = CaseOf[
  _Symbol[_, children_List] := Apply[Part[$upperFormal, Mod[$nh++, 26, 1]], $$ /@ children];
  _Symbol[_, None]          := Part[$lowerFormal, Mod[$nl++, 26, 1]];
];
