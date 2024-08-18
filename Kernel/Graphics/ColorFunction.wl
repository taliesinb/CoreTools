SystemExports[
  "GraphicsDirective",
    PastelHue, VibrantHue, RainbowHue, RedBlueColorFunction,
  "Function",
    ChooseNumericColorFunction, ApplyAutomaticColoring,
    HashToColor, UniqueColor,
  "Head",
    NumericColorFunction, DiscreteColorFunction, HashValue,
  "GraphicsFunction",
    PixelHash
];

PackageExports[
  "Function",
    CompileColorFunction, ApplyColorFunctionToArray
];

(**************************************************************************************************)

CoreBoxes[HashValue[hash_Int]] := TagBox[ToBoxes @ PixelHash @ hash, Deploy];

splitHash[hash_Int] := Mod[BitShiftRight[hash, {0, 16, 32, 48}], 2^16];
PixelHash[None] := Image[{{Gray, Gray}, {Gray, Gray}}, ImageSize -> {8, 8}*2];
PixelHash[hash_Int] := Tooltip[Image[
  Partition[Map[NiceHue, splitHash[hash] / 2^16], 2],
  ImageSize -> {8, 8}*2
], hash];

(*************************************************************************************************)

(* TO INVESTIGATE:

Internal`ApplyColorFunction (* takes list of colors, but doesn't interpolate *)
Internal`SimplifyColor

DarkColorGradient
PastelColorGradient
BoldColorGradient
SapphireGradient
M10DefaultGradient
NeonColorGradient
*)

(*************************************************************************************************)

General::badColorFunctionSpecification = "Bad color function specification: ``.";

makeCF[head_, fn_, args___] := CatchMessages[head, Module[
  {res = fn[args]},
  Which[
    Head[res] === InternalData, ConstructNoEntryExpr[head, res],
    Head[res] === head, res,
    True, ThrowMsg["badColorFunctionSpecification", HoldForm[head[args]]]; $Failed
  ]
]];

(*************************************************************************************************)

General::badColors = "Color list contains non-colors.";
General::badColorsValuesLength = "Value and color lists must be lists of the same length.";

checkValuesColor[values_, colors_] := (
  If[!SameLengthQ[values, colors], ThrowMsg["badColorsValuesLength"]];
  If[!ColorVectorQ[colors], ThrowMsg["badColors"]];
);

unpackRules = CaseOf[
  Rule[k_List, v_List] := {k, v};
  Rule[k_List, Auto]   := {k, Auto};
  kvs:{__Rule}         := {Keys @ k, Values @ v};
  spec_                := ThrowMsg["badColorFunctionSpecification", spec];
];

(*************************************************************************************************)

DiscreteColorFunction::tooManyUniqueColors = "The list of values is too large (having `` values) to choose an automatic coloring."

Options[DiscreteColorFunction] = {DefaultValue -> Gray};

makeDiscreteColorFunction[spec_, opts_] :=
  makeDiscreteColorFunction[Sequence @@ unpackRules @ spec, opts];

makeDiscreteColorFunction[values_List, opts_] :=
  makeDiscreteColorFunction[values, Auto, opts];

makeDiscreteColorFunction[values2_List, Auto, opts_] := Locals[
  values = Union @ values2;
  count = Len @ values;
  If[BooleanVectorQ @ values, values = {False, True}];
  colors = Which[
    values === {False, True},        $BooleanColors,
    count <= Len[$MediumColorPalette],     Take[$MediumColorPalette, count],
    count <= 2 * Len[$MediumColorPalette], Take[Join[$LightColorPalette, $DarkColorPalette], count],
    True,                            ThrowMsg["tooManyUniqueColors", count]
  ];
  makeDiscreteColorFunction[values, colors, opts]
]

makeDiscreteColorFunction[values2_, colors2_, opts_] := Locals[
  values = values2; colors = colors2;
  UnpackOptionsAs[DiscreteColorFunction, opts, defaultValue];
  checkValuesColor[values, colors];
  order = Ordering[values];

  values = Append[DefaultValue] @ Part[values, order];
  colors = Append[defaultValue] @ Part[colors, order];
  rgbValues = ColorToRGBArray @ colors;

  rules = ZipMap[{lit, col} |-> Verbatim[lit] -> col, values, rgbValues];
  Part[rules, -1, 1] = _;
  dispatch = Dispatch @ rules;

  colorAssoc = UDictThread[values, colors];
  rgbAssoc = UDictThread[values, rgbValues];

  fn = If[MemberQ[values, _List],
    dcfSlowPath[colorAssoc],
    dcfFastPath[colorAssoc, defaultValue]
  ];

  InternalData[fn, rgbAssoc, Last @ rgbValues, dispatch]
];

(*************************************************************************************************)

DiscreteColorFunction[id_InternalData][value_] := Part[id, 1][value];

d_dcfFastPath[list_List ? ListVectorQ] := Map[d, list];
d_dcfFastPath[list_] := Lookup[First @ d, list, Last @ d];

d_dcfSlowPath[elem_]     := Lookup[First @ d, Key @ elem, dcfSlowPath2[d, elem]];

dcfSlowPath2[d_, e_]     := Block[{$a = First @ d}, dcfSlowPath3 @ e];
dcfSlowPath3[e_List]     := Lookup[$a, Key @ e, Map[dcfSlowPath3, e]];
dcfSlowPath3[e_]         := Lookup[$a, e, $a[DefaultValue]];

CompileColorFunction[DiscreteColorFunction[InternalData[_, rgbAssoc_, default_, dispatch_]], depth_] :=
  Switch[
    depth,
    0, in |-> Lookup[rgbAssoc, Key @ in, default],
    1, in |-> Lookup[rgbAssoc, in, default],
    _, in |-> Replace[in, dispatch, {depth}]
  ];

(*************************************************************************************************)

ChooseNumericColorFunction[data_List] := Locals[
  minMax = MinMax[data /. {Infinity -> Nothing}];
  minMax //= DeleteCases[_Missing | None | Null | $Failed];
  {values, colors, ticks} = pickBiGradient @@ minMax;
  NumericColorFunction[values, colors, Ticks -> ticks]
];

(*************************************************************************************************)

Options[NumericColorFunction] = {Ticks -> None, DefaultValue -> Gray};

NumericColorFunction::tooFewEntries = "Value and color lists must have length at least 2.";
NumericColorFunction::nonNumericValues = "Provided values were not numeric."

makeNumericColorFunction[ab:Num2P, opts_] :=
  ChooseNumericColorFunction[ab];

makeNumericColorFunction[spec_, opts_] :=
  makeNumericColorFunction[Sequence @@ unpackRules[spec], opts];

makeNumericColorFunction[Interval[{a_, b_}], colors_List, opts_] :=
  makeNumericColorFunction[Lerp[a, b, Into @ Len @ colors], colors, opts];

makeNumericColorFunction[values2_, colors_, opts_] := Locals[

  UnpackOptionsAs[NumericColorFunction, opts, ticks, defaultValue];

  values = values2;
  If[MatchQ[values, Num2P -> _List],
    ibound = MinMax @ First @ values;
    obound = MinMax @ Last @ values;
    values = Rescale[Last @ values, obound, ibound]
  ];

  values = EnsurePackedReals[values, ThrowMsg["nonNumericValues"]];

  checkValuesColor[values, colors];
  If[Len[values] < 2, ThrowMsg["tooFewEntries"]];

  okLabArray = ColorToOKArray @ colors;
  fn = Interpolation[Transpose @ {values, okLabArray}, InterpolationOrder -> 1];

  InternalData[MinMax @ values, fn, defaultValue, values, ticks]
];

(*************************************************************************************************)

NumericColorFunction[InternalData[bounds_, fn_, def_, ___]][value_] :=
  If[RealValuedNumericQ[value] || NumberArrayQ[value],
    OKArrayToColor @ fn @ Clip[value, bounds],
    def
  ];

(*************************************************************************************************)

CompileColorFunction[NumericColorFunction[InternalData[bounds_, fn_, def_, ___]], _] :=
  ClipOp[bounds] /* fn /* OKArrayToRGBArray;

(*************************************************************************************************)

$negValues = {-1., -0.9, -0.8, -0.6, -0.3, 0.};
$posValues = {0., 0.3, 0.6, 0.8, 0.9, 1.};
$negPosValues = Join[$negValues, Rest @ $posValues];

$negColors := $negColors = HexToColorList @ "#31437e #165e9d #3a7dbf #7aacce #ceefef #ffffff";
$posColors := $posColors = HexToColorList @ "#ffffff #efef7b #ff7b4a #d63822 #b50700 #722a40";
$negPosColors := $negPosColors = Join[$negColors, Rest @ $posColors];

RedBlueColorFunction[]                 := RedBlueColorFunction[] = NumericColorFunction[$negPosValues, $negPosColors];
RedBlueColorFunction[a:NumP]           := RedBlueColorFunction[{-a, a}];
RedBlueColorFunction[{a:NumP, b:NumP}] := NumericColorFunction[{a, b} -> $negPosValues, $negPosColors];

(*************************************************************************************************)

pickBiGradient[min_ ? Negative, max2_ ? Positive] := Locals[
  max = Max[Abs @ min, Abs @ max2];
  max = pickNice[max, max - min, Ceiling];
  {$negPosValues * max, $negPosColors, 3}
];

pickBiGradient[0|0., max_] :=
  {$posValues * pickNice[max, max, Ceiling], $posColors, 2};

pickBiGradient[min_, 0|0.] :=
  {$negValues * -pickNice[-min, -min, Ceiling], $negColors, 2};

pickBiGradient[min_ ? Negative, max_ ? Negative] :=
  MapAt[Minus, pickBiGradient[-min, -max], 1];

pickBiGradient[min_ ? Positive, max_ ? Positive] /; min <= max / 10. :=
  pickBiGradient[0, max];

pickBiGradient[min_ ? Positive, max_ ? Positive] /; min <= max / 5. :=
  {interpolated[pickNice[min, min, Floor], pickNice[max, max, Ceiling], Len @ $posColors], $posColors, Auto};

$rainbowColors = {$Red, $Orange, $Green, $Blue, $Pink};

pickBiGradient[min2_ ? Positive, max2_ ? Positive] := Locals[
  dx = max2 - min2;
  min = pickNice[min2, dx, Floor];
  max = pickNice[max2, dx, Ceiling];
  range = Range[min, max, (max - min) / (Len[$rainbowColors] - 1)];
  {range, $rainbowColors, Auto}
];

powerNext[val_ ? Negative, func_] := -powerNext[val, func];
powerNext[val_, func_] := Power[10, func @ Log10 @ val];
powerNext[0|0., _] := {0, 0};

roundNext[val_, func_, Full] := powerNext[val, func];
roundNext[val_, func_, 0] := val;
roundNext[val_, func_, frac_] := func[val, frac * powerNext[val, Ceiling]];

pickNice[val_, dx_, func_] := Locals[
  candidates = roundNext[val, func, #]& /@ {Full, 1., .5, .25, .2, .1, .05, .02, .01, 0};
  tol = Abs[dx/8.];
  SelectFirst[candidates, Abs[# - val] <= tol&]
];

interpolated[a_, b_, n_] := Table[b * i + a * (1 - i), {i, 0, 1, 1/(n-1)}];

(*************************************************************************************************)

General::badColorChannel = "Array provided color channel of size `` != 3.";
General::badColorRange = "Array provided RGB values outside the range [0, 1].";
General::badColorRank = "Automatically colored array should be of rank `` or ``, but had rank ``.";

ApplyColorFunctionToArray[_, {}, _] := None;

ApplyColorFunctionToArray[type:Automatic|None, array2_, depth1_] := Locals[
  array = ToPacked @ array2;
  bounds = colorFn = None;

  depth2 = depth1 + 1;
  dims = Dims @ array;
  rank = Len @ dims;
  If[MemberQ[dims, 0], Return @ None];
  If[!MatchQ[rank, depth1 | depth2],
    ThrowMsg["badColorRank", depth1, depth2, rank]];

  If[!PackedQ[array] && ContainsQ[array, Real] && ContainsQ[array, Int],
    If[PackedRealsQ[nArrray = N @ array], array = narray]];

  If[PackedQ[array], bounds = MinMax @ array];

  Which[
    rank === depth2,
      If[Last[dims] =!= 3, ThrowMsg["badColorChannel", Last @ dims]];
      If[!unitBoundsQ[bounds], ThrowMsg["badColorRange"]];
    ,
    type === None,
      If[!unitBoundsQ[type], ThrowMsg["badColorRange"]];
      Null
    ,
    PackedQ[array] && unitBoundsQ[bounds],
      Null
    ,
    PackedIntsQ[array] && discreteBoundsQ[bounds],
      pal = ColorToRGBArray @ $MediumColorPalette;
      array = ExtractIndices[pal, If[min == 0, array + 1, array]]
    ,
    PackedRealsQ[array],
      colorFn = ChooseNumericColorFunction @ array
    ,
    PackedArrayQ[array, Complex] || ComplexPresentQ[array],
      colorFn = ComplexHue
    ,
    ArrayQ[array, 2, ColorQ],
      array //= ColorToRGBArray
    ,
    True,
      colorFn = Last @ ApplyAutomaticColoring @ Catenate @ array
  ];

  If[colorFn === None,
    array,
    ApplyColorFunctionToArray[colorFn, array, depth1]
  ]
];

unitBoundsQ[{min_, max_}] := TrueQ[0 <= min <= max <= 1];
discreteBoundsQ[{min_, max_}] := TrueQ[(0 <= min <= 1) && max < 8];

ApplyColorFunctionToArray[colorFn_, array_, depth_] := Locals[
  If[Head[colorFn] === NumericColorFunction,
    rgbArray = CompileColorFunction[colorFn, depth] @ array
  ,
    rgbArray = ToPackedReals @ Map[colorFn, array, {depth}];
    If[!PackedRealsQ[rgbArray] && ArrayQ[rgbArray, 2, ColorQ],
      rgbArray //= ColorToRGBArray];
  ];
  EnsurePackedReals[rgbArray,
    ThrowMsg["badColorFunctionValues", colorFn, firstNonRGB @ rgbArray]]
];

General::badColorFunctionValues = "ColorFunction -> `` produced non-RGB values, first was: ``.";

firstNonRGB[arr_] := Locals[
  pos = FirstPosition[arr, Except[{_ ? NumberQ, _ ? NumberQ, _ ? NumberQ}], {}, {2}, Heads -> False];
  Extract[arr, pos]
];

(*************************************************************************************************)

ApplyAutomaticColoring[data_List] := Locals[
  If[ColorVectorQ[data], {data, Return @ Identity}];
  $MediumColorPalette = Part[$MediumColorPalette, {1, 2, 3, 5, 4, 6}];

  posIndex = KeySort @ PositionIndex @ data;

  containsInd = KeyExistsQ[posIndex, Indeterminate];
  containsNull = KeyExistsQ[posIndex, Null];
  If[containsInd, indPos = posIndex[Indeterminate]; KeyDropFrom[posIndex, Indeterminate]];
  If[containsNull, nullPos = posIndex[Null]; KeyDropFrom[posIndex, Null]];

  uniqueValues = Keys @ posIndex;
  count = Len @ uniqueValues;
  sortedUniqueValues = Sort @ uniqueValues;

  colorFunction = Which[
    ColorVectorQ[uniqueValues],        Id,
    sortedUniqueValues === {0, 1},     DiscreteColorFunction[{0, 1}, $BooleanColors],
    sortedUniqueValues === {-1, 1},    DiscreteColorFunction[{-1, 1}, {$Red, $Blue}],
    sortedUniqueValues === {-1, 0, 1}, DiscreteColorFunction[{-1, 0, 1}, {$Red, $White, $Blue}],
    Len[uniqueValues] == 1,            DiscreteColorFunction[uniqueValues, {Gray}],
    permRangeQ[uniqueValues],          $MediumColorPalette = discreteColorPalette @ count;
                                       DiscreteColorFunction[uniqueValues, Auto],
    nUniqueValues = N[uniqueValues];
    RealVectorQ[nUniqueValues],        NumericColorFunction[nUniqueValues, Auto],
    True,                              DiscreteColorFunction[uniqueValues, Auto]
  ];
  If[FailureQ[colorFunction], Return @ {$Failed, $Failed}];

  normalFunction = Normal @ colorFunction;
  colors = Map[normalFunction, uniqueValues];
  colorsValues = Transpose[{colors, uniqueValues}];
  If[containsInd, AppendTo[colorsValues, {White, Indeterminate}]; AppendTo[posIndex, Indeterminate -> indPos]];
  If[containsNull, AppendTo[colorsValues, {Transparent, Null}]; AppendTo[posIndex, Null -> nullPos]];

  colorGroups = Merge[RuleThread[colorsValues, Values @ posIndex], Catenate];
  colorList = ConstList[White, Len @ data];
  (* invert the PositionIndex-like association *)
  KeyValueMap[Set[Part[colorList, #2], First[#1]]&, colorGroups];

  (* {colorList, colorGroups, colorFunction} *)
  {colorList, colorFunction}
];

permRangeQ[e_] := Length[e] <= 12 && Or[PermutedRangeQ[vals], PermutedRangeQ[vals + 1]];

discreteColorPalette = CaseOf[
  4 := Part[$DiscreteColorPalette, {1,2,3,5}];
  5 := Part[$DiscreteColorPalette, {1,2,3,5,6}];
  $[n_ /; n <= 9] := Take[$DiscreteColorPalette, n];
  _ := $DiscreteColorPalette;
];

(**************************************************************************************************)

MakeBoxes[cf_NumericColorFunction ? ExprNoEntryQ, StandardForm] :=  colorFunctionBoxes[cf]
MakeBoxes[cf_DiscreteColorFunction ? ExprNoEntryQ, StandardForm] := colorFunctionBoxes[cf]

makeGradientRaster[{min_, max_}, fn_, size_, transposed_] := Locals[
  range = max - min;
  dx = range / size;
  spaced = N @ Range[min, max, dx];
  offsets = (values - min) / dx;
  row = fn @ spaced;
  array = {row};
  arrayRange = {{min - dx, 0}, {max, 1}};
  If[transposed, array //= Transpose; arrayRange = Rev /@ arrayRange];
  Raster[ToPackedReals @ array, arrayRange]
];

Clear[colorFunctionBoxes];
colorFunctionBoxes[cf:NumericColorFunction[id_InternalData]] := Locals[
  bounds = First @ id;
  fn = CompileColorFunction[cf, 1];
  raster = makeGradientRaster[bounds, fn, 100, False];
  graphics = Graphics[raster,
    ImageSize -> {100, 8},
    PlotRangePadding -> 0, PlotRange -> {All, {0, 1}},
    ImagePadding -> 0, BaselinePosition -> Scaled[-0.1], AspectRatio -> Full
  ];
  NiceObjectBoxes["NumericColorFunction", ToBoxes /@ {bounds, graphics}]
]

colorFunctionBoxes[cf:DiscreteColorFunction[id_InternalData]] := Locals[
  rgbAssoc = P2 @ id;
  entries = Sort @ KeyValueMap[
    CodePane[#1, FontColor -> RGBColor[#2]]&,
    KeyDrop[DefaultValue] @ rgbAssoc
  ];
  NiceObjectBoxes["DiscreteColorFunction", ToBoxes @ entries]
]

(**************************************************************************************************)

defineNamedColorFunction[sym_Symbol -> name_String] := (
  DeclareListable[sym];
  sym[r_ ? RealValuedNumericQ] := Blend["BalancedHue", r];
);

defineNamedColorFunction[PastelHue  -> "PastelColorGradient"]
defineNamedColorFunction[VibrantHue -> "VibrantColorGradient"]
defineNamedColorFunction[RainbowHue -> "DarkColorGradient"]

(**************************************************************************************************)

DeclareHoldAllComplete[createColorFunction]

cf_DiscreteColorFunction ? ExprEntryQ := createColorFunction[cf];
cf_NumericColorFunction ? ExprEntryQ  := createColorFunction[cf];

createColorFunction[e:_[_InternalData]] := HoldSetNoEntryExpr @ e;

createColorFunction[DiscreteColorFunction[args__, opts___Rule]] := makeCF[DiscreteColorFunction, makeDiscreteColorFunction, args, {opts}];
createColorFunction[NumericColorFunction[args__, opts___Rule]]  := makeCF[NumericColorFunction,  makeNumericColorFunction,  args, {opts}];
