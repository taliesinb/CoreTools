SystemExports[
  "FormHead",
    BinaryDigitsForm,
  "GraphicsFunction",
    CompactArrayPlot, BitPlot, BinaryArrayPlot, MeshImage, SmartArrayPlot,
  "Option",
    ShowDimensions,
  "SpecialVariable",
    $ArrayPlotMagnitude
];

PackageExports[
  "FormHead",
    NiceObject, ObjectField,
  "BoxFunction",
    NiceObjectBoxes, ObjectFieldBox
];

(*************************************************************************************************)

MakeBoxes[NiceObject[head_String, args_, opts___], StandardForm] := Locals[
  argBoxes = Which[
    ListQ[args], ToBoxes /@ args,
    DictQ[args], ToBoxes /@ KeyValueMap[ObjectField, args],
    True,        ToBoxes  @ args
  ];
  argBoxes /. InterpretationBox[b_, _] :> b;
  NiceObjectBoxes[head, argBoxes, opts]
];

NiceObjectBoxes[head_, args2_, margin:Except[_Rule]:0, opts___Rule] := Locals[
  args = args2;
  cLines = TrueQ @ Lookup[{opts}, ColumnLines, False];
  If[DictQ[args], args = KeyValueMap[ObjectFieldBox, args]];
  args = If[!cLines && ListQ[args], commaSep @ args, args];
  argItems = Which[
    EmptyQ[args],  "",
    SingleQ[args] || cLines, HPadBox[margin] /@ args,
    ListQ[args],   MapFirstLast[LPadBox[margin], RPadBox[margin], args],
    True,          HPadBox[margin] @ args
  ];
  gridBox = GridBox[
    List @ ToList[
      ItemBox["", Frame -> {{True, False}, {True, True}}, ItemSize -> {.2, All}],
      argItems,
      ItemBox["", Frame -> {{False, True}, {True, True}}, ItemSize -> {.2, All}]
    ],
    ColumnLines -> If[cLines, {False, {LightGray}}, False],
    opts, FrameStyle -> AbsoluteThickness[1],
    ColumnBackgrounds -> GrayLevel[1],
    ColumnSpacings -> 0, GridFrameMargins -> {{0.,0.},{0.,0.}},
    BaselinePosition -> {{1,2}, Baseline}, RowMinHeight -> 0.5
  ];
  headedBox = RowBox[{StyleBox[head, FontWeight -> "SemiBold"], gridBox}];
  StyleBox[headedBox, LineBreakWithin -> False, AutoSpacing -> False]
];

commaSep[list_] := Riffle[list, StyleBox[",\[ThinSpace]", Gray]];

(*************************************************************************************************)

MakeBoxes[ObjectField[k_, v_], StandardForm] := ObjectFieldBox[MakeBox @ k, MakeBox @ v];
MakeBoxes[ObjectField[k_, v_], StandardForm] := ObjectFieldBox[MakeBox @ k, MakeBox @ v];

ObjectFieldBox[k_, v_] := RBox[
  StyleBox[k, $DarkGray, Italic],
  ":", "\[VeryThinSpace]", v
  ];

(**************************************************************************************************)

SetStrict[MeshImage]

General::meshImageBlockSize = "Block size `` is not a positive integer.";
General::meshImageDataNumeric = "Input data should be a numeric array."
General::meshImageDataDims = "Input data should be of shape (w * h * 3) or (w * h), but was ``."

Options[MeshImage] = {
  Frame      -> True,
  Mesh       -> True,
  FrameStyle -> GrayLevel[0.4],
  MeshStyle  -> GrayLevel[0.4]
};

MeshImage[array2_, blockSize_, opts___] := CatchMessages[MeshImage, iMeshImage[array2, blockSize, opts]];

iMeshImage[array2_, blockSize_, opts___Rule] := Locals[

  UnpackSymbolsAs[MeshImage, {opts}, frame, mesh, frameStyle, meshStyle];

  If[!PosIntQ[blockSize], ThrowMessage["meshImageBlockSize", blockSize]];

  AssertOptVal[Frame, frame, BoolQ];
  AssertOptVal[Mesh, mesh, BoolQ];

  Switch[frameStyle,
    None,                frame = False,
    GrayLevel[UnitNumP], frameStyle = P1 @ frameStyle,
    _,                   ThrowOptVal[FrameStyle, frameStyle]];

  meshFading = None;
  Switch[meshStyle,
    None,                mesh = False,
    Opacity[UnitNumP],   meshFading = OneMinus @ P1 @ meshStyle,
    GrayLevel[UnitNumP], meshStyle = P1 @ meshStyle,
    _,                   ThrowOptVal[Mesh, meshStyle]];

  array = checkArray @ array2;
  array = EnsurePackedReals[array, ThrowRealArray[array2, {2, 3}]];
  dims = Dims @ array;
  If[!MatchQ[dims, {_, _, 3} | {_, _}], ThrowMessage["meshImageDataDims", dims]];

  {h, w} = Take[dims, 2];

  If[!mesh,
    image = ImageResize[Image[array], blockSize * w,  Resampling -> "NearestLeft"];
    If[frame, image = ImagePad[image, 1, frameStyle]];
    size = {w, h} * blockSize + If[frame, 2, 0];
    Return @ Image[image, ImageSize -> All];
  ];

  d = If[frame || meshFading =!= None, 0, 1];
  {b1, b2} = blockSize + {-1, 1};
  {h2, w2} = 1 + {h, w} * b2 - 2d;

  If[meshFading =!= None,
    $pixels = ConstList[0., {3, h2, w2}];
    fade = ConstList[1., {3, h2, w2}];
    ScanIndexed[paintBlockAdditive, ToPacked @ array, {2}];
    Do[multRow[0.5, r * b2 + 1], {r, 1, h - 1}];
    Do[multCol[0.5, c * b2 + 1], {c, 1, w - 1}];
    Do[Part[fade, All, r * b2 + 1, All] = meshFading, {r, 0, h}];
    Do[Part[fade, All, All, c * b2 + 1] = meshFading, {c, 0, w}];
    $pixels *= fade;
    SetNone[frameStyle, meshFading];
    If[frame =!= None, paintFrame[{All}, {}]];
    Return @ Image[$pixels, Interleaving -> False, ImageSize -> All]
  ];

  hasColor = Len[dims] == 3;
  $pixels = ToPackedReals @ ConstList[N @ meshStyle, If[hasColor, {h2, w2, 3}, {h2, w2}]];
  If[frame, If[hasColor, paintFrame[{}, {All}], paintFrame[{}, {}]]];
  $pixels //= ToPackedReals;
  ScanIndexed[If[hasColor && b1 == 2, paintBlockSafe, paintBlock], array, {2}];
  Image[$pixels, ImageSize -> All]
];

getFrameMeshColor = CaseOf[
  GrayLevel[n_]   := N @ n;
  Opacity[_]      := 0;
  spec_           := ThrowOptVal[Frame, spec];
]

paintFrame[{cl___}, {cr___}] := (
  Part[$pixels, cl, All,    1, cr] = frameStyle;
  Part[$pixels, cl, All,   w2, cr] = frameStyle;
  Part[$pixels, cl,   1,  All, cr] = frameStyle;
  Part[$pixels, cl,   h2, All, cr] = frameStyle;
);

paintBlockAdditive[v_, {r_, c_}] := Module[
  {r1 = (r * b2) - d, c1 = (c * b2) - d},
  Part[$pixels, All, (r1 - b1)-1 ;; r1+1, (c1 - b1)-1 ;; c1+1] += v;
];

multRow[val_, row_] := Part[$pixels, All, row, All] *= val;
multCol[val_, col_] := Part[$pixels, All, All, col] *= val;

(* because otherwise the slice of size 3 gets matched with the size 3 of v *)
paintBlockSafe[v_, {r_, c_}] := Module[
  {r1 = (r * b2) - d, c1 = (c * b2) - d, p1, p2},
  p1 = (r1 - b1) ;; r1; p2 = (c1 - b1) ;; c1;
  Part[$pixels, p1, p2, 1] = P1[v];
  Part[$pixels, p1, p2, 2] = P2[v];
  Part[$pixels, p1, p2, 3] = P3[v];
];

paintBlock[v_, {r_, c_}] := Module[
  {r1 = (r * b2) - d, c1 = (c * b2) - d},
  Part[$pixels, (r1 - b1) ;; r1, (c1 - b1) ;; c1] = v;
];

(**************************************************************************************************)

SetStrict[CompactArrayPlot]

Options[CompactArrayPlot] = {
  PixelConstrained -> Auto,
  ColorFunction -> Auto,
  (* ColorLegend -> None, *)
  Frame -> True,
  Mesh -> Automatic,
  MeshStyle -> Opacity[0.2],
  ImageSize -> {{50, 200}, {50, 200}}
};

CompactArrayPlot::badRank = "Array should be of rank 2 or 3, but had rank ``.";

CompactArrayPlot[array2_, opts___Rule] := Locals @ CatchMessages[CompactArrayPlot,

  UnpackSymbolsAs[CompactArrayPlot, {opts}, pixelConstrained, colorFunction, frame, mesh, meshStyle, imageSize];

  {w, h} = EnsurePair[imageSize];
  {w1, w2} = EnsurePair[w, PosIntQ];
  {h1, h2} = EnsurePair[h, PosIntQ];

  array = checkArray @ array2;
  pixelArray = ApplyColorFunctionToArray[colorFunction, array, 2];


  If[pixelArray === None, Return[Spacer[1]]];

  rc = {r, c} = Dims[pixelArray, 2]+1;
  maxPC = None;
  pc = pixelConstrained;

  If[Head[pc] === UpTo,
    maxPC = First @ pc; pc = Auto];
  SetAuto[pc, Floor @ Clip[Min /@ {{h1, w1} / rc, {h2, w2} / rc}, {1, 15}]];
  If[IntQ[maxPC], Part[pc, 2] = Min[Part[pc, 2], maxPC]];
  {p1, p2} = EnsurePair[pc, PosIntQ];

  retries = 1;
  Label[retry];
  ps = Select[Range[p1, p2], (# * c <= w2) && (# * r <= h2)&];
  wp = Mean[{w1, w2}]; hp = Mean[{h1, h2}];
  If[ps === {}, If[retries == 5, ps = {p1},
    {w1, h1} = Ceiling[{w1, h1} * 1.5]; w2 *= 1.5; h2 *= 1.5; retries++; Goto[retry]]];
  pb = MinimalBy[ps, Dist[# * (c+1), wp]^2 + Dist[# * (r+1), hp]^2&];
  pc = Round @ Median @ pb;
  SetAuto[mesh, pc > 3];

  graphics = iMeshImage[
    pixelArray, pc,
    Frame -> frame, Mesh -> mesh, MeshStyle -> meshStyle
  ];

  (* SetAuto[colorLegend, colorFunction];
     If[colorLegend =!= None, graphics //= ApplyLegend[colorLegend]];
  *)
  graphics
];

(**************************************************************************************************)

(* from NiceGrid *)
unitRange[n_] := Range[0, n-1]/(n-1);

upTo[len_, 1] := {1};
upTo[len_, n_] /; len <= n := Range @ len;
upTo[len_, n_] := Floor[unitRange[n] * (len - 1) + 1];
findBestAxisSplit[dims_] := Locals[
  i = FirstVectorIndexOf[dims, Min @ dims];
  {i, Delete[Range @ Len @ dims, i]}
];
findBestAxisSplit[dims_] := FirstVectorIndexOf[dims, Min @ dims];

splitAndSample[arr_] := splitAndSample[arr, Dims @ arr];
splitAndSample[arr_, dims_] := Locals[
  i = findBestAxisSplit[dims];
  indices = upTo[Part[dims, i], 5];
  spec = ConstList[All, Len @ dims];
  table = Table[
    n -> Extract[arr, ReplacePart[spec, i -> n]],
    {n, indices}
  ];
  {i, table}
];

Options[SmartArrayPlot] = {
  PixelConstrained -> UpTo[20],
  ImageSize        -> {500, 200},
  ShowDimensions   -> True
};

SetInitial[$ArrayPlotMagnitude, Automatic];

SmartArrayPlot[array_, opts___Rule] := Locals @ CatchMessages[SmartArrayPlot,

  arr = checkArray @ array;

  UnpackSymbolsAs[SmartArrayPlot, {opts}, imageSize, pixelConstrained, showDimensions];

  {w, h} = Round @ imageSize;
  dims = Dims @ arr;
  minMax = MinMax @ arr;
  mag = $ArrayPlotMagnitude;
  SetAuto[mag,
    abs = Max @ Abs @ minMax;
    Which[abs <= 0.001, 0.001, abs <= 0.01, 0.01, abs <= 0.1, 0.1, abs <= 1, 1, abs <= 10, 10, abs <= 100, 100, True, 1000]
  ];
  colFn = RedBlueColorFunction[mag];
  cap = CompactArrayPlot[#, PixelConstrained -> pixelConstrained,
    ColorFunction -> colFn, ImageSize -> {{20, w}, {20, h}}]&;
  isplit = None;
  rank = Len @ dims;
  If[insert = (First[dims] === 1 && rank > 1),
    dims //= Rest; arr //= First; rank--];
  res = Switch[rank,
    1, cap @ {arr},
    2, cap @ arr,
    3,
        {isplit, split} = splitAndSample[arr, dims];
        nSplit = Len[split];
        If[nSplit === 1,
          cap @ Part[split, 1, 2]
        ,
          w = Ceiling[w / nSplit];
          els = Labeled[cap @ #2, #1, Top, Spacings -> {0,0}]& @@@ split;
          Row[els, " "]
       ]
    ,
    True,
      $Failed
  ];
  If[!showDimensions, Return @ res];
  If[insert, PrependTo[dims, 1]];
  If[IntQ[isplit], dims //= MapAt[Style[#, Red]&, isplit]];
  dimsRow = Row[Riffle[dims, $timesStr], BaseStyle -> Bold];
  boundsRow = Row[RealString[#, 2]& /@ minMax, "\[VeryThinSpace]-\[VeryThinSpace]"];
  grid = Grid[
    {{res}, {Tooltip[dimsRow, boundsRow]}},
    Spacings -> {0.2,.5}, Background -> {{}, {White, GrayLevel[0.95]}},
    BaseStyle -> {ShowStringCharacters -> False}
  ];
  Style[grid, FontSize -> 10, FontFamily -> "Source Code Pro", LineBreakWithin->False, AutoSpacing -> False]
]

$timesStr = Style["\[ThinSpace]\[Times]\[ThinSpace]", FontWeight -> Plain, FontColor -> GrayLevel[0.5]];

(**************************************************************************************************)

checkArray[array_NumericArray] := Normal @ array;
checkArray[array_List] := array;
checkArray[array_] := ThrowMessage["notPlottableArray", array];

General::notPlottableArray = "Cannot plot non-array ``."

(**************************************************************************************************)

BitPlot = BinaryArrayPlot;

Options[BinaryArrayPlot] = {
  PixelConstrained -> 4
}

BinaryArrayPlot[array_, opts___Rule] :=
  BinaryArrayPlot[array, Auto, opts];

BinaryArrayPlot[array2_, digits2:(_Int|Auto), opts___Rule] := Locals @ CatchMessages[BinaryArrayPlot,

  UnpackSymbolsAs[BinaryArrayPlot, {opts}, pixelConstrained];

  array = checkArray @ array2;
  digits = digits2;
  {min, max} = MinMax @ array;
  Which[
    VecQ[array, NonNegativeIntegerQ],
      SetAuto[digits, If[max == 0, 1, Floor[1 + Log2 @ max]]];
      array = IntDigits[array, 2, digits];
    ,
    MatrixQ[array, NonNegativeIntegerQ],
      If[IntQ[digits] && LengthN[array] > digits,
        array = Take[array, All, digits]];
      If[max > 1, ThrowMessage["notBinaryArray", array]];
    ,
    True,
      ThrowMessage["notBinaryArray", array]
  ];
  CompactArrayPlot[1 - array, PixelConstrained -> pixelConstrained]
];

BinaryArrayPlot::notBinaryArray = "Input `` is not a binary array."

(**************************************************************************************************)

BinaryDigitsForm