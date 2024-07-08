SystemExports[
  "GraphicsFunction",
    CompactArrayPlot, BinaryArrayPlot, MeshImage, SmartArrayPlot,
  "SpecialVariable",
    $ArrayPlotMagnitude
];

PackageExports[
  "FormHead",
    NiceObject,
  "BoxFunction",
    NiceObjectBoxes
];

(*************************************************************************************************)

MakeBoxes[NiceObject[head_String, args_, opts___Rule], StandardForm] := Locals[
  argBoxes = If[ListQ[args], ToBoxes /@ args, ToBoxes @ args];
  argBoxes /. InterpretationBox[b_, _] :> b;
  NiceObjectBoxes[head, argBoxes, opts]
];

NiceObjectBoxes[head_, args_, opts___Rule] := StyleBox[
  RowBox[{StyleBox[head, Bold], GridBox[
    List @ ToList[
      ItemBox["", Frame -> {{True, False}, {True, True}}, ItemSize -> {.2, All}],
      If[ListQ[args], commaSep @ args, args],
      ItemBox["", Frame -> {{False, True}, {True, True}}, ItemSize -> {.2, All}]
    ],
    opts, FrameStyle -> AbsoluteThickness[2],
    ColumnSpacings -> 0, GridFrameMargins -> {{0.,0.},{0.,1.}},
    BaselinePosition -> {{1,2}, Baseline}, RowMinHeight -> 0.5
  ]}],
  LineBreakWithin -> False, AutoSpacing -> False
];

commaSep[list_] := Riffle[list, StyleBox[",\[ThinSpace]", Gray]];

(**************************************************************************************************)

DeclareStrict[MeshImage]

MeshImage::meshImageBlockSize = "Block size `` is not a positive integer.";
MeshImage::meshImageDataNumeric = "Input data should be a numeric array."
MeshImage::meshImageDataDims = "Input data should be of shape (w * h * 3) or (w * h), but was ``."

Options[MeshImage] = {
  Frame      -> True,
  Mesh       -> True,
  FrameStyle -> GrayLevel[0.4],
  MeshStyle  -> GrayLevel[0.4]
};

MeshImage[array2_, blockSize_, OptionsPattern[]] := Locals @ CatchError[MeshImage,

  UnpackOptions[frame, mesh, frameStyle, meshStyle];

  If[!PosIntQ[blockSize], ReturnFailed["meshImageBlockSize", blockSize]];
  If[!BoolQ[frame], ThrowOptionMsg[Frame, frame]];
  If[!BoolQ[mesh],  ThrowOptionMsg[Mesh, mesh]];

  Switch[frameStyle,
    None,                frame = False,
    GrayLevel[UnitNumP], frameStyle = P1 @ frameStyle,
    _,                   ThrowOptionMsg[FrameStyle, frameStyle]];

  meshFading = None;
  Switch[meshStyle,
    None,                mesh = False,
    Opacity[UnitNumP],   meshFading = OneMinus @ P1 @ meshStyle,
    GrayLevel[UnitNumP], meshStyle = P1 @ meshStyle,
    _,                   ThrowOptionMsg[Mesh, meshStyle]];

  array = EnsurePackedReals[array2, ThrowRealArrayMsg[array2, {2, 3}]];
  dims = Dims @ array;
  If[!MatchQ[dims, {_, _, 3} | {_, _}], ReturnFailed["meshImageDataDims", dims]];

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
    ScanIndexed[paintBlockAdditive, ToPackedArray @ array, {2}];
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
  spec_           := ThrowOptionMsg[Frame, spec];
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

DeclareStrict[CompactArrayPlot]

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

CompactArrayPlot[array_, OptionsPattern[]] := Locals @ CatchError[CompactArrayPlot,

  UnpackOptions[pixelConstrained, colorFunction, frame, mesh, meshStyle, imageSize];

  {w, h} = EnsurePair[imageSize];
  {w1, w2} = EnsurePair[w, PosIntQ];
  {h1, h2} = EnsurePair[h, PosIntQ];

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

  graphics = MeshImage[
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
  ImageSize -> {500, 200},
  "ShowDimensions" -> True
};

SetInitial[$ArrayPlotMagnitude, Automatic];

SmartArrayPlot[array_, OptionsPattern[]] := Locals[
  arr = array;
  If[NumericArrayQ[arr], arr = Normal @ array];
  UnpackOptions[imageSize, pixelConstrained, showDimensions];
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
  iSplit = None;
  rank = Len @ dims;
  If[insert = (F[dims] === 1 && rank > 1),
    dims //= Rest; arr //= First; rank--];
  res = Switch[rank,
    1, cap @ {arr},
    2, cap @ arr,
    3,
        {iSplit, split} = splitAndSample[arr, dims];
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
  If[IntQ[iSplit], dims //= MapAt[Style[#, Red]&, iSplit]];
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

Options[BinaryArrayPlot] = {
  PixelConstrained -> 4
}

BinaryArrayPlot[array_, opts:OptionsPattern[]] :=
  BinaryArrayPlot[array, Auto, opts];

BinaryArrayPlot[array_, digits:(_Int|Auto), OptionsPattern[]] := Locals[
  UnpackOptions[pixelConstrained];
  {min, max} = MinMax @ array;
  Which[
    VecQ[array, NonNegativeIntegerQ],
      SetAuto[digits, If[max == 0, 0, Floor[1 + Log2 @ max]]];
      array = IntDigits[array, 2, digits];
    ,
    MatrixQ[array, NonNegativeIntegerQ],
      If[IntQ[digits] && LastDim[array] > digits,
        array = Take[array, All, digits]];
      If[max > 1, ReturnFailed[]];
    ,
    True,
      ReturnFailed[];
  ];
  CompactArrayPlot[1 - array, PixelConstrained -> pixelConstrained]
];


