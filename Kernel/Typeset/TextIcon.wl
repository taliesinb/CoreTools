PackageExports[
  "BoxFn", TextIconBox, NamedTextIconBox
];

(**************************************************************************************************)

SetBoxFn[NamedTextIconBox, TextIconBox];

NamedTextIconBox["Times", opts_:{}, pad_:{{1,1}, {1,1}}] := TextIconBox[
  StyleBox[LineBox[{{{-1, -1}, {1, 1}}, {{-1, 1}, {1, -1}}}], Seq @@ opts],
  {{-1, 1}, {-1, 1}}, {1, 1} * .6, None, 10, pad
];

NamedTextIconBox["SquareUnion", opts_:{}, pad_:{{1,1}, {1,1}}] := TextIconBox[
  StyleBox[LineBox[{{-1, 1}, {-1, -1}, {1, -1}, {1, 1}}], Seq @@ opts],
  {{-1, 1}, {-1, 1}}, {0.7, .9}, None, 9, pad
];

TextIconBox[boxes_, bounds_, baseImageSize_, background_, bshift_, pad:{{l_, r_}, {b_, t_}}] :=
  DynamicBox[AdjustmentBox[
    Construct[
      GraphicsBox,
      boxes,
      PRange -> bounds, PMargin -> 0, AspectRatio -> Full, PClip -> False,
      ImageSize -> Ceiling[baseImageSize * Round @ CurrentValue[FontSize] + {l + r, b + t} , .5],
      ImagePadding -> pad, BLinePos -> Axis, Background -> background
    ],
    BoxBaselineShift -> (-bshift / Round @ CurrentValue[FontSize])
  ], TrackedSymbols :> {}
];
