PackageExports[
  "IOFunction",
    ToImage,     ToImageSize,     ToImageData,
    ToTextImage, ToTextImageSize, ToTextImageData,
    ToImageUncached, ToImageListPNG,
  "Predicate",
    ErrorImageQ
];

SessionExports[
  "CacheVariable", $ToImageCache, $ToImageSizeCache, $ToImageDataCache
];

(**************************************************************************************************)

(* ImageFormattingWidth *)

SetInitial[$ToImageCache, $ToImageSizeCache, $ToImageDataCache, UDict[]];

toExportNotebook[expr_, background_] := toExportCell[expr, Background -> background];

(* see Scratch/Rasterization.txt *)

(*************************************************************************************************)

$exportCellOptions = Sequence[
  ShowCellBracket -> False, CellMargins -> 0,
  CellFrame -> None, CellFrameMargins -> 0, CellContext -> "Global`",
  GraphicsBoxOptions -> {ImageSize -> Medium}, Graphics3DBoxOptions -> {ImageSize -> Medium}
];

toExportCell[c:(Cell[CellGroupData[cells_, ___], ___] | CellGroupData[cells_, ___]), opts___] :=
  Notebook[List @ c, opts, ShowCellBracket -> False, CellContext -> "Global`"];

toExportCell[Cell[args___], opts___] :=
  Cell[args, $exportCellOptions, opts];

toExportCell[expr_, opts___] := Cell[
  BoxData @ ToBoxes @ expr, "Output",
  $exportCellOptions, opts
];

(*************************************************************************************************)

toExportPacket[expr_] := toExportPacket[expr, None];

toExportPacket[expr_, background_, verbose_:False] := ExportPacket[
  toExportNotebook[expr, background],
  "ImageObjectPacket", ColorSpace -> RGBColor, Verbose -> verbose, "AlphaChannel" -> False,
  "DataCompression" -> False, ImageResolution -> 144
];

toExportPacket[expr_, Transparent, verbose_:False] := ExportPacket[
  toExportNotebook[expr, Transparent],
  "ImageObjectPacket", ColorSpace -> RGBColor, Verbose -> verbose, "AlphaChannel" -> True,
  "DataCompression" -> False, ImageResolution -> 144
];

(*************************************************************************************************)

ToImage::usage =
"ToImage[expr$] is like %Rasterize, but faster.
ToImage[expr$, background$] specifies a background color.
* the MT stylesheet will be used if MT template boxes are present."

ToImage::fail = "Failed to rasterize expression ``.";
ToImage[expr_, bg_:None] :=
  CachedTo[$ToImageCache, Hash @ {expr, bg}, iToImage[expr, bg]];

ToImageUncached[expr_, bg_:None] :=
  iToImage[expr, bg];

iToImage[expr_, bg_] := Locals[
  res = CallFrontEnd @ toExportPacket[expr, bg];
  If[ImageQ[res], res, ErrorMsg[ToImage::fail, expr]]
];

(*************************************************************************************************)

SetStrict @ ToImageListPNG;

$imageFilePath = DataPath["Cache", "ToImageList"];

ToImageListPNG[list_List] := Locals @ CatchMessages[
  path = PathJoin[$imageFilePath, Base36Hash[list] <> ".png"];
  If[FileExistsQ[path],
    images = ImageReadPNG[path];
    If[!VectorQ[images, ImageQ], ReturnFailed[]];
    images //= Map[Image[#, ImageSize -> ImageDimensions[#]/2]&];
    Return @ {images, path}
  ];
  images = If[Len[list] > 8,
    MonitorProgress @ Map[ToImage, list],
    Map[ToImage, list]
  ];
  If[!VectorQ[images, ImageQ], ReturnFailed[]];
  EnsureDirectory @ $imageFilePath;
  ImageWritePNG[path, images, "None", "", Automatic, "", "", Automatic, 32768];
  {images, path}
];

(*************************************************************************************************)

ToImageData::usage =
"ToImageData[expr$] is like %ToImage but returns a tuple containing the image and various metadata.
ToImageData[expr$, background$] specifies a background color.
* the tuple is {image$, size$, bounds$, regions$}."

ToImageData[expr_, bg_:None] :=
  CachedTo[$ToImageDataCache, Hash @ {expr, bg}, iToImageData[expr, bg]];

iToImageData[expr_, bg_] := Locals[
  res = CallFrontEnd @ toExportPacket[expr, bg, True];
  If[!MatchQ[res, {_Image, {__Rule}}], ReturnMsg[ToImageData::fail, expr]];
  {image, metadata} = res;
  {boundingBox, baseline, regions} = Lookup[metadata, {"BoundingBox", Baseline, "Regions"}];
  rasterSize = bboxToRasterSize[{Transpose @ boundingBox, baseline}];
  regions = Map[toImageRegion, regions];
  {image, rasterSize, boundingBox, regions}
];

toImageRegion[{anno_, {{a_, b_}, {c_, d_}}}] := anno -> {{a, d}, {b, c}};

(*************************************************************************************************)

ToImageSize::fail = "Failed to obtain raster size for input with head ``.";
ToImageSize[expr_, returnBaseline_:False] :=
  If[returnBaseline, Id, TakeOp[2]] @ iToImageSize1[expr];

iToImageSize1[expr_] :=
  CachedTo[$ToImageSizeCache, Hash @ expr, iToImageSize2 @ expr];

iToImageSize2[expr_] := Locals[
  res = CallFrontEnd @ Insert[toExportPacket[expr, None], "BoundingBox", 2];
  bbox = bboxToRasterSize @ res;
  If[FailureQ[bbox], ErrorMsg[ToImageSize::fail, expr], bbox]
];

bboxToRasterSize[other_] := $Failed;
bboxToRasterSize[{{{x1_, x2_}, {y1_, y2_}}, d_}] := Locals[
  {w, h} = Floor[({y1 - x1, y2 - x2} * 2) + 1 / 2];
  d2 = Floor[d * 2 + 1/2];
  {w, h, d2}
 ]

(**************************************************************************************************)

ToTextImage[Text[content_, Shortest[___], opts___Rule]] :=
  ToImage[styleAsText[content, opts]];

(*************************************************************************************************)

ToTextImageSize[Text[content_, Shortest[___], opts___Rule], returnBaseline_:False] :=
  ToImageSize[styleAsText[content, opts], returnBaseline] / 2;

(*************************************************************************************************)

ToTextImageData[Text[content_, Shortest[___], opts___Rule]] :=
  ToImageData[styleAsText[content, opts]];

(*************************************************************************************************)

(* TODO: remove this form modifier thing *)
styleAsText[a_, l___] := Style[a, "Graphics", l];
styleAsText[a_, l___, BaseStyle -> s_, r___] := Style[a, "Graphics", Sequence @@ ToList @ s, l, r];

(**************************************************************************************************)

SetPred1 @ ErrorImageQ;

ErrorImageQ[image_Image] :=
  Count[Catenate @ ImageData[image, "Byte"], {255, 242, 242} | {255, 89, 89, 20}] > 50;
