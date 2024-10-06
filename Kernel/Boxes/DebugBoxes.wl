PackageExports[
  "BoxFunction", ColorFrameBox, PathBox,
  "Function",    ShortPathStr
];

(**************************************************************************************************)

SetBoxFn @ ColorFrameBox;

ColorFrameBox[str_Str, color:ColorP, sz_:10] :=
  NoLineBreakBox @ ToBoxes @ Framed[
    Style[str, FontSize -> sz, $colorFrameStyle],
    Background -> color, FrameStyle -> Darker[color, .2],
    $colorFrameOpts
  ];

$colorFrameStyle = Seq[
  FontFamily -> "Source Code Pro",
  Bold, FontColor -> Black,
  ShowStringCharacters -> False
];

$colorFrameOpts = Seq[
  ContentPadding -> False, RoundingRadius -> 2,
  ImageSize -> {Auto, 13}, FrameMargins -> {{5, 5}, {0, 0}},
  BaselinePosition -> Baseline
];

(**************************************************************************************************)

SetBoxFn @ PathBox;

PathBox[path_Str, line_:None] := Locals[
  type = toPathType @ path;
  color = Lookup[$pathTypeColor, type, $LightRed];
  pathElems = FileNameSplit @ path;
  shortPath = ShortPathStr @ path;
  If[IntQ[line], shortPath = StrJoin[shortPath, ":", IntStr @ line]];
  ColorFrameBox[shortPath, color]
];

$pathTypeColor = Dict[
  None      -> $LightRed,
  Directory -> $LightBlue,
  File      -> GrayLevel[0.9],
  "URL"     -> $LightPurple
];

$pathColStyle = Seq[
  LineBreakWithin -> Automatic,
  ShowStringCharacters -> False,
  FontWeight -> "DemiBold",
  FontSize -> 13,
  FontFamily -> "Source Code Pro"
];

toPathType[path_Str] := If[
  StrStartsQ[path, ("http" | "https" | "git" | "file" | "ssh") ~~ ":"], "URL",
  Quiet @ FileType @ path
];


(* If[!$Interactives,
    pathBox,
    pathCol = ColumnBox[{
      Row[fmtPathElem[GrayLevel[0.93]] /@ Most[pathElems], Style["/", GrayLevel[0.7]]],
      fmtPathElem[RGBColor[.93,.93,1]] @ Last @ pathElems
    }];
    pathCol = StyleBox[pathCol, $pathColStyle];
    ClickBox[
      NiceTooltipBox[pathBox, pathCol],
      openMsgPath[path, line]
    ]
  ];
];

fmtPathElem[col_][str_] := StyleBox[
  If[str === "", "", Pane @ insertBreaks @ str],
  LineBreakWithin -> False,
  Background -> col
]

insertBreaks[str_] := InsertLinebreaks[str, 80];

 *)

ShortPathStr[ostr_Str] := ShortPathStr[ostr, 25];

ShortPathStr[ostr_Str, target_Int] := Locals[
  str = StrRep[ostr, $HomeDirectory <> $PathnameSeparator -> "~/"];
  If[StrLen[str] <= target, Return @ str];
  segs = Rev @ FileNameSplit @ str;
  segs2 = Rev @ TakeWhile[n = 0; segs, (n += StrLen[#] + 1) < target&];
  If[segs2 === {}, segs2 = Take[segs, 1]];
  If[Len[segs2] < Len[segs], PrependTo[segs2, "\[Ellipsis]"]];
  str2 = FileNameJoin @ segs2;
  If[StrLen[str2] > target, str2 = truncPathElem[str2, target]];
  If[StrLen[str2] < StrLen[str], str2, str]
];

_ShortPathStr := "????";

truncPathElem[str_, target_] :=
  FileNameJoin @ MapLast[
    delChars[#, StrLen[str] - target]&,
    FileNameSplit @ str
  ];

delChars[str_, del_] /; del <= 2 := str;
delChars[str_, del_] /; del >= StrLen[str]-1 := LDotsS;
delChars[str_, del_] := Block[{l, m, r},
  m = Floor[StrLen[str] / 2];
  l = Ceiling[m - del / 2];
  r = Floor[m + del / 2];
  StringReplacePart[str, LDotsS, {l, r - 1}]
]
