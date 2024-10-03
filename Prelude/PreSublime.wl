BeginPackage["Prelude`", {"Session`"}];

SystemExports[
  "IOFunction",
    SublimeOpen,
    SublimeSeek,
    SublimeRun,

  "Head",
    FileLocation,

  "Option",
    SublimeProject,
    OpenInNewWindow
];

SessionExports[
  "Variable",
    $SublimeApplicationPath,
    $SublimeProjectsPath,
    $SublimeRunCount
];

Begin["`Sublime`Private`"]

(*************************************************************************************************)

If[!IntegerQ[Session`$SublimeRunCount], Session`$SublimeRunCount = 0];

(* TODO: find equivalents for other OSes *)
If[!StringQ[Session`$SublimeApplicationPath],
  Session`$SublimeApplicationPath = "/Applications/Sublime\\ Text.app/Contents/SharedSupport/bin/subl"];

If[!StringQ[Session`$SublimeProjectsPath],
  Session`$SublimeProjectsPath = ExpandFileName @ "~/git/project-files"];

(*************************************************************************************************)

SublimeRun[args2___] := Module[
  {args = Flatten @ List @ args2},
  If[Session`$SublimeRunCount++ > 16, Return[$Failed]];
  If[!VectorQ[args, StringQ], $Failed,
    If[Run[StringJoin[Session`$SublimeApplicationPath, " ", Riffle[Flatten @ List @ args, " "]]] === 0, Null, $Failed]
  ]
]

(*************************************************************************************************)

Options[SublimeOpen] = {
  SublimeProject  -> Automatic,
  OpenInNewWindow -> True
};

SublimeOpen[spec_, OptionsPattern[]] := Block[
  {arg, $lastSubPath = None,
   project = OptionValue[SublimeProject],
   newWindow = OptionValue[OpenInNewWindow]},
  arg = procSublimeArg @ spec;
  If[project === Automatic, project = findProjFile0 @ $lastSubPath];
  opts = Which[StringQ[project], {"--project", project}, newWindow, "-n", True, {}];
  SublimeRun[arg, opts]
];

(*************************************************************************************************)

SublimeSeek[path_String, tcount_Integer] := Block[
  {stream, pos1 = 1, pos2 = 1, count = 0, expr, chars1, chars2, iline,
    $NewSymbol, $Context = "DummyContext`", $ContextPath = {"DummyContext`", "System`"}},
  stream = OpenRead[path];
  Off[General::shdw];
  While[count < tcount,
    pos1 = pos2;
    expr = Read[stream, HoldComplete @ Expression];
    pos2 = StreamPosition[stream];
    If[expr =!= HoldComplete[Null], count += 1];
    If[expr === $Failed, Break[]];
    If[expr === EndOfFile, Break[]];
  ];
  Clear[expr];
  On[General::shdw];
  SetStreamPosition[stream, pos1];
  While[
    pos1 = StreamPosition[stream];
    MemberQ[{32, 9, 10}, Read[stream, Byte]], Null];
  SetStreamPosition[stream, 0];
  chars2 = ReadList[stream, Byte, pos2 - 1];
  Close[stream];
  chars1 = Take[chars2, pos1];
  iline = Ceiling[(Count[chars1, 10] + Count[chars2, 10])/2] + 1;
  iline = Count[chars1, 10] + 1;
  SublimeOpen[path -> iline]
];

(*************************************************************************************************)

SublimeOpen::invalid = "`` is not a valid element for SublimeOpen.";
procSublimeArg[other_] := (Message[SublimeOpen::invalid, other]; $Failed);
procSublimeArg[list_List] := Map[procSublimeArg, list];
procSublimeArg[Rule[path_String, "StreamPos" -> n_Integer]] := procSublimeArg[path -> findCharPos[path, n]];
procSublimeArg[Rule[path_String, n_Integer]] := StringJoin[procSublimeArg[path], ":", IntegerString[n]];
procSublimeArg[path_String] := StringJoin["'", $lastSubPath = ExpandFileName[path], "'"];

(*************************************************************************************************)

findCharPos[path_String, n_Integer] := Module[{stream},
  If[!FileExistsQ[path], Return @ 1];
  stream = OpenRead[path];
  chars = ReadList[stream, Byte, n];
  Close[path];
  If[!ListQ[path], Return @ 1];
  Count[chars, 10]
]

(*************************************************************************************************)

findProjFile0[path_String] := Block[{key, projPaths},
  Do[
    projPaths = $projectsPathsAssoc[key];
    If[AnyTrue[projPaths, StringStartsQ[path, #]&], Return[key, Block]],
    {key, Keys @ $projectsPathsAssoc}
  ];
  findProjFile1 @ If[FileType[path] === File, FileNameDrop @ path, path]
];

$projectsPathsAssoc := $projectsPathsAssoc = If[
  FileExistsQ[Session`$SublimeProjectsPath],
  Association @ Map[# -> loadSublimeProjectPaths[#]&, FileNames["*.sublime-project", Session`$SublimeProjectsPath]],
  Association[]
];

(*************************************************************************************************)

$invalidProjDirs = None | $RootDirectory | $HomeDirectory | FileNameJoin[{$HomeDirectory, "git"}];
findProjFile1[$invalidProjDirs] := None;

findProjFile1[path_String] := First[
  FileNames["*.sublime-project", path],
  findProjFile1 @ FileNameDrop @ path
];

(*************************************************************************************************)

$projPathRegex = RegularExpression["\"path\":\\s*\"([/.][^\"\n]+)\""] :> "$1";
loadSublimeProjectPaths[path_] := loadSublimeProjectPaths[path] = Module[
  {str, paths, base},
  str = ByteArrayToString @ ReadByteArray @ path;
  paths = StringCases[str, $projPathRegex];
  base = FileNameDrop @ path;
  paths //= Map[If[StringStartsQ[#, "./" | "../"], FileNameJoin[{base, #}], #]&];
  ExpandFileName /@ paths
]

(*************************************************************************************************)

FileLocation /: SystemOpen[FileLocation[s_, n_:None]] := openFileLoc[s, n];

Format[FileLocation[s_String], OutputForm] := StringJoin["\"", s, "\""];
Format[FileLocation[s_String, n_Integer], OutputForm] := StringJoin["\"", s, ":", IntegerString @ n, "\""];
MakeBoxes[FileLocation[s_String], StandardForm] := fileLocBoxes[s];
MakeBoxes[FileLocation[s_String], TraditionalForm] := fileLocBoxes[s];
MakeBoxes[FileLocation[s_String, n_Integer], StandardForm] := fileLocBoxes[s, n];
MakeBoxes[FileLocation[s_String, n_Integer], TraditionalForm] := fileLocBoxes[s, n];

fileLocBoxes[path_String, line_:None] := With[
  {type = If[StringStartsQ[path, ("http" | "https" | "git" | "file" | "ssh") ~~ ":"], "URL", Quiet @ FileType @ path]},
  {color = Switch[type, Directory, RGBColor["#6caff4"], File, GrayLevel[0.9], "URL", RGBColor["#bbaff2"], _, RGBColor["#ff775e"]],
   pathElems = FileNameSplit @ path},
  {pathRow = ToBoxes @ Column[{
    Row[fmtPathElem[GrayLevel[0.93]] /@ Most[pathElems], Style["/", GrayLevel[0.7]]],
    fmtPathElem[RGBColor[.93,.93,1]] @ Last @ pathElems
  }]},
  clickBox[
    codeTooltipBoxes[
      tightColoredBoxes[If[IntegerQ[line], StringJoin[shortenPath @ path, ":", IntegerString @ line], shortenPath @ path], color],
      StyleBox[pathRow, LineBreakWithin -> Automatic, ShowStringCharacters -> False, FontWeight -> "DemiBold", FontSize -> 13, FontFamily -> "Source Code Pro"],
      {700, 200}
    ],
    openFileLoc[path, line]
  ]
];

fmtPathElem[col_][str_] := Style[
  If[str === "", "", Pane @ str],
  LineBreakWithin -> False,
  Background -> col
];

shortenPath[path_] := Module[{str, n, segs, segs2, str2},
  str = StringReplace[path, $HomeDirectory <> $PathnameSeparator -> "~/"];
  If[StringLength[str] <= 45, Return @ str];
  n = 0;
  segs = Reverse @ FileNameSplit @ str;
  segs2 = Reverse @ TakeWhile[segs, (n += StringLength[#]) < 45&];
  If[segs2 === {}, segs2 = Take[segs, 1]];
  If[Length[segs2] < Length[segs], PrependTo[segs2, "\[Ellipsis]"]];
  str2 = FileNameJoin @ segs2;
  If[StringLength[str2] > 45, str2 = truncateFileElemTo[str2, 45]];
  If[StringLength[str2] < StringLength[str], str2, str]
];

truncateFileElemTo[str_, n_] :=
  FileNameJoin @ MapAt[trimStrOut[#, StringLength[str] - n]&, FileNameSplit @ str, -1];

trimStrOut[str_, n_] := Which[
  n < 3,          str,
  n >= StringLength[str]-1,  "\[Ellipsis]",
  True, Block[{l, m, r},
    m = Floor[StringLength[str] / 2];
    l = Ceiling[m - n/2]; r = Floor[m + n/2];
    StringReplacePart[str, "\[Ellipsis]", {l, r-1}]
  ]
];

openFileLoc[path_, _] := SublimeOpen[path];
openFileLoc[path_, loc_Integer] := SublimeOpen[path -> loc];

(*************************************************************************************************)

SetAttributes[clickBox, HoldRest];

clickBox[box_, body_] := TagBox[
  TagBox[box, EventHandlerTag[{
    {"MouseClicked", 1} :> body,
    Method -> "Preemptive",
    PassEventsDown -> Automatic, PassEventsUp -> True
    }]
  ],
  MouseAppearanceTag["LinkHand"]
];

tightColoredBoxes[str_String, color_, sz_:10] := ToBoxes @ Style[Framed[
  Style[str, FontFamily -> "Source Code Pro", FontSize -> sz, Bold, FontColor -> Black, ShowStringCharacters -> False],
  Background -> color, FrameStyle -> Darker[color, .2],
  ContentPadding -> False, RoundingRadius -> 2,
  ImageSize -> {Automatic, 13}, FrameMargins -> {{5, 5}, {0, 0}},
  BaselinePosition -> Baseline
], LineBreakWithin -> False];

codeTooltipBoxes[exprBoxes_, tooltipBoxes_, {w_, h_}] := TagBox[
  TooltipBox[
    exprBoxes,
    PaneBox[tooltipBoxes,
      ImageMargins -> {{5, 5}, {5, 5}},
      ImageSize -> {{20, w}, {20, h}},
      Background -> GrayLevel[1]
    ],
    Alignment -> Center,
    TooltipStyle -> {Background -> GrayLevel[1], CellFrameColor -> None, CellFrame -> 0},
    TooltipDelay -> 0
  ],
  MouseAppearanceTag["Arrow"]
];

(*************************************************************************************************)

End[]

EndPackage[]