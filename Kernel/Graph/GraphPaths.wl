SystemExports[
  "SymbolicHead", GraphPath
]

(**************************************************************************************************)

CoreBox[GraphPath[e___List]] := GridBox[
  List @ List @ ItemBox[
    RiffledRowBox["\[LeftArrow]"] @ Map[pathElemBoxes, {e}],
    Frame -> {{False, False}, {True, False}},
    FrameStyle -> Dashed
  ]
];

pathElemBoxes[{out_, key_}]       := SubscriptBox[MakeBoxes @ out, MakeBoxes @ key];
pathElemBoxes[{out_, key_, tag_}] := SubsuperscriptBox[MakeBoxes @ out, MakeBoxes @ key, MakeBoxes @ tag];

