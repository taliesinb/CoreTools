SystemExports[
  "FormHead",
    ClickForm, ClickFormOp, NiceTooltip, ErrorTooltip
];

PackageExports[
  "BoxFn",
    ClickBox, ClickBoxOp, NoClickBox, EventsBox,
    NiceTooltipBox, ErrorTooltipBox,
    CursorIconBox, StatusAreaBox
];

(**************************************************************************************************)

SetBoxFn @ NoClickBox;

NoClickBox[box_] := CursorIconBox["Arrow"]  @ EventsBox[{"MouseClicked", _} :> Null] @ box;

(**************************************************************************************************)

SetForm1 @ SetHoldR @ ClickForm;

CoreBox[ClickForm[expr_, body_]]          := ClickBox[MakeBoxes @ expr, body];
CoreBox[ClickForm[expr_, body1_, body2_]] := ClickBox[MakeBoxes @ expr, body1, body2];

SetBoxFn @ SetHoldR @ ClickBox;

ClickBox[box_, body_] := CursorIconBox["LinkHand"] @ EventsBox[{"MouseClicked", 1} :> body] @ box;
ClickBox[box_, body1_, body2_] := CursorIconBox["LinkHand"] @ EventsBox[{{"MouseClicked", 1} :> body1, {"MouseClicked", 2} :> body2}] @ box;

(**************************************************************************************************)

SetHoldA @ ClickFormOp;

ClickFormOp[body_][expr_]           := ClickForm[expr, body];
ClickFormOp[body1_, body2_][expr_]  := ClickForm[expr, body1, body2];

SetCurryBoxFn @ SetHoldA @ ClickBoxOp;

ClickBoxOp[body_][box_]             := ClickBox[box, body];
ClickBoxOp[body1_, body2_][box_]    := ClickBox[box, body1, body2];

(**************************************************************************************************)

SetCurry2BoxFn @ CursorIconBox;

CursorIconBox[boxes_, icon_Str] := TagBox[boxes, MouseAppearanceTag[icon]];

(**************************************************************************************************)

SetCurry2BoxFn @ StatusAreaBox;

StatusAreaBox[$Failed, _]        := $Failed;
StatusAreaBox[boxes_, label_Str] := TagBox[boxes, Identity, TagBoxNote -> label];
StatusAreaBox[boxes_, label_]    := TagBox[boxes, Identity, TagBoxNote -> ToString[label, InputForm]];

(**************************************************************************************************)

SetCurry2BoxFn @ EventsBox;

EventsBox[boxes_, rules_] := TagBox[boxes, EventHandlerTag @ ToList[rules, $eventHandlerRules]];

$eventHandlerRules = {Method -> "Preemptive", PassEventsDown -> Automatic, PassEventsUp -> False};

(**************************************************************************************************)

"NiceTooltip[expr$, tooltip$] displays expr$ with a well-formatted tooltip of tooltip$."
"ErrorTooltip[expr$, margs$$] displays expr$ with an well-formatted error tooltip similiar to Message[margs$$]."

SetForm1[NiceTooltip, ErrorTooltip];

CoreBox[NiceTooltip[expr_, tooltip_]] := NiceTooltipBox[MakeBoxes @ expr, BlockInteractive @ MakeBoxes @ tooltip];
CoreBox[ErrorTooltip[expr_, rest___]] := ErrorTooltipBox[MakeBoxes @ expr, rest];

(**************************************************************************************************)

SetCurry2BoxFn @ NiceTooltipBox;

NiceTooltipBox[$Failed, ___] := $Failed;
NiceTooltipBox[boxes_, tooltipBoxes_, opts___] := TagBox[
  TooltipBox[
    boxes,
    PaneBox[tooltipBoxes,
      ImageMargins -> {{5, 5}, {5, 5}},
      ImageSize -> {{20, All}, {15, All}}
    ],
    Alignment -> Center,
    TooltipStyle -> {opts, Background -> GrayLevel[1], CellFrameColor -> None, CellFrame -> 0},
    TooltipDelay -> 0
  ],
  MouseAppearanceTag["Arrow"]
];

(**************************************************************************************************)

SetBoxFn @ ErrorTooltipBox;

ErrorTooltipBox[boxes_] := FrameBox[boxes,
  ContentPadding -> False, FrameStyle -> $LightRed,
  Background -> RGBColor[1,0.95,0.95]
];

ErrorTooltipBox[boxes_, rest__] := NiceTooltipBox[
  ErrorTooltipBox @ boxes,
  BlockInteractive @ MsgBox[rest]
];
