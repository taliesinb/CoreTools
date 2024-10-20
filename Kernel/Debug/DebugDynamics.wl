PackageExports[
  "DebuggingFunction", DynamicCurve, DynamicMakeBox
];

(**************************************************************************************************)

circlePoints[2] := {{-1, 0}, {1, 0}};
circlePoints[n_] := Rev @ CirclePoints[n];

DynamicCurve[n_Int, fn_] := Rep[
  MakeHoldComplete[fn, FmX],
  HoldC[body_] :>
    DynamicModule @@ Hold[
      {FmX = circlePoints[n]},
      If[$PackageCurrentlyLoading, "LOADING",
        LocatorPane[Dynamic[FmX], Graphics[
          Dynamic[If[TrueQ[Session`$CoreToolsLoaded], body, "UNLOADED"], TrackedSymbols :> {FmX}],
          PlotRange -> 1.1, Frame -> True, FrameTicks -> None, Axes -> None
        ]]
      ],
      TrackedSymbols :> {FmX}
    ]
];

DynamicCurve[{n_Int, specSeq__}, fn_] := With[
  {specList = {specSeq}},
  {specData = MapThread[toDynSpec, {{specSeq}, Take[$formals, Len @ specList]}]},
  {specVars = Prepend[FmX] @ Col1[specData],
   initList = Prepend[N @ circlePoints[n]] @ Col2[specData]},
  {specSets = MapThread[$set$, {specVars, initList}],
   controls = Col3[specData],
   heldFn = PostCompose[fn, PrivHold]},
  Rep[
    MakeHoldComplete[fn, Sequence @@ specVars],
    HoldC[body_] :>
      Apply[DynamicModule, Hold[
        specSets,
        Labeled[
          If[$PackageCurrentlyLoading, "LOADING", LocatorPane[Dynamic[FmX], Graphics[
            Dynamic[If[TrueQ[Session`$CoreToolsLoaded], body, "UNLOADED"], TrackedSymbols :> specVars],
            PlotRange -> 1.1, Frame -> True, FrameTicks -> None, Axes -> None
          ]]],
          Row[{
            Button[Style["\[DownArrow]", Bold], PrintInputCell @ specVars, Appearance -> None],
            "  ", Column[controls], "  ",
            Button[Style["\[DownArrow]", Bold], PrintInputCell @ heldFn @@ specVars, Appearance -> None]
          }]
        ],
        TrackedSymbols :> specVars
      ] /. $set$ -> Set]
  ]
];

$formals = {\[FormalA], \[FormalB], \[FormalC], \[FormalD], \[FormalE], \[FormalF], \[FormalD]}

toDynSpec[{min_ ? NumericQ, max_ ? NumericQ}, sym_] := {sym, Avg[min, max], Slider[Dynamic @ sym, {min, max}]};
toDynSpec[max_ ? NumericQ, sym_] := toDynSpec[{0, max}, sym];
toDynSpec[list_List, sym_] := {sym, First @ list, RadioButtonBar[Dynamic @ sym, list]};

(* DynamicCurve[3, {Red, Map[Disk[#, .1] &, #]} &] *)

(**************************************************************************************************)

SetHoldF[DynamicMakeBox];

DynamicMakeBox[body_] := Block[
  {Print, EchoPrint, RawPrint},
  Catch[
    TrapMessages[
      ToBoxes @ TimeConstrained[body, .2],
      Throw["\"MSGS\"", $sde$]
    ],
    $sde$
  ]
];
