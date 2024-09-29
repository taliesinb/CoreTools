BeginPackage["Prelude`"]

SessionExports[

  "Variable",
    $SessionLastEvaluationTime,
    $SessionEvaluationsCount,
    $SessionCurrentEvaluationStartTime,
    $SessionCurrentEvaluationPrintCount,
    $SessionMaxEvaluationPrintCount,
    $CurrentEvaluationCellState, $EvaluationsSinceDict,

  "SpecialVariable",
    $PreEvaluationHook, $PostEvaluationHook, $ShiftReturnHookInstalled,

  "IOFunction",
    CreateCachedBox,
    CachedBox,

  "Function",
    DefaultPreEvaluationHook, DefaultPostEvaluationHook,
    SaveEvaluationCellState, RestoreEvaluationCellState,
    InstallSessionHooks, UninstallSessionHooks,
    UniqueSessionID,

  "Predicate",
    SessionEvaluatedSinceQ
];

Begin["`Session`Private`"]

(*************************************************************************************************)

(* if $PreEvaluationHook etc are in the normal context path, the FE will save them wrong between reloads *)

(*************************************************************************************************)

(*
do print veto!
Internal`$PrintFormatter = Function[Null,
  If[TrueQ[$SessionEvaluationsCount,
  HoldAllComplete
];
*)

(*************************************************************************************************)

SetAttributes[{CreateCachedBox, $held$}, HoldAllComplete];

If[!AssociationQ[$boxCache], $boxCache = Association[]];

CreateCachedBox[expr_]       := iCreateCachedBox[$held$ @ expr, Hash @ Unevaluated @ expr]
CreateCachedBox[expr_, key_] := iCreateCachedBox[$held$ @ expr, key];

iCreateCachedBox[held_, key_] := (
  If[!KeyExistsQ[$boxCache, key],
    If[Len[$boxCache] > 32, $boxCache = Drop[$boxCache, 8]];
    $boxCache[key] = held;
  ];
  DynamicBox[Last @ CachedBox[key, {"\"EXPIRED\""}], TrackedSymbols :> {}]
);

(*************************************************************************************************)

CachedBox[key_, _] := Quiet @ List @ iCachedBox[key, Lookup[$boxCache, Key @ key, $missing$]];

iCachedBox[key_, evaluated_]    := evaluated;
iCachedBox[key_, $missing$]     := StyleBox["\"EXPIRED\"", Red];
iCachedBox[key_, $held$[body_]] := Set[$boxCache[key], Check[body, StyleBox["\"MESSAGES\"", Red]]];

(*************************************************************************************************)

If[!IntegerQ[System`Private`$UniqueSessionID], System`Private`$UniqueSessionID = 0];

UniqueSessionID[] := (System`Private`$UniqueSessionID++);

(*************************************************************************************************)

DefaultPreEvaluationHook[] := If[FrontEnd`Private`$KernelName =!= "LinkSnooper",
  $SessionCurrentEvaluationPrintCount = 0;
  $SessionCurrentEvaluationStartTime = SessionTime[];
  $SessionEvaluationsCount = If[IntegerQ[$SessionEvaluationsCount], $SessionEvaluationsCount, 0] + 1;
  SaveEvaluationCellState[];
];

DefaultPostEvaluationHook[] := If[FrontEnd`Private`$KernelName =!= "LinkSnooper",
  $SessionLastEvaluationTime = SessionTime[] - $SessionCurrentEvaluationStartTime;
  RestoreEvaluationCellState[];
];

(*************************************************************************************************)

If[!IntegerQ[$SessionMaxEvaluationPrintCount], $SessionMaxEvaluationPrintCount = 32];
If[!IntegerQ[$SessionEvaluationsCount], $SessionEvaluationsCount = 0];
If[!IntegerQ[$SublimeRunCount], $SublimeRunCount = 0];

$SessionCurrentEvaluationPrintCount = $SessionCurrentEvaluationStartTime = 0;

(* $KernelName only gets set after a little delay so we remove this noisy stuff from LinkSnooper kernels when we can *)
SaveEvaluationCellState[] := Block[
  {notebook, cell, cellInfo, cursorPos},

  $CurrentEvaluationCellState = None;
  $SublimeRunCount = 0;
  notebook = EvaluationNotebook[];

  cell = EvaluationCell[];
  If[Head[cell] =!= CellObject, Return[$Failed, Block]];

  cellInfo = Developer`CellInformation[cell];
  If[!MatchQ[cellInfo, {__Rule}], Return[$Failed, Block]];
  cursorPos = First[Lookup[cellInfo, "CursorPosition"], None];
  If[!IntegerQ[cursorPos], Return[$Failed, Block]];

  cell = First[SelectedCells[notebook], None];
  If[cell === None, Return[$Failed, Block]];

  $CurrentEvaluationCellState = {notebook, cell, cursorPos};
];

(*************************************************************************************************)

RestoreEvaluationCellState[] := If[ListQ[$CurrentEvaluationCellState], Module[
  {notebook, cell, cursorPos},
  {notebook, cell, cursorPos} = $CurrentEvaluationCellState;
  SelectionMove[cell, Before, CellContents];
  SelectionMove[notebook, Next, Character, cursorPos];
  SelectionMove[notebook, After, Character];
]];

(*************************************************************************************************)

InstallSessionHooks[] := If[
  !TrueQ[$ShiftReturnHookInstalled] && $FrontEnd =!= Null && FrontEnd`Private`$KernelName =!= "LinkSnooper",
  $ShiftReturnHookInstalled = True;
  Block[{$ContextPath = {"System`"}, $Context = "DummyContext`"},
  SetOptions[$FrontEndSession, FrontEndEventActions -> {
    {"MenuCommand", "HandleShiftReturn"} :> (
      $PreEvaluationHook[];
      FrontEndTokenExecute["EvaluateCells"];
      $PostEvaluationHook[]
    )
  }]];
];

UninstallSessionHooks[] := (
  SetOptions[$FrontEndSession, FrontEndEventActions -> {}];
);

(*************************************************************************************************)

If[!AssociationQ[$EvaluationsSinceDict], $EvaluationsSinceDict = Association[]];

SessionEvaluatedSinceQ[key_] := First @ {
  $SessionEvaluationsCount > Lookup[$EvaluationsSinceDict, key, 0],
  $EvaluationsSinceDict[key] = $SessionEvaluationsCount;
};

(*************************************************************************************************)

$PreEvaluationHook = DefaultPreEvaluationHook;
$PostEvaluationHook = DefaultPostEvaluationHook;

InstallSessionHooks[];

(*************************************************************************************************)

End[]

EndPackage[]