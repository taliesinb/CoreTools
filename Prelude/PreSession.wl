BeginPackage["Prelude`", {"Session`"}];

SessionExports[

  "Variable",
    $LastEvaluationTime,
    $EvaluationsCount,
    $CurrentEvaluationStartTime,
    $CurrentEvaluationCellState,
    $EvaluationsSinceDict,

  "SpecialVariable",
    $PreEvaluationHook,
    $PostEvaluationHook,
    $ShiftReturnHookInstalled,
    $SessionIDs,

  "SpecialFunction",
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

AppendTo[$ContextPath, "Session`"];

(* if $PreEvaluationHook etc are in the normal context path, the FE will save them wrong between reloads *)

(*************************************************************************************************)

(*
do print veto!
Internal`$PrintFormatter = Function[Null,
  If[TrueQ[Session`$EvaluationsCount,
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
    If[Length[$boxCache] > 32, $boxCache = Drop[$boxCache, 8]];
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

If[!IntegerQ[$SessionIDs], $SessionIDs = 0];

UniqueSessionID[] := ($SessionIDs++);

(*************************************************************************************************)

DefaultPreEvaluationHook[] := If[FrontEnd`Private`$KernelName =!= "LinkSnooper",
  $CurrentEvaluationStartTime = SessionTime[];
  $EvaluationsCount = If[IntegerQ[$EvaluationsCount], $EvaluationsCount, 0] + 1;
  SaveEvaluationCellState[];
];

DefaultPostEvaluationHook[] := If[FrontEnd`Private`$KernelName =!= "LinkSnooper",
  $LastEvaluationTime = SessionTime[] - $CurrentEvaluationStartTime;
  RestoreEvaluationCellState[];
];

(*************************************************************************************************)

If[!IntegerQ[$EvaluationsCount], $EvaluationsCount = 0];
If[!IntegerQ[$SublimeRunCount], $SublimeRunCount = 0];

$CurrentEvaluationStartTime = 0;

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
  $EvaluationsCount > Lookup[$EvaluationsSinceDict, key, 0],
  $EvaluationsSinceDict[key] = $EvaluationsCount;
};

(*************************************************************************************************)

$PreEvaluationHook  = DefaultPreEvaluationHook;
$PostEvaluationHook = DefaultPostEvaluationHook;

InstallSessionHooks[];

(*************************************************************************************************)

End[]

EndPackage[]