SystemExports[
  "SymbolicHead",
    FormalTimes, FormalPlus, FormalPower,
    FormalFn, Choice, HoldChoice,
  "Symbol",
    FormalOne, FormalZero,
  "Function",
    FormalDistribute,
    EnumerateChoices,
  "Predicate",
    FormalPlusQ
];

PackageExports[
  "SymbolicHead", FTimes, FPlus, FPower, DistHold,
  "Symbol",       FOne, FZero
  "BoxFunction",  FormalSymbolBox
];

(**************************************************************************************************)

DefineAliasRules[
  FTimes -> FormalTimes,
  FPlus  -> FormalPlus,
  FPower -> FormalPower,
  FOne   -> FormalOne,
  FZero  -> FormalZero
];

SetAttributes[FormalPlus,  {Flat, Orderless}];
SetAttributes[FormalTimes, {Flat, Orderless}];

(**************************************************************************************************)

SetPred1 @ FormalPlusQ

FormalPlusQ[_FormalPlus] := True;

(**************************************************************************************************)

FormalFn[fn_][args___] := FormalDistribute @ NoEval @ fn[args];

(**************************************************************************************************)

CoreBox[FormalPlus[args__]]  := RiffBox[$formalPlus][MapMakeBox[{args}]];
CoreBox[FormalTimes[args__]] := RiffBox[$formalTimes][MapMakeBox[{args}]];
CoreBox[FormalPower[a_, b_]] := SuperscriptBox[MakeBoxes @ a, MakeBoxes @ b];

FormalSymbolBox[box_] := MarginBox[.5] @ StyleBox["\[RawPlus]",
  FontFamily -> "Roboto",
  FontSize -> (2 + Inherited), FontColor -> $DarkBlue
];

$formalPlus  := $formalPlus  = FormalSymbolBox["\[RawPlus]"];
$formalTimes := $formalTimes = FormalSymbolBox["\[Times]"];

