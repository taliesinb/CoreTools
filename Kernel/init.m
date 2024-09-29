If[MemberQ[$ContextPath, "MathTools`"],
  Print["MathTools` loaded, aborting."];
  Abort[]
];

(*************************************************************************************************)

BeginPackage["CoreTools`"];

ApplyEchoSugar;

$CoreToolsPath = ExpandFileName @ FileNameDrop @ $InputFileName;
$CoreToolsRootPath = FileNameDrop @ $CoreToolsPath;
$PreludeInitFile = FileNameJoin[{$CoreToolsRootPath, "Prelude", "init.m"}];

(*************************************************************************************************)

Begin["Init`Private`"];

(*************************************************************************************************)

Prelude`$SymbolAliases = Data`UnorderedAssociation[];
Get @ $PreludeInitFile;

(*************************************************************************************************)

ApplyEchoSugar[e_] := e;
ApplyEchoSugar[e_] /; Internal`LiterallyOccurringQ[e, Factorial] :=
  ReplaceAll[e, $derivativeSugarRules];

$derivativeSugarRules = Dispatch @ {
  HoldPattern[Set[Factorial[lhs_], rhs_]]        :> System`EchoH[lhs = rhs],
  (* HoldPattern[Set[Derivative[2][lhs_], rhs_]] :> System`EchoH[lhs, rhs], *)
  HoldPattern[Factorial[syms:{__Symbol}]]        :> System`EchoH[syms],
  HoldPattern[Factorial[sym_Symbol]]             :> System`EchoH[sym],
  HoldPattern[Factorial[e_]]                     :> System`NiceEcho[e],
  HoldPattern[Factorial2[e_]]                    :> System`EchoH[e]
};

(*************************************************************************************************)

If[$MessagePrePrint === MessageArgumentForm, Unset @ $MessagePrePrint];
If[$PrePrint === OutputExpressionForm, Unset @ $PrePrint];

PreludeLoadPackage[
  "CoreTools`",
  File @ FileNameJoin[{$CoreToolsPath, "LoadList.txt"}],
  "CodePreprocessor" -> ApplyEchoSugar,
  "ContextPath" -> {"Prelude`", "Session`"}
];

(*************************************************************************************************)

End[];
EndPackage[];

(*************************************************************************************************)

If[!Prelude`PackageLoadCompletedQ["CoreTools`"],
  System`Private`$CoreToolsLoaded = False;
  General::coreToolsLoadFailed = "CoreTools didn't load successfully.";
  Message[General::coreToolsLoadFailed];
  $ContextPath = DeleteCases[$ContextPath, "CoreTools`"]; (* allows us to retry later *)
,
  System`Private`$CoreToolsLoaded = True;
  If[TrueQ @ System`Private`$UseCoreToolsPrePrintFns, Symbol["CoreTools`SetPrePrintFns"][]];
];