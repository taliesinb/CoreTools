If[MemberQ[$ContextPath, "MathTools`"],
  Print["MathTools` loaded, aborting."];
  Abort[]
];

(*************************************************************************************************)

BeginPackage["CoreTools`"];

ApplyEchoSugar;

Session`$CoreToolsPath = ExpandFileName @ FileNameDrop @ $InputFileName;
Session`$CoreToolsRootPath = FileNameDrop @ Session`$CoreToolsPath;

(*************************************************************************************************)

Begin["Init`Private`"];

(*************************************************************************************************)

$SymbolAliases = Data`UnorderedAssociation[];
$NameAliases = Data`UnorderedAssociation[];

Get @ FileNameJoin[{Session`$CoreToolsRootPath, "Prelude", "init.m"}];
If[!TrueQ[Session`$PreludeLoaded],
  Print["Prelude didn't load, can't load CoreTools`."];
  Abort[]
];

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
  File @ FileNameJoin[{Session`$CoreToolsPath, "_CoreTools.txt"}],
  "CodePreprocessor" -> ApplyEchoSugar,
  "ContextPath" -> {"Prelude`", "Session`"}
];

(*************************************************************************************************)

End[];
EndPackage[];

(*************************************************************************************************)

If[!Prelude`PackageLoadCompletedQ["CoreTools`"],
  Session`$CoreToolsLoaded = False;
  General::coreToolsLoadFailed = "CoreTools didn't load successfully.";
  Message[General::coreToolsLoadFailed];
  $ContextPath = DeleteCases[$ContextPath, "CoreTools`"]; (* allows us to retry later *)
,
  Session`$CoreToolsLoaded = True;
  If[TrueQ @ Session`$UseCoreToolsPrePrintFns, Symbol["CoreTools`SetPrePrintFns"][]];
];