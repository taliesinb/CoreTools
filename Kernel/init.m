Quiet[
  Unprotect[System`PrintableASCIIQ, System`Into, System`AssociationMap, System`StringPadLeft, System`StringPadRight, System`DefaultValue];
  ClearAll[FrontEnd`Private`PrimeControlEqualPump, System`Into, System`AssociationMap, System`StringPadLeft, System`StringPadRight, System`PrintableASCIIQ, System`DefaultValue];
  Protect[FrontEnd`Private`PrimeControlEqualPump];
  If[OwnValues[Failure] =!= {}, Unprotect[Failure]; ClearAll[Failure]]; (* if GU hasn't loaded, prevent Failure from causing autoload *)
  If[OwnValues[ExternalObject] =!= {}, Unprotect[ExternalObject, ExternalFunction]; ClearAll[ExternalObject, ExternalFunction]]; (* same for ExternalEvaluate *)
];

(* explanation of the above:

DefaultValue: useful symbol for options, but autoloads templates.

PrintableASCIIQ, AssociationMap: we want to avoid loading GU, and these are simple functions, so
we re-implement them ourselves.

PrimeControlEqualPump: the FE uses this symbol to pre-load WolframAlpha (which goes on to load *many*
other packages!). this slows down every single kernel spin-up considerably, and it can happen in the middle
of another evaluation, causing chaos and misery. unfortunately the definitions are only attached after startup at
some unknown time, and then later loaded by a scheduled task, so we prevent the definitions getting attached.
*)

(*************************************************************************************************)

BeginPackage["CoreTools`"];

ApplyEchoSugar;

$CoreToolsPath = ExpandFileName @ FileNameDrop @ $InputFileName;
$CoreToolsRootPath = FileNameDrop @ $CoreToolsPath;
$PreludeInitFile = FileNameJoin[{$CoreToolsRootPath, "Prelude", "init.m"}];

(*************************************************************************************************)

Begin["Init`Private`"];

AssociationMap[f_][expr_] := AssociationMap[f, expr];
AssociationMap[fn_, expr_List] := Association @ Map[z |-> Rule[z, fn[z]], expr];
AssociationMap[fn_, assoc_Association ? AssociationQ] := Association @ Map[fn, Normal @ assoc];
AssociationMap[_, expr_] := RuleCondition[Message[AssociationMap::invrp, expr]; Fail];

(*************************************************************************************************)

System`$SymbolAliases = Data`UnorderedAssociation[];
Get @ $PreludeInitFile;

(*************************************************************************************************)

ApplyEchoSugar[e_] := e;
ApplyEchoSugar[e_] /; Internal`LiterallyOccurringQ[e, Factorial] :=
  ReplaceAll[e, $derivativeSugarRules];

$derivativeSugarRules = Dispatch @ {
  HoldPattern[Set[Factorial[lhs_], rhs_]] :> System`EchoH[lhs = rhs],
  (* HoldPattern[Set[Derivative[2][lhs_], rhs_]] :> System`EchoH[lhs, rhs], *)
  HoldPattern[Factorial[syms:{__Symbol}]]  :> System`EchoH[syms],
  HoldPattern[Factorial[sym_Symbol]] :> System`EchoH[sym],
  HoldPattern[Factorial[e_]] :> System`NiceEcho[e],
  HoldPattern[Factorial2[e_]] :> System`EchoH[e]
};

(*************************************************************************************************)

If[$MessagePrePrint === CoreTools`MsgPrePrint, $MessagePrePrint =.];

(* Block[{$SymbolAliases = Data`UnorderedAssociation[]}, *)
  Prelude`Packages`LoadPackage[
    "CoreTools`",
    File @ FileNameJoin[{$CoreToolsPath, "LoadList.txt"}],
    "CodePreprocessor" -> ApplyEchoSugar,
    "Verbose" -> False
  ];
(*   $NewSymbolAliases = $SymbolAliases;
];

$SymbolAliases = Join[$SymbolAliases, $NewSymbolAliases];
Clear[$NewSymbolAliases];
 *)


(*************************************************************************************************)

End[];
EndPackage[];

(*************************************************************************************************)

If[!Prelude`Packages`PackageLoadCompletedQ["CoreTools`"],
  General::coreToolsLoadFailed = "CoreTools didn't load successfully.";
  Message[General::coreToolsLoadFailed];
  $ContextPath = DeleteCases[$ContextPath, "CoreTools`"]; (* allows us to retry later *)
,
  If[DownValues[MsgPrePrint] =!= {}, $MessagePrePrint = MsgPrePrint];
];