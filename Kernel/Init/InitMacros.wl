SystemExports[
  "ControlFlow",     FunctionReturn,
  "SpecialVariable", $DollarSymbols
];

PackageExports[
  "Function",
    Ensure, MacroHead,
  "SpecialFunction",
    ExpandMacros, RefreshMacroRules,
    MacroHold, MacroSet, MacroSetDelayed, MacroRuleDelayed
  "MessageFunction",
    MacroError,
  "MetaFunction",
    DefinePartialMacro,
    DeclareMacroDefSym,
    SimpleMacroDefs, PatternMacroDefs, ComplexMacroDefs, PartialMacroDefs,
  "Predicate",
    ContainsMacrosQ, FreeOfMacrosQ, FreeOfPureMacrosQ,
  "SpecialVariable",
    $LastMacroFailure
];

PrivateExports[
  "Function",
    HoldMMap, HoldMRaise, HoldMLower,
  "SpecialFunction",
    MacroEval, MacroHook,
  "TagVariable",
    $ReturnTarget,
    $MacroHead,
    $MacroSrcLoc,
  "SpecialVariable",
    $CompiledMacroRules,
    $AllMacroSyms,
    $PureMacroSyms,
    $PartialMacroRules,
    $PartialMacroSyms,
    $MacroSugarRules,
    $ReturnTag,
    $MacroRules,
    $PartialMacroRules
];

(**************************************************************************************************)

SetHoldR[Ensure];

Ensure[expr_, testFn_, else_] := If[TrueQ @ testFn @ expr, expr, else];
Ensure[testFn_, else_][expr_] := If[TrueQ @ testFn @ expr, expr, else];

(*************************************************************************************************)

SetHoldA @ MacroError;

MacroError::error = "A macro error eccurred.";
MacroError[msg_Str, args___] := With[
  {head = First @ MacroHead[Hold @ General]},
  MacroError[MessageName[head, msg], args]
];

MacroError[args___] /; TrueQ[$PackageCurrentlyLoading] := Then[
  ErrorPrint["Macro error at ", RawBoxes @ SourceLocationBox[$CurrentPackageFile, $CurrentPackageExprCount]];
  ErrorMessage[args];
  AbortPackageLoading[];
];

MacroError[args___] := Then[ErrorMessage[args], Abort[]];

(*************************************************************************************************)

SetHoldA[MacroHold];
SetHoldC[MacroSet, MacroSetDelayed, MacroRuleDelayed];

MacroHold::usage = "MacroHold[$$] will be stripped during macro expansion.";

$DollarSymbols = {$1, $2, $3, $4, $5, $6, $7, $8, $9};

$MacroSugarRules = Data`UnorderedAssociation[
  HoldP[Message] -> {
  HoldP[Message[msgName_Str, msgArgs___]] :>
    Message[MessageName[$MacroHead, msgName], msgArgs]
  }
];

RefreshMacroRules[] := (generateMacroRules[];)

(* this ensures we have only one set of rules for each head *)
$MacroRules = Data`UnorderedAssociation[
  HoldP[$$]             -> {HoldP[$$] -> $MacroHead},
  HoldP[SetM]           -> {HoldP[SetM] -> Set},
  HoldP[SetDM]          -> {HoldP[SetDM] -> SetD},
  HoldP[RuleDM]         -> {HoldP[RuleDM] -> RuleD},
  HoldP[FunctionReturn] -> {} (* ensure its a symbol *)
];

$PartialMacroRules = Data`UnorderedAssociation[];

invalidateMacroRules[] := (
  Clear[$AllMacroSyms, $PureMacroSyms, $CompiledMacroRules];
  $AllMacroSyms    := (generateMacroRules[]; $AllMacroSyms);
  $PureMacroSyms   := (generateMacroRules[]; $PureMacroSyms);
  $CompiledMacroRules := (generateMacroRules[]; $CompiledMacroRules);
);

invalidateMacroRules[];

$basicMacroSymP = {HoldP[HoldM], HoldP[SetM], HoldP[SetDM], HoldP[RuleDM]};

generateMacroRules[] := (
  Clear[$AllMacroSyms, $PureMacroSyms, $CompiledMacroRules];
  $AllMacroSyms = Sort @ Append[Join[Keys @ $MacroRules, Keys[$SymbolAliases, HoldP]], $basicMacroSymP];
  $PartialMacroSyms = Keys @ $PartialMacroRules;
  $PureMacroSyms = Complement[$AllMacroSyms, $PartialMacroSyms];
  defineMacroPredicates[
    Extract[$AllMacroSyms, {All, 1}, Hold],
    Extract[$PureMacroSyms, {All, 1}, Hold]
  ];
  $CompiledMacroRules = Dispatch @ Catenate @ Join[$MacroRules, $MacroSugarRules];
);

(*************************************************************************************************)

ContainsMacrosQ::usage = "ContainsMacrosQ[...] returns True if macro symbols are present.";

SetHoldA[ContainsMacrosQ, FreeOfMacrosQ, FreeOfPureMacrosQ]

defineMacroPredicates[Hold[symbols_List], Hold[pureSymbols_List]] := (
  FreeOfMacrosQ[e_]     := VFreeQ[NoEval @ e, NoEval @ symbols];
  FreeOfPureMacrosQ[e_] := VFreeQ[NoEval @ e, NoEval @ pureSymbols];
  ContainsMacrosQ[e_]   := VContainsQ[NoEval @ e, NoEval @ symbols];
);

(*************************************************************************************************)

SetHoldC[HoldMMap]

HoldMMap[fn_, list_List] := HoldMRaise @ Map[fn, NoEval @ list];

HoldMRaise[list_List]  := Thread[list, HoldM];
HoldMLower[hold_HoldM] := Thread[list, HoldM];

(*************************************************************************************************)

General::internalMacroError = "Internal macro error: ``.";
General::invalidMacroDefinition = "Not a valid macro definition: ``.";

setMacroHelper[sym_Sym] := SetD[e_sym, MacroError[sym::internalMacroError,     HoldForm @ e]];
setMacroDefSym[sym_Sym] := SetD[e_sym, MacroError[sym::invalidMacroDefinition, HoldForm @ e]];

(*************************************************************************************************)

SetStrict @ DeclareMacroDefSym;

DeclareMacroDefSym[defSym_Sym, fn_Sym] := Then[
  SetHoldC @ defSym,
  setMacroHelper @ fn,
  setMacroDefSym @ defSym,
  ExpandMacros[hc:HoldC[_defSym]]             := hc,
  ExpandMacros[hc:HoldC[Then[_defSym, Null]]] := hc,
  defSym[def_SetD]             := procMacroDefSingle[fn, HoldC @ def],
  defSym[defs__SetD]           := procMacroDefGroup0[fn, HoldC @ {defs}],
  defSym[head_Sym | Hold[head_Sym], defs__SetD] := procMacroDefManual[fn, HoldC @ {defs}, Hold @ head],
  Null
];

setMacroHelper @ procMacroDefSingle;
setMacroHelper @ procMacroDefGroup0;
setMacroHelper @ procMacroDefManual;

procMacroDefSingle[fn_, HoldC[_[lhs_, rhs_]]] := With[
  {head = PatHead[lhs]},
  Clear @@ head;
  fn[head, HoldC @ List @ RuleD[lhs, rhs]];
  invalidateMacroRules[]
];

procMacroDefGroup0[fn_, defs:HoldC[_List]] := Module[
  {holds, grouped},
  holds = HoldCLower @ defs;
  grouped = Normal @ GroupBy[holds, Apply @ DefHead];
  Scan[procMacroDefGroup1[fn], grouped];
  invalidateMacroRules[]
];

procMacroDefManual[fn_, defs:HoldC[_List], head_Hold] := Then[
  Clear @@ head,
  fn[head, SetDsToRuleDs @ defs],
  invalidateMacroRules[]
];

procMacroDefGroup1[fn_][head_Hold -> defs:List[__HoldC]] := Then[
  Clear @@ head,
  fn[head, SetDsToRuleDs @ HoldCRaise @ defs]
];

procMacroDefGroup1[fn_][e___] :=
  ErrorMessage[General::internalMacroError, HoldC[fn, e]];

(*************************************************************************************************)

SimpleMacroDefs::usage =
"SimpleMacroDefs[lhs := rhs, ...] defines simple macros that are expanded at load time.
* the special symbol $MacroHead will be substituted with symbol whose top-level definition contains the macro.
* if you wish to use MacroHold, you should be using DefineComplexMacro instead."

PatternMacroDefs::usage =
"PatternMacroDefs[lhs := rhs, ...] is like SimpleMacroDefs but is designed for symbols that represent patterns.";

DeclareMacroDefSym[SimpleMacroDefs, simpleMacroDef];
DeclareMacroDefSym[PatternMacroDefs, simpleMacroDef];

simpleMacroDef[Hold[sym_], rules:HoldC[{__RuleD}]] := With[
  {hrules = ToHoldPRuleDs @ rules},
  If[FreeQ[hrules, MacroHold],
    DownValues[sym]                = hrules;
    $MacroRules[HoldP[sym]]        = hrules;
    $PartialMacroRules[HoldP[sym]] = hrules;
    invalidateMacroRules[],
    ErrorMsg[SimpleMacroDefs::notSimpleMacro, HoldForm @ sym];
  ]
];

SimpleMacroDefs::notSimpleMacro = "Macro rules for `` contains MacroHold, use ComplexMacroDefs.";

(*************************************************************************************************)

DefinePartialMacro::usage =
"DefinePartialMacro[lhs := rhs, ...] defines macros for a symbol that is also an ordinary
function, but has certain usages that appear in macros.
* Rule LHS must be wrapped in HoldPattern."

SetStrict @ DefinePartialMacro;

DefinePartialMacro[sym_Symbol, rule_RuleD] := Then[
  $MacroRules[HoldPattern[sym]] = {rule};
  $PartialMacroRules[HoldPattern[sym]] = {rule};
  invalidateMacroRules[];
];

(*************************************************************************************************)

ComplexMacroDefs::usage =
"ComplexMacroDefs[lhs := rhs, ...] defines a complex macro associated with symbol that is expanded at load time.
ComplexMacroDefs[head$, ...] gives a parent symbol if the head is not at top level.
* unlike SimpleMacroDefs, ComplexMacroDefs will set up top-level UpValues that automatically apply the macro,
and will additionally allow it to use MacroHold.
* $MacroHead is the name of the symbol using the macro in its definition."

DeclareMacroDefSym[ComplexMacroDefs, complexMacroDef];

complexMacroDef[Hold[sym_Sym], HoldC[rules_List]] := Then[
  UpValues[sym]           = toUpRules[sym],
  DownValues[sym]         = Map[toDownRule,  NoEval @ rules],
  $MacroRules[HoldP[sym]] = Map[toInnerRule, NoEval @ rules]
];

setMacroHelper @ toDownRule;
setMacroHelper @ toUpRules;
setMacroHelper @ toInnerRule;

SetHoldA[toDownRule, toUpRules, toInnerRule];

toUpRules[sym_] := {
  HoldP[$LHS:Set [_, _sym]] :> MacroHook[$LHS],
  HoldP[$LHS:SetD[_, _sym]] :> MacroHook[$LHS]
};

toInnerRule[_[lhs_, rhs_]]    := RuleD[HoldP @ lhs, RuleEval @ rhs];
toDownRule[head_[lhs_, rhs_]] := head[HoldP[lhs], MacroEval @ Evaluate @ rhs];

(*************************************************************************************************)

SetHoldA @ MacroEval;

MacroEval[expr_] :=
  First @ ReplaceRepeated[ExpandMacros @ HoldC @ expr, MacroHold[h_] :> h];

(*************************************************************************************************)

SetStrict @ SetHoldA @ MacroHook;

MacroHook[s_] := Block[
  {$activeMHead = getParentHead @ s},
  With[{head = First[$activeMHead, MacroHook]},
  CatchMessages[head, MacroEval @ s]
]];

(*************************************************************************************************)

DeclaredHere[$MacroHead];

(*************************************************************************************************)

SetHoldA @ MacroHead;

MacroHead[] := MacroHead[MacroError[MacroHead::noHead]];
MacroHead[else_] := If[HasIValueQ[$activeMHead], $activeMHead, else];

MacroHead::noMacroHead = "No parent head found for macro invocation.";

(*************************************************************************************************)

ExpandMacros::usage = "ExpandsMacros[HoldC[...]] expands macros that are present.";

ExpandMacros[hc_ ? FreeOfMacrosQ] := hc;
ExpandMacros[hc_] := Check[
  checkDone @ subRets @ subHolds @ subMps @ attachSLocs @ ReplaceRepeated[ReplaceAll[hc, $SymbolAliases], $CompiledMacroRules],
  $Failed
];

(*************************************************************************************************)

General::expansionFailed = "Macro(s) `` failed to expand in ``. Code available as $LastMacroFailure.";
checkDone[hc_ ? FreeOfPureMacrosQ] := hc;
checkDone[hc_] := MacroError["expansionFailed",
  $LastMacroFailure = hc;
  HoldForm @@@ Select[$PureMacroSyms, !FreeQ[hc, #]&],
  hc
];

(*************************************************************************************************)

attachSLocs[hc_] /; VFreeQ[hc, $ExceptingSyms] && VFreeQ[hc, NoEval @ {Unimplemented, InternalError}] := hc;
attachSLocs[hc_] := AttachSrcLocs @ hc;

attachSLocs[hc_] := hc;

(*************************************************************************************************)

subHolds[hc_] := ReplaceRepeated[hc, MacroHold[e_] :> e];

(*************************************************************************************************)

subRets[hc_] /; VFreeQ[hc, FunctionReturn] := hc;

subRets[hc_] := If[HasIValueQ[$ReturnTarget],
  hc /. FunctionReturn[a_] :> Return[a, Block],
  subRets2[hc]
];

subRets2[hc:HoldC[SetD[_, _Block] | _Block]] /; Count[hc, Block] === 1 :=
  hc /. FunctionReturn[a_] :> Return[a, Block];

subRets2[hc:HoldC[SetD[_, _Module] | _Module]] /; Count[hc, Module] === 1 :=
  hc /. FunctionReturn[a_] :> Return[a, Block];

subRets2[HoldC[(sd:SetD)[lhs_, FunctionReturn[a_]]]] :=
  HoldC[sd[lhs, a]];

$uniqueID = 0;
subRets2[hc:HoldC[(sd:SetD)[lhs_, rhs_]]] := With[{id = $uniqueID++},
  HoldC[sd[lhs, Catch[rhs, $ReturnTag[id]]]] /.
    FunctionReturn[a_] :> Throw[a, $ReturnTag[id]]];

subRets2[HoldC[hc_;]] := subRets2 @ HoldC @ hc;
subRets2[hc:HoldC[{__SetD}]] := Thread[Map[subRets, Thread @ hc], HoldC];

(* subRets2[HoldC[e_]] := With[{id = $uniqueID++},
  HoldC[Catch[e, $ReturnTag[id]]] /.
    FunctionReturn[a_] :> Throw[a, $ReturnTag[id]]];
 *)
subRets2[e_] := ErrorPrint["Error finding returns in: ", HoldForm @ e];

(*************************************************************************************************)

subMps[hc_] /; VFreeQ[hc, NoEval @ {$MacroHead, $MacroSrcLoc}] := hc;
subMps[hc_] := subSL @ subMps2 @ hc;

subSL[hc_] := ReplaceAll[hc, $MacroSrcLoc :> RuleEval @ SourceLocation[]];

subMps2[hc:HoldC[lhs_SetD | lhs_Set | Then[lhs_SetD | lhs_Set, Null]]] :=
  ReplaceAll[hc, $MacroHead -> getParentHead[lhs]];

subMps2[hc_] := Block[
  {mpsPositions = ReverseSortBy[Position[hc, $MacroHead], Length],
   $sdPositions = Position[hc, _SetD], $hc = hc},
  ReplacePart[hc, Map[pos |-> Rule[pos, findParentHead[hc, pos]], mpsPositions]]
];

findParentHead[hc_, pos_] := Block[{parentSD},
  parentSd = SelectFirst[$sdPositions, prefixListQ[#, pos]&, None];
  If[parentSd === None,
    If[HasIValueQ[$activeMHead],
      Return[$activeMHead, Block],
      MacroError[General::noMacroParent, HoldForm @ hc];
    ]
  ];
  Extract[$hc, parentSd, getParentHead]
];

prefixListQ[_, _] := False;
prefixListQ[a_List, b_List] := Length[a] <= Length[b] && a === Take[b, Length @ a];

General::noMacroParent = "Could not resolve macro parent in ``.";

(*************************************************************************************************)

SetHoldC[getParentHead]

getParentHead[(SetD|Set)[lhs_, _]] :=
  MacroHold @@ Replace[
    PatHead @ lhs,
    $Failed :> MacroError["macroParentLHS", HoldForm @ lhs]
  ];

getParentHead[lhs_] := MacroError["macroParentLHS", HoldForm @ lhs];

(*************************************************************************************************)

General::macroParentLHS = "Could not find a head symbol for the LHS of SetD, being ``.";

