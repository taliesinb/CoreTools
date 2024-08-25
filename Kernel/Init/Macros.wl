SystemExports[
  "MetaFunction",
    DefineVariableMacro, DefinePatternMacro, DefineSimpleMacro, DefineComplexMacro, DefinePartialMacro,
  "ControlFlowFunction",
    FunctionReturn,
  "SpecialFunction",
    ExpandMacros, MacroHold, RefreshMacroRules,
  "SpecialVariable",
    $MacroParentSymbol, $MacroSourceLocation, $DollarSymbols
];

PackageExports[
  "MetaFunction",
    DeclareMacroDefine,
  "Predicate",
    ContainsMacrosQ, FreeOfMacrosQ, FreeOfPureMacrosQ,
  "SpecialVariable",
    $CompiledMacroRules,
    $AllMacroSymbols, $PureMacroSymbols, $PartialMacroSymbols,
    $MacroRules, $PartialMacroRules, $LastMacroFailure, $MacroSugarRules,
    $FunctionReturnTag
];

PrivateExports[
  "SpecialFunction",
    TopLevelEvaluateMacro, TopLevelSetDelayedMacro,
  "SpecialVariable",
    $FunctionReturnTarget
];

(*************************************************************************************************)

$DollarSymbols = {$1, $2, $3, $4, $5, $6, $7, $8, $9};

$MacroSugarRules = Data`UnorderedAssociation[
  HoldPattern[Message] -> {
  HoldPattern[Message[msgName_Str, msgArgs___]] :>
    Message[MessageName[$MacroParentSymbol, msgName], msgArgs]
  }
];

RefreshMacroRules[] := (generateMacroRules[];)

(* this ensures we have only one set of rules for each head *)
$MacroRules = Data`UnorderedAssociation[
  HoldPattern[$$]             -> {HoldPattern[$$] -> $MacroParentSymbol},
  HoldPattern[FunctionReturn] -> {} (* ensure its a symbol *)
];

$PartialMacroRules = Data`UnorderedAssociation[];

invalidateMacroRules[] := (
  Clear[$AllMacroSymbols, $PureMacroSymbols, $CompiledMacroRules];
  $AllMacroSymbols    := (generateMacroRules[]; $AllMacroSymbols);
  $PureMacroSymbols   := (generateMacroRules[]; $PureMacroSymbols);
  $CompiledMacroRules := (generateMacroRules[]; $CompiledMacroRules);
);

invalidateMacroRules[];

generateMacroRules[] := (
  Clear[$AllMacroSymbols, $PureMacroSymbols, $CompiledMacroRules];
  $AllMacroSymbols = Sort @ Append[Join[Keys @ $MacroRules, Keys[$SymbolAliases, HoldPattern]], HoldPattern @ MacroHold];
  $PartialMacroSymbols = Keys @ $PartialMacroRules;
  $PureMacroSymbols = Complement[$AllMacroSymbols, $PartialMacroSymbols];
  defineMacroPredicates[
    Extract[$AllMacroSymbols, {All, 1}, Hold],
    Extract[$PureMacroSymbols, {All, 1}, Hold]
  ];
  $CompiledMacroRules = Dispatch @ Catenate @ Join[$MacroRules, $MacroSugarRules];
);

General::invalidMacroDefinition = "Not a valid macro definition: ``.";

(*************************************************************************************************)

defineMacroPredicates[Hold[symbols_List], Hold[pureSymbols_List]] := (
  FreeOfMacrosQ[expr_]     :=    Internal`LiterallyAbsentQ[Unevaluated @ expr, Unevaluated @ symbols];
  FreeOfPureMacrosQ[expr_] :=    Internal`LiterallyAbsentQ[Unevaluated @ expr, Unevaluated @ pureSymbols];
  ContainsMacrosQ[expr_]   := Internal`LiterallyOccurringQ[Unevaluated @ expr, Unevaluated @ symbols];
);

(*************************************************************************************************)

MacroHold::usage = "MacroHold[$$] will be stripped during macro expansion.";

DeclareHoldAll[MacroHold];

(*************************************************************************************************)

setupInvMacroMsg[macroDefSym_] := (
  e_macroDefSym := (Message[macroDefSym::invalidMacroDefinition, HoldForm @ e]; $Failed)
);

(*************************************************************************************************)

DefineVariableMacro::usage =
"DefineVariableMacro[symbol, value] defines a macro that expands symbol to a value at load time."

DeclareHoldAll[DefineVariableMacro]
setupInvMacroMsg[DefineVariableMacro]

DefineVariableMacro[sym_Symbol, value_] := (
  sym = value;
  $MacroRules[HoldPattern[sym]] = {HoldPattern[sym] :> value};
  invalidateMacroRules[];
);

(*************************************************************************************************)

$macroDefineHeadP = Alt[];

DeclareMacroDefine[macroDefSym_Symbol] := (
  DeclareHoldAll[macroDefSym];
  (* we do this so that macros don't expand before they are defined, e.g. if they are already
  defined and we reload a file *)
  ExpandMacros[h:HoldComplete[_macroDefSym]] := h;
  ExpandMacros[h:HoldComplete[CompoundExpression[_macroDefSym, Null]]] := h;
  macroDefSym[sym_Symbol, rule_RuleDelayed] := macroDefSym[sym, {rule}];
  setupInvMacroMsg[macroDefSym];
);

(*************************************************************************************************)

DeclareMacroDefine[DefineSimpleMacro]

DefineSimpleMacro::usage =
"DefineSimpleMacro[symbol, lhs :> rhs] defines a simple macro associated with symbol that is expanded at load time.
DefineSimpleMacro[symbol, rules] defines several rules.
* the special symbol $MacroParentSymbol will be substituted with symbol whose top-level definition contains the macro.
* if you wish to use MacroHold, you should be using DefineComplexMacro instead."

(* since we may have previously set up downvalues for sym, we can't allow the rule LHS to evaluate *)
DefineSimpleMacro[sym_Symbol, rules:{__RuleDelayed}] := With[
  {heldRules = (Clear[sym]; toSimpleRules @ rules)},
  If[!FreeQ[heldRules, MacroHold],
    Message[DefineSimpleMacro::notSimpleMacro, sym];
    $Failed
  ,
    DownValues[sym]               = heldRules;
    $MacroRules[HoldPattern[sym]] = heldRules;
    $PartialMacroRules[HoldPattern[sym]] = heldRules;
    invalidateMacroRules[];
  ]
];
DefineSimpleMacro::notSimpleMacro = "Macro rules for `` contains MacroHold, use DefineComplexMacro.";

DeclareHoldAll[toSimpleRules]
toSimpleRules[rules_] := MapAt[HoldPattern, Unevaluated @ rules, {All, 1}];

(*************************************************************************************************)

DefinePatternMacro::usage =
"DefinePatternMacro[symbol, lhs :> rhs] is like DefineSimpleMacro but is designed for symbols
that represent patterns."

DeclareMacroDefine[DefinePatternMacro]

DefinePatternMacro[sym_Symbol, rules:{__RuleDelayed}] :=
  DefineSimpleMacro[sym, rules];

(*************************************************************************************************)

DefinePartialMacro::usage =
"DefinePartialMacro[symbol, lhs :> rhs] defines macro rules for a symbol that is also an ordinary
function, but has certain usages that appear in macros."

DeclareMacroDefine[DefinePartialMacro]

DefinePartialMacro[sym_Symbol, rules:{__RuleDelayed}] := With[
  {heldRules = toSimpleRules[rules]},
  $MacroRules[HoldPattern[sym]] = heldRules;
  $PartialMacroRules[HoldPattern[sym]] = heldRules;
  invalidateMacroRules[];
];

(*************************************************************************************************)

DefineComplexMacro::usage =
"DefineComplexMacro[symbol, lhs :> rhs] defines a complex macro associated with symbol that is expanded at load time.
DefineComplexMacro[symbol, rules] defines several rules.
* unlike DefineSimpleMacro, DefineComplexMacro will set up top-level UpValues that automatically apply the macro,
and will additionally allow it to use MacroHold.
* $MacroParentSymbol is the name of the symbol using the macro in its definition."

DeclareMacroDefine[DefineComplexMacro]

DefineComplexMacro[sym_Symbol, rule_RuleDelayed] := DefineComplexMacro[sym, List @ rule];

DefineComplexMacro[sym_Symbol, rules:{__RuleDelayed}] := (
  Clear[sym];
  UpValues[sym]                 = toUpRules[sym];
  DownValues[sym]               = HoldMap[toDownRule, rules];
  $MacroRules[HoldPattern[sym]] = HoldMap[toInnerRule, rules];
  invalidateMacroRules[];
);

(*************************************************************************************************)

DeclareHoldAll[toDownRule, toUpRules, toInnerRule];

toInnerRule[_[lhs_, rhs_]] := RuleDelayed[HoldPattern @ lhs, RuleCondition @ rhs];

toDownRule[head_[lhs_, rhs_]] := head[HoldPattern[lhs], TopLevelEvaluateMacro @ Evaluate @ rhs];

toUpRules[sym_] := {
  HoldPattern[$LHS:       Set[_, _sym]] :> TopLevelSetDelayedMacro[$LHS],
  HoldPattern[$LHS:SetDelayed[_, _sym]] :> TopLevelSetDelayedMacro[$LHS]
};

(*************************************************************************************************)

DeclareHoldAll[TopLevelEvaluateMacro, TopLevelSetDelayedMacro]

TopLevelEvaluateMacro[expr_] :=
  First @ ReplaceRepeated[ExpandMacros @ HoldComplete @ expr, MacroHold[h_] :> h];

$currentMacroParentSymbol = $MacroParentSymbol;

TopLevelSetDelayedMacro[___] := Null;
TopLevelSetDelayedMacro[s_] := Block[
  {$currentMacroParentSymbol = getParentHead @ s, head},
  head = First @ $currentMacroParentSymbol;
  CatchMessages[head, TopLevelEvaluateMacro @ s]
];

(*************************************************************************************************)

ContainsMacrosQ::usage = "ContainsMacrosQ[...] returns True if macro symbols are present.";

DeclareHoldAll[ContainsMacrosQ, FreeOfMacrosQ, FreeOfPureMacrosQ]

(*************************************************************************************************)

ExpandMacros::usage = "ExpandsMacros[HoldComplete[...]] expands macros that are present.";

ExpandMacros::messagesOccurred = "Messages occurred during macro expansion.";
ExpandMacros[hc_ ? FreeOfMacrosQ] := hc;
ExpandMacros[hc_] := Check[
  checkDone @ subRets @ subHolds @ subMps @ attachSLocs @ ReplaceRepeated[ReplaceAll[hc, $SymbolAliases], $CompiledMacroRules],
  $Failed
];

(*************************************************************************************************)

General::expansionFailed = "Macro(s) `` failed to expand in ``. Code available as $LastMacroFailure.";
checkDone[hc_ ? FreeOfPureMacrosQ] := hc;
checkDone[hc_] := ThrowMsg["expansionFailed", Beep[];
  $LastMacroFailure = hc;
  HoldForm @@@ Select[$PureMacroSymbols, !FreeQ[hc, #]&],
  hc
];

(*************************************************************************************************)

attachSLocs[hc_] /; VFreeQ[hc, $ExceptingSymbols] && VFreeQ[hc, NoEval @ {Unimplemented, InternalError}] := hc;
attachSLocs[hc_] := InsertWithSourceLocations @ hc;

(*************************************************************************************************)

subHolds[hc_] := ReplaceRepeated[hc, MacroHold[e_] :> e];

(*************************************************************************************************)

subRets[hc_] /; Internal`LiterallyAbsentQ[hc, FunctionReturn] := hc;

subRets[hc_] := If[System`Private`HasImmediateValueQ[$FunctionReturnTarget],
  hc /. FunctionReturn[a_] :> Return[a, Block],
  subRets2[hc]
];

subRets2[hc:HoldComplete[SetDelayed[_, _Block] | _Block]] /; Count[hc, Block] === 1 :=
  hc /. FunctionReturn[a_] :> Return[a, Block];

subRets2[hc:HoldComplete[SetDelayed[_, _Module] | _Module]] /; Count[hc, Module] === 1 :=
  hc /. FunctionReturn[a_] :> Return[a, Block];

subRets2[HoldComplete[(sd:SetDelayed)[lhs_, FunctionReturn[a_]]]] :=
  HoldComplete[sd[lhs, a]];

$uniqueID = 0;
subRets2[hc:HoldComplete[(sd:SetDelayed)[lhs_, rhs_]]] := With[{id = $uniqueID++},
  HoldComplete[sd[lhs, Catch[rhs, $FunctionReturnTag[id]]]] /.
    FunctionReturn[a_] :> Throw[a, $FunctionReturnTag[id]]];

subRets2[HoldComplete[hc_;]] := subRets2 @ HoldComplete @ hc;
subRets2[hc:HoldComplete[{__SetDelayed}]] := Thread[Map[subRets, Thread @ hc], HoldComplete];

(* subRets2[HoldComplete[e_]] := With[{id = $uniqueID++},
  HoldComplete[Catch[e, $FunctionReturnTag[id]]] /.
    FunctionReturn[a_] :> Throw[a, $FunctionReturnTag[id]]];
 *)
subRets2[e_] := ErrorPrint["Error finding returns in: ", HoldForm @ e];

(*************************************************************************************************)

subMps[hc_] /; Internal`LiterallyAbsentQ[hc, {$MacroParentSymbol, $MacroSourceLocation}] := hc;
subMps[hc_] := subSL @ subMps2 @ hc;

subSL[hc_] := ReplaceAll[hc, $MacroSourceLocation :> RuleEval @ SourceLocation[]];

subMps2[hc:HoldComplete[lhs_SetDelayed | lhs_Set | CompoundExpression[lhs_SetDelayed | lhs_Set, Null]]] :=
  ReplaceAll[hc, $MacroParentSymbol -> getParentHead[lhs]];

subMps2[hc_] := Block[
  {mpsPositions = ReverseSortBy[Position[hc, $MacroParentSymbol], Length],
   $sdPositions = Position[hc, _SetDelayed], $hc = hc},
  ReplacePart[hc, Map[pos |-> Rule[pos, findParentHead[pos]], mpsPositions]]
];

findParentHead[pos_] := Block[{parentSD},
  parentSd = SelectFirst[$sdPositions, prefixListQ[#, pos]&, None];
  If[parentSd === None,
    If[System`Private`HasImmediateValueQ[$currentMacroParentSymbol],
      Return[$currentMacroParentSymbol, Block];
      ThrowMsg["noMacroParent", HoldForm @ hc];
    ]
  ];
  Extract[$hc, parentSd, getParentHead]
];

prefixListQ[_, _] := False;
prefixListQ[a_List, b_List] := Length[a] <= Length[b] && a === Take[b, Length @ a];

General::noMacroParent = "Could not resolve macro parent in ``.";

(*************************************************************************************************)

DeclareHoldAllComplete[getParentHead]

getParentHead[(SetDelayed|Set)[lhs_, _]] :=
  MacroHold @@ Replace[
    PatHeadSym @ lhs,
    $Failed :> ThrowMsg["macroParentLHS", HoldForm @ lhs]
  ];

getParentHead[lhs_] := ThrowMsg["macroParentLHS", HoldForm @ lhs]

General::macroParentLHS = "Could not find a head symbol for the LHS of SetDelayed, being ``.";

