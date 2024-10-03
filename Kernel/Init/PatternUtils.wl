SystemExports[
  "Function",
    ToBlankPattern, ToBlankSequencePattern, ToBlankNullSequencePattern, ToAlternativesPattern,
    PatternHead, DefinitionHead, PatternSymbols, PatternLHS,
    StripPatternSymbols, StripPatternConditions,
  "ControlFlow",
    AllowRHSPatterns
];

PackageExports[
  "Function",
    SetDsToRuleDs, ToHoldPRuleDs,
    ToBlankP, ToBlankSeqP, ToBlankNullSeqP, ToAltP,
    PatHead, DefHead, PatSyms, PatLHS,
    StripPatSyms, StripPatConds,
    MakeSet, MakeSetDelayed, MakeUpSetDelayed, MakeTagSetDelayed
];

(*************************************************************************************************)

DefineAliasRules[
  ToBlankP         -> ToBlankPattern,
  ToBlankSeqP      -> ToBlankSequencePattern,
  ToBlankNullSeqP  -> ToBlankNullSequencePattern,
  ToAltP           -> ToAlternativesPattern,
  PatHead          -> PatternHead,
  DefHead          -> DefinitionHead,
  PatSyms          -> PatternSymbols,
  PatLHS           -> PatternLHS,
  StripPatSyms     -> StripPatternSymbols,
  StripPatConds    -> StripPatternConditions
];

(*************************************************************************************************)

SetDsToRuleDs[h:HoldC[{__SetD}]]  := Apply[RuleD, h, {2}];
ToHoldPRuleDs[HoldC[r:{__RuleD}]] := MapAt[HoldP, NoEval @ r, {All, 1}];

e_SetDsToRuleDs := Message[SetDsToRuleDs::internalError, HoldForm @ e];
e_ToHoldPRuleDs := Message[ToHoldPRuleDs::internalError, HoldForm @ e];

SetDsToRuleDs::internalError = "An internal error occurred: ``..";
ToHoldPRuleDs::internalError = "An internal error occurred: ``.";

(*************************************************************************************************)

PatternHead::usage = "PatternHead[patt$] gives the symbol head (or just the symbol) which the pattern will match.";

SetAttributes[{DefHead, PatHead, PatSyms}, HoldAllComplete]

PatHead[VVerbatim[e_]]                := PatHead[e];
PatHead[VHoldP[e_]]                   := PatHead[e];
PatHead[VPattern[_, e_]]              := PatHead[e];
PatHead[VPatternTest[e_, _]]          := PatHead[e];
PatHead[VCondition[e_, _]]            := PatHead[e];
PatHead[VBlank[s_Symbol ? HAtomQ]]    := Hold[s];
PatHead[h_[___]]                      := PatHead[h];
PatHead[s_Symbol ? HAtomQ]            := Hold[s];
PatHead[___]                          := $Failed;

DefHead[Set[l_, _]]                   := PatHead @ l;
DefHead[SetDelayed[l_, _]]            := PatHead @ l;
DefHead[TagSetDelayed[_, l_, _]]      := PatHead @ l;
DefHead[TagSet[_, l_, _]]             := PatHead @ l;
DefHead[RuleDelayed[l_, _]]           := PatHead @ l;
DefHead[Rule[l_, _]]                  := PatHead @ l;
DefHead[_UpSetDelayed]                := Unimplemented;
DefHead[_UpSet]                       := Unimplemented;
DefHead[___]                          := $Failed;

PatSyms[e_]  := DeleteDuplicates @ Cases[NoEval @ e, VPattern[s:SymP, _] :> Hold[s]];
PatSyms[___] := $Failed;

(*************************************************************************************************)

PatLHS[lhs_Rule]          := StripPatSyms @ First[lhs];
PatLHS[lhs_RuleDelayed]   := StripPatSyms @ Extract[lhs, 1, HoldP];
PatLHS[list:RuleLVecP]    := ToAltP @ StripPatSyms @ Map[PatLHS, list];
PatLHS[lhs:DictP]         := PatLHS @ Normal @ lhs;
PatLHS[_]                 := $Failed;

(*************************************************************************************************)

StripPatSyms[patt_]  := ReplaceRepeated[patt, VPattern[_, p_] :> p];
StripPatConds[patt_] := ReplaceRepeated[patt, {VPatternTest[p_, _] :> p, VCondition[p_, _] :> p}];

(*************************************************************************************************)

General::notConvertableToBlank = "Cannot convert `` to a Blank or related.";

ToBlankP[s_Sym]                  := Blank @ s;
ToBlankP[a_Alt]                  := Map[ToBlankP, a];
ToBlankP[a_List]                 := Alt @@ Map[ToBlankP, a];
ToBlankP[Hold[s_Sym]]            := HoldP @ Blank @ s;
ToBlankP[VHoldP[s_Sym]]          := HoldP @ Blank @ s;
ToBlankP[e_]                     := (Message[ToBlankP::notConvertableToBlank, e]; _);

ToBlankSeqP[s_Sym | {s_Sym}]     := BlankSequence @ s;
ToBlankSeqP[a_Alt]               := Repeated @ Map[ToBlankP, a];
ToBlankSeqP[a_List]              := ToBlankSeqP[Alt @@ a];
ToBlankSeqP[Hold[s_Sym]]         := HoldP @ BlankSeq @ s;
ToBlankSeqP[VHoldP[s_Sym]]       := HoldP @ BlankSeq @ s;
ToBlankSeqP[e_]                  := (Message[ToBlankSeqP::notConvertableToBlank, e]; __);

ToBlankNullSeqP[s_Sym | {s_Sym}] := BlankNullSequence @ s;
ToBlankNullSeqP[a_Alt]           := RepeatedNull @ Map[ToBlankP, a];
ToBlankNullSeqP[a_List]          := ToBlankNullSeqP[Alt @@ a];
ToBlankNullSeqP[Hold[s_Sym]]     := HoldP @ BlankNullSeq @ s;
ToBlankNullSeqP[VHoldP[s_Sym]]   := HoldP @ BlankNullSeq @ s;
ToBlankNullSeqP[e_]              := (Message[ToBlankNullSeqP::notConvertableToBlank, e]; ___);

ToAltP[{}]        := $Failed;
ToAltP[{a_}]      := a;
ToAltP[list_List] := Alt @@ list;
ToAltP[_]         := $Failed;

(*************************************************************************************************)

SetAttributes[{MakeSet, MakeSetDelayed, MakeUpSetDelayed, MakeTagSetDelayed}, HoldAll];

MakeSet[lhs_, rhs_]                     := Hold[Set[lhs, rhs]];
MakeSetDelayed[lhs_, rhs_]              := Hold[SetDelayed[lhs, rhs]];
MakeUpSetDelayed[lhs_, rhs_]            := Hold[UpSetDelayed[head, lhs, rhs]];
MakeTagSetDelayed[head_Sym, lhs_, rhs_] := Hold[TagSetDelayed[head, lhs, rhs]];

(*************************************************************************************************)

SetAttributes[AllowRHSPatterns, {HoldAllComplete}];

AllowRHSPatterns[body_] := Quiet[body, RuleDelayed::rhs];

