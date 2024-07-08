PackageExports[
  "Function",
    ToBlank, ToBlankSequence, ToBlankNullSequence, ToAltPattern,
    PatternHeadSymbol, PatternBoundSymbols,
    ToLHSPattern,
    RemovePatternSymbols, RemovePatternTests
];

(*************************************************************************************************)

General::notConvertableToBlank = "Cannot convert `` to a Blank or related.";

ToBlank[s_Symbol]                          := Blank @ s;
ToBlank[a_Alt]                             := Map[ToBlank, a];
ToBlank[a_List]                            := Alt @@ Map[ToBlank, a];
ToBlank[VHoldP[s_Symbol]]                  := HoldPattern @ Blank @ s;
ToBlank[e_]                                := (Message[ToBlank::notConvertableToBlank, e]; _);

ToBlankSequence[s_Symbol | {s_Symbol}]     := BlankSequence @ s;
ToBlankSequence[a_Alt]                     := Repeated @ Map[ToBlank, a];
ToBlankSequence[a_List]                    := ToBlankSequence[Alt @@ a];
ToBlankSequence[VHoldP[s_Symbol]]          := HoldPattern @ BlankSequence @ s;
ToBlankSequence[e_]                        := (Message[ToBlankSequence::notConvertableToBlank, e]; __);

ToBlankNullSequence[s_Symbol | {s_Symbol}] := BlankNullSequence @ s;
ToBlankNullSequence[a_Alt]                 := RepeatedNull @ Map[ToBlank, a];
ToBlankNullSequence[a_List]                := ToBlankNullSequence[Alt @@ a];
ToBlankNullSequence[VHoldP[s_Symbol]]      := HoldP @ BlankNullSequence @ s;
ToBlankNullSequence[e_]                    := (Message[ToBlankNullSequence::notConvertableToBlank, e]; ___);

ToAltPattern[{}]        := $Failed;
ToAltPattern[{a_}]      := a;
ToAltPattern[list_List] := Alt @@ list;
ToAltPattern[_]         := $Failed;

ToLHSPattern[lhs_Rule]          := RemovePatternSymbols @ First[lhs];
ToLHSPattern[lhs_RuleDelayed]   := RemovePatternSymbols @ Extract[lhs, 1, HoldP];
ToLHSPattern[list:RuleLikeVecP] := ToAltPattern @ RemovePatternSymbols @ Map[ToLHSPattern, list];
ToLHSPattern[lhs:Assoc]         := ToLHSPattern @ Normal @ lhs;
ToLHSPattern[_]                 := $Failed;

(*************************************************************************************************)

RemovePatternSymbols[a_] := ReplaceRepeated[a, VPattern[_, p_] :> p];
RemovePatternTests[a_]   := ReplaceRepeated[a, {VPatternTest[p_, _] :> p, VCondition[p_, _] :> p}];

(*************************************************************************************************)

(*************************************************************************************************)

PatternHeadSymbol::usage = "PatternHeadSymbol[patt$] gives the symbol head (or just the symbol) which the pattern will match.";

SetAttributes[{PatternHeadSymbol, PatternBoundSymbols}, HoldAllComplete]

PatternHeadSymbol[VVerbatim[e_]]                := PatternHeadSymbol[e];
PatternHeadSymbol[VHoldP[e_]]                   := PatternHeadSymbol[e];
PatternHeadSymbol[VPattern[_, e_]]              := PatternHeadSymbol[e];
PatternHeadSymbol[VPatternTest[e_, _]]          := PatternHeadSymbol[e];
PatternHeadSymbol[VCondition[e_, _]]            := PatternHeadSymbol[e];
PatternHeadSymbol[VBlank[s_Symbol ? HoldAtomQ]] := Hold[s];
PatternHeadSymbol[h_[___]]                      := PatternHeadSymbol[h];
PatternHeadSymbol[s_Symbol ? HoldAtomQ]         := Hold[s];
PatternHeadSymbol[___]                          := $Failed;

PatternBoundSymbols[e_] := DeleteDuplicates @ Occurences[Hold[e], VPattern[s_Symbol ? HoldSymbolQ, _] :> Hold[s]];
PatternBoundSymbols[___] := $Failed;