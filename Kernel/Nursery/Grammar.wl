PackageExports[
  "Function",
    FindGrammarType, GrammarAnnotate,
  "TypeHead",
    GrammarOf,
  "Head",
    GrammarSymbol, GrammarAnnotation, GrammarPattern
];

PrivateExports[
  "Head", GSym, GAnno,
  "SpecialFunction", AnnoSplice
];

(*************************************************************************************************)

DefineAliasRules[GSym -> GrammarSymbol, GAnno -> GrammarAnnotation, GPattern -> GrammarPattern]

(*************************************************************************************************)

SetHoldC[GrammarSymbol, GrammarAnnotation]

$excludedSymbols = {
  Association, List, DirectedInfinity, None, Null, SparseArray, NumericArray,
  Rule, RuleDelayed, HoldPattern
};

makeCapturePatt[alt_] := (Rule | RuleDelayed)[alt | alt[__] | VHoldP[alt] | VHoldP[alt[___]], _];

(* while it's against the spirit of this experiment, i could condition on the LHS
of a Rule, RuleDelayed, or SetDelayed being a GrammarSymbol or containing one
as the receiver of a pattern *)

(* ok, the smart thing to do is have GrammarSymbols be not just pure symbols but *actually*
little patterns, like GrammarSymbol[foo], GrammarSymbol[Rule[foo, _]], GrammarSymbol[foo[___]],
GrammarSymbol[RuleDelayed[foo[___]].

this is like the keying in the generalized trie.

so perhaps a better way of doing this would be to rewrite the original tree once, looking for
capturable subtrees and replacing them with the appropriate mini pattern, via a
GrammarAnnotated[GrammarSymbol[fragment], original]

Then! I go back and take Occs of the GrammarAnnotateds, and for each one i replace all interior
GrammarAnnotated with their first argument. then i can produce edges from these.

but later i will treat them as lists and call fromCommonType on them.

experiment with pulling out all the graphics from the documentation to make a big training set.

my GrammarAnnotated are essentially little candidate constructors.

to find a common type, we may want to do different things for fixed arguments versus options.
so might be useful to have an OptionsOf in the main type finder. it matches a sequence of
zero or more rules in any order whose keys are exact matches to the corresponding patterns.

i can certainly recognize this pattern when given a list that starts off non-option and turns
option. but i can also recognize it when given an opaque expression that I *know* has options.

*)

FindGrammarType[expr_] := Locals[
  syms = Compl[ContainedSymbols @ expr, $excludedSymbols];
  alt = ToAltP @ syms;

  alt2 = Alt[makeCapturePatt[alt], alt];
  posList = Sort @ Position[expr, alt2];
  tags = Extract[expr, posList];
  posList = TrimRight[0] /@ posList;
  $leafRule = a:Alt[ToBlankP[alt], alt] :> RuleEval[makeLeaf @ a];
  subs = Extract[expr, posList, makeRoot];
  FromVertexOutLists[
    Merge[subs, Occs[_GrammarSymbol]],
    PlotTheme -> "CoreGrammar",
    VertexLabelFunction -> First
  ]

  (* grouped = GroupAgainst[subs, tags]; *)
];

DefineGraphTheme["CoreGrammar", {
  ThemeParent -> "Core",
  VertexLabels -> "Name",
  Options -> {ImagePadding -> 30}
}];

SetHoldC[GrammarSymbol, makeRoot, makeLeaf];

makeRoot[(Rule | RuleDelayed)[lhs_, rhs_]] := With[
  {gs = GrammarSymbol @@ PatternHead[lhs]},
  Rule[gs, ReplaceAll[HoldComplete[args], $leafRule]]
];

makeRoot[head_Sym[args___]] := With[{gs = GrammarSymbol[head]}, Rule[gs, ReplaceAll[HoldC[args], $leafRule]]];
makeRoot[head_]             := GrammarSymbol[head] -> None;

makeLeaf[head_Sym[args___]] := GrammarSymbol[head];
makeLeaf[head_]             := GrammarSymbol[head];

(*************************************************************************************************)

FindGrammarType[expr_, 2] := Locals[
  annoExpr = GrammarAnnotate @ expr;
  subExprs = Occurrences[annoExpr, GAnno[tag_, body_] :> Rule[GPattern[tag], HoldC[body] /. GAnno[kidTag_, _] :> GPattern[kidTag]]];
  FromVertexOutLists[
    Merge[subExprs, Occs[_GPattern]],
    PlotTheme -> "CoreGrammar",
    GraphLayout -> {"LayeredDigraphEmbedding", "Orientation" -> Left},
    VertexLabelFunction -> None,
    ImageSize -> 800, ImagePadding -> {{150, 150}, {50, 50}}
  ]

  (* grouped = GroupAgainst[subs, tags]; *)
];

(*************************************************************************************************)

SetHoldC[AnnoSplice]

$excludedSymbols = {
  Association, List, DirectedInfinity, None, Null, SparseArray, NumericArray,
  Rule, RuleDelayed,
  HoldPattern, Verbatim, Condition, PatternTest,
  Pattern, Blank, BlankSequence, BlankNullSequence,
  HoldComplete
};

GrammarAnnotate[expr_] := Locals[
  syms = Compl[ContainedSymbols @ NoEval @ expr, $excludedSymbols];
  symP = ToAltP @ syms;
  $annoSyms = syms;
  $annoRules = $baseAnnoRules /. FmS -> symP;
  result = attachAnnos[expr];
  result //. (AnnoSplice[a_] :> a) //. (h_[l___, AnnoSplice[m___], r___] :> h[l, m, r])
];

SetHoldC[makeAnno, attachAnnos, annoRecQ]

annoRecQ[body_] := VContainsQ[NoEval @ body, $annoSyms];
makeAnno[tag_, body_ ? annoRecQ] := GAnno[tag, body] /. FmR -> AnnoSplice;
makeAnno[tag_, body_ ? annoRecQ] := GAnno[tag, body] /. FmR[args___] :> RuleEval[attachAnnos[args]];
attachAnnos[args___] := ReplaceAll[AnnoSplice[args], $annoRules];

(*************************************************************************************************)

CoreBox[GrammarAnnotation[tag_, body_]] :=
  NiceTooltipBox[FrameBox[MakeBoxes @ body, FrameMargins -> 0, FrameStyle -> $DarkGreen, ContentPadding -> False], CodePaneBox @ tag];

CoreBox[GrammarSymbol[sym_]] := StyleBox[MakeBoxes @ sym, FontColor -> $DarkBlue];

CoreBox[GrammarPattern[body_]] := MakeBoxes @@ ReplaceRepeated[Hold[body], {VVerbatim[v_] :> v, VHoldP[v_] :> v}];

(*************************************************************************************************)

$annoPatts = List[
  (h:SetLSymP)[VHoldP[$[___]], $2_],
  (h:RuleLSymP)[$, $1_],
  $[$1___],
  $
];

$toAnnoLHS = {$ -> sym$:FmS};
$toAnnoTag = {VPattern[DollarP,  p_] :> p, VPattern[s_, _] :> s, $ -> GSym[sym$]};
$toAnnoBod = {VPattern[d:DollarP, _] :> FmR[d], VPattern[s_, _] :> s, $ -> sym$};
$toAnnoBod2 = VVerbatim[v_] :> v;

toAnnoRule[lhs_, tag_, bod_] := RuleDelayed[lhs, RuleEval @ makeAnno[tag, bod]];
makeAnnoRules[] := ZipMap[toAnnoRule, $annoPatts /. $toAnnoLHS, $annoPatts /. $toAnnoTag, $annoPatts /. $toAnnoBod /. $toAnnoBod2];
$baseAnnoRules := $baseAnnoRules = makeAnnoRules[];

(*************************************************************************************************)
