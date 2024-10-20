SystemExports[
  "PlotOption",        Theme, SubThemes,
  "Function",          ThemedSymbols, ThemeNames, ThemeRules, GetThemes,
  "Predicate",         ThemeActiveQ
];

PackageExports[
  "Function",          AbsoluteThemes, AbsoluteThemeRules, RelativeThemeRules, LookupThemedOptions, DropThemeRules,
  "ScopingFunction",   BlockTheme,
  "MutatingFunction",  SetTheme, BindTheme, ResetTheme,
  "MetaFunction",      SetThemedSymbol, UnsetThemedSymbol, DefineThemes, ClearThemes,
  "Predicate",         ThemedRulesQ
];

SessionExports[
  "Predicate",         ThemedSymbolQ,
  "RegistryVariable",  $ThemeRules, $ThemeNames, $ThemeUsers,
  "CacheVariable",     $OptionKeysCache, $OptionRulesCache,
  "TransientVariable", $ActiveThemes, $ActiveSubThemes, $LocalThemes, $LocalThemedOptions
];

PrivateExports[
  "Function",          LookupThemedOptionsAndInfo
];

(**************************************************************************************************)

"Theme is an option.
* It supports the following settings:
| None | do not apply any themes |
| Inherit | apply the default theme specified by Options |
| 'name$' | apply a given theme |
| {'name$1', 'name$2', ...} | apply several themes |
* The first Theme -> in a rule list 'wins'.
* SubThemes -> is an additive alternative.
* The position it occurs in a rule list determines precedence.
"

"$LocalThemes[sym$] is a list of the active themes and subthemes for sym$, updated when calling UnpackSymbolsAs against a themed symbol.";
"$LocalThemedOptions is a dict mapping symbols to the a list of their themed options, updated when calling UnpackSymbolsAs against a themed symbol.";

(**************************************************************************************************)

General::noThemes = "Symbol `` has no defined themes.";
General::notThemedSymbol = "Expected a themed symbol ``.";
General::unknownTheme = "Symbol `` does not have a theme ``. Known themes: ``.";
General::notThemeRules = "Expected a list of rules for themes of ``: ``.";
General::notThemeSpec = "Expected None, a string, or a list of strings: ``.";
General::badThemeOpt = "The some options were looked up for `` but don't exist: ``."
General::notThemeBinding = "Not a valid theme binding: ``.";
General::notGlobalTheme = "`` is not a theme for any symbol.";

msgThemeRules[spec_]        := msgTheme["notThemeRules", $head, spec, {}];
msgThemeSymbol[sym_]        := msgTheme["noThemes", sym, $Invalid];
msgThemeNotSym[spec_]       := msgTheme["notThemedSymbol", spec, $Invalid];
msgThemeSpec[spec_]         := msgTheme["notThemeSpec", spec, {}];
msgThemeName[sym_, name_]   := msgTheme["unknownTheme", sym, name, FullRow[Lookup[$ThemeNames, sym, {}], ","], {}];
msgThemeBadOpt[fn_, keys_]  := msgTheme["badThemeOpt", fn, First[Complement[keys, $OptionKeysCache @ fn], "?"], Automatic];
msgBadBinding[spec_]        := msgTheme["notThemeBinding", spec, $Failed];
msgGlobalTheme[name_]       := msgTheme["notGlobalTheme", name, {}];

SetHoldR @ withHead;

$head = General;
msgTheme[msg_, args___, def_] := With[{h = $head}, Message[MessageName[h, msg], args]; def];
withHead[head_, body_] := Block[{$head = head}, body];

(**************************************************************************************************)

checkSym = ExtendCaseOf[
  $[sym_ ? ThemedSymbolQ] := sym;
  $[sym_Sym]              := msgThemeSymbol[sym];
  $[sym_]                 := msgThemeNotSym[sym];
];

checkSymList[e_List] := Select[e, checkSym];

checkThemes[sym_] := ExtendCaseOf[
  None                  := None;
  {}                    := {};
  name_Str | {name_Str} := If[ElementQ[name, $ThemeNames @ sym], name, msgThemeNameSym[sym, name]];
  names_List            := Module[
    {flat = DelDups @ Flatten @ names},
    If[SubsetOfQ[flat, $ThemeNames @ sym], flat, msgThemeNameSym[sym, flat]]
  ];
  other_ := msgThemeNameSym[sym, other]
];

msgThemeNameSym[sym_ ? ThemedSymbolQ, name_] := msgThemeName[sym, name];
msgThemeNameSym[sym_Sym, name_]              := (msgThemeSymbol[sym]; {})
msgThemeNameSym[sym_, name_]                 := (msgThemeNotSym[sym]; {})

(**************************************************************************************************)

SetPred1[ThemedRulesQ, ThemedSymbolQ]

"ThemedSymbolQ[sym$] gives True if sym$ has theme rules associated with it."
"ThemedRulesQ[rules$] gives True if rules$ contains Theme or SubThemes rules.";

ThemedRulesQ[{}] := False;
ThemedRulesQ[_List ? (KeyExistsQ[Theme])] := True;
ThemedRulesQ[_List ? (KeyExistsQ[SubThemes])] := True;

(**************************************************************************************************)

DropThemeRules = CaseOf[
  {}         := {};
  rules_List := DelCases[rules, _[Theme | SubThemes, _]];
];

(**************************************************************************************************)

"AbsoluteThemes[sym$] looks up the the pair {themes, subthemes$} of set themes for sym$.
* These are either the default themes given by Options, or what has currently been set by SetThemes."

SetStrict[AbsoluteThemes];

AbsoluteThemes[sym_] := withHead[AbsoluteThemes, flatThemes @ activeThemes @ checkSym @ sym];
AbsoluteThemes[]     := withHead[AbsoluteThemes, Merge[{$ActiveThemes, $ActiveSubThemes}, flatThemes]];

flatThemes = CaseOf[
  {}        := {};
  None      := {};
  Inherit   := {};
  s:{_Str}  := s;
  s_Str     := s;
  list_List := DelDups @ Flatten @ list;
  e_        := msgThemeSpec[e];
];

activeThemes[$Invalid] := {};
activeThemes[sym_]     := List[$ActiveThemes @ sym, $ActiveSubThemes @ sym];

(**************************************************************************************************)

"AbsoluteThemeRules[sym$, rules$] returns List[themedRules$, themes$, subthemes$], where themedRules$ includes the defaults from Options.";
"RelativeThemeRules[sym$, rules$] returns List[themedRules$, themes$, subthemes$], where themedRules$ just resolves the given rules.";

SetStrict[AbsoluteThemeRules, RelativeThemeRules];

AbsoluteThemeRules[sym_]         := withHead[AbsoluteThemeRules, dedupThemeRules @ absoluteRules[checkSym @ sym, {}]];
AbsoluteThemeRules[sym_, rules_] := withHead[AbsoluteThemeRules, dedupThemeRules @ absoluteRules[checkSym @ sym, rules]];
RelativeThemeRules[sym_, rules_] := withHead[RelativeThemeRules, dedupThemeRules @ relativeRules[checkSym @ sym, rules]];

dedupThemeRules[{rules_, themes_, subThemes_}] := JoinOptions[Theme -> themes, SubThemes -> subThemes, rules];

(**************************************************************************************************)

SetStrict[LookupThemedOptionsAndInfo, LookupThemedOptions];

LookupThemedOptionsAndInfo[sym_, opts_List, keys_List] := Locals[
  $head = LookupThemedOptionsAndInfo;
  {rules, themes, subThemes} = absoluteRules[sym, opts];
  allThemes = flatThemes @ List[themes, subThemes];
  dict = UDict[Reverse @ rules, Theme -> allThemes, SubThemes -> {}];
  List[allThemes, dict, lookupKeys[sym, dict, keys]]
];

LookupThemedOptions[sym_, opts_List, keys_List] :=
  withHead[LookupThemedOptions, lookupKeys[sym, First @ absoluteRules[sym, opts], keys]];

lookupKeys[sym_, opts_, keys_] := Then[
  CheckOptKeys[sym -> $OptionKeysCache[sym], opts],
  Lookup[opts, keys, msgThemeBadOpt[sym, keys]]
];

(**************************************************************************************************)

"relativeRules[sym$, rules$] resolves *just* rules:
* the first Theme that is encountered is expanded.
* all SubThemes that are encountered are expanded.
a tuple of {rules, themes, subthemes} is returned."

"absoluteRules[sym$, rules$] is like relativeRules:
* it also adds the global options."

relativeRules[sym_, {}] := {{}, Inherit, {}};
absoluteRules[sym_, {}] := expandingFor[sym, expandDefaults[]];

relativeRules[sym_, rules_] := {rules, Inherit, {}};
relativeRules[sym_, rules_ ? ThemedRulesQ] := expandingFor[sym, expandRules1 @ rules];

absoluteRules[sym_, rules_] := expandingFor[sym, {expandRules0 @ rules, expandDefaults[]}];

SetHoldR[expandingFor];
expandingFor[$Invalid, body_] := {{}, {}, {}};
expandingFor[sym_, body_] := Block[
  {$head = sym, $winningThemes = Inherit, $allSubThemes = {}, $theme$ = Theme},
  List[Flatten @ body, $winningThemes, flatThemes @ $allSubThemes]
];

expandDefaults[] := List[
  expandThemes @ $ActiveSubThemes @ $head,
  If[$winningThemes === Inherit,
    $winningThemes = $ActiveThemes @ $head;
    expandThemes @ $winningThemes, {}],
  $OptionRulesCache @ $head
];

expandRules0 = CaseOf[
  {}                        := {};
  rules_List ? ThemedRulesQ := expandRules1 @ rules;
  rules_List                := rules;
  other_                    := msgThemeRules[other];
];

expandRules1 = CaseOf[
  {}                        := {};
  rules_List                := expandRules2[Lookup[rules, {$theme$, SubThemes}, Null], rules];
  other_                    := msgThemeRules[other];
];

expandRules2[{Null, Null}, userRules_] :=
  userRules;

expandRules2[{themes_ ? setWinningThemes, subThemes_ ? addSubThemes}, userRules_] :=
  List[userRules, expandThemes @ subThemes, expandThemes @ themes];

setWinningThemes[Null | Inherit] := True;
setWinningThemes[theme_]  := ($winningThemes = flatThemes @ theme; $theme$ = Null; True);

addSubThemes[Null]       := True
addSubThemes[subthemes_] := (AppendTo[$allSubThemes, subthemes]; True);

SetListable[expandThemes];

expandThemes = ExtendCaseOf[
  Null      := {};
  None      := {};
  Inherit   := {};
  list_List := Map[expandRules0, lookupRules[$head, flatThemes @ list]];
  theme_    := expandRules0 @ lookupRules[$head, theme];
];

(**************************************************************************************************)

lookupRules[sym_, name_] :=
  Lookup[$ThemeRules @ sym, name, msgThemeNameSym[sym, name]; List[]];

lookupRulesDict[sym_] :=
  Lookup[$ThemeRules, sym, msgThemeSymbol[sym]; Dict[]];

(**************************************************************************************************)

SetStrict @ SetHoldR @ BlockTheme;

BlockTheme[spec_, body_] := IBlock[
  {$ActiveThemes},
  BindTheme @ spec;
  body
];

(**************************************************************************************************)

ResetTheme[sym_Sym] := msgThemeSymbol[ResetTheme, sym];
ResetTheme[other_]  := msgThemeNotSym[ResetTheme, other];

(**************************************************************************************************)

SetTheme = CaseOf[
  $[list_List, theme_] := setTheme[checkSymList @ list, theme];
  $[All, theme_]       := setThemeGlobal[theme];
  $[sym_ , theme_]     := setTheme[checkSym @ sym, theme];
  $[theme_]            := setThemeGlobal[theme];
];

setTheme = CaseOf[
  $[$Invalid, _]       := Null;
  $[All, theme_]       := setThemeGlobal[theme];
  $[list_List, theme_] := Scan[$[#, theme]&, list];
  $[sym_, None]        := ($ActiveThemes[sym] = {};);
  $[sym_, Inherit]     := (ResetTheme[sym];);
  $[sym_, themes_]     := doSet[sym, themes];
];

doSet[sym_, themes_] := withHead[SetTheme, Set[$ActiveThemes[sym], checkThemes[sym, themes]];];

setThemeGlobal = CaseOf[
  names_List := With[
    {syms = Union @@ Map[getThemeUsers, names]}, Scan[
    setTheme[#, Inter[names, $ThemeNames[#]]]&,
    syms
  ]];
  name_      := setTheme[getThemeUsers @ name, name];
];

getThemeUsers[name_] := Lookup[$ThemeUsers, name, msgGlobalTheme[name]]

(**************************************************************************************************)

BindTheme = ExtendCaseOf[
  str_Str       := SetTheme[str];
  Rule[a_, b_]  := SetTheme[a, b];
  rules:___Rule := (MapApply[SetTheme, rules]);
];

s_SetTheme := msgBadBinding[SetTheme, HoldForm @ s];

(**************************************************************************************************)

ThemedSymbols[] := Keys @ $ThemeNames;

(**************************************************************************************************)

ThemeNames = CaseOf[
  $[] := Sort @ Keys @ $ThemeUsers;
  All := KeySort @ Dict @ $ThemeNames;
  s_  := Lookup[$ThemeNames, checkSym @ s, {}]
];

ThemeRules /: Set[ThemeRules[lhs___], rhs_] := DefineThemes[lhs, rhs];

(**************************************************************************************************)

"SetThemedSymbol[sym$] declares sym$ to be themed.
* LookupSymbolsAs will automatically resolve themes against these symbols.
* It is automatically called when appropriate."

SetStrict[SetThemedSymbol, UnsetThemedSymbol];

SetThemedSymbol[sym_Sym] := Module[
  {opts = Options @ sym, nonThemeOpts},
  nonThemeOpts = DropThemeRules @ opts;
  If[opts === {}, ReturnFailed[SetThemedSymbol::noOptions, sym]];
  If[!HasKeyQ[opts, Theme] || !HasKeyQ[opts, SubThemes], ReturnFailed[SetThemedSymbol::noThemeOpt, sym]];
  If[Lookup[opts, SubThemes] =!= {}, ReturnFailed[SetThemedSymbol::defaultSubthemes, sym]];
  ThemedSymbolQ[sym]     = True;
  $OptionRulesCache[sym] = nonThemeOpts;
  $OptionKeysCache[sym]  = Keys @ opts;
  $ThemeRules[sym]       = Dict[];
  setupReset[sym, FirstCase[opts, _[Theme, _]]];
  If[MissingQ[$ActiveSubThemes[sym]], $ActiveSubThemes[sym] = {}];
];

setupReset[sym_, _[Theme, theme_]] := Then[
  If[MissingQ[$ActiveThemes[sym]], $ActiveThemes[sym] = {theme}];
  SetD[ResetTheme[sym], doReset[sym, theme]]
];

setupReset[sym_, e_]               := ErrorPrint[SetThemedSymbol, sym -> e];

doReset[sym_, default_] := withHead[ResetTheme, doSet[sym, default]];

UnsetThemedSymbol[_Sym] := $Invalid;
UnsetThemedSymbol[sym_Sym ? ThemedSymbolQ] := Then[
  unregisterName[sym, $ThemeNames @ sym],
  Unset[{
    $ThemeNames[sym],
    $ThemeRules[sym],
    $ActiveThemes[sym],
    $ActiveSubThemes[sym],
    $OptionRulesCache[sym],
    $OptionKeysCache[sym],
    ResetTheme[sym]
  }]
];

SetListable[unregisterName];

unregisterName[sym_, name_] := DeleteFrom[$ThemeUsers[name], sym];

SetThemedSymbol::noOptions = "There appear to be no options (yet) for ``. Reorder your definitions.";
SetThemedSymbol::noThemeOpt = "There must be a default Theme and SubTheme option for ``.";
SetThemedSymbol::defaultSubthemes = "Default SubThemes must be {} for ``.";

(**************************************************************************************************)

"DefineThemes[sym$, name$ -> rules$] defines an additional theme for sym$."

SetStrict @ DefineThemes;

DefineThemes[sym_Sym, rules__Rule] := DefineThemes[sym, {rules}];
DefineThemes[sym_Sym, dict_Dict]   := DefineThemes[sym, Normal @ dict];
DefineThemes[sym_Sym, specs_List] := Locals[
  IfFailed[SetThemedSymbol @ sym, ReturnFailed[]];
  $keys = DeleteCases[Theme -> _] @ $OptionKeysCache @ sym; $sym = sym;
  defs = Map[checkDefRule, specs];
  names = Keys @ defs;
  DPrint["Defining themes ", names, " for ", sym];
  Scan[name |-> KeyUnionTo[$ThemeUsers, name, {sym}], names];
  result = BindTo[$ThemeRules[sym], defs];
  $ThemeNames[sym] = Keys @ $ThemeRules[sym];
];

checkDefRule[spec_] := badThemeDef["themeDefSpec", spec];
checkDefRule[spec:Rule[name_, rules_]] := Which[
  !StrQ[name],       badThemeDef["themeDefName", $sym, name],
  !RuleLVecQ[rules], badThemeDef["themeDefRules", name, $sym, rules],
  CheckSubsetOfQ[Keys @ rules, $keys, DefineThemes -> "themeDefKeys", name, $sym], spec,
  True, Nothing
];

badThemeDef[msg_, args___] := (Message[MessageName[DefineThemes, msg], args]; Nothing);

DefineThemes::themeDefSpec = "Theme definition for symbol `` was not a rule: ``.";
DefineThemes::themeDefName = "Theme name for symbol `` must be a string, not ``.";
DefineThemes::themeDefRules = "Theme `` for symbol `` must be a list of rules, not ``.";
DefineThemes::themeDefKeys = "Theme `3` for symbol `4` specifies keys `1` that aren't known options `2`.";

(**************************************************************************************************)

"ClearThemes[] removes all theme rules."

SetStrict @ ClearThemes;

ClearThemes[] := Then[
  $ActiveThemes       = UDict[];
  $ActiveSubThemes    = UDict[];
  $LocalThemes        = UDict[];
  $LocalThemedOptions = UDict[];
  $OptionRulesCache = $OptionKeysCache = UDict[];
  $ThemeRules = $ThemeNames = $ThemeUsers = UDict[];
];

(**************************************************************************************************)

If[!HasIValueQ[$ThemeRules], ClearThemes[]];

(**************************************************************************************************)

SetStrict[ThemeRules];

"ThemeRules[sym$] gives the association of theme rules for sym$.
ThemeRules[sym$, 'name$'] gives the rules for theme called 'name$'.
ThemeRules[sym$] = Dict['name$1' -> rules$1, $$] defines the themes for sym$ simultaneously.
ThemeRules[sym$, 'name$] = rules$ defines an additional theme.
* You can also use DefineThemes and ClearThemes."

ThemeRules[All]            := $ThemeRules;
ThemeRules[sym_]           := lookupRulesDict @ sym;
ThemeRules[sym_, name_Str] := lookupRules[sym, name];

Protect[ThemeRules];
