SystemExports[
  "GraphicsOption",    Theme
  "FormHead",          Themed,
  "Function",          ThemeRules, ActiveThemes,
  "Predicate",         ThemeActiveQ
];

PackageExports[
  "Function",          ThemedOptions, LookupThemedOptions, ActiveThemeRules, ExpandThemeRules,
  "ScopingFunction",   BlockTheme,
  "MutatingFunction",  SetTheme,
  "BoxFunction",       ThemedBox,
  "MetaFunction",      SetThemedSymbol, UnsetThemedSymbol, DefineThemes, ClearThemes,
  "Predicate",         ThemedRulesQ, ThemedOptsQ, ThemedSymbolQ
];

SessionExports[
  "RegistryVariable",  $ThemeRules, $ThemeNames, $ThemeUsers,
  "CacheVariable",     $OptionKeysCache, $OptionRulesCache,
  "TransientVariable", $ActiveThemes, $CurrentThemes
];

PrivateExports[
  "PatternSymbol",     ThemeSpecP, SetThemeFast
];

(**************************************************************************************************)

General::noThemes = "Symbol `` has no defined themes.";
General::notThemedSymbol = "Expected a themed symbol ``.";
General::unknownTheme = "Symbol `` does not have a theme ``. Known themes: ``.";
General::badThemeRules = "Expected a list of rules: ``.";
General::badThemeOpt = "The some options were looked up for `` but don't exist: ``."
General::badThemeBinding = "Not a valid theme binding: ``.";
General::unknownGlobalTheme = "`` is not a theme for any symbol.";

msgThemeRules[sym_, spec_]  := (Message[sym::badThemeRules, sym, spec]; {});
msgThemeBadSym[sym_, fn_]   := Message[sym::noThemes, fn];
msgThemeNotSym[sym_, fn_]   := Message[sym::notThemedSymbol, fn];
msgBadTheme[sym_, name_]    := Message[sym::unknownTheme, sym, name, FullRow[$ThemeNames @ sym, ","]];
msgThemeBadOpt[sym_, fn_, keys_] := Message[sym::badThemeOpt, fn, First[Complement[keys, $OptionKeysCache @ fn], "?"]];
msgBadBinding[sym_, spec_]  := Message[sym::badThemeBinding, spec];
msgGlobalTheme[sym_, name_] := Message[sym::unknownGlobalTheme, name];

testSym[_ ? ThemedSymbolQ] := True;
testSym[sym_Sym] := Then[msgThemeBadSym[sym, sym], False];
testSym[sym_]    := Then[msgThemeNotSym[General, sym], False];

(**************************************************************************************************)

DefinePatternRules[
  ThemeSpecP -> Alt[_Str, _Sym -> StrOrVecP]
];

(**************************************************************************************************)

lookupRules[sym_, name_] :=
  Lookup[$ThemeRules @ sym, name, msgBadTheme[sym, name]; List[]];

lookupRulesDict[sym_] :=
  Lookup[$ThemeRules, sym, msgThemeSymbol[sym]; Dict[]];

(**************************************************************************************************)

SetPred1[ThemedRulesQ, ThemedOptsQ, ThemedSymbolQ]

"ThemedSymbolQ[sym$] gives True if sym$ has theme rules associated with it."
"ThemedRulesQ[rules$] gives True if rules$ contains a Theme rule.";
"ThemedOptsQ[rule$1, rule$2, $$] gives True if one of the rule$i is a Theme rule.";

ThemedRulesQ[_List ? (KeyExistsQ[Theme])] := True;
ThemedOptsQ[___, Rule[Theme, _], ___]     := True;
ThemedSymbolQ[s_Sym] := KeyExistsQ[$ThemeRules, s];

(**************************************************************************************************)

"ActiveThemes[sym$] gives the list of active themes for $sym.
This always includes 'Default' last, which expands to the original option defaults for sym.
It also includes the themes as defined by SetTheme."

SetStrict[ActiveThemes, ThemeActiveQ];

ActiveThemes[sym_ ? testSym]           := $ActiveThemes @ sym;
ActiveThemes[_]                        := List[];
ActiveThemes[]                         := $CurrentThemes;

ThemeActiveQ[name_]                    := VContainsQ[$CurrentThemes, name];
ThemeActiveQ[sym_Sym ? testSym, name_] := VContainsQ[$ActiveThemes[sym], name];

(**************************************************************************************************)

"ActiveThemeRules[sym$] looks up the list of options rule lists.
* ActiveThemes[sym$] is used to obtain the active theme names, which are looked up.
* ExpandThemeRules[rules$] expands any Theme -> ... rules that are present."

SetStrict @ ActiveThemeRules;

ActiveThemeRules[sym_ ? testSym] := Flatten @ activeThemeRules @ sym;
ActiveThemeRules[_]              := List[];

activeThemeRules[sym_] := expandThemes[sym, addTheme @ $ActiveThemes @ sym];

defaultRules[sym_] := expandRules[sym, $OptionRulesCache @ sym];

(**************************************************************************************************)

"ExpandThemeRules[sym$, rules$] recursively expands Theme -> ... in rules, in-place.";

SetStrict @ ExpandThemeRules;

ExpandThemeRules[sym_ ? testSym, rules_] := expandRules[sym, rules];
ExpandThemeRules[_, rules_]              := rules;

expandRules[sym_] := CaseOf[
  {}                        := {};
  rules_List                := rules;
  rules_List ? ThemedRulesQ := expandThemedRules[sym, rules];
  other_                    := msgThemeRules[sym, other];
];

expandThemedRules[sym_] := CaseOf[
  rules_List                := List[expandThemes[sym, addTheme @ Lookup[rules, Theme]], rules];
  other_                    := msgThemeRules[sym, other];
];

expandThemes[sym_] := ExtendCaseOf[
  None                      := {};
  name_Str                  := expandRules[sym, lookupRules[sym, name]];
  names_List ? StrVecQ      := expandRules[sym, Flatten @ lookupRules[sym, names]];
  other_                    := Then[msgBadTheme[sym, other]; {}];
];

addTheme[None] := None;
addTheme[theme_] /; ListQ[$active] := (AppendTo[$active, theme]; theme)
addTheme[theme_] := theme;

(**************************************************************************************************)

SetCoreSubBox[Themed]

CoreBox[Themed[spec___][expr_]] := ThemedBox[MakeBox @ expr, spec];

SetBoxFn @ SetHoldC @ themedBox;

ThemedBox[expr_, spec:ThemeSpecP]   := BlockTheme[spec, MakeBox @ expr];
ThemedBox[expr_, spec:ThemeSpecP..] := BlockTheme[{spec}, MakeBox @ expr];

(**************************************************************************************************)

SetStrict @ SetHoldR @ BlockTheme;

BlockTheme[spec_, body_] := IBlock[
  {$ActiveThemes},
  setActive1 @ spec;
  body
];

(**************************************************************************************************)

SetTheme[spec:SymOrVecP, themes:Alt[__Str, None]] := (setActive2[spec, {}]);
SetTheme[spec:ThemeSpecP]   := (setActive1[spec];)
SetTheme[spec:ThemeSpecP..] := (setActive1[List @ spec];)

s_SetTheme := msgBadBinding[SetTheme, HoldForm @ s];

(**************************************************************************************************)

setActive2[{}, _] := Null;
setActive2[syms:{__Sym}, themes_] := Scan[sym |-> setActive2[sym, themes], syms];
setActive2[sym_Sym | {sym_Sym}, themes_] := If[ThemedSymbolQ[sym],
  $ActiveThemes[sym] = DelDups @ Map[testName[sym, $ThemeNames @ sym], ToList @ themes],
  msgThemeBadSym[SetTheme, sym]
];

testName[sym_, names_][name_] := If[VContainsQ[names, name], name, msgBadTheme[sym, name]; Nothing];

SetListable[setActive1];

setActive1[sym_ -> name_]  := setActive2[sym, name];
setActive1[name_Str]       := setActive2[Lookup[$ThemeUsers, name, msgGlobalTheme[name]; {}], name];
setActive1[spec_]          := msgBadBinding[SetTheme, spec];

(**************************************************************************************************)

"ThemedOptions[sym$] returns the options for sym$ according to its currently active themes.
ThemedOptions[sym$, {opt$1 -> val$1, $$}] overrides these options with user options.
* If opts$ contains Theme -> 'name$' or Theme -> {'name$1', $$}, these will be applied and \
the currently active themes ignored."

SetStrict @ ThemedOptions;

ThemedOptions = CaseOf[
  $[sym_]                  := $[sym, {}];
  $[sym_ ? testSym, opts_] := DelDupsBy[themedOptions[sym, opts], First];
  $[_, opts_]              := opts;
];

themedOptions[sym_] := CaseOf[
  {}                       := Flatten @ List[activeThemeRules @ sym, defaultRules @ sym];
  opts_ ? ThemedRulesQ     := Flatten @ List[opts, expandThemedRules[sym, opts], defaultRules @ sym];
  opts_List                := Flatten @ List[opts, activeThemeRules @ sym, defaultRules @ sym];
  other_                   := msgThemeRules[sym, other];
];

(**************************************************************************************************)

SetStrict @ LookupThemedOptions;

LookupThemedOptions[sym_, opts_List, keys_List] := Locals[
  $active = {};
  themedOpts = themedOptions[sym, opts];
  List[DelDups @ Flatten @ $active, lookupKeys[sym, themedOpts, keys]]
];

lookupKeys[sym_, opts_, keys_] := Then[
  CheckOptKeys[sym -> $OptionKeysCache[sym], opts];
  Lookup[opts, keys, msgThemeBadOpt[LookupThemedOptions, sym, keys]; $Failed]
];

(**************************************************************************************************)

ThemeRules /: Set[ThemeRules[lhs___], rhs_] := DefineThemes[lhs, rhs];

(**************************************************************************************************)

"SetThemedSymbol[sym$] declares sym$ to be themed.
* LookupSymbolsAs will automatically resolve themes against these symbols.
* It is automatically called when appropriate."

SetStrict[SetThemedSymbol, UnsetThemedSymbol];

SetThemedSymbol[sym_Sym] := Module[
  {opts = Options @ sym},
  If[opts === {},
    Message[SetThemedSymbol::notOptionsSet, sym];
    Return @ $Failed
  ];
  ThemedSymbolQ[sym] = True;
  $OptionRulesCache[sym] = opts;
  $OptionKeysCache[sym] = Keys @ opts;
  $ThemeRules[sym] = Dict[];
  If[!ListQ[$ActiveThemes[sym]], $ActiveThemes[sym] = {}];
];

UnsetThemedSymbol[sym_Sym ? ThemedSymbolQ] := Then[
  unregisterName[sym, $ThemeNames @ sym],
  Unset[{$ThemeNames[sym], $OptionRulesCache[sym], $OptionKeysCache[sym], $ThemeRules[sym]}]
];

SetListable[unregisterName];

unregisterName[sym_, name_] := DeleteFrom[$ThemeUsers[name], sym];

SetThemedSymbol::notOptionsSet = "There appear to be no options yet for ``. Reorder your definitions.";

(**************************************************************************************************)

"DefineThemes[sym$, name$ -> rules$] defines an additional theme for sym$."

SetStrict @ DefineThemes;

DefineThemes[sym_Sym, rules__Rule] := DefineThemes[sym, {rules}];
DefineThemes[sym_Sym, dict_Dict]   := DefineThemes[sym, Normal @ dict];
DefineThemes[sym_Sym, specs_List] := Locals[
  IfFailed[SetThemedSymbol @ sym, ReturnFailed[]];
  $keys = $OptionKeysCache @ sym; $sym = sym;
  defs = Map[checkDefRule, specs];
  names = Keys @ defs;
  Scan[name |-> KeyUnionTo[$ThemeUsers, name, {sym}], names];
  result = BindTo[$ThemeRules[sym], defs];
  $ThemeNames[sym] = Keys @ $ThemeRules[sym];
];

checkDefRule[spec_] := badThemeDef["themeDefSpec", spec];
checkDefRule[spec:Rule[name_, rules_]] := Which[
  !StrQ[name],       badThemeDef["themeDefName", $sym, name],
  !RuleLVecQ[rules], badThemeDef["themeDefRules", name, $sym, rules],
  CheckSubsetOfQ[Keys @ rules, $keys, DefineThemes, "themeDefKeys", name, $sym], spec,
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
  $ActiveThemes = UDict[];
  $CurrentThemes = None;
  $OptionRulesCache = $OptionKeysCache = UDict[];
  $ThemeRules = $ThemeNames = $ThemeUsers = UDict[];
];

(**************************************************************************************************)

If[!HasIValueQ[$ThemeRules], ClearThemes[]];

$CurrentThemes::usage = "$CurrentThemes gets set to the list of themes when calling UnpackSymbolsAs against a themed symbol.";

(**************************************************************************************************)

SetStrict[ThemeRules];

"ThemeRules[sym$] gives the association of theme rules for sym$.
ThemeRules[sym$, 'name$'] gives the rules for theme called 'name$'.
ThemeRules[sym$] = Dict['name$1' -> rules$1, $$] defines the themes for sym$ simultaneously.
ThemeRules[sym$, 'name$] = rules$ defines an additional theme.
* You can also use DefineThemes and ClearThemes."

ThemeRules[sym_Sym]           := lookupRulesDict @ sym;
ThemeRules[sym_Sym, name_Str] := If[ThemedSymbolQ[sym], lookupRules[sym, name], msgThemeSym[sym]; {}];

Protect[ThemeRules];
