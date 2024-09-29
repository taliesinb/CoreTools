PackageExports[
  "MetaFunction", DefineAliasRules, DefinePatternRules
  "IOFunction",   LoadAliasTables
];

(*************************************************************************************************)

Protect[$, $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $$, $LHS, $RHS]

SetAttributes[{DefineAliasRules, DefinePatternRules, defineAlias, definePattern}, HoldAllComplete];

DefineAliasRules[rules___Rule]   := iDefineRules[defineAlias,   Hold @ rules];
DefinePatternRules[rules___Rule] := iDefineRules[definePattern, Hold @ rules];

iDefineRules[fn_, held_] := With[
  {syms = Part[held, All, 1]},
  $SymbolAliasesDirty = True;
  UnprotectClearAll @@ syms;
  Scan[fn, held];
  Protect @@ syms;
  invalidateMacroRules[];
];

defineAlias[aliasSym_Symbol -> targetSym_Symbol] := (
  $SymbolAliases[aliasSym] = targetSym;
  Set[aliasSym, targetSym];
);

definePattern[patternSym_Symbol -> rhs_] := (
  $SymbolAliases[patternSym] = rhs;
  Set[patternSym, rhs];
);

DefineAliasRules::notValidRule = "Expected rule mapping symbol to symbol: ``.";
DefinePatternRules::notValidRule = "Expected rule mapping symbol to pattern: ``.";

defineAlias[e_]      := (Message[DefineAliasRules::notValidRule,   HoldForm @ e]; $Failed)
definePattern[e_]    := (Message[DefinePatternRules::notValidRule, HoldForm @ e]; $Failed)
e_DefineAliasRules   := (Message[DefineAliasRules::notValidRule,   HoldForm @ e]; $Failed)
e_DefinePatternRules := (Message[DefinePatternRules::notValidRule, HoldForm @ e]; $Failed)

(*************************************************************************************************)

aliasPath[name_] := PathJoin[{$CoreToolsRootPath, "Kernel", "Aliases", name}];

loadAliasFile[path_, context_] := Block[{$Context = context, $ContextPath = {"System`", context}},

];

LoadAliasTables[] := Module[{internal, package, system},
  {internal, package, system} = Map[aliasPath, {"InternalAliases.wl", "PackageAliases.wl", "SystemAliases.wl"}];

];