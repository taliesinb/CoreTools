SystemExports[
  "MetaFunction", DefineAliasRules, DefineLiteralRules, DefinePatternRules
];

PackageExports[
  "SlotVariable", $, $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $$,
  "MetaFunction", NonCanonAliases
];

(*************************************************************************************************)

Protect[$, $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $$]

SetAttributes[{DefineAliasRules, DefinePatternRules, DefineLiteralRules, NonCanonAliases, defineAlias, definePattern}, HoldAllComplete];

NonCanonAliases[body_] := Block[{$isCanon = False}, body];

DefineAliasRules[rules___Rule]   := iDefineRules[defineAlias,   Hold @ rules];
DefinePatternRules[rules___Rule] := iDefineRules[definePattern, Hold @ rules];
DefineLiteralRules[rules___Rule] := iDefineRules[definePattern, Hold @ rules];

iDefineRules[fn_, held_] := With[
  {syms = Part[held, All, 1]},
  $SymbolAliasesDirty = True;
  UnprotectClear @@ syms;
  Scan[fn, held];
  Protect @@ syms;
  invalidateMacroRules[];
];

$isCanon = True;
defineAlias[aliasSym_Symbol -> targetSym_Symbol] := With[
  {name = HoldSymbolName[aliasSym]},
  $SymbolAliases[aliasSym] = targetSym;
  $NameAliases[name] = targetSym;
  If[$isCanon, HoldSymbolNameAlias[targetSym] = name];
  Set[aliasSym, targetSym];
];

definePattern[patternSym_Symbol -> Evaluate[rhs_]] :=
  With[{rhs2 = rhs}, definePattern[patternSym -> rhs]];

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

