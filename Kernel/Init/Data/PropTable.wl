PackageExports[
  "DataHead", PropTable,
  "Pred",     PropTableExistsQ, PropTableHasExprQ, PropTableHasPropQ,
  "PatSym",   StoreP,
  "Fn",       PropTableGetProp, PropTableGetExpr, PropTableGetFull,
              PropTableExprDict, PropTableFullDict,
              PropTableLength,
  "MutFn",    PropTableSetProp, PropTableDropExpr, PropTableDropProp, PropTableDropFull
];

(*************************************************************************************************)

DefineAliasRules[
  PropTable     -> Language`ExpressionStore,
  PropTableNew  -> Language`NewExpressionStore
];

(*************************************************************************************************)

PropTableExistsQ[p:PropTable[_Str]] := Internal`UnsafeQuietCheck[p["remove"[$dummy, $dummy]]; True, False];
_PropTableExistsQ                   := False;

(*************************************************************************************************)

PropTableHasExprQ[p_PropTable, expr_]        := p["containsQ"[expr]];
PropTableHasPropQ[p_PropTable, expr_, prop_] := p["containsQ"[expr, prop]];

(*************************************************************************************************)

PropTableGetProp[p_PropTable, expr_, prop_]  := p["get"[expr, prop]];
PropTableGetExpr[p_PropTable, expr_, prop_]  := p["get"[expr]];
PropTableGetFull[p_PropTable]                := p["listTable"[]];

(*************************************************************************************************)

PropTableFullDict[p_PropTable]                := PairsToDict @ Map[PairsToDict, p["listTable"[]]];
PropTableExprDict[p_PropTable, expr_]         := PairsToDict @ p["get"[expr]];
PropTableLength[p_PropTable]                  := Len @ p["listTable"[]];

(*************************************************************************************************)

PropTableSetProp[p_PropTable, expr_, prop_, value_] := p["put"[expr, prop, value]];

(*************************************************************************************************)

PropTableDropExpr[p_PropTable, expr_]         := p["remove"[expr]];
PropTableDropProp[p_PropTable, expr_, prop_]  := p["remove"[expr, prop]];
PropTableDropFull[p_PropTable]                := Scan[p["remove"[First @ #]]&, p["listTable"[]]];

