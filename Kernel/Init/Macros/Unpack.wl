PackageExports[
  "MutatingFunction",
    UnpackOptions, UnpackOptionsAs,
    UnpackSymbols, UnpackSymbolsAs,
      BindSymbols,   BindSymbolsAs,
    UnpackAnnotations,
    UnpackDict, PackDict,
    UnpackTuple,
  "PatternSymbol",
    SymRSeqP
];

(**************************************************************************************************)

DefinePatternRules[
  SymRSeqP -> Repeated[_Sym | Rule[_Sym, _]]
];

(**************************************************************************************************)

UnpackOptions::usage =
"UnpackOptions[sym$1, sym$2, $$] looks up options associated with capitalized versions of sym$i.";

UnpackOptionsAs::usage =
"UnpackOptionsAs[head$, options$, sym$1, sym$2, $$] unpacks option from options$ with matching names to sym$i,
using head$ for default value."

UnpackDict::usage =
"UnpackDict[assoc$, sym$1, sym$2, $$] takes association whose string keys are capitalized versions
of sym$i and sets the corresponding symbols to their values.";

UnpackSymbols::usage =
"UnpackSymbols[rules$, sym$1, sym$2, $$] unpacks symbols from a list of rules or Dict.
The keys sought will be uppercased versions of sym$i on $ContextPath."

UnpackSymbolsAs::usage =
"UnpackSymbolsAs[head$, rules$, sym$1, sym$2, $$] unpacks symbols from a list of rules or Dict.
* The keys sought will be uppercased versions of sym$i on $ContextPath.
* Options[head$] is used as a fallback.
* If head$ has registered themes these will be resolved via ThemedLookupOptions.
"

BindSymbols::usage =
"BindSymbols[rules$, sym$1, sym$2, $$] unpacks the given symbols from a list of rules.
* All of these symbols are localized using InheritVar, so that if they are not specified in rules,
* they will retain their previous values.
* The keys sought will be uppercased versions of sym$i on $ContextPath."

BindSymbolsAs::usage =
"BindSymbolsAs[head$, rules$, sym$1, sym$2, $$] is like BindSymbols but issues messages from head$ \
for unknown keys."

(**************************************************************************************************)

DeclaredHere[UnpackOptions, UnpackOptionsAs, UnpackDict, UnpackSymbols, UnpackSymbolsAs, BindSymbols, BindSymbolsAs, UnpackAnnotations];

ComplexMacroDefs[
  UnpackOptions[syms__Sym]                         := unpkOpts[None,  symsToVarsKeys @ List @ syms],
  UnpackOptions[spec:SymRSeqP]                     := unpkOpts[None,  specToVarsKeys @ List @ spec],
  UnpackOptionsAs[head_Sym, rules_, syms__Sym]     := unpkOpts[rules, symsToVarsKeys @ List @ syms, head],
  UnpackOptionsAs[head_Sym, rules_, spec:SymRSeqP] := unpkOpts[rules, specToVarsKeys @ List @ spec, head],
  UnpackDict[assoc_, syms__Symbol]                 := unpkDict[assoc, symsToVarsKeys @ List @ syms],
  UnpackDict[assoc_, spec:SymRSeqP]                := unpkDict[assoc, specToVarsKeys @ List @ spec],
  UnpackSymbols[rules_, syms__Sym]                 := unpkSyms[rules, symsToVarsOpts @ List @ syms],
  UnpackSymbols[rules_, spec:SymRSeqP]             := unpkSyms[rules, specToVarsOpts @ List @ spec],
  UnpackSymbolsAs[head_Sym, rules_, syms__Sym]     := unpkSyms[rules, symsToVarsOpts @ List @ syms, head],
  UnpackSymbolsAs[head_Sym, rules_, spec:SymRSeqP] := unpkSyms[rules, specToVarsOpts @ List @ spec, head],
  BindSymbols[rules_, syms__Sym]                   := bindSyms[rules, symsToVarsOpts @ List @ syms],
  BindSymbols[rules_, spec:SymRSeqP]               := bindSyms[rules, specToVarsOpts @ List @ spec],
  BindSymbolsAs[head_Sym, rules_, syms__Sym]       := bindSyms[rules, symsToVarsOpts @ List @ syms, head],
  BindSymbolsAs[head_Sym, rules_, spec:SymRSeqP]   := bindSyms[rules, specToVarsOpts @ List @ spec, head],
  UnpackAnnotations[obj_, syms__Sym]               := unpkAnno[obj,   symsToVarsOpts @ List @ syms],
  UnpackAnnotations[obj_, spec:SymRSeqP]           := unpkAnno[obj,   symsToVarsOpts @ List @ spec]
];

(**************************************************************************************************)

SetHoldF[unpkOpts, unpkSyms, unpkDict, unpkAnnos, bindSyms];

unpkOpts[None,   HoldM[vars_] <-> keys_]        := HoldM @ Set[vars, OptionValue[keys]]
unpkOpts[rules_, HoldM[vars_] <-> keys_, head_] := HoldM @ Set[vars, OptionValue[head, {rules}, keys]]
(* unpkOpts[rules_, HoldM[vars_] <-> syms_, head_] := HoldM @ Set[vars, LookupOptionsAs[ToList @ rules, syms, head]]; *)

unpkDict[assoc_, HoldM[vars_] <-> keys_]        := HoldM @ Set[vars, Lookup[assoc, keys, ThrowMsg["badAssociation", keys]]]

unpkSyms[rules_, HoldM[vars_] <-> syms_]        := HoldM @ Set[vars, Lookup[rules, syms, None]];
unpkSyms[rules_, HoldM[vars_] <-> syms_, head_] := HoldM @ Set[vars, LookupOptionsAs[ToList @ rules, syms, head]];

bindSyms[rules_, HoldM[vars_] <-> syms_]        := With[{vsd = toVSD[vars, syms]}, HoldM[BindFrom[InheritVar[vars], vsd, rules]]];
bindSyms[rules_, HoldM[vars_] <-> syms_, head_] := With[{vsd = toVSD[vars, syms]}, HoldM[BindFrom[InheritVar[vars], vsd, rules, ErrorOptKeyFn @ head]]];

unpkAnno[obj_,   HoldM[vars_] <-> syms_]        := HoldM @ Set[vars, VectorReplace[AnnotationValue[obj, syms], $Failed :> None]]

SetHoldC @ toVSD;
toVSD[vars_, syms_] := MapThread[RuleD, NoEval @ {syms, vars}];

(**************************************************************************************************)

unpkSyms[rules_, HoldM[vars_] <-> syms_, head_ ? ThemedSymbolQ] :=
  HoldM @ Then[
    InheritVar[$LocalThemes, $LocalThemedOptions];
    {$LocalThemes[head], $LocalThemedOptions[head], vars} = LookupThemedOptionsAndInfo[head, ToList @ rules, syms];
  ];

(**************************************************************************************************)

General::badAssociation = "One or more fields in `` were missing from the association.";

(**************************************************************************************************)

PackDict::usage = "PackDict[sym$1, sym$2, $$] creates an association whose keys are the title-cased names of sym_i and values are their values.";

ComplexMacroDefs[
  PackDict[syms__Symbol] := mPackDict[List @ syms]
];

SetHoldC[mPackDict, packAssocRule];

mPackDict[syms_] := With[
  {rules = HoldMap[packAssocRule, syms]},
  HoldM[Association[rules]]
];

packAssocRule[s_Symbol] := ToUpperCase1[SymName @ s] -> HoldM[s];

(**************************************************************************************************)

General::badTuple = "Argument `` should be a single value or a list of `` values."

ComplexMacroDefs[
  UnpackTuple[val_, syms__Symbol] := mUnpackTuple[val, syms]
];

SetHoldC[mUnpackTuple];

mUnpackTuple[val_, s1_Symbol, s2_Symbol] :=
  HoldM @ If[ListQ[val],
    If[Length[val] != 2, ThrowMsg["badTuple", val, 2]]; {s1, s2} = val,
    s1 = s2 = val
  ];

mUnpackTuple[val_, s1_Symbol, s2_Symbol, s3_Symbol] :=
  HoldM @ If[ListQ[val],
    If[Length[val] != 3, ThrowMsg["badTuple", val, 3]]; {s1, s2, s3} = val,
    s1 = s2 = s3 = val
  ];

mUnpackTuple[val_, s1_Symbol, s2_Symbol, s3_Symbol, s4_Symbol] :=
  HoldM @ If[ListQ[val],
    If[Length[val] != 4, ThrowMsg["badTuple", val, 4]]; {s1, s2, s3, s4} = val,
    s1 = s2 = s3 = s4 = val
  ];

mUnpackTuple[val_, s1_Symbol, s2_Symbol, s3_Symbol, s4_Symbol, s5_Symbol] :=
  HoldM @ If[ListQ[val],
    If[Length[val] != 5, ThrowMsg["badTuple", val, 5]]; {s1, s2, s3, s4, s5} = val,
    s1 = s2 = s3 = s4 = s5 = val
  ];

(**************************************************************************************************)

SetHoldC[symsToVarsKeys, symsToVarsOpts, specToVarsKeys, specToVarsOpts, symsToKeys, symsToOpts]

symsToVarsKeys[syms:{__Sym}] := HoldM[syms] <-> symsToKeys[syms];
symsToVarsOpts[syms:{__Sym}] := HoldM[syms] <-> symsToOpts[syms];

specToVarsKeys[specs_List]   := HoldMMap[specToVar, specs] <-> Map[specToKey, NoEval @ specs];
specToVarsOpts[specs_List]   := HoldMMap[specToVar, specs] <-> Map[specToOpt, NoEval @ specs];

symsToKeys[syms_List] := Map[symToKey, NoEval @ syms];
symsToOpts[syms_List] := Map[symToOpt, NoEval @ syms];

(**************************************************************************************************)

SetHoldF[specToVar, specToKey, specToOpt, symToKey, symToOpt, symToOpt2]

specToVar[(sym_Sym -> _) | sym_Sym] := HoldM @ sym;

specToKey[(_ -> key_Str)] := key;
specToKey[(_ -> key_Sym)] := key;
specToKey[sym_Sym]        := symToKey @ sym;

specToOpt[(_ -> sym_Sym) | sym_Sym] := symToOpt @ sym;

symToKey[sym_Sym] := nameToKey0 @ SymName @ sym;
symToOpt[sym_Sym] := symToOpt2[sym, SymName @ sym];

symToOpt2[sym_Sym, name_ ? UpperCase1Q] := sym;
symToOpt2[sym_Sym, name_] := nameToOpt0 @ name;

(**************************************************************************************************)

nameToKey0[str_Str] := nameToKey0[str] = nameToKey2 @ nameToKey1 @ str;
nameToKey1[str_Str] := nameToKey1[str] = ToUpperCase1 @ StrTrimL[str, "$"];
nameToKey2[str_Str] := Which[
  StrStartsQ[str, "Json"], "JSON" <> StringDrop[str, 4],
  StrEndsQ[str, "Fn"],     StringDrop[str, -2] <> "Function",
  True,                    str
];

(**************************************************************************************************)

nameToOpt0[str_Str] := nameToOpt0[str] = nameToOpt1 @ nameToKey1 @ str;
nameToOpt1[str_Str] := If[NameQ[str], Symbol @ str, Lookup[$NameAliases, str, nameToOpt2 @ StrRep[str, "Fn" -> "Function"]]];
nameToOpt2[str_Str] := If[NameQ[str], Symbol @ str, MacroError["noCorrespondingSymbol", str]];

General::noCorrespondingSymbol = "No symbol found corresponding to ``.";

