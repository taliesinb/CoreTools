PackageExports[
  "MutatingFunction",
    UnpackOptions, UnpackOptionsAs, UnpackAnnotations,
    PackAssociation, UnpackAssociation,
    UnpackTuple
];

(**************************************************************************************************)

UnpackOptions::usage = "UnpackOptions[sym$1, sym$2, $$] looks up options associated with capitalized versions of sym$i.";

DefineComplexMacro[UnpackOptions, UnpackOptions[syms__Symbol] :> mUnpackOptions[{syms}]]
SetHoldC[mUnpackOptions];

mUnpackOptions[syms_] := With[
  {names = symsToCapStrings @ syms},
  MacroHold[syms = OptionValue[names]]
];

(**************************************************************************************************)

UnpackOptionsAs::usage =
"UnpackOptionsAs[head$, options$, sym$1, sym$2, $$] unpacks option from options$ with matching names to sym$i, \
using head$ for default value."

DefineComplexMacro[UnpackOptionsAs, UnpackOptionsAs[head_Symbol, opts_, syms__Symbol] :> mUnpackOptions[head, opts, {syms}]]
SetHoldC[mUnpackOptionsAs]

mUnpackOptions[head_, opts_, syms_] := With[
  {names = symsToCapStrings @ syms},
  MacroHold[syms = OptionValue[head, {opts}, names]]
];

(**************************************************************************************************)

DefineComplexMacro[UnpackAnnotations, UnpackAnnotations[obj_, syms__Symbol] :> mUnpackAnnos[obj, {syms}]]
SetHoldC[mUnpackOptionsAs]

mUnpackAnnos[obj_, syms_] := With[
  {capSymbols = symsToCapSymbols @ syms},
  MacroHold[syms = VectorReplace[AnnotationValue[obj, capSymbols], $Failed :> None]]
];

(**************************************************************************************************)

PackAssociation::usage = "PackAssociation[sym$1, sym$2, $$] creates an association whose keys are the title-cased names of sym_i and values are their values.";
UnpackAssociation::usage = "UnpackAssociation[assoc$, sym$1, sym$2, $$] takes association whose string keys are capitalized versions of sym$i and sets the corresponding symbols to their values.";

DefineComplexMacro[PackAssociation, PackAssociation[syms__Symbol] :> mPackAssociation[{syms}]]
DefineComplexMacro[UnpackAssociation, UnpackAssociation[assoc_, syms__Symbol] :> mUnpackAssociation[assoc, {syms}]]
SetHoldC[mPackAssociation, packAssocRule, mUnpackAssociation];

mPackAssociation[syms_] := With[
  {rules = HoldMap[packAssocRule, syms]},
  MacroHold[Association[rules]]
];

packAssocRule[s_Symbol] := ToUpperCase1[HoldSymbolName[s]] -> MacroHold[s];

(**************************************************************************************************)

General::badAssociation = "One or more fields in `` were missing from the association.";

mUnpackAssociation[assoc_, syms_] := With[
  {names = symsToCapStrings @ syms},
  MacroHold[syms = Lookup[assoc, names, ThrowMsg["badAssociation", names]]]
];

(**************************************************************************************************)

General::badTuple = "Argument `` should be a single value or a list of `` values."

DefineComplexMacro[UnpackTuple, UnpackTuple[val_, syms__Symbol] :> mUnpackTuple[val, syms]]
SetHoldC[mUnpackTuple];

mUnpackTuple[val_, s1_Symbol, s2_Symbol] :=
  MacroHold @ If[ListQ[val],
    If[Length[val] != 2, ThrowMsg["badTuple", val, 2]]; {s1, s2} = val,
    s1 = s2 = val
  ];

mUnpackTuple[val_, s1_Symbol, s2_Symbol, s3_Symbol] :=
  MacroHold @ If[ListQ[val],
    If[Length[val] != 3, ThrowMsg["badTuple", val, 3]]; {s1, s2, s3} = val,
    s1 = s2 = s3 = val
  ];

mUnpackTuple[val_, s1_Symbol, s2_Symbol, s3_Symbol, s4_Symbol] :=
  MacroHold @ If[ListQ[val],
    If[Length[val] != 4, ThrowMsg["badTuple", val, 4]]; {s1, s2, s3, s4} = val,
    s1 = s2 = s3 = s4 = val
  ];

mUnpackTuple[val_, s1_Symbol, s2_Symbol, s3_Symbol, s4_Symbol, s5_Symbol] :=
  MacroHold @ If[ListQ[val],
    If[Length[val] != 5, ThrowMsg["badTuple", val, 5]]; {s1, s2, s3, s4, s5} = val,
    s1 = s2 = s3 = s4 = s5 = val
  ];

(**************************************************************************************************)

SetHoldC[symsToCapStrings, symsToCapSymbols]

symsToCapStrings[syms_] := Map[
  Function[sym, toOptionNameStr @ HoldSymbolName @ sym, HoldAllComplete],
  Unevaluated @ syms
];

symsToCapSymbols[syms_] := Map[
  Function[sym, toCapSymbol @ HoldSymbolName @ sym, HoldAllComplete],
  Unevaluated @ syms
];

General::noCorrespondingSymbol = "No symbol found corresponding to ``.";
toCapSymbol[str_String] := toCapSymbol[str] = Module[
  {str2 = toOptionNameStr[str]},
  Which[
    NameQ[str2], Symbol[str2],
    NameQ[str2 //= StringReplace["Fn" -> "Function"]], Symbol[str2],
    True, ThrowMsg["noCorrespondingSymbol", str]
  ]
];

toOptionNameStr[str_String] := toOptionNameStr[str] =
  makeOptionNameStr @ StrTrimL[str, "$"];

makeOptionNameStr[str_] := Which[
  StrStartsQ[str, "json"], "JSON" <> StringDrop[str, 4],
  True,                    ToUpperCase1 @ str
];


