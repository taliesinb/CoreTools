BeginPackage["Prelude`"]

Begin["`Overrides`Private`"]

(*************************************************************************************************)

List[
  UnprotectClearAll[
    System`Comap, System`PrintableASCIIQ, System`AssociationMap, System`StringPadLeft, System`StringPadRight,
    System`Into, System`DefaultValue,
    FrontEnd`Private`PrimeControlEqualPump
  ];
  Protect[FrontEnd`Private`PrimeControlEqualPump];
  If[OwnValues[Failure] =!= {}, Unprotect[Failure]; ClearAll[Failure]];
];

(* explanation of the above:

DefaultValue: useful symbol for options, but autoloads templates.

PrintableASCIIQ, AssociationMap: we want to avoid loading GU, and these are simple functions, so
we re-implement them ourselves.

PrimeControlEqualPump: the FE uses this symbol to pre-load WolframAlpha (which goes on to load *many*
other packages!). this slows down every single kernel spin-up considerably, and it can happen in the middle
of another evaluation, causing chaos and misery. unfortunately the definitions are only attached after startup at
some unknown time, and then later loaded by a scheduled task, so we prevent the definitions getting attached.
*)

(*************************************************************************************************)

Comap[fns_, arg_]         := Map[fn |-> fn[arg], fns];
Comap[fns_, arg_, level_] := Map[fn |-> fn[arg], fns, level];
Comap[fns_][arg_]         := Comap[fns, arg];

(*************************************************************************************************)

StringPadLeft [list_List] :=  StringPadLeft[list, Automatic];
StringPadRight[list_List] := StringPadRight[list, Automatic];

StringPadLeft [s_, n_, p_String:Null] := RuleCondition @ strPad[s, n, toPadCodes @ p, PadLeft];
StringPadRight[s_, n_, p_String:Null] := RuleCondition @ strPad[s, n, toPadCodes @ p, PadRight];

 s_StringPadLeft := RuleCondition[Message[StringPadLeft::strse, 1, HoldForm[s]]; Fail];
s_StringPadRight := RuleCondition[Message[StringPadRight::strse, 1, HoldForm[s]]; Fail];

toPadCodes[Null] = 32;
toPadCodes[p_String] := ToCharacterCode @ p;

strPad[{}, _, _, _] := {};
strPad[s_String, n_Integer, p_, fn_] := FromCharacterCode @ fn[ToCharacterCode @ s, n, p];
strPad[strs_List ? Developer`StringVectorQ, n_, p_, fn_] := strPadList[strs, n, p, fn];
_strPad := Fail;

strPadList[strs_, Automatic, p_, fn_] := strPadList[strs, Max @ StringLength @ strs, p, fn];
strPadList[strs_, n_Integer, p_, fn_] := FromCharacterCode @ Map[fn[#, n, p]&, ToCharacterCode @ strs];
_strPadList := Fail;

(*************************************************************************************************)

AssociationMap[f_][expr_] := AssociationMap[f, expr];
AssociationMap[fn_, expr_List] := Association @ Map[z |-> Rule[z, fn[z]], expr];
AssociationMap[fn_, assoc_Association ? AssociationQ] := Association @ Map[fn, Normal @ assoc];
AssociationMap[_, expr_] := RuleCondition[Message[AssociationMap::invrp, expr]; Fail];

(*************************************************************************************************)

Attributes[PrintableASCIIQ] = {Listable};

PrintableASCIIQ[""] := False;
PrintableASCIIQ[str_String] := Block[{min, max},
  {min, max} = MinMax @ ToCharacterCode @ str;
  min >= 9 && max <= 127
];

PrintableASCIIQ[_] := False;

(*************************************************************************************************)

Protect[Comap, StringPadLeft, StringPadRight, AssociationMap, PrintableASCIIQ];

(*************************************************************************************************)

End[]

EndPackage[]
