BeginPackage["Prelude`Packages`"]

System`PackageExports[
  "DataHead",
    System`UAssociation,
  "Function",
    System`HoldLength, System`HoldSequenceLength, System`HoldByteCount,
    System`HoldSymbolName, System`HoldSymbolNameAlias, System`HoldSymbolContext, System`HoldSymbolPath,
    System`NaturalNumberString, System`FullIntegerString,
  "Predicate",
    System`HoldListQ, System`HoldAssociationQ, System`HoldPackedArrayQ, System`PackedListQ,
    System`HoldStringQ, System`HoldIntegerQ, System`HoldNaturalQ, System`HoldNumberQ, System`HoldBooleanQ,
  "Variable",
    System`$FormalSymbols
];

Begin["`Private`"]

(*************************************************************************************************)

UAssociation = Data`UnorderedAssociation;
PackedListQ  = Developer`PackedArrayQ;

(* PackedListQ isn't called PackedArrayQ to avoid shadowing problems if someone calls Needs["Developer`"] *)

(*************************************************************************************************)

SetAttributes[{HoldLength, HoldSequenceLength, HoldByteCount}, HoldAllComplete];

HoldLength[e_]       := Length @ Unevaluated @ e;
e_HoldSequenceLength := Length @ Unevaluated @ e;
e_SequenceLength     := Length @ Unevaluated @ e;
HoldByteCount[e_]    := ByteCount @ Unevaluated @ e;


(*************************************************************************************************)

SetAttributes[{HoldStringQ, HoldIntegerQ, HoldNaturalQ, HoldNumberQ, HoldBooleanQ}, HoldAllComplete];

HoldStringQ[_String ? Developer`HoldAtomQ] = True;
HoldIntegerQ[_Integer ? Developer`HoldAtomQ] = True;
HoldNaturalQ[(_Integer ? Developer`HoldAtomQ) ? NonNegative] = True;
HoldNumberQ[Alternatives[_Integer, _Real, _Rational] ? Developer`HoldAtomQ] = True;
HoldBooleanQ[False | True] = True;

_HoldStringQ  = False;
_HoldIntegerQ = False;
_HoldNaturalQ = False;
_HoldNumberQ  = False;
_HoldBooleanQ = False;

(*************************************************************************************************)

SetAttributes[{HoldListQ, HoldAssociationQ, HoldPackedArrayQ}, HoldAllComplete];

HoldListQ[_List] = True;

HoldAssociationQ[_Association ? Developer`HoldAtomQ] = True;

HoldPackedArrayQ[a_List] := Developer`PackedArrayQ @ Unevaluated @ a;

_HoldListQ        = False;
_HoldAssociationQ = False;
_HoldPackedArrayQ = False;

(*************************************************************************************************)

SetAttributes[{HoldSymbolNameAlias, HoldSymbolName, HoldSymbolContext, HoldSymbolPath}, HoldAllComplete];

HoldSymbolName::usage = "HoldSymbolName[sym$] gives the name of sym$ without evaluating sym$.";
HoldSymbolContext::usage = "HoldSymbolContext[sym$] gives the full context of sym$ without evaluating sym$.";
HoldSymbolPath::usage = "HoldSymbolPath[sym$] gives the context and name of sym$ without evaluating sym$.";

HoldSymbolName[sym_Symbol ? Developer`HoldAtomQ] := SymbolName @ Unevaluated @ sym;
HoldSymbolName[_] := $Failed;

HoldSymbolNameAlias[sym_Symbol ? Developer`HoldAtomQ] := SymbolName @ Unevaluated @ sym;
HoldSymbolNameAlias[_] := $Failed;

(* TODO: why isn't this just Context? *)
HoldSymbolContext[sym_Symbol ? Developer`HoldAtomQ] := Internal`SymbolContext @ Unevaluated @ sym;
HoldSymbolContext[_] := $Failed;

HoldSymbolPath[sym_Symbol] := StringJoin[HoldSymbolContext @ sym, HoldSymbolName @ sym];
HoldSymbolPath[list_List] := Map[HoldSymbolPath, Unevaluated @ list];
HoldSymbolPath[_] := $Failed;

(*************************************************************************************************)

$FormalSymbols = {
  \[FormalA], \[FormalB], \[FormalC], \[FormalD], \[FormalE],
  \[FormalF], \[FormalG], \[FormalH], \[FormalI], \[FormalJ],
  \[FormalK], \[FormalL], \[FormalM], \[FormalN], \[FormalO],
  \[FormalP], \[FormalQ], \[FormalR], \[FormalS], \[FormalT],
  \[FormalU], \[FormalV], \[FormalW], \[FormalX], \[FormalY],
  \[FormalZ],
  \[FormalCapitalA], \[FormalCapitalB], \[FormalCapitalC],
  \[FormalCapitalD], \[FormalCapitalE], \[FormalCapitalF],
  \[FormalCapitalG], \[FormalCapitalH], \[FormalCapitalI],
  \[FormalCapitalJ], \[FormalCapitalK], \[FormalCapitalL],
  \[FormalCapitalM], \[FormalCapitalN], \[FormalCapitalO],
  \[FormalCapitalP], \[FormalCapitalQ], \[FormalCapitalR],
  \[FormalCapitalS], \[FormalCapitalT], \[FormalCapitalU],
  \[FormalCapitalV], \[FormalCapitalW], \[FormalCapitalX],
  \[FormalCapitalY], \[FormalCapitalZ]
};

(*************************************************************************************************)

(* IntegerString is badly named! *)

NaturalNumberString[n_Integer ? Positive] := IntegerString @ n;
NaturalNumberString[ns_ ? listableNatsQ]  := IntegerString @ items;
NaturalNumberString[expr_]                := (Message[NaturalNumberString::badArg1, expr]; $Failed);

FullIntegerString[n_Integer]                := fullIntStr @ n;
FullIntegerString[ns_List | ns_Association] := If[listableNatsQ @ ns, IntegerString @ ns, fullIntStr @ ns];
FullIntegerString[expr_]                    := (Message[FullIntegerString::badArg1, expr]; $Failed);

listableNatsQ[e_List ? Developer`PackedArrayQ] := TrueQ @ NonNegative @ Min @ e;
listableNatsQ[e:(_List | _Association)]        := TrueQ @ NonNegative @ Min @ e;

(*************************************************************************************************)

SetAttributes[fullIntStr, Listable];

fullIntStr[n_Integer ? Negative] := StringJoin["-", IntegerString @ n];
fullIntStr[n_Integer]            := IntegerString @ n;
fullIntStr[expr_]                := (Message[FullIntegerString::badArg1, expr]; $Failed);

e_NaturalNumberString := Message[NaturalNumberString::badArgCount, HoldForm @ e];
e_FullIntegerString   := Message[FullIntegerString::badArgCount, HoldForm @ e];

NaturalNumberString::badArg1 = "Not a natural number or container of these: ``."
FullIntegerString::badArg1 = "Not an integer or container of these: ``."
NaturalNumberString::badArgCount = FullIntegerString::badArgCount = "Exactly one argument expected: ``."

(*************************************************************************************************)

End[]

EndPackage[]