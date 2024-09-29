SystemExports[
  "DataHead",
    UnorderedSet, OrderedSet, Multiset,
  "Function",
    ToUnorderedSet, ToOrderedSet, ToMultiset,
    SetElements,
    SetSupport,
    SetLength,
    SetMap,
    SetSelect,
    SetJoin, SetUnion, SetIntersection, SetComplement,
    SetLookup,
    LookupSet, SetKeyTake, SetKeyDrop,
    SetFilter,
    SetAdd,
  "MutatingFunction",
    SetUnionTo, SetAddTo,
  "Predicate",
    SetEmptyQ, SetNonEmptyQ,
    SetHasQ, SameSetElemsQ, SetSubsetOfQ, SetSupersetOfQ, SetIntersectsQ, SetNotIntersectsQ
];

PackageExports[
  "DataHead",
    USet, OSet, MSet,
  "Function",
    ToUSet, ToOSet, ToMSet,
    SetElems, SetLen,
    SetInter, SetCompl,
  "BoxFunction",     SetBoxes,
  "Symbol",          EmptyUSet, EmptyOSet, EmptyMSet,
  "PatternSymbol",   USetP, OSetP, MSetP, ASetP, SetSymP, ESetP,
  "PatternHead",     USetD, OSetD, MSetD, ASetD,
  "SpecialFunction", MakeOSet, MakeUSet, MakeMSet
];

PrivateExports[
  "MetaFunction",
    DefineSetDispatch1,
    DefineSetDispatch2,
    DefineSetDispatch12,
    DefineSetDispatch1N,
    DefineSetNaryImpl
];

(*************************************************************************************************)

DefineAliasRules[
  ToUSet   -> ToUnorderedSet,
  ToOSet   -> ToOrderedSet,
  SetElems -> SetElements,
  SetLen   -> SetLength,
  SetInter -> SetIntersection,
  SetCompl -> SetComplement
];

DefinePatternRules[
  SetSymP -> Alt[OSet, USet, MSet],
  USetP   -> _USet ? SealedQ,
  OSetP   -> _OSet ? SealedQ,
  MSetP   -> _MSet ? SealedQ,
  ASetP   -> (_OSet | _USet | _MSet) ? SealedQ,
  ESetP   -> _Sym[_ ? UnsafeEmptyQ] ? SealedQ  (* permissive, but fast *)
];

AllowRHSPatterns[
  PatternMacroDefs[USetD, VPattern[sym_Symbol, USetD] := HoldP[USet[_sym] ? SealedQ]];
  PatternMacroDefs[OSetD, VPattern[sym_Symbol, OSetD] := HoldP[OSet[_sym] ? SealedQ]];
  PatternMacroDefs[MSetD, VPattern[sym_Symbol, MSetD] := HoldP[MSet[_sym] ? SealedQ]];
  PatternMacroDefs[ASetD, VPattern[sym_Symbol, ASetD] := HoldP[(USet|OSet|MSet)[_sym] ? SealedQ]];
];

(*************************************************************************************************)

SetStrict[MakeUSet, MakeOSet, MakeMSet];

MakeUSet[a_Dict] := MakeSealed[USet, a];
MakeOSet[a_Dict] := MakeSealed[OSet, a];
MakeMSet[a_Dict] := MakeSealed[MSet, a];

EmptyUSet = MakeSealed[USet, EmptyUDict];
EmptyOSet = MakeSealed[OSet, EmptyODict];
EmptyMSet = MakeSealed[MSet, EmptyUDict];

(*************************************************************************************************)

SetAttributes[SetBoxes, HoldAllComplete];

MakeBoxes[h_USet ? SealedQ, StandardForm]    := SetBoxes[h];
MakeBoxes[h_USet ? SealedQ, TraditionalForm] := SetBoxes[h];

MakeBoxes[h_OSet ? SealedQ, StandardForm]    := SetBoxes[h];
MakeBoxes[h_OSet ? SealedQ, TraditionalForm] := SetBoxes[h];

MakeBoxes[h_MSet ? SealedQ, StandardForm]    := SetBoxes[h];
MakeBoxes[h_MSet ? SealedQ, TraditionalForm] := SetBoxes[h];

SetBoxes[USet[a_Dict]] := AngleRowBox @ MapApply[MakeBoxes, Keys[a, HoldComplete]];
SetBoxes[OSet[a_Dict]] := AngleRowBox @ MapApply[MakeBoxes, Keys[a, HoldComplete]];
SetBoxes[MSet[a_Dict]] := AngleRowBox @ KeyValueMap[msetBoxes, a];

msetBoxes[k_, 1]  := MakeBoxes @ k;
msetBoxes[k_, n_] := SubBox[MakeBoxes @ k, MakeBoxes @ n];

(*************************************************************************************************)

SetHoldC[sealUSet, sealOSet, sealMSet];

sealUSet[USet[]]      := EmptyUSet;
sealUSet[USet[e_]]    := MakeUSet @ UDict[e -> True];
sealUSet[USet[es__]]  := MakeUSet @ UDict @ TrueRules @ List @ es;

sealOSet[OSet[]]      := EmptyOSet;
sealOSet[OSet[e_]]    := MakeMSet @ ODict[e -> True];
sealOSet[OSet[es__]]  := MakeOSet @ ODict @ TrueRules @ List @ es;

sealMSet[MSet[]]      := EmptyMSet;
sealMSet[MSet[e_]]    := MakeMSet @ UDict[e -> 1];
sealMSet[MSet[es__]]  := MakeMSet @ UDict @ Counts @ List @ es;

(*************************************************************************************************)

(* these are like ToList: they merges any sets/lists at the top level *)

ToUSet[]         := EmptyUSet;
ToUSet[u_USet]   := u;
ToUSet[OSet[a_]] := MakeUSet @ UDict @ a;
ToUSet[MSet[a_]] := MakeUSet @ Map[Positive, a];
ToUSet[a_]       := MakeUSet @ ToUSetDict[a, True];
ToUSet[a__]      := MakeUSet @ ToUSetDict[ArgList[a], True];

(*************************************************************************************************)

ToOSet[]         := EmptyOSet;
ToOSet[o_OSet]   := o;
ToOSet[a_]       := MakeOSet @ ToOSetDict[a, True];
ToOSet[a__]      := MakeOSet @ ToOSetDict[ArgList[a], True];

(*************************************************************************************************)

ToMSet[]         := EmptyMSet;
ToMSet[m_MSet]   := m;
ToMSet[USet[a_]] := MakeMSet @ Map[Boole, a];
ToMSet[OSet[a_]] := MakeMSet @ Map[Boole, a];
ToMSet[a_]       := MakeMSet @ ToMSetDict[a, True];
ToMSet[a__]      := MakeMSet @ ToMSetDict[ArgList[a], True];

(*************************************************************************************************)

(* TODO: lift all these out shared patterns out, since they also
happen with dispatch versions *)

DefineSetDispatch1[head_, impl_] := Then[
  SetStrict @ impl,
  USet /: head[u_USet] := impl[u],
  OSet /: head[o_OSet] := impl[o],
  MSet /: head[m_MSet] := impl[m]
];

DefineSetDispatch12[head_, impl_] := Then[
  SetStrict @ impl,
  USet /: head[u_USet, a_] := impl[u, a],
  OSet /: head[o_OSet, a_] := impl[o, a],
  MSet /: head[m_MSet, a_] := impl[m, a]
];

DefineSetDispatch1N[head_, impl_] := Then[
  SetStrict @ impl,
  USet /: head[u_USet, a___] := impl[u, a],
  OSet /: head[o_OSet, a___] := impl[o, a],
  MSet /: head[m_MSet, a___] := impl[m, a]
];

DefineSetDispatch2[head_, impl_] := Then[
  SetStrict @ impl,
  USet /: head[a_, u_USet] := impl[a, u],
  OSet /: head[a_, o_OSet] := impl[a, o],
  MSet /: head[a_, m_MSet] := impl[a, m]
];

DefineSetNaryImpl[sym_, usym_, osym_, msym_] := Then[
  sym[]               := EmptyOSet;
  sym[u_USet]         := u;
  sym[o_OSet]         := o;
  sym[m_MSet]         := m;
  sym[u_USet, args__] := usym[u, args];
  sym[o_OSet, args__] := osym[o, args];
  sym[m_MSet, args__] := msym[m, args];
];

(*************************************************************************************************)

DefineSetDispatch1[EmptyQ, SetEmptyQ];
DefineSetDispatch1[NonEmptyQ, SetNonEmptyQ];

SetEmptyQ[SetSymP[a_]] := EmptyQ @ a;
SetNonEmptyQ[SetSymP[a_]] := NonEmptyQ @ a;

(*************************************************************************************************)

DefineSetDispatch1[Len, SetLen];

SetLen[USet[a_]] := Len @ a;
SetLen[OSet[a_]] := Len @ a;
SetLen[MSet[a_]] := Total @ a;

(*************************************************************************************************)

DefineSetDispatch1[Normal, SetElems];

SetElems[USet[a_]] := Keys @ a;
SetElems[OSet[a_]] := Keys @ a;
SetElems[MSet[a_]] := KeyValueMap[repMElem, a];

SetElems[USet[a_], h_] := Keys[a, h];
SetElems[OSet[a_], h_] := Keys[a, h];
SetElems[MSet[a_], h_] := KeyValueMap[repMElem, KeyMap[h, a]];

repMElem[k_, 1]  := k;
repMElem[k_, 0]  := Nothing;
repMElem[k_, n_] := Splice @ ConstList[k, n];

(*************************************************************************************************)

SetStrict @ SetSupport;

SetSupport[]    := {};
SetSupport[e_]  := support @ e;
SetSupport[e__] := Union @ Map[support, NoEval @ e];

support[USet[a_]]   := Keys @ a;
support[OSet[a_]]   := Keys @ a;
support[MSet[a_]]   := Keys @ a;
support[dict_Dict]  := Keys @ dict;
support[list_List]  := DelDups @ list;
support[e_]         := ThrowNotSetMsg @ e;

(*************************************************************************************************)

DefineSetDispatch1N[Join,  SetJoin];
DefineSetNaryImpl[SetJoin, uSetUnion, oSetUnion, mSetJoin];

mSetJoin[a_, b__]      := MakeMSet @ Merge[ToMSetDict /@ {a, b}, Max];

(*************************************************************************************************)

SetAdd[USet[a_], elem_] := MakeUSet[Append[a, elem -> True]];
SetAdd[OSet[a_], elem_] := MakeUSet[Append[a, elem -> True]];
SetAdd[MSet[a_], elem_] := MakeMSet[Append[a, elem -> Lookup[a, elem, 0]+1]];

SetStrict @ SetHoldF @ SetAddTo;

SetAddTo[sym_Sym, elem_] := If[SetHasQ[sym, elem], True,
  sym = SetAdd[sym, elem]; False
];

(*************************************************************************************************)

SetStrict @ SetHoldF @ SetUnionTo;

SetUnionTo[sym_Sym, {}]        := sym;
SetUnionTo[sym_Sym, ESetP]     := sym;
SetUnionTo[sym_Sym, set:ASetP] := sym = fastUnionSetTo[sym, set];
SetUnionTo[sym_Sym, list_List] := sym = fastUnionListTo[sym, list];
SetUnionTo[lhs_, rhs_]         := lhs = SetUnion[lhs, rhs];

fastUnionSetTo[USet[a_], USet[b_]] := MakeUSet[Join[a, b]];
fastUnionSetTo[USet[a_], OSet[b_]] := MakeUSet[Join[a, b]];
fastUnionSetTo[OSet[a_], OSet[b_]] := MakeOSet[Join[a, b]];
fastUnionSetTo[a:ASetP, b_] := SetUnion[a, b];
fastUnionSetTo[a_, b_] := Message[SetUnionTo::badset, b, a];

fastUnionListTo[USet[a_], list_List] := MakeUSet[Append[a, Thread[list -> True]]];
fastUnionListTo[OSet[a_], list_List] := MakeOSet[Append[a, Thread[list -> True]]];
fastUnionListTo[MSet[a_], list_List] := MakeMSet[Merge[{a, Thread[list -> 1]}, Total]];
fastUnionListTo[a_, b_] := Message[SetUnionTo::badset, b, a];

SetUnionTo::badset = "Trying to add `` to a non-set ``."

(*************************************************************************************************)

DefineSetDispatch1N[Union, SetUnion];
DefineSetNaryImpl[SetUnion, uSetUnion, oSetUnion, mSetUnion];

uSetUnion[EmptyUSet, b_USet] := b;
uSetUnion[a_USet, EmptyUSet] := a;
uSetUnion[_[a_], USet[b_]] := MakeUSet @ Join[a, b];
uSetUnion[_[a_], b__]      := MakeUSet @ ToUSetDict @ ArgList[a, b];

oSetUnion[EmptyOSet, b_OSet] := b;
uSetUnion[a_OSet, EmptyOSet] := a;
oSetUnion[_[a_], OSet[b_]] := MakeOSet @ Join[a, b];
oSetUnion[_[a_], b__]      := MakeOSet @ ToOSetDict @ ArgList[a, b];

mSetUnion[EmptyMSet, b_MSet] := b;
uSetUnion[a_MSet, EmptyMSet] := a;
mSetUnion[_[a_], MSet[b_]] := MakeMSet @ UDictPlus[a, b];
mSetUnion[_[a_], b__]      := MakeMSet @ ToMSetDict @ ArgList[a, b];

(*************************************************************************************************)

DefineSetDispatch1N[Inter, SetInter];
DefineSetNaryImpl[SetInter, uSetInter, oSetInter, mSetInter];

uSetInter[EmptyUSet, ___] := EmptyUSet;
uSetInter[_[a_], b__] := MakeUSet @ KeyTake[a, SetSupport @ b];

oSetInter[EmptyUSet, ___] := EmptyOSet;
oSetInter[_[a_], b__] := MakeOSet @ KeyTake[a, SetSupport @ b];

mSetInter[EmptyMSet, ___] := EmptyMSet;
mSetInter[a_, b__]    := MakeMSet @ DelCases[0] @ msetDictMin @ Map[ToMSetDict, {a, b}];

msetDictMin[list_] := Merge[list, minBy[Len @ list]];
minBy[n_][e_] := If[Len[e] < n, 0, Min @ n];

(*************************************************************************************************)

DefineSetDispatch1N[Compl, SetCompl];
DefineSetNaryImpl[SetCompl, uSetCompl, oSetCompl, mSetCompl];

uSetCompl[_[a_], b__] := MakeUSet @ KeyDrop[a, SetSupport @ b];
oSetCompl[_[a_], b__] := MakeOSet @ KeyDrop[a, SetSupport @ b];
mSetCompl[_[a_], b__] := MakeMSet @ Select[Positive] @ DictPlus[a, -ToMSetDict[ArgList[b], False]];

(*************************************************************************************************)

DefineSetDispatch2[Map, SetMap];

SetMap[f_, USet[a_]] := MakeUSet @ KeyMap[f, a];
SetMap[f_, OSet[a_]] := MakeOSet @ KeyMap[f, a];
SetMap[f_, MSet[a_]] := MakeMSet @ UDictSum @ MapCol1[f, Normal @ a];

(*************************************************************************************************)

DefineSetDispatch12[Map,    SetLookup];
DefineSetDispatch12[SetMap, SetLookup];

SetLookup[USet[a_], e_List] := Lookup[a, e, False];
SetLookup[OSet[a_], e_List] := Lookup[a, e, False];
SetLookup[MSet[a_], e_List] := Lookup[a, e, 0];

(*************************************************************************************************)

DefineSetDispatch2[KeyTake, SetKeyTake];
DefineSetDispatch2[KeyDrop, SetKeyDrop];

SetKeyTake[dict_, (USet|OSet|MSet)[a_]] := KeyTake[dict, Keys @ a];
SetKeyDrop[dict_, (USet|OSet|MSet)[a_]] := KeyDrop[dict, Keys @ a];

(*************************************************************************************************)

DefineSetDispatch2[Map, Lookup, LookupSet];

LookupSet[expr_, USet[set_]] := ToUSet @ Lookup[expr, Keys @ set, Nothing];
LookupSet[expr_, OSet[set_]] := ToOSet @ Lookup[expr, Keys @ set, Nothing];
LookupSet[expr_, MSet[set_]] := ToMSet @ Lookup[expr, Keys @ set, Nothing];

(*************************************************************************************************)

SetStrict[SetFilter]

SetFilter[ESetP, _]         := {};
SetFilter[USet[a_], e_List] := Pick[e, Lookup[a, e, False]];
SetFilter[OSet[a_], e_List] := Pick[e, Lookup[a, e, False]];
SetFilter[MSet[a_], e_List] := Pick[e, Positive /@ Lookup[a, e, 0]];

(*************************************************************************************************)

DefineSetDispatch12[Select, SetSelect];

SetSelect[e:ESetP, _]   := e;
SetSelect[USet[a_], f_] := MakeUSet @ KeySelect[a, f];
SetSelect[OSet[a_], f_] := MakeOSet @ KeySelect[a, f];
SetSelect[MSet[a_], f_] := MakeMSet @ KeySelect[a, f];

(*************************************************************************************************)

DefineSetDispatch12[SameSetQ, SameSetElemsQ];

SetPred2 @ SameSetElemsQ;
SameSetElemsQ[a_, a_] := True;
SameSetElemsQ[_[a_], _[b_]] := SameKeysQ[a, b];

DefineSetDispatch12[SubsetOfQ,      SetSubsetOfQ];
DefineSetDispatch12[SupersetOfQ,    SetSupersetOfQ];
DefineSetDispatch12[IntersectsQ,    SetIntersectsQ];
DefineSetDispatch12[NotIntersectsQ, SetNotIntersectsQ];

(* TODO: check <= for MSet *)
SetSubsetOfQ[a_, b_]      := SubsetOfQ[toKeys @ a, toKeys @ b];
SetSupersetOfQ[a_, b_]    := SupersetOfQ[toKeys @ a, toKeys @ b];
SetIntersectsQ[a_, b_]    := IntersectsQ[toKeys @ a, toKeys @ b];
SetNotIntersectsQ[a_, b_] := NotIntersectsQ[toKeys @ a, toKeys @ b];

toKeys[_]        := InternalError;
toKeys[a_List]   := a;
toKeys[OSet[a_]] := Keys[a];
toKeys[USet[a_]] := Keys[a];
toKeys[MSet[a_]] := Keys[a];

(*************************************************************************************************)

USet[a_][item_]          := KeyExistsQ[a, item];
OSet[a_][item_]          := KeyExistsQ[a, item];
MSet[a_][item_]          := Lookup[a, Key @ item, 0];

SetStrict @ SetHasQ;

SetHasQ[USet[a_], item_] := KeyExistsQ[a, item];
SetHasQ[OSet[a_], item_] := KeyExistsQ[a, item];
SetHasQ[MSet[a_], item_] := KeyExistsQ[a, item];

(*************************************************************************************************)

u_USet ? UnsealedQ := sealUSet[u];
o_OSet ? UnsealedQ := sealOSet[o];
m_MSet ? UnsealedQ := sealMSet[m];

