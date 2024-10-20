SystemExports[
  "SpecialFunction",
    InternalPart
];

PackageExports[
  "SpecialFunction",
    IPart,
    IPartOp,
    ClearIPartRules,
    LookupIPartRules
];

SessionExports[
  "SpecialFunction",
    IPartFn,
  "Variable",
    $LastSampledIndex
];

(**************************************************************************************************)

InternalPart[a___] := IPartFn[a];

DefineAliasRules[
  IPart -> InternalPart
];

(**************************************************************************************************)

IPartOp[]         := Id;
IPartOp[p___][a_] := IPartFn[a, p];

(**************************************************************************************************)

IPartFn[a_]               := a;
IPartFn[s_Symbol, ___]    := s;
IPartFn[l:ListDictP, All] := l;
IPartFn[Missing["KeyAbsent", k_], ___] := ErrorMessage[General::badPart, k];
i_IPartFn                              := badIPartFn[i];

IPartFn[EmptyP, p1_]            := ErrorMessage[General::empty];
IPartFn[l:ListDictP, p1_]       := iPartOne[l, p1];
IPartFn[l:ListDictP, p1_, pN__] := iPartRec[l, p1, IPartOp[pN]];

$part1P = _Int | _Str | _Key | Sampled | _SelectFirst | _Scaled;

iPartOne[l_, p:$part1P] := iPartLD1[l, p];
iPartOne[l_, p_]        := iPartLDN[l, p];

iPartRec[l_, p:$part1P, fn_] := fn  @ iPartLD1[l, p];
iPartRec[l_, p_,        fn_] := If[fn === Id, Id, Map[fn]] @ iPartLDN[l, p];

iPartLD1 = CaseOf[
  $[l_, i_Int]             := PartOr[l, i, None];
  $[d_Dict, k_Str]         := Lookup[d, k, wildKey[d, k]] /; StrHasQ[k, "*"];
  $[d_Dict, k:(_Str|_Key)] := Lookup[d, k, None];
  $[l_, f_SelectFirst]     := f @ l;
  $[l_, Sampled]           := sample1[l, Auto];
  $[l_, Scaled[f_]]        := Part[l, Ceiling[Len[l] * f]];
  $[l_, p_]                := badLDPart[l, p];
];

iPartLDN = CaseOf[
  $[l:EmptyP, _]            := l;
  $[l_, All]                := l;
  $[l_, Into[n_]]           := Part[l, spaced[n, Len @ l]];
  $[l_, f_Select]           := f @ l;
  $[l_, p_Span]             := PartOr[l, p, None];
  $[d_Dict, ks_List]        := Lookup[d, ks, None];
  $[l_, is_List]            := PartOr[l, #, None]& /@ is;
  $[l_, Sampled[i_Int]]     := sampled[l, i, Auto];
  $[l_, Sampled[i_Int, s_]] := sampled[l, i, s];
  $[l_, p_ ? predQ]         := Select[l, p];
  $[l_, p_]                 := badLDPart[l, p];
];


qSymQ[s_Sym ? HMaybeFnQ] := StrEndsQ[SymName[s], "Q"];
predQ[s_Sym ? qSymQ] := True;
predQ[((_Sym ? qSymQ)[___]) ? HMaybeFnQ] := True;
predQ[_] := False;

wildKey[d_, k_] := Vals @ KeySelect[d, StrMatchQ @ k];

sample1[e_ ? EmptyQ, _] := None;
sample1[e_, seed_]      := BlockSeed[Part[e, $LastSampledIndex = RandomInteger[{1, Len @ e}]], seed];

sampled[e_, n_, seed_] := BlockSeed[sampled[e, n], seed];
sampled[e_, n_] /; Len[e] <= n := e;
sampled[l_List, n_] := RandomSample[l, n];
sampled[d_Dict, n_] := Part[d, $LastSampledIndex = RandomSample[Range @ Len @ d, n]];

spaced[1, m_] := Ceiling[m/2];
spaced[2, m_] := {1, m};
spaced[n_, m_] /; n >= m := All;
spaced[n_, m_] := Ceiling[Range[0.0, 1.0, 1.0 / (n-1)] * (m-1) + 1];

SetHoldC @ badInternalPart;
badInternalPart[IPartFn[a_, p_, ___]] := IssueMessage[Head[a], "noPart", Head[a], p];

badLDPart[l_, p_] := ErrorMessage[General::badLDPart, p, Head @ l, Len @ l];

General::badLDPart = "Part `` inapprioriate for `` of length ``.";
General::badPart = "Invalid part spec: ``.";
General::noPart = "Object `` does not have a part ``.";
General::empty = "Object is empty.";

(**************************************************************************************************)

ClearIPartRules[h_Sym] := (
  DownValues[IPartFn] = Select[DownValues[IPartFn], VFreeQ[First @ #, h]&];
);

(**************************************************************************************************)

LookupIPartRules[h_Sym] := Select[DownValues[IPartFn], VContainsQ[First @ #, h]&];

(**************************************************************************************************)

InternalPart /: FmL:SetD[_InternalPart, _] := ReleaseHold[Hold[FmL] /. InternalPart -> IPartFn];

Protect[InternalPart];