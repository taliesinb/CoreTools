SystemExports[
  "Function",
    Second, Third, Fourth,
    FirstSecond, SecondFirst,
    FirstLast, FirstRest, MostLast,
    MaybePart, PartOr, PartOf
];

PackageExports[
  "Function",
    Dup1, Dup2, Dup3, Dup4,
  "SymbolicHead",
    Sampled,
  "SpecialFunction",
    InternalPart,
    InternalPartOp,
    ClearInternalPartRules,
    LookupInternalPartRules
];

(**************************************************************************************************)

Dup1[e_] := {e};
Dup2[e_] := {e, e};
Dup3[e_] := {e, e, e};
Dup4[e_] := {e, e, e, e};

(**************************************************************************************************)

InternalPart[a_]              := a;
InternalPart[s_Symbol, ___]   := s;
InternalPart[a_, All]         := a;
InternalPart[Missing["KeyAbsent", k_], ___] := ErrorMessage[General::badPart, k];
i_InternalPart                              := badInternalPart[i];

InternalPart[EmptyP, p1_]            := ErrorMessage[General::empty];
InternalPart[l:ListDictP, p1_]       := iPartOne[l, p1];
InternalPart[l:ListDictP, p1_, pN__] := iPartRec[l, p1, InternalPartOp[pN]];

$part1P = _Int | _Str | _Key | Sampled | _SelectFirst | _Scaled;

iPartOne[l_, p:$part1P] := iPartLD1[l, p];
iPartOne[l_, p_]        := iPartLDN[l, p];

iPartRec[l_, p:$part1P, fn_] := fn  @ iPartLD1[l, p];
iPartRec[l_, p_,        fn_] := fn /@ iPartLDN[l, p];

iPartLD1 = CaseOf[
  $[l_, i_Int]             := PartOr[l, i, None];
  $[d_Dict, k_Str | k_Key] := Lookup[d, k, None];
  $[l_, f_SelectFirst]     := f @ l;
  $[l_, Sampled]           := Part[l, RandomInteger[{1, Len @ l}]];
  $[l_, Scaled[f_]]        := Part[l, Ceiling[Len[l] * f]];
  $[l_, p_]                := ErrorMessage[General::badPart, p];
];

iPartLDN = CaseOf[
  $[l:EmptyP, _]           := l;
  $[l_, All]               := l;
  $[l_, Into[n_]]          := Part[l, spaced[n, Len @ l]];
  $[l_, f_Select]          := f @ l;
  $[l_, p_Span]            := PartOr[l, p, None];
  $[d_Dict, ks_List]       := Lookup[d, ks, None];
  $[l_, is_List]           := PartOr[l, #, None]& /@ is;
  $[l_, Sampled[i_Int]]    := If[i >= Len[l], l, sampled[l, i]];
  $[l_, p_]                := ErrorMessage[General::badPart, p];
];

sampled[l_List, n_] := RandomSample[l, n];
sampled[d_Dict, n_] := Part[d, RandomSample[Range @ Len @ d, n]];

spaced[1, m_] := Ceiling[m/2];
spaced[2, m_] := {1, m};
spaced[n_, m_] /; n >= m := All;
spaced[n_, m_] := Ceiling[Range[0.0, 1.0, 1.0 / (n-1)] * (m-1) + 1];

SetHoldC @ badInternalPart;
badInternalPart[InternalPart[a_, p_, ___]] := IssueMessage[Head[a], "noPart", Head[a], p];

General::badPart = "Invalid part spec: ``.";
General::noPart = "Object `` does not have a part ``.";
General::empty = "Object is empty.";

(**************************************************************************************************)

InternalPartOp[]         := Id;
InternalPartOp[p___][a_] := InternalPart[a, p];

(**************************************************************************************************)

ClearInternalPartRules[h_Sym] := (
  DownValues[InternalPart] = Select[DownValues[InternalPart], VFreeQ[First @ #, h]&];
);

(**************************************************************************************************)

LookupInternalPartRules[h_Sym] := Select[DownValues[InternalPart], VContainsQ[First @ #, h]&];

(**************************************************************************************************)

(* InternalPart[a_, p__] := Message[applyPartRules[a, InternalPartRules[a], Unsequence @ p];

applyPartRules[a_, r_List, p_] :=
 *)
(**************************************************************************************************)

SetHoldR[Second, Third, Fourth, FirstSecond, SecondFirst]

Second::usage = "Second[e$] gives Part[e, 2] or None.\nSecond[e$, else$] evaluates else if there is no second part.";
Third::usage  =  "Third[e$] gives Part[e, 3] or None.\nThird[e$, else$] evaluates else if there is no third part.";
Fourth::usage = "Fourth[e$] gives Part[e, 4] or None.\nFourth[e$, else$] evaluates else if there is no fourth part.";

Second[e_, f_:None] := FastQuietCheck[Part[e, 2], f];
 Third[e_, f_:None] := FastQuietCheck[Part[e, 3], f];
Fourth[e_, f_:None] := FastQuietCheck[Part[e, 4], f];

FirstSecond[e_] := {First @ e, Second @ e};
SecondFirst[e_] := {Second @ e, First @ e};

(**************************************************************************************************)

SetHoldR[FirstLast]

FirstLast::usage = "FirstLast[e$] gives {First[e], Last[e]}, or {None, None}.\nFirstLast[e$, else$] gives {else, else} in this case.";
FirstLast[_ ? EmptyQ, f_:None] := Dup2 @ f;
FirstLast[e_, Blank01]         := {First @ e, Last @ e};

SetStrict[FirstRest, MostLast]

FirstRest::usage = "FirstRest[e$] gives {First[e], Rest[e]}. It throws an error if e$ is empty.";
MostLast::usage  =  "MostLast[e$] gives {Most[e], Last[e]}. It throws an error if e$ is empty.";

FirstRest[e_] := FastQuietCheck[{First @ e, Rest @ e}, ThrowMsg["expressionEmpty1", e]];
MostLast[e_]  := FastQuietCheck[{ Most @ e, Last @ e}, ThrowMsg["expressionEmpty1", e]];

General::expressionEmpty1 = "First argument was an empty expression ``."

(**************************************************************************************************)

PartOf::usage = "PartOf[p$$, e$] gives Part[e$, p$$] or $Failed.\nThe operator forms are PartOp (curry p$) or PartOfOp (curry e$)."

PartOf[p___, e_] := FastQuietCheck @ Part[e, p];

(**************************************************************************************************)

SetCurry2[MaybePart]

MaybePart::usage ="MaybePart[e$, p$] gives Part[e$, p$$] or $Failed.
MaybePart[p$] is the single-part operator form of MaybePart.
For deeper parts, use PartOp."

MaybePart[e_, p_] := FastQuietCheck @ Part[e, p];

(**************************************************************************************************)

SetCurry23[PartOr]
SetHoldR[PartOr]

(* same as SafePart *)
PartOr::usage =
"PartOr[e$, p$, else$] gives Part[e$, p$] or else$.
PartOr[p$, else$] is the operator form of PartOr."
PartOr[e_, p_, else_] := FastQuietCheck[Part[e, p], else];

