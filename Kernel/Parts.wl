SystemExports[
  "Function",
    Second, Third, Fourth,
    FirstLast, FirstRest, MostLast,
    MaybePart, PartOr, PartOf
];

PackageExports[
  "Function",
    Dup1, Dup2, Dup3, Dup4
];

(**************************************************************************************************)

Dup1[e_] := {e};
Dup2[e_] := {e, e};
Dup3[e_] := {e, e, e};
Dup4[e_] := {e, e, e, e};

(**************************************************************************************************)

DeclareHoldRest[Second, Third, Fourth]

Second::usage = "Second[e$] gives Part[e, 2] or None.\nSecond[e$, else$] evaluates else if there is no second part.";
Third::usage  =  "Third[e$] gives Part[e, 3] or None.\nThird[e$, else$] evaluates else if there is no third part.";
Fourth::usage = "Fourth[e$] gives Part[e, 4] or None.\nFourth[e$, else$] evaluates else if there is no fourth part.";

Second[e_, f_:None] := FastQuietCheck[Part[e, 2], f];
 Third[e_, f_:None] := FastQuietCheck[Part[e, 3], f];
Fourth[e_, f_:None] := FastQuietCheck[Part[e, 4], f];

(**************************************************************************************************)

DeclareHoldRest[FirstLast]

FirstLast::usage = "FirstLast[e$] gives {First[e], Last[e]}, or {None, None}.\nFirstLast[e$, else$] gives {else, else} in this case.";
FirstLast[_ ? EmptyQ, f_:None] := Dup2 @ f;
FirstLast[e_, _]               := {First @ e, Last @ e};

DeclareStrict[FirstRest, MostLast]

FirstRest::usage = "FirstRest[e$] gives {First[e], Rest[e]}. It throws an error if e$ is empty.";
MostLast::usage  =  "MostLast[e$] gives {Most[e], Last[e]}. It throws an error if e$ is empty.";

FirstRest[e_] := FastQuietCheck[{First @ e, Rest @ e}, ThrowMsg["expressionEmpty1", e]];
MostLast[e_]  := FastQuietCheck[{ Most @ e, Last @ e}, ThrowMsg["expressionEmpty1", e]];

General::expressionEmpty1 = "First argument was an empty expression ``."

(**************************************************************************************************)

PartOf::usage = "PartOf[p$$, e$] gives Part[e$, p$$] or $Failed.\nThe operator forms are PartOp (curry p$) or PartOfOp (curry e$)."

PartOf[p__, e_] := FastQuietCheck @ Part[e, p];

(**************************************************************************************************)

DeclareCurry2[MaybePart]

MaybePart::usage ="MaybePart[e$, p$] gives Part[e$, p$$] or $Failed.
MaybePart[p$] is the single-part operator form of MaybePart.
For deeper parts, use PartOp."

MaybePart[e_, p_] := FastQuietCheck @ Part[e, p];

(**************************************************************************************************)

DeclareCurry23[PartOr]
DeclareHoldRest[PartOr]

(* same as SafePart *)
PartOr::usage =
"PartOr[e$, p$, else$] gives Part[e$, p$] or else$.
PartOr[p$, else$] is the operator form of PartOr."
PartOr[e_, p_, else_] := FastQuietCheck[Part[e, p], else];

