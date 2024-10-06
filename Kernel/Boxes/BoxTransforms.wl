PackageExports[
  "BoxFunction",   DepthTruncateBoxes, FlattenStyleBox, ApplyStyleBox, EndStyleBox, ApplyIndentBox,
  "FormHead",      DepthTruncateForm, EndStyled, EndSpaced, SepStyled, SepSpaced,
  "Function",      FlattenStyle, ApplyStyle,
  "Operator",      FormBurrowing, BoxBurrowing, StyleOp, StyleBoxOp,
  "PatternSymbol", ThruFormP, ThruBoxP,
  "Predicate",     ThruFormSymQ, ThruBoxSymQ, AtomFormHeadQ, CompoundFormHeadQ
];

(**************************************************************************************************)

FlattenStyle[Style[Style[expr_, s1___], s2___]] := FlattenStyle @ Style[expr, s1, s2];
FlattenStyle[expr_] := expr;

SetBoxFn[FlattenStyleBox];
FlattenStyleBox[StyleBox[StyleBox[boxes_, s1___], s2___]] := FlattenStyleBox @ StyleBox[boxes, s1, s2];
FlattenStyleBox[boxes_] := boxes;

(**************************************************************************************************)

(* TODO: replace this with CompoundFormHeadP *)

DefinePatternRules[
  ThruFormP -> Alt[_EventHandler, _Tooltip, _Style, _RaiseForm, _LowerForm, _MarginForm, _PadForm, _ClickForm, _NiceTooltip, _Framed, _Pane],
  ThruBoxP  -> Alt[_TagBox, _TooltipBox, _StyleBox, _AdjustmentBox, _ItemBox, _PaneBox, _FrameBox]
];

SetPred1[ThruFormSymQ, ThruBoxSymQ];

ThruFormSymQ[head_Sym] := MatchQ[head, ThruFormP];
ThruBoxSymQ[head_Sym]  := MatchQ[head, ThruBoxP];

(**************************************************************************************************)

s_ApplyStyle[expr:ThruFormP]    := MapFirst[s, expr];
s_ApplyStyle[expr_]             := Style[expr, Seq @@ s];

s_ApplyStyleBox[boxes:ThruBoxP] := MapFirst[s, boxes];
s_ApplyStyleBox[boxes_]         := StyleBox[boxes, Seq @@ s];

(**************************************************************************************************)

DeclaredHere[FormBurrowing, BoxBurrowing];

fn_FormBurrowing[expr:ThruFormP] := MapFirst[fn, expr];
fn_FormBurrowing[expr_]          := First[fn] @ expr;

fn_BoxBurrowing[boxes:ThruBoxP]  := MapFirst[fn, boxes];
fn_BoxBurrowing[boxes_]          := First[fn] @ boxes;

(**************************************************************************************************)

StyleOp[] = Id;
StyleOp[None] = Id;
StyleOp[spec___][Nothing] := Nothing;
StyleOp[spec___][e_] := Style[e, spec];

StyleBoxOp[] = Id;
StyleBoxOp[None] = Id;
StyleBoxOp[spec___][Nothing] := Nothing;
StyleBoxOp[spec___][e_] := StyleBox[e, spec];

SetCurryBoxFn @ StyleBoxOp;

(**************************************************************************************************)

SetFormO[EndStyled, EndSpaced, SepStyled, SepSpaced];

SetCurryBoxFn[EndStyleBox, EndSpaceBox, SepStyleBox, SepSpaceBox];

SystemBox[EndStyled[s___][e_]] := EndStyleBox[s] @ MakeBox @ e;
SystemBox[EndSpaced[n_][e_]]   := EndSpaceBox[n] @ MakeBox @ e;
SystemBox[SepStyled[s___][e_]] := SepStyleBox[s] @ MakeBox @ e;
SystemBox[SepSpaced[n_][e_]]   := SepSpaceBox[n] @ MakeBox @ e;

EndStyleBox[s__][boxes_]    := BoxBurrowing[endStyle[s]] @ boxes;
SepStyleBox[s__][boxes_]    := BoxBurrowing[sepStyle[s]] @ boxes;
EndSpaceBox[n:NumP][boxes_] := BoxBurrowing[endSpace[n]] @ boxes;
SepSpaceBox[n:NumP][boxes_] := BoxBurrowing[sepSpace[n]] @ boxes;

endStyle[s___][RowBox[{l_, m___, r_}]] := RowBox[{StyleBox[l, s], m, StyleBox[r, s]}];
endSpace[n:NumP][RowBox[{l_, m___, r_}]] := RowBox[{l, SpacerBox[n], m, SpacerBox[n], r}];

f_sepStyle[RowBox[{l_, m_RowBox, r_}]] := RowBox[{l, f @ m, r}];
f_sepStyle[RowBox[bs_List]]            := RowBox[MapAt[StyleBoxOp @@ f, bs, Span[2, -2, 2]]];

f_sepSpace[RowBox[{l_, m_RowBox, r_}]] := RowBox[{l, f @ m, r}];
f_sepSpace[RowBox[bs_List]]            := RowBox @ Insert[bs, SpacerBox @@ f, List /@ Range[3, Len[bs], 2]];

_endStyle[b_] := b;
_endSpace[b_] := b;
_sepStyle[b_] := b;
_sepSpace[b_] := b;

(**************************************************************************************************)

SetBoxFn @ ApplyIndentBox;

ApplyIndentBox[boxes_, n_:0] := doIndent[n][boxes];

doIndent[n_?Negative] := Id;
doIndent[n_][StyleBox[boxes_, s___]]      := StyleBox[doIndent[n] @ boxes];
doIndent[n_][RowBox[list_]]               := Map[doIndent[n], list];
doIndent[n_][RowBox[{l_Str, m_, r_Str}]]  := RowBox[{l <> "\n", addNewlines[n-1][m], "\n" <> r}];
doIndent[_][boxes_] := boxes;

addNewlines[n_?Negative] := Id;
addNewlines[n_][RowBox[els:{___, ",", ___}]] := RowBox[Map[procLine[n], els]];
procLine[n_][","] := Splice[{",", "\n"}];
procLine[n_][b_]  := RowBox[{"  ", doIndent[n-1][b]}];

(*************************************************************************************************)

SystemBox[DepthTruncateForm[expr_, n_Int]] := At[DepthTruncateBoxes, MakeBoxes @ expr, n];

(*************************************************************************************************)

SetHoldC @ DepthTruncateBoxes;

DepthTruncateBoxes[boxes_, n_Int] := If[n <= 0, CDotsS, Block[{$d = n}, truncBox @ boxes]];

SetHoldC[truncBox, incDepth1, incDepth2];

truncBox = CaseOf[
  s_Str                             := s;
  InterpBox[b_, ___]                := $ @ b;
  TBox[args_List, name_]            := TBox[truncTBox[name, args], name];
  RowBox[b:NestRowBoxListP]            := RowBox @ HoldMap[$, b] // incDepth1;
  RowBox[b_List]                    := RowBox @ HoldMap[$, b];
  e:(h:Box0SymP)[___]               := e;
  (h:Box1SymP)[a_, r___]            := With[{a1 = $ @ a},             h[a1, r]];
  (h:Box1VecSymP)[a_List, r___]     := With[{a1 = HoldMap[$, a]},    h[a1, r]];
  (h:Box1MatSymP)[a:{__List}, r___] := With[{a1 = HoldMap2[$, a]},     h[a1, r]] // incDepth2;
  (h:Box2SymP)[a_, b_, r___]        := With[{a1 = $ @ a, b1 = $ @ b}, h[a1, b1, r]];
  e:otherSym$[___]                  := e;
  e:(h:SymP)[___]                   := BoxFnErrBox[h, e];
  e_                                := BoxFnErrBox[e];
,
  {otherSym$ -> Join[Box1RVecSymP, Box2RVecSymP, BoxDValSymP, BoxDSymP]}
];

incDepth1[body_] := If[$d <= 1, CDotsS, Block[{$d = $d - 1}, body]];
incDepth2[body_] := If[$d <= 2, CDotsS, Block[{$d = $d - 2}, body]];

SetHoldC[truncTBox, maybeTrunc];

truncTBox = CaseOf[
  $[_, {}]                           := {};
  $["RowDefault", args_]             := HoldMap[truncBox, args];
  $["RowWithSeparators", {a_, r___}] := Prepend[a] @ HoldMap[truncBox, {r}];
  $[_, args_]                        := HoldMap[maybeTrunc, args];
];

maybeTrunc = CaseOf[
  b:BoxTruncSymP[___] := incDepth1 @ truncBox @ b;
  b_                  := b;
];