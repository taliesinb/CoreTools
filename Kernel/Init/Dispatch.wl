PackageExports[
  "MetaFunction",
    DecFullDispatch1,  DecFullDispatch2,  DecFullDispatch12,  DecFullDispatch13,  DecFullDispatch1N,  DecFullDispatch3,
    DecUElemDispatch1, DecUElemDispatch2, DecUElemDispatch12, DecUElemDispatch13, DecUElemDispatch1N,
    DecOElemDispatch1, DecOElemDispatch2, DecOElemDispatch12, DecOElemDispatch13, DecOElemDispatch1N,
  "MessageFunction",
    ThrowUndefinedBehavior
];

PrivateExports[
  "SpecialFunction",
    SealedFullDispatch1,  SealedFullDispatch2,  SealedFullDispatch12,  SealedFullDispatch13,  SealedFullDispatch1N,  SealedFullDispatch3,
    SealedUElemDispatch1, SealedUElemDispatch2, SealedUElemDispatch12, SealedUElemDispatch13, SealedUElemDispatch1N,
    SealedOElemDispatch1, SealedOElemDispatch2, SealedOElemDispatch12, SealedOElemDispatch13, SealedOElemDispatch1N
];

(*************************************************************************************************)

DeclarationFunctionDefinitions[
  DecFullDispatch1[sym_Sym]       := SetDelayed[e:sym[_ ? SealedQ],       SealedFullDispatch1[e]],
  DecFullDispatch2[sym_Sym]       := SetDelayed[e:sym[_, _ ? SealedQ],    SealedFullDispatch2[e]],
  DecFullDispatch3[sym_Sym]       := SetDelayed[e:sym[_, _, _ ? SealedQ], SealedFullDispatch3[e]],
  DecFullDispatch12[sym_Sym]      := SetDelayed[e:sym[_ ? SealedQ, _],    SealedFullDispatch12[e]],
  DecFullDispatch13[sym_Sym]      := SetDelayed[e:sym[_ ? SealedQ, _, _], SealedFullDispatch13[e]],
  DecFullDispatch1N[sym_Sym]      := SetDelayed[e:sym[_ ? SealedQ, ___],  SealedFullDispatch1N[e]],
  DecUElemDispatch1[sym_Sym]  := SetDelayed[e:sym[_ ? SealedQ],       SealedUElemDispatch1[e]],
  DecUElemDispatch2[sym_Sym]  := SetDelayed[e:sym[_ ? SealedQ],       SealedUElemDispatch1[e]],
  DecUElemDispatch12[sym_Sym] := SetDelayed[e:sym[_ ? SealedQ, _],    SealedUElemDispatch12[e]],
  DecUElemDispatch13[sym_Sym] := SetDelayed[e:sym[_ ? SealedQ, _, _], SealedUElemDispatch13[e]],
  DecUElemDispatch1N[sym_Sym] := SetDelayed[e:sym[_ ? SealedQ, ___],  SealedUElemDispatch1N[e]],
  DecOElemDispatch1[sym_Sym]  := SetDelayed[e:sym[_ ? SealedQ],       SealedOElemDispatch1[e]],
  DecOElemDispatch2[sym_Sym]  := SetDelayed[e:sym[_ ? SealedQ],       SealedOElemDispatch1[e]],
  DecOElemDispatch12[sym_Sym] := SetDelayed[e:sym[_ ? SealedQ, _],    SealedOElemDispatch12[e]],
  DecOElemDispatch13[sym_Sym] := SetDelayed[e:sym[_ ? SealedQ, _, _], SealedOElemDispatch13[e]],
  DecOElemDispatch1N[sym_Sym] := SetDelayed[e:sym[_ ? SealedQ, ___],  SealedOElemDispatch1N[e]]
];

(*************************************************************************************************)

$dispatchFns = SetHoldC[
  SealedFullDispatch1,  SealedFullDispatch2,  SealedFullDispatch12,  SealedFullDispatch13,  SealedFullDispatch1N,  SealedFullDispatch3,
  SealedUElemDispatch1, SealedUElemDispatch2, SealedUElemDispatch12, SealedUElemDispatch13, SealedUElemDispatch1N,
  SealedOElemDispatch1, SealedOElemDispatch2, SealedOElemDispatch12, SealedOElemDispatch13, SealedOElemDispatch1N
];

setupDispatchFn[sym_Sym] := SetDelayed[sym[a1_, ___], ThrowUndefinedBehavior[sym, HoldHead[a1, HoldForm]]];

Scan[setupDispatchFn, $dispatchFns];

(*************************************************************************************************)

General::undefinedBehavior = "The behavior of `` is undefined for `` objects."
ThrowUndefinedBehavior[fnSym_, head_] := ThrowMsg["undefinedBehavior", fnSym, head];

(*************************************************************************************************)
