PackageExports[
  "FmtFn",       CoreBox, MakeCoreBox, ToCoreBox, ClearCoreBoxRegistry,
  "SpecVar",     $CoreFormatting, $CoreInteractive,
  "ScopingFn",   BlockFormatting, BlockInteractive,
  "Predicate",   HasCoreBoxQ, HasCoreSubBoxQ, CoreBoxSubHeadQ, CoreBoxHeadQ
];

SessionExports[
  "CacheVar",    $CoreBoxHeadDict, $CoreBoxStore, $CoreBoxSubStore,
  "IOFunction",  MakeCBox
];

PrivateExports[
  "MetaFn",      SetCoreBox, SetCoreSubBox
];

(**************************************************************************************************)

SetHoldC[MakeCBox];

MakeCBox[_] := $Failed;

(**************************************************************************************************)

CoreBox /: SetDelayed[CoreBox[lhs_], rhs_] := (
  setupCoreBox @ PatHead @ lhs;
  MakeCBox[lhs] := rhs;
);

CoreBox::unknownHead = "Don't know which symbol CoreBox rules are associated with."
setupCoreBox[_]             := Message[CoreBox::unknownHead];
setupCoreBox[Hold[sym_Sym]] := (SetCoreBox[sym]; setupCoreBox[sym] := Null);

Protect[CoreBox];

(**************************************************************************************************)

SetHoldC[MakeCoreBox];

ToCoreBox[e_]   := MakeCBox @ e;
MakeCoreBox[e_] := MakeCBox @ e;

Protect[MakeCoreBox];

(**************************************************************************************************)

SetHoldC[BlockFormatting, BlockInteractive];

 BlockFormatting[body_] := Block[{ $CoreFormatting = False}, body];
BlockInteractive[body_] := Block[{$CoreInteractive = False}, body];

(**************************************************************************************************)

DeclareDeclare @ SetHoldA @ SetCoreBox;

SetCoreBox[sym_Symbol] := With[
  {name = SymName[sym]},

  $CoreBoxHeadDict[Hold[sym]] = True;
  KeyStoreAdd[$CoreBoxStore, NoEval @ sym];

  SetD[
    MakeBoxes[LHS:sym | _sym, _] /; $CoreFormatting,
    With[{res = MakeCBox @ LHS}, res /; res =!= $Failed]
  ];
];

(**************************************************************************************************)

DeclareDeclare @ SetHoldA @ SetCoreSubBox;

SetCoreSubBox[sym_Symbol] := With[
  {name = SymbolName[sym]},

  $CoreBoxHeadDict[Hold[sym]] = True;

  KeyStoreAdd[$CoreBoxStore,    NoEval @ sym];
  KeyStoreAdd[$CoreBoxSubStore, NoEval @ sym];

  SetD[
    MakeBoxes[LHS:(_sym[___]), _] /; $CoreFormatting,
    With[{res = MakeCBox @ LHS}, res /; res =!= $Failed]
  ];
];

(**************************************************************************************************)

SetInitial[$CoreFormatting, True];
SetInitial[$CoreInteractive, True];

ClearCoreBoxRegistry[] := Then[
  $CoreBoxHeadDict = UDict[];
  $CoreBoxStore = KeyStoreNew[{}];
  $CoreBoxSubStore = KeyStoreNew[{}];
  Clear[MakeCBox];
  MakeCBox[_] := $Failed;
];

If[!HasIValueQ[$CoreBoxStore], ClearCoreBoxRegistry[]];

(**************************************************************************************************)

SetHoldC @ SetPred1[CoreBoxHeadQ, CoreBoxSubHeadQ]

CoreBoxHeadQ[s_Sym]      := StoreKeyQ[$CoreBoxStore,      NoEval @ s];
CoreBoxSubHeadQ[s_Sym]   := StoreKeyQ[$CoreBoxSubStore,   NoEval @ s];

(**************************************************************************************************)

SetPred1 @ SetHoldC[HasCoreBoxQ, HasCoreSubBoxQ]

(* TODO: switch to using $CoreBoxStore *)
HasCoreBoxQ[s_Sym]    := Lookup[$CoreBoxHeadDict, Hold @ s, False];
HasCoreSubBoxQ[s_Sym] := Lookup[$CoreBoxHeadDict, Hold @ s, False];
