SystemExports[
  "MutatingFunction",
    SetNotMissing, SetSequence,
    BlockSet, BlockAssociate, BlockJoin, BlockAppend,
    BlockIncrement, BlockDecrement,
    BlockTrue, BlockFalse,
    BlockContext,
    BlockUnprotect,
    MinTo, MaxTo, MinMaxTo
];

(*************************************************************************************************)

SetHoldF @ SetNotMissing;

SetNotMissing[{s___}][{v___}] := setNM[s][v];
SetNotMissing[s_, v_] := SetNotMissing[s][v];

SetHoldA @ setNM;

setNM[][] := Null;
setNM[s1_, sn___][v1_, vn___] := Then[If[NotMissingQ[v1], Set[s1, v1]], setNM[sn][vn]];

(*************************************************************************************************)

SetStrict @ SetHoldA[SetSequence, MinTo, MaxTo, MinMaxTo];

SetSequence[lhs1_, lhs2__, rhs_] := Set[lhs1, SetSequence[lhs2, rhs]];
SetSequence[lhs1_, rhs_]         := Set[lhs1, rhs];

MinTo[s_, e_] := Set[s, Min[s, e]];
MaxTo[s_, e_] := Set[s, Max[s, e]];
MinMaxTo[s_, e_] := Set[s, MinMax[{s, e}]];

(*************************************************************************************************)

SetStrict[BlockSet, BlockAssociate, BlockJoin, BlockAppend, BlockIncrement, BlockDecrement, BlockTrue, BlockFalse, BlockContext, BlockUnprotect];
SetHoldA[BlockSet, BlockAssociate, BlockJoin, BlockAppend, BlockIncrement, BlockDecrement, BlockTrue, BlockFalse, BlockContext, BlockUnprotect]

BlockSet[var_Sym, val_, body_]         := Block[{var = val}, body];
BlockSet[{v1_, v2_}, val_, body_]      := Block[{v1 = val, v2 = val}, body];
BlockSet[{v1_, v2_, v3_}, val_, body_] := Block[{v1 = val, v2 = val, v3 = val}, body];

BlockAssociate[var_, rules_, body_] := InheritedBlock[{var}, AssociateTo[var, rules]; body];
BlockJoin[var_, item_, body_]       := Block[{var = Join[var, item]}, body];
BlockAppend[var_, item_, body_]     := Block[{var = Append[var, item]}, body];

BlockIncrement[var_, body_]       := Block[{var = var + 1}, body];
BlockDecrement[var_, body_]       := Block[{var = var - 1}, body];

BlockTrue[var_, body_]             := Block[{var = True}, body];
BlockTrue[{v1_, v2_}, body_]       := Block[{v1 = True, v2 = True}, body];
BlockTrue[{v1_, v2_, v3_}, body_]  := Block[{v1 = True, v2 = True, v3 = True}, body];

BlockFalse[var_, body_]            := Block[{var = False}, body];
BlockFalse[{v1_, v2_}, body_]      := Block[{v1 = False, v2 = False}, body];
BlockFalse[{v1_, v2_, v3_}, body_] := Block[{v1 = False, v2 = False, v3 = False}, body];

BlockContext[context_, body_]               := Block[{$Context = context, $ContextPath = {"System`"}}, body];
BlockContext[context_, contextPath_, body_] := Block[{$Context = context, $ContextPath = contextPath}, body];

BlockUnprotect[var_Sym, body_]     := WithLocalSettings[Unprotect[var], body, Protect[var]];
BlockUnprotect[{vars__Sym}, body_] := WithLocalSettings[Unprotect[vars], body, Protect[vars]];
