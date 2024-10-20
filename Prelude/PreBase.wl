BeginPackage["Prelude`", {"Session`"}];

PackageExports[

  "PackageFunction",
    SystemExports,
    PackageExports,
    PrivateExports,
    SessionExports,
    CustomExports,
    PackageImports,

    DeclareFilePrivates,
    DeclareFileLocals,
    DeclarePackagePrivates,
    DeclarePackageLocals,
    DeclarePackageGlobals,
    DeclareSystemPrivates,
    DeclareSystemGlobals,

  "MetaFunction",
    DeclareArity,
    DeclareStrict,
    DeclareHoldFirst,
    DeclareHoldRest,
    DeclareHoldAll,
    DeclareHoldAllComplete,
    DeclareListable,
    DeclareUsage
];

SystemExports[

  "Option",
    Caching,
    Logging,
    LogLevel,
    Verbose,

  "SymbolicHead",
    Rectangular,
    Circular,

  "IOFunction",
    FileUnixTime,

  "MutatingFunction",
    UnprotectClearAll, UnprotectClear,

  "SpecialFunction",
    ToInputString, HoldToInputString, FromInputString,

  "DataHead",
    UAssociation,

  "HoldFunction",
    EvaluateMap,
    HoldMap, HoldScan, HoldApply, HoldConstruct, HoldMapSequence,
    HoldHead, HoldLength, HoldSequenceLength,
    HoldByteCount, HoldHash,
    HoldSymbolName, HoldSymbolNameAlias, HoldSymbolContext, HoldSymbolPath,

  "SequenceFunction",
    MapSequence, SequenceLength,

  "Function",
    NaturalNumberString, FullIntegerString,

  "ControlFlow",
    QuietCheck, TrapMessages,

  "MessageFunction",
    CheckedRHS,

  "Predicate",
    HoldListQ, HoldAssociationQ, HoldPackedArrayQ, PackedListQ,
    HoldStringQ, HoldIntegerQ, HoldNaturalQ, HoldNumberQ, HoldBooleanQ, HoldColorQ,
    InvalidQ, CorruptQ, ValidFileQ, KeyAbsentQ

  "TagVariable",
    $Invalid,
    $Corrupt,

  "Variable",
     $FormalSymbols,

  "HoldHead",
    PrivateHoldComplete, PrivateSequence, PrivateHoldCompleteSequence,

  "MetaFunction",
    DeclaredHere,

  "FormHead",
    OutputExpressionForm, MessageArgumentForm
];

SessionExports[

  "SpecialVariable",

    $PreludeLoaded,
    $PreludeDir,
    $PreludeFiles,

    $CoreToolsLoaded,
    $CoreToolsPath,
    $CoreToolsRootPath
];

Begin["`Base`Private`"]

(**************************************************************************************************)

General::invalidUsage      = "Invalid arguments: ``."
General::invalidArguments  = "Invalid arguments: ``."
General::unimplemented     = "An unimplemented code path was encountered.";
General::internalError     = "An internal error occurred.";

(**************************************************************************************************)

Protect[$Invalid, $Corrupt]

InvalidQ[$Invalid] := True;
CorruptQ[$Corrupt] := True;

CorruptQ[_] := False;
InvalidQ[_] := False;

(**************************************************************************************************)

ValidFileQ[path_String] := FileType[path] === File;
ValidFileQ[_]           := False;

(**************************************************************************************************)

KeyAbsentQ[a_, k_] := !KeyExistsQ[a, k];
KeyAbsentQ[k_][a_] := !KeyExistsQ[a, k];

(**************************************************************************************************)

DeclareStrict[fns__Symbol] := Scan[DeclareStrict, Unevaluated @ fns];

DeclareStrict[fn_Symbol] := SetDelayed[
  \[FormalCapitalL]_fn,
  Message[fn::args, HoldForm @ \[FormalCapitalL]];
  $Failed
];

DeclareStrict[DeclareStrict];

(**************************************************************************************************)

DeclareHoldAllComplete[fns__Symbol] := SetAttributes[{fns}, HoldAllComplete];
DeclareHoldFirst[fns__Symbol]       := SetAttributes[{fns}, HoldFirst];
DeclareHoldRest[fns__Symbol]        := SetAttributes[{fns}, HoldRest];
DeclareHoldAll[fns__Symbol]         := SetAttributes[{fns}, HoldAll];
DeclareListable[fns__Symbol]        := SetAttributes[{fns}, Listable];

DeclareStrict[DeclareHoldAllComplete, DeclareHoldFirst, DeclareHoldRest, DeclareHoldAll, DeclareListable];

(**************************************************************************************************)

DeclareUsage::noResolve = "Could not resolve symbol name ``.";
DeclareUsage[usage2_String] := Module[{name, str, usage},
  usage = StringTrim[usage2, {" ", "\n", "`"}];
  {name, str} = StringSplit[usage, {" ", "["}, 2];
  If[!NameQ[name], Message[DeclareUsage::noResolve, name]; Return @ $Failed];
  hsym = ToExpression[name, InputForm, Hold];
  setUsage[hsym, usage];
];

setUsage[Hold[sym_], str_String] := MessageName[sym, "usage"] = str;

DeclareStrict @ DeclareUsage;

(**************************************************************************************************)

DeclareHoldAllComplete[QuietCheck]

QuietCheck[body_]        := QuietCheck[body, $Failed];
QuietCheck[body_, else_] := Quiet @ Check[body, else];

(**************************************************************************************************)

DeclareHoldFirst[TrapMessages]

TrapMessages[body_, handlerFn_] := Internal`HandlerBlock[{"Message", handlerFn}, body];

(**************************************************************************************************)

DeclareHoldAllComplete[CheckedRHS, iCheckedRHS1];

CheckedRHS /: SetDelayed[\[FormalCapitalL]_, \[FormalCapitalR]_CheckedRHS] :=
  iCheckedRHS1[\[FormalCapitalL], \[FormalCapitalR]];

iCheckedRHS1[lhs_, CheckedRHS[test:Except[_List], body_]] :=
  iCheckedRHS1[lhs, CheckedRHS[{test, "invalidUsage", Internal`ConditionalValueLHS}, body]];

iCheckedRHS1[lhs:(head_Symbol[___]), CheckedRHS[conds:{_, _String, ___}.., body_]] := iCheckedRHS2[
  head, HoldComplete[lhs],
  Part[HoldComplete @ conds, All, 1],
  Part[HoldComplete @ conds, All, 2;;],
  HoldComplete @ body
];

iCheckedRHS1[lhs_, rhs_] := Message[General::invalidUsage, HoldForm[lhs := rhs]];

iCheckedRHS2[head_, HoldComplete[lhs_], HoldComplete[conds__], HoldComplete[msgs__List], HoldComplete[body_]] := (
  SetDelayed[\[FormalCapitalL]_head, $Failed];
  SetDelayed[lhs, Internal`ConditionalValueBody[head, List @ conds, List @ msgs, body]]
);

(**************************************************************************************************)

DeclareArity[n_Integer,            s__Symbol] := decArity[n, n, List @ s];
DeclareArity[n_Integer;;m_Integer, s__Symbol] := decArity[n, m, List @ s];
DeclareArity[n_Integer;;All,       s__Symbol] := decArity[n, Infinity, List @ s];

SetAttributes[decArity, Listable];

decArity[min_, max_, sym_] := SetDelayed[
  \[FormalCapitalL]_sym,
  Developer`CheckArgumentCount[\[FormalCapitalL], min, max]; $Failed
];

DeclareStrict[DeclareArity];

(**************************************************************************************************)

DeclareHoldAllComplete[UnprotectClearAll, UnprotectClear];

UnprotectClearAll[e___] := (Unprotect[e]; ClearAll[e]);
UnprotectClear[e___]    := (Unprotect[e]; Clear[e]);

(**************************************************************************************************)

DeclareHoldAllComplete[SystemExports, PackageExports, PrivateExports, SessionExports, CustomExports, PackageImports];

(**************************************************************************************************)

DeclareHoldAllComplete[HoldToInputString];

HoldToInputString[e_]   := ToString[Unevaluated @ e, InputForm];
ToInputString[e_]       := ToString[Unevaluated @ e, InputForm];

FromInputString[str_ ? Developer`StringOrStringVectorQ]        := ToExpression[str, InputForm];
FromInputString[str_ ? Developer`StringOrStringVectorQ, head_] := ToExpression[str, InputForm, head];

_FromInputString := (Message[FromInputString::invalidUsage]; $Failed);

FromInputString::invalidUsage = "Invalid call to FromInputString.";

(*************************************************************************************************)

(* these can't be cleared! *)
(* MakeBoxes[OutputExpressionForm[e_], StandardForm] := MakeBoxes @ e;
MakeBoxes[MessageArgumentForm[e_], StandardForm]  := MakeBoxes @ e;
 *)
(*************************************************************************************************)

DeclareHoldAllComplete[PrivateHoldComplete, privateHoldBoxes];

(* we use this to wrap e.g. held values in Assocs and RuleDelayeds but we know it won't be
shown and won't be generated by any other code that these functions might be trying to debug *)

MakeBoxes[p_PrivateHoldComplete, StandardForm] := privateHoldBoxes[p];

privateHoldBoxes[_]                             := "BAD PrivateHoldComplete";
privateHoldBoxes[PrivateHoldComplete[]]         := RowBox @ {"Sequence", "[", "]"};
privateHoldBoxes[PrivateHoldComplete[e1_]]      := MakeBoxes[e1];
privateHoldBoxes[PrivateHoldComplete[e1_, e2_]] := RowBox @ {"Sequence", "[", RowBox @ {MakeBoxes[e1], ",", MakeBoxes[e2]}, "]"};
privateHoldBoxes[PrivateHoldComplete[es__]]     := RowBox @ {"Sequence", "[", RowBox @ Riffle[{Map[MakeBoxes, Unevaluated @ es]}, ","], "]"};

(*************************************************************************************************)

DeclareHoldAllComplete[privateSeqBoxes];

MakeBoxes[p_PrivateSequence, StandardForm] := privateSeqBoxes[p];

privateSeqBoxes[_]                      := "BAD PrivateSequence";
privateSeqBoxes[PrivateSequence[es___]] := MakeBoxes[Sequence[es]];

(*************************************************************************************************)

DeclareHoldAllComplete[PrivateHoldCompleteSequence, privateHoldSeqBoxes];

MakeBoxes[p_PrivateHoldCompleteSequence, StandardForm] := privateHoldSeqBoxes[p];

privateHoldSeqBoxes[_]                                     := "BAD PrivateHoldCompleteSequence";
privateHoldSeqBoxes[PrivateHoldCompleteSequence[]]         := "";
privateHoldSeqBoxes[PrivateHoldCompleteSequence[e1_]]      := MakeBoxes[e1];
privateHoldSeqBoxes[PrivateHoldCompleteSequence[e1_, e2_]] := RowBox @ {MakeBoxes[e1], ",", MakeBoxes[e2]};
privateHoldSeqBoxes[PrivateHoldCompleteSequence[es__]]     := RowBox @ Riffle[{Map[MakeBoxes, Unevaluated @ es]}, ","];

(*************************************************************************************************)

UAssociation = Data`UnorderedAssociation;
PackedListQ  = Developer`PackedArrayQ;

(* PackedListQ isn't called PackedArrayQ to avoid shadowing problems if someone calls Needs["Developer`"] *)

(*************************************************************************************************)

DeclareHoldAllComplete[HoldLength, HoldByteCount, HoldHead, HoldHash];

HoldByteCount[e_] := ByteCount @ Unevaluated @ e;
HoldLength[e_]    := Length @ Unevaluated @ e;
HoldHead[e_]      := Head @ Unevaluated @ e;
HoldHead[e_, fn_] := Head[Unevaluated @ e, fn];
HoldHash[e_]      := Hash @ Unevaluated @ e;

(**************************************************************************************************)

e_SequenceLength         := Length @ Unevaluated @ e;

MapSequence[f_]          := Sequence[];
MapSequence[f_, arg_]    := f[arg];
MapSequence[f_, args___] := Map[f, Unevaluated @ args];

(**************************************************************************************************)

DeclareHoldAllComplete[HoldSequenceLength];

e_HoldSequenceLength := Length @ Unevaluated @ e;

(**************************************************************************************************)

EvaluateMap::usage = "EvaluateMap[fn$, expr$] maps fn$ over the parts of expr$, forcing evaluation despite the head being held.";

(* unlike Map, EvaluateMap[f, Hold[1,2,3]] actually does something *)
EvaluateMap[f_, list_List]         := Map[f, list];
EvaluateMap[f_, assoc_Association] := Map[f, assoc];
EvaluateMap[f_, h_[args___]]       := Construct[h, HoldMapSequence[f, args]];

(**************************************************************************************************)

HoldMap::usage = "HoldMap[fn$, expr$] maps fn$ over expr$ without evaluating its parts first.";
HoldScan::usage = "HoldScan[fn$, expr$] scans fn$ over expr$ without evaluating its parts first.";
HoldApply::usage = "HoldApply[fn$, expr$] applies fn$ to expr$ without evaluating its parts first.";
HoldConstruct::usage = "HoldConstruct[fn$, arg$1, arg$2] evaluates fn$[arg$1, arg$2, $$] without evaluating arg$i.";
HoldMapSequence::usage = "HoldMapSequence[fn$, arg$1, arg$2] returns Sequence[fn[arg$1], fn[arg$2], $$] without evaluating arg$i.";

DeclareHoldAllComplete[HoldMap, HoldScan, HoldApply, HoldConstruct, HoldMapSequence];
DeclareStrict[HoldMap, HoldScan, HoldApply, HoldConstruct, HoldMapSequence];

HoldMap[fn_, expr_]                     := Map[fn, Unevaluated @ expr];
HoldMap[fn_Function, expr_]             := Map[toHoldFn @ fn, Unevaluated @ expr];

HoldScan[fn_, expr_]                    := Scan[fn, Unevaluated @ expr];
HoldScan[fn_Function, expr_]            := Scan[toHoldFn @ fn, Unevaluated @ expr];

HoldApply[_,           {}]              := {};
HoldApply[fn_,         _[args___]]      := fn[Unevaluated @ expr];
HoldApply[fn_Function, _[args___]]      := toHoldFn[fn][Unevaluated @ expr];

HoldConstruct[fn_]                      := fn[];
HoldConstruct[fn_, args___]             := Construct[fn, Unevaluated @ args];
HoldConstruct[fn_Function, args___]     := Construct[toHoldFn @ fn, Unevaluated @ args];

HoldMapSequence[fn_]                    := Sequence[];
HoldMapSequence[fn_, arg_]              := fn[Unevaluated @ arg];
HoldMapSequence[fn_Function, args__]    := Map[toHoldFn @ f, Unevaluated @ args];
HoldMapSequence[fn_, args__]            := Map[fn, Unevaluated @ args];

toHoldFn[fn_]             := fn;
toHoldFn[_[body_]]        := Function[Null, body, HoldAllComplete];
toHoldFn[_[args_, body_]] := Function[args, body, HoldAllComplete];

(*************************************************************************************************)

DeclareHoldAllComplete[HoldStringQ, HoldIntegerQ, HoldNaturalQ, HoldNumberQ, HoldBooleanQ, HoldColorQ];

HoldStringQ[_String ? Developer`HoldAtomQ] = True;
HoldIntegerQ[_Integer ? Developer`HoldAtomQ] = True;
HoldNaturalQ[(_Integer ? Developer`HoldAtomQ) ? NonNegative] = True;
HoldNumberQ[Alternatives[_Integer, _Real, _Rational] ? Developer`HoldAtomQ] = True;
HoldColorQ[c:Alternatives[_GrayLevel, _RGBColor, _CMYKColor, _Hue, _XYZColor, _LABColor, _LCHColor, _LUVColor]] := ColorQ[Unevaluated @ c];
HoldBooleanQ[False | True] = True;

_HoldStringQ  = False;
_HoldIntegerQ = False;
_HoldNaturalQ = False;
_HoldNumberQ  = False;
_HoldBooleanQ = False;
_HoldColorQ   = False;

(*************************************************************************************************)

DeclareHoldAllComplete[HoldListQ, HoldAssociationQ, HoldPackedArrayQ];

HoldListQ[_List] = True;

HoldAssociationQ[_Association ? Developer`HoldAtomQ] = True;

HoldPackedArrayQ[a_List] := Developer`PackedArrayQ @ Unevaluated @ a;

_HoldListQ        = False;
_HoldAssociationQ = False;
_HoldPackedArrayQ = False;

(*************************************************************************************************)

DeclareHoldAllComplete[HoldSymbolNameAlias, HoldSymbolName, HoldSymbolContext, HoldSymbolPath];

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

DeclareListable[FileUnixTime];

FileUnixTime[path_String] := If[FileExistsQ[path], UnixTime @ FileDate @ path, 0];
FileUnixTime[_] := $Failed;

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