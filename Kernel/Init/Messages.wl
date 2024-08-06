PackageExports[
  "MessageFunction",
    TopLevelEvaluationFunction, ThrowErrorMessage,
    ThrowError, CatchError, TryCatchError, CatchErrorAsFailure,
    MsgPrePrint, MsgHold,
    CheckForUnknownOptions, UnknownOptionError, ThrowUnknownOptionError,
    ThrowOptionError,
    SameQOrThrow, SameLenQOrThrow, SameSetQOrThrow, SubsetOfQOrThrow, LookupOrThrow, LookupListOrThrow,
    OptionError, ErrorMessage, AssertThat,
  "SpecialFunction",
    Unimplemented, InternalError
];

PrivateExports[
  "MessageFunction",
    CoreToolsErrorHandler, CoreToolsErrorMessage, DisableErrorHandler,
  "SpecialSymbol",
    CoreToolsErrorTag
];

(**************************************************************************************************)

(*
Ideas:
ErrorMessage["Foo", 3, 4] issues a message, evals to $Failed.
ErrorMessage["blah" -> "foo bar baz `` bam`", 3, 4, 5] attachs a message to General, then as above.
ThrowErrorMessage[...] postpones this, throwing and catching at a CatchError, where the symbol is known.
*)

declareHAC[e___] := SetAttributes[{e}, HoldAllComplete];

(**************************************************************************************************)

SetAttributes[ErrorMessage, HoldFirst];

ErrorMessage /: SetDelayed[$LHS_, ErrorMessage[msgName_Str, args___]] := With[
  {sym = First @ PatternHeadSymbol @ $LHS},
  SetDelayed @@ Hold[$LHS, ErrorMessage[MessageName[sym, msgName], args]]
];

ErrorMessage[msg_MessageName, args___] := (
  Message[msg, args];
  $Failed
);

(**************************************************************************************************)

General::unimplemented = "An unimplemented code path was encountered.";
General::internalError = "An internal error occurred.";

Unimplemented := ThrowMsg["unimplemented"];
InternalError := ThrowMsg["internalError"];

(**************************************************************************************************)

AssertThat[True] := Null;
AssertThat[_]    := InternalError;

(**************************************************************************************************)

declareHAC[TryCatchError, CoreToolsTryHandler];

TryCatchError[body_, else_] := Block[{$willCatch = True},
  Catch[body, CoreToolsErrorTag, CoreToolsTryHandler[else]]
];

CoreToolsTryHandler[else_][_, _] := else;

(**************************************************************************************************)

CatchError /: SetDelayed[$LHS_, CatchError[body_]] := With[
  {head = First @ PatternHeadSymbol @ $LHS},
  SetDelayed @@ Hold[$LHS, CatchError[head, body]]
];

CatchError::usage = "CatchError[head, body] catches errors thrown by ThrowErrorMessage and ThrowError."

declareHAC[CatchError];

$willCatch = False;
CatchError[head_Symbol, body_] := Block[{$willCatch = True}, Catch[body, CoreToolsErrorTag, CoreToolsErrorHandler[head]]];

CatchError::noMsgDefined = "Caught an error, but no message with name `` is defined for catching symbol ``.";

CoreToolsErrorHandler[_][value_, _] := value;
CoreToolsErrorHandler[msgHead_][CoreToolsErrorMessage[msgName_String, msgArgs___], _] := (
  If[!StringQ[MessageName[msgHead, msgName]] && !StringQ[MessageName[General, msgName]],
    Message[CatchError::noMsgDefined, msgName, msgHead],
    Message[MessageName[msgHead, msgName], msgArgs]
  ];
  $Failed
);

CoreToolsErrorHandler[msgHead_][FunctionReturnValue[value_]] := value;

DefinePartialMacro[CatchError,
  CatchError[body_] :> CatchError[$MacroParentSymbol, body]
];


(**************************************************************************************************)

declareHAC[DisableErrorHandler];

DisableErrorHandler[body_] := Block[{$willCatch = False}, body];

(**************************************************************************************************)

declareHAC[CatchErrorAsFailure];

CatchErrorAsFailure::usage =
"CatchFailure[head, body] catches errors thrown by ThrowErrorMessage and ThrowError, returning a Failure object.
CatchFailure[head, body, fn] applies fn to the failure."

CatchErrorAsFailure[name_, body_, fn_:Identity] := Catch[body, CoreToolsErrorTag, errorAsFailureHandler[name, fn]];

errorAsFailureHandler[name_, _][value_, _] := Failure["UnknownFailure", Association[]];

errorAsFailureHandler[name_, fn_][CoreToolsErrorMessage[msgName_String, msgArgs___], _] :=
  fn @ Failure[name, Association[
    "MessageTemplate" :> MessageName[General, msgName],
    "MessageParameters" -> {msgArgs}
  ]];

(**************************************************************************************************)

$willCatch = False;

ThrowErrorMessage::usage = "
ThrowErrorMessage['name', body] throws a message to CatchError where it is issued.
ThrowErrorMessage['quiet', ...] is equivalent to ThrowError[].
"

ThrowErrorMessage[msgName_String, msgArgs___] :=
  ThrowError @ CoreToolsErrorMessage[msgName, msgArgs];

ThrowErrorMessage["quiet", ___] :=
  ThrowError[];

(**************************************************************************************************)

DeclareStrict[SameQOrThrow, SameLenQOrThrow, SameSetQOrThrow, SubsetOfQOrThrow, LookupOrThrow, LookupListOrThrow];

SameQOrThrow[a_, b_, msg_Str, args___]                := If[a === b, True, ThrowMsg[msg, a, b, args]];
SameLenQOrThrow[a_, b_, msg_Str, args___]             := If[Len[a] === Len[b], True, ThrowMsg[msg, Len[a], Len[b], args]];
SameSetQOrThrow[a_, b_, msg_Str, args___]             := If[SameSetQ[a, b], True, ThrowMsg[msg, Compl[a, b], Compl[b, a], args]];
SubsetOfQOrThrow[a_, b_, msg_Str, args___]            := If[SubsetOfQ[a, b], True, ThrowMsg[msg, Compl[a, b], b, args]];
LookupOrThrow[dict_, key_, msg_Str, args___]          := Lookup[dict, Key @ key, ThrowMsg[msg, key, args]];
LookupListOrThrow[dict_, keys_List, msg_Str, args___] := Lookup[dict, keys, ThrowMsg[msg, Compl[keys, Keys @ dict], args]];

(**************************************************************************************************)

DeclareStrict[CheckForUnknownOptions, UnknownOptionError, ThrowUnknownOptionError];

CheckForUnknownOptions[head_Symbol] := Null;

CheckForUnknownOptions[head_Symbol, key_ -> _] :=
  If[!OptionKeyQ[head, key], ThrowUnknownOptionError[head, key]];

CheckForUnknownOptions[head_Symbol, opts___] := Locals[
  validKeys = OptionKeys @ head;
  actualKeys = Keys @ FlatList @ opts;
  badKeys = Compl[actualKeys, validKeys];
  If[NonEmptyQ[badKeys], ThrowUnknownOptionError[head, First @ badKeys]];
];

General::unknownOption = "`` is not a known option to ``. Available options are ``.";
UnknownOptionError[head_, key_] := ErrorMessage["unknownOption", key, head, OptionKeys @ head];
ThrowUnknownOptionError[head_, key_] := ThrowErrorMessage["unknownOption", key, head, OptionKeys @ head];

(**************************************************************************************************)

DeclareStrict[OptionError, ThrowOptionError]

OptionError[opt_, val_] := ErrorMessage["invalidOption", opt, val];
ThrowOptionError[opt_, val_] := ThrowErrorMessage["invalidOption", opt, val];
General::invalidOption = "The setting `` -> `` is not valid.";

(**************************************************************************************************)

ThrowError::usage = "
ThrowError[] returns a $Failed from the containing CatchError.
ThrowError[expr$] returns $expr from the containing CatchError.
"

General::uncaughtMessage = "The message name \"``\" was thrown but not caught. Aborting.";

ThrowError[] := ThrowError @ $Failed;

ThrowError[e_] /; $willCatch := Throw[e, CoreToolsErrorTag];

ThrowError[CoreToolsErrorMessage[name_, args___]] := (
  If[StringQ[MessageName[General, name]],
    Message[MessageName[General, name], args]];
  Message[General::uncaughtMessage, name]; Abort[]
);

ThrowError[_] :=
  (Message[General::uncaughtError]; Abort[]);

ThrowError::invalidThrowError = "Invalid call to ThrowError.";
t_ThrowError := (Message[ThrowError::invalidThrowError, HoldForm[t]]; ThrowError[]);

(**************************************************************************************************)

declareHAC[TopLevelEvaluationFunction];

TopLevelEvaluationFunction[body_] := CatchError[TopLevelEvaluationFunction, body];
TopLevelEvaluationFunction[bodies___] := TopLevelEvaluationFunction[CompoundExpression[bodies]];

(* this relies on a customized stylesheet that contains e.g.:
Cell[StyleData["Code"],
  CellEvaluationFunction -> Function[boxes,
    If[DownValues[System`TopLevelEvaluationFunction] === {},
      ToExpression @ boxes,
      ToExpression[boxes, StandardForm, System`TopLevelEvaluationFunction]]]
]
*)

CoreToolsErrorHandler[TopLevelEvaluationFunction][value_, _] := (
  Message[General::uncaughtError];
  value
);
General::uncaughtError = "ThrowError occurred without a surrounding CatchError.";

CoreToolsErrorHandler[TopLevelEvaluationFunction][msg:CoreToolsErrorMessage[msgName_String, msgArgs___], tag_] := (
  Message[General::uncaughtErrorMessage];
  CoreToolsErrorHandler[General][msg, tag];
);
General::uncaughtErrorMessage = "ThrowErrorMessage occurred without a surrounding CatchError.";

(**************************************************************************************************)

declareHAC[MsgPrePrint, msgBoxes];

MsgPrePrint[LiteralCommaStringForm[s:{__Str}]] := StringRiffle[s, ", "];
MsgPrePrint[LiteralStringForm[s_Str]] := s;
MsgPrePrint[$PrintLiteral[s_Str]] := s;
MsgPrePrint[f_Failure]      := FailureString @ f;
MsgPrePrint[b_RawBoxes]     := b;
MsgPrePrint[HoldForm[e_]]   := MsgPrePrint @ e;
MsgPrePrint[HoldForm[e___]] := MsgPrePrint @ CoreToolsSequence @ e;
MsgPrePrint[e_String ? HoldAtomQ] /; StringContainsQ[e, " " | "/"] := e;
MsgPrePrint[e_]             := If[ByteCount[HoldComplete[e]] > 20000,
  RawBoxes @ msgBoxes @ e,
  NicePaster[RawBoxes @ msgBoxes @ e, e]
];

msgBoxes[e_] := Block[{$MessagePrePrint = Automatic}, PaneBox[
  ConstrainedMakeBoxes[CoreToolsHold[e], 120, 30, 6],
  BaseStyle -> {$msgStyleOptions}, ImageSize -> {UpTo[650], UpTo[60]},
  BaselinePosition -> Baseline,
  StripOnInput -> True
]];

$msgStyleOptions = Sequence[
  ShowAutoStyles       -> False,
  ShowStringCharacters -> True,
  LineSpacing          -> {1, 0},
  AutoIndent           -> True,
  AutoSpacing          -> True,
  LineIndent           -> .5,
  Hyphenation          -> False,
  FontFamily           -> "Source Code Pro",
  FontWeight           -> "DemiBold",
  PrintPrecision       -> 3,
  NumberMarks          -> False,
  ShowInvisibleCharacters -> True,
  GraphicsBoxOptions   -> {ImageSize -> 100}
];


