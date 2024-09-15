PackageExports[
  "ControlFlowFunction",
    Try, TryElse,

  "MessageFunction",
    IssueMessage, GeneralMessage, ErrorMessage,

    ThrowMessage,
    CatchMessages,
    CatchAsFailure,

    ThrowException,
    ThrowErrorValue,

    CheckForUnknownOptions, UnknownOptionMsg,
    ThrowUnknownOptionMsg, ThrowOptionMsg,
    SameQOrThrow, SameLenQOrThrow, SameSetQOrThrow, SubsetOfQOrThrow, LookupOrThrow, LookupListOrThrow,
    OptionMsg,
    AssertThat,

  "MetaFunction",
    DefineException,

  "FormHead",
    ExceptionForm,

  "SpecialFunction",
    Unimplemented, InternalError,
    MsgPrePrint,

  "Predicate",
    GeneralMessageQ, SymbolMessageQ
];

PrivateExports[
  "Function",             InsertWithSourceLocations, CreateFailure,
  "SymbolicHead",         MessageException, FailureException, LiteralException,
  "ControlFlowFunction",  WithSourceLocation, HandleExceptions, DisableHandleExceptions, UnhandledException,
  "SpecialFunction",      TryElseHandler, TryHandler, CatchAsMessageHandler, CatchAsFailureHandler,
  "SpecialSymbol",        ExceptionTag,
  "SpecialVariable",      $HandlerSet, $SourceLocationStack
];

(**************************************************************************************************)

(*
Ideas:
ErrorMessage["Foo", 3, 4] issues a message, evals to $Failed.
ErrorMessage["blah" -> "foo bar baz `` bam`", 3, 4, 5] attachs a message to General, then as above.
ThrowErrorMessage[...] postpones this, throwing and catching at a CatchMessages, where the symbol is known.
*)

(**************************************************************************************************)

SetPred1 @ GeneralMessageQ;

GeneralMessageQ[name_Str] := StringQ[MessageName[General, name]];

GeneralMessage[name_Str, args___] := Then[Message[MessageName[General, name], args], $Failed];

(**************************************************************************************************)

SetHoldF @ SetPred2 @ SymbolMessageQ;

SymbolMessageQ[sym_Sym, name_Str] := StringQ[MessageName[sym, name]] || GeneralMessageQ[name];

(**************************************************************************************************)

SetHoldR @ WithSourceLocation;

WithSourceLocation[loc_, body_] := Block[{$SourceLocationStack = Append[$SourceLocationStack, loc]}, body];

InsertWithSourceLocations[hc_] := With[
  {loc = SourceLocation[]},
  ReplaceAll[hc, {
    e:($ExceptingSymbolP[___]) :> WithSourceLocation[loc, e],
    u:HoldP[Unimplemented | InternalError] :> WithSourceLocation[loc, u]
  }]
];

$SourceLocationStack = {};

(**************************************************************************************************)

IssueMessage::usage =
"IssueMessage[head$, 'name$', args$$] issues the message with name 'name$' against symbol head$.
* if the message doesn't exist a fallback message will be issued."

IssueMessage[head_ -> slocs_, args___] := Then[
  If[slocs =!= {}, ErrorPrint @ Row[DelDups @ ToList[$SourceLocationStack, slocs], " > "]];
  IssueMessage[head, args]
];

IssueMessage[msgHead_Sym, msgName_String, msgArgs___] := Then[
  If[!SymbolMessageQ[msgHead, msgName],
    Message[MessageName[msgHead, "missingMessage"], msgName, msgHead],
    If[!StringQ[MessageName[msgHead, msgName]],
      MessageName[msgHead, msgName] = MessageName[General, msgName]]; (* ? *)
    Message[MessageName[msgHead, msgName], msgArgs]
  ],
  $Failed
];



e_IssueMessage := GeneralMessage["invalidMessage", IssueMessage, HoldForm @ e];

General::missingMessage = "No message with name `` is defined for symbol ``.";
General::invalidMessage = "An invalid call to `` was encountered: ``.";

(**************************************************************************************************)

SetExcepting @ SetHoldF @ ErrorMessage;

DefinePseudoMacro[ErrorMessage,
ErrorMessage[msg_Str, args___] :> IssueMessage[$MacroParentSymbol, msg, args]
];

ErrorMessage[msg_Str, args___]                       := IssueMessage[General, msg, args];
ErrorMessage[MessageName[sym_Sym, msg_Str], args___] := IssueMessage[sym, msg, args];

e_ErrorMessage := GeneralMessage["invalidMessage", ErrorMessage, HoldForm @ e];

(**************************************************************************************************)

General::unimplemented = "An unimplemented code path was encountered.";
General::internalError = "An internal error occurred.";

Unimplemented := ThrowMsg["unimplemented"];
InternalError := ThrowMsg["internalError"];

(**************************************************************************************************)

SetExcepting @ AssertThat;

AssertThat[True] := Null;
AssertThat[_]    := InternalError;

(**************************************************************************************************)

SetHoldF[HandleExceptions, DisableHandleExceptions];

$HandlerSet = False;

HandleExceptions[body_, handler_] := Block[{$HandlerSet = True}, Catch[body, ExceptionTag, exceptionHandler @ handler]];
DisableHandleExceptions[body_]    := Block[{$HandlerSet = False}, body];

exceptionHandler[handler_][exception_, _] := handler[exception];

(**************************************************************************************************)

SetHoldC[TryElse, TryElseHandler];

TryElse[body_, else_] := HandleExceptions[body, TryElseHandler[else]];

TryElseHandler[else_][_] := else;

(**************************************************************************************************)

Try::usage =
"Try[body$] catches exceptions and displays them, returning $Failed."

SetHoldC @ Try;

DefinePseudoMacro[Try, Try[body_] :> Try[$MacroParentSymbol, body]];

Try[head_Sym, body_] := HandleExceptions[body, TryHandler[head]];

TryHandler[head_][exception_] := TryHandler[head, exception];
TryHandler[head_, LiteralException[sloc_, value_]]                       := value;
TryHandler[head_, MessageException[sloc_, Inherited, name_Str, args___]] := IssueMessage[head -> sloc, name, args];
TryHandler[head_, MessageException[sloc_, symbol_, name_Str, args___]]   := IssueMessage[symbol -> sloc, name, args];

(**************************************************************************************************)

CatchMessages::usage =
"CatchMessages[head$, body$] catches errors thrown by ThrowMessage.
CatchMessages[body$] uses the current head."

SetHoldC @ CatchMessages;

DefinePseudoMacro[CatchMessages,
CatchMessages[body_] :> CatchMessages[$MacroParentSymbol, body]
];

CatchMessages[head_Symbol, body_] := HandleExceptions[body, CatchAsMessageHandler[head]];

CatchAsMessageHandler[head_][exception_] := CatchAsMessageHandler[head, exception];
CatchAsMessageHandler[head_, LiteralException[sloc_, value_]]                       := value;
CatchAsMessageHandler[head_, MessageException[sloc_, Inherited, name_Str, args___]] := IssueMessage[head -> sloc, name, args];
CatchAsMessageHandler[head_, MessageException[sloc_, symbol_, name_Str, args___]]   := IssueMessage[symbol -> sloc, name, args];
cm:CatchAsMessageHandler[_, __] := (Print[HoldForm[cm]]);

(**************************************************************************************************)

SetHoldC @ CatchAsFailure;

CatchAsFailure::usage =
"CatchAsFailure['name$', body] catches exceptions thrown by ThrowMessage etc, returning a Failure object.
CatchAsFailure['name$', body, fn] applies fn to the failure."

CatchAsFailure[name_, body_, fn_:Identity] := HandleExceptions[body, CatchAsFailureHandler[name, fn]];

CatchAsFailureHandler[name_, fn_][exception_] := CatchAsFailureHandler[name, fn, exception];
CatchAsFailureHandler[name_, fn_, LiteralException[sloc_, value_]]                          := Failure["UnknownFailure", Dict["SourceLocation" -> sloc]];
CatchAsFailureHandler[name_, fn_, MessageException[sloc_, Inherited, msgName_Str, args___]] := fn @ CreateFailure[sloc, General, name, msgName, args];
CatchAsFailureHandler[name_, fn_, MessageException[sloc_, head_, msgName_Str, args___]]     := fn @ CreateFailure[sloc, head, name, msgName, args];

CreateFailure[sloc_, failureName_, symbol_, msgName_, msgArgs___] :=
  Failure[failureName, Dict[
    "MessageTemplate" :> MessageName[symbol, msgName],
    "MessageParameters" -> {msgArgs},
    "SourceLocation"  -> sloc
  ]];

(**************************************************************************************************)

ThrowMessage::usage =
"ThrowMessage['name', body] throws a message to CatchMessages where it is issued.
ThrowMessage['quiet', ...] is equivalent to RaiseException[]."

SetExcepting @ ThrowMessage;

ThrowMessage[sym_Symbol -> msgName_String, msgArgs___] := ThrowException @ MessageException[$SourceLocationStack, sym, msgName, msgArgs];
ThrowMessage[msgName_String, msgArgs___] := ThrowException @ MessageException[$SourceLocationStack, Inherited, msgName, msgArgs];
ThrowMessage["quiet", ___]               := ThrowException @ LiteralException[$SourceLocationStack, $Failed];
m_ThrowMessage                           := ThrowMessage[ThrowMessage -> "invalidThrowMessage", HoldForm @ m];

ThrowMessage::invalidThrowMessage = "Invalid call to ThrowMessage: ``.";

(**************************************************************************************************)

SetExcepting @ SetStrict[SameQOrThrow, SameLenQOrThrow, SameSetQOrThrow, SubsetOfQOrThrow, LookupOrThrow, LookupListOrThrow];

SameQOrThrow[a_, b_, msg_Str, args___]                := If[a === b, True, ThrowMsg[msg, a, b, args]];
SameLenQOrThrow[a_, b_, msg_Str, args___]             := If[Len[a] === Len[b], True, ThrowMsg[msg, Len[a], Len[b], args]];
SameSetQOrThrow[a_, b_, msg_Str, args___]             := If[SameSetQ[a, b], True, ThrowMsg[msg, Compl[a, b], Compl[b, a], args]];
SubsetOfQOrThrow[a_, b_, msg_Str, args___]            := If[SubsetOfQ[a, b], True, ThrowMsg[msg, Compl[a, b], b, args]];
LookupOrThrow[dict_, key_, msg_Str, args___]          := Lookup[dict, Key @ key, ThrowMsg[msg, key, args]];
LookupListOrThrow[dict_, keys_List, msg_Str, args___] := Lookup[dict, keys, ThrowMsg[msg, Compl[keys, Keys @ dict], args]];

(**************************************************************************************************)

SetExcepting @ SetStrict[CheckForUnknownOptions, UnknownOptionMsg, ThrowUnknownOptionMsg];

CheckForUnknownOptions[head_Symbol] := Null;

CheckForUnknownOptions[head_Symbol, key_ -> _] :=
  If[!OptionKeyQ[head, key], ThrowUnknownOptionMsg[head, key]];

CheckForUnknownOptions[head_Symbol, opts___] := Locals[
  validKeys = OptionKeys @ head;
  actualKeys = Keys @ FlatList @ opts;
  badKeys = Compl[actualKeys, validKeys];
  If[NonEmptyQ[badKeys], ThrowUnknownOptionMsg[head, First @ badKeys]];
];

General::unknownOption = "`` is not a known option to ``. Available options are ``.";
UnknownOptionMsg[head_, key_]      := ErrorMessage["unknownOption", key, head, OptionKeys @ head];
ThrowUnknownOptionMsg[head_, key_] := ThrowMessage["unknownOption", key, head, OptionKeys @ head];

(**************************************************************************************************)

SetExcepting @ SetStrict[OptionMsg, ThrowOptionMsg]

OptionMsg[opt_, val_]      := ErrorMessage["invalidOption", opt, val];
ThrowOptionMsg[opt_, val_] := ThrowMessage["invalidOption", opt, val];
General::invalidOption = "The setting `` -> `` is not valid.";

(**************************************************************************************************)

ThrowErrorValue::usage =
"ThrowErrorValue[value$] returns value$ from the containing HandleException-based body."

ThrowErrorValue[value_] := ThrowException @ LiteralException[None, value];

(**************************************************************************************************)

ThrowException::usage =
"ThrowException[] returns a $Failed from the containing HandleException-based body.
ThrowException[exception$] throws an exception, which should be a MessageException, FailureException, or LiteralException."

General::uncaughtMessage = "The message name \"``\" was thrown but not caught. Aborting.";

ThrowException[] := ThrowException @ LiteralException[None, $Failed];
ThrowException[e_] /; $HandlerSet := Throw[e, ExceptionTag];
ThrowException[e_]                := UnhandledException @ e;

ThrowException::invalidThrowException = "Invalid call to ThrowException.";
t_ThrowException := Then[Message[ThrowException::invalidThrowException, HoldForm[t]], ThrowException[]];

(**************************************************************************************************)

UnhandledException[MessageException[sloc_, Inherited, args___]] :=
  UnhandledException @ MessageException[sloc, General, args];

UnhandledException[MessageException[sloc_, sym_, name_, args___]] := Then[
  If[SymbolMessageQ[sym, name], IssueMessage[sym, name, args]];
  unhandledMessage[sloc];
  Abort[]
];

UnhandledException[LiteralException[sloc_, value_]] :=
  Then[unhandledMessage[sloc], Abort[]];

UnhandledException[e_] :=
  Then[unhandledMessage[None], Abort[]];

unhandledMessage[None | {}]  := GeneralMessage["uncaughtException"];
unhandledMessage[sloc_] := GeneralMessage["uncaughtExceptionSrc", sloc];

General::uncaughtException = "Exception occurred without a handler set. Aborting.";
General::uncaughtExceptionSrc = "Exception occurred without a handler set. Aborting. Source location: ``.";

(**************************************************************************************************)

MakeBoxes[sl:SourceLocation[_Str, _Int], StandardForm] := ToBoxes @ fmtSourceLoc[sl];
MakeBoxes[sl:SourceLocation[_Str, _Int], TraditionalForm] := ToBoxes @ fmtSourceLoc[sl];

fmtSourceLoc[SourceLocation[path_Str, int_Int]] :=
  ClickForm[CodeStyle["../" <> FileNameTake[path]], Print[path]; SublimeSeek[path, int]];

(**************************************************************************************************)

SetHoldC[MsgPrePrint, msgBoxes];

MsgPrePrint[loc_SourceLocation] := fmtSourceLoc @ loc;
MsgPrePrint[locs:{___SourceLocation}] := Row[fmtSourceLoc /@ DelDups[locs], ", "];
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


