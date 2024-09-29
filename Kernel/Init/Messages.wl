PackageExports[
  "ControlFlow",
    Try, TryElse,

  "MessageFunction",
    GeneralMessage,
    ErrorMessage,

    CatchMessages,
    CatchAsFailure,

    ThrowException,
    ThrowErrorValue,

    ThrowOnUnknownOptions, MessageOnUnknownOptions,
    IssueMessage, IssueUnknownOptionMessage, IssueOptionMessage,
    ThrowMessage, ThrowUnknownOptionMessage, ThrowOptionMessage,

    SameQOrThrow, SameLenQOrThrow, SameSetQOrThrow, SubsetOfQOrThrow, LookupOrThrow, LookupListOrThrow,
    AssertThat,

  "DebuggingFunction", Panic,
  "SpecialFunction",   Unimplemented, InternalError,
  "Predicate",         GeneralMessageQ, SymbolMessageQ
];

PrivateExports[
  "Function",        AttachSrcLocs,
  "SymbolicHead",    MessageException, FailureException, LiteralException,
  "ControlFlow",     SetSrcLoc, HandleExceptions, DisableHandleExceptions, UnhandledException, ExceptionHandlerStack,
  "BoxFunction",     SrcLoxBox,
  "Predicate",       SrcLocQ,
  "SpecialFunction", TryElseHandler, TryHandler, CatchAsMessageHandler, CatchAsFailureHandler,
  "TagSymbol",       ExceptionTag,
  "SpecialVariable", $HandlerSet, $SrcLocStack
];

(**************************************************************************************************)

(*
Ideas:
ErrorMessage["Foo", 3, 4] issues a message against General, evals to $Failed.
ErrorMessage[Foo::name, 3, 4] or ErrorMessage[Foo -> "name", 3, 4].
ThrowErrorMessage[...] postpones this, throwing and catching at a CatchMessages, where the symbol is known.
*)

(* this is a general catch-all. *)
General::badUsage          = "`` is not a valid usage."
General::unknownOption     = "`` is not a known option to ``, which are: ``.";
General::unknownOptionAnon = "`` is not a known option.";
General::invalidOption     = "The setting `` -> `` is not valid.";
General::unimplemented     = "An unimplemented code path was encountered.";
General::internalError     = "An internal error occurred.";

Unimplemented := ThrowMessage["unimplemented"];
InternalError := ThrowMessage["internalError"];

(**************************************************************************************************)

SetExcepting @ SetHoldF @ ErrorMessage;

PseudoMacroDef[
  ErrorMessage[msg_Str, args___] := IssueMessage[$MacroHead, msg, args]
];

ErrorMessage[msg_Str, args___]                       := IssueMessage[General, msg, args];
ErrorMessage[MessageName[sym_Sym, msg_Str], args___] := IssueMessage[sym, msg, args];

e_ErrorMessage := GeneralMessage["invalidMessage", ErrorMessage, HoldForm @ e];

(**************************************************************************************************)

SetPred1 @ GeneralMessageQ;

GeneralMessageQ[name_Str] := StringQ[MessageName[General, name]];

GeneralMessage[name_Str, args___] := Then[Message[MessageName[General, name], args], $Failed];

(**************************************************************************************************)

SetHoldF @ SetPred2 @ SymbolMessageQ;

SymbolMessageQ[sym_Sym, name_Str] := StringQ[MessageName[sym, name]] || GeneralMessageQ[name];

(**************************************************************************************************)

SetHoldR @ SetSrcLoc;

SetSrcLoc[loc_, body_] := Block[{$SrcLocStack = Append[$SrcLocStack, loc]}, body];

(**************************************************************************************************)

AttachSrcLocs[hc_] := With[
  {loc = SourceLocation[]},
  ReplaceAll[hc, {
    w_SetSrcLoc                   :> w,
    e:($ExceptingSymP[___])             :> SetSrcLoc[loc, e],
    u:HoldP[Unimplemented | InternalError] :> SetSrcLoc[loc, u]
  }]
];

$SrcLocStack = {};

(**************************************************************************************************)

Panic::usage = "Panic[] calls Abort[], and could save more information in future.";

Panic[] := With[
  {stack = ExceptionHandlerStack[]},
  If[stack =!= {}, ErrorPrint["Stack: ", stack]];
  Abort[]
];

(**************************************************************************************************)

IssueMessage::usage =
"IssueMessage[head$, 'name$', args$$] issues the message with name 'name$' against symbol head$.
* if the message doesn't exist a fallback message will be issued."

IssueMessage[head_ -> slocs_, args___] := Then[
  If[slocs =!= {}, ErrorPrint @ Row[DelDups @ ToList[$SrcLocStack, slocs], " > "]];
  IssueMessage[head, args]
];

IssueMessage[msgHead_Sym, msgName_String, msgArgs___] := Then[
  If[!SymbolMessageQ[msgHead, msgName],
    Message[MessageName[msgHead, "unknownMessage"], msgName, msgHead, PrivSeq @ msgArgs],
    If[!StringQ[MessageName[msgHead, msgName]],
      MessageName[msgHead, msgName] = MessageName[General, msgName]]; (* ? *)
    Message[MessageName[msgHead, msgName], msgArgs]
  ],
  $Failed
];

e_IssueMessage := GeneralMessage["invalidMessage", IssueMessage, HoldForm @ e];

General::unknownMessage = "No message called '``' on symbol '``' or General. Message arguments were: ``.";
General::invalidMessage = "An invalid call to `` was encountered: ``.";

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

ExceptionHandlerStack[] := Cases[
     Stack[Block[_, Catch[_, ExceptionTag, exceptionHandler @ _]]],
  HoldForm[Block[_, Catch[_, ExceptionTag, exceptionHandler @ head_]]] :>
    fromHandler @ head
];

fromHandler[CatchAsMessageHandler[sym_Sym]] := sym;
fromHandler[e_] := e;

(**************************************************************************************************)

SetHoldC[TryElse, TryElseHandler];

TryElse[body_, else_] := HandleExceptions[body, TryElseHandler[else]];

TryElseHandler[else_][_] := else;

(**************************************************************************************************)

Try::usage =
"Try[body$] catches exceptions and displays them, returning $Failed."

SetHoldC @ Try;

PseudoMacroDef[
  Try[body_] := Try[$MacroHead, body]
];

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

PseudoMacroDef[
  CatchMessages[body_] := CatchMessages[$MacroHead, body]
];

CatchMessages[head_Symbol, body_] := HandleExceptions[body, CatchAsMessageHandler[head]];

CatchAsMessageHandler[head_][exception_] := CatchAsMessageHandler[head, exception];
CatchAsMessageHandler[head_, LiteralException[sloc_, value_]]                       := value;
CatchAsMessageHandler[head_, MessageException[sloc_, Inherited, name_Str, args___]] := IssueMessage[head -> sloc, name, args];
CatchAsMessageHandler[head_, MessageException[sloc_, symbol_, name_Str, args___]]   := IssueMessage[symbol -> sloc, name, args];
cm:CatchAsMessageHandler[_, __] := ErrorPrint["Failure to handle: ", HoldForm[cm]];

(**************************************************************************************************)

SetHoldC @ CatchAsFailure;

CatchAsFailure::usage =
"CatchAsFailure['name$', body] catches exceptions thrown by ThrowMessage etc, returning a Failure object.
CatchAsFailure['name$', body, fn] applies fn to the failure."

CatchAsFailure[name_, body_, fn_:Identity] := HandleExceptions[body, CatchAsFailureHandler[name, fn]];

CatchAsFailureHandler[name_, fn_][exception_] := CatchAsFailureHandler[name, fn, exception];
CatchAsFailureHandler[name_, fn_, LiteralException[sloc_, value_]]                          := Failure["UnknownFailure", Dict["SourceLocation" -> sloc]];
CatchAsFailureHandler[name_, fn_, MessageException[sloc_, Inherited, msgName_Str, args___]] := fn @ createFailure[sloc, General, name, msgName, args];
CatchAsFailureHandler[name_, fn_, MessageException[sloc_, head_, msgName_Str, args___]]     := fn @ createFailure[sloc, head, name, msgName, args];
cf:CatchAsFailureHandler[_, __] := ErrorPrint["Failure to handle: ", HoldForm[cm]];

(**************************************************************************************************)

createFailure[sloc_, failureName_, symbol_, msgName_, msgArgs___] :=
  Failure[failureName, Dict[
    "MessageTemplate" :> MessageName[symbol, msgName],
    "MessageParameters" -> {msgArgs},
    "SourceLocation"  -> sloc
  ]];

(**************************************************************************************************)

ThrowMessage::usage =
"ThrowMessage['name', body] throws a message to CatchMessages where it is issued.
ThrowMessage['quiet', ...] is equivalent to RaiseException[]."

SetExcepting @ SetHoldF @ ThrowMessage;

ThrowMessage[MessageName[sym_Sym, name_Str], args___] := iThrowMessage[sym -> name, args];
ThrowMessage[args___]                                 := iThrowMessage[args];

iThrowMessage[sym_Sym -> msgName_Str, msgArgs___] := ThrowException @ MessageException[$SrcLocStack, sym, msgName, msgArgs];
iThrowMessage[msgName_String, msgArgs___]         := ThrowException @ MessageException[$SrcLocStack, Inherited, msgName, msgArgs];
iThrowMessage["quiet", ___]                       := ThrowException @ LiteralException[$SrcLocStack, $Failed];
iThrowMessage[args___]                            := ThrowException @ MessageException[$SrcLocStack, ThrowMessage, "invalidThrowMessage", HoldForm @ ThrowMessage[args]];

ThrowMessage::invalidThrowMessage = "Invalid call to ThrowMessage: ``.";

(**************************************************************************************************)

SetExcepting @ SetStrict[SameQOrThrow, SameLenQOrThrow, SameSetQOrThrow, SubsetOfQOrThrow, LookupOrThrow, LookupListOrThrow];

SameQOrThrow[a_, b_, msg_Str, args___]                := If[a === b, True, ThrowMessage[msg, a, b, args]];
SameLenQOrThrow[a_, b_, msg_Str, args___]             := If[Len[a] === Len[b], True, ThrowMessage[msg, Len[a], Len[b], args]];
SameSetQOrThrow[a_, b_, msg_Str, args___]             := If[SameSetQ[a, b], True, ThrowMessage[msg, Compl[a, b], Compl[b, a], args]];
SubsetOfQOrThrow[a_, b_, msg_Str, args___]            := If[SubsetOfQ[a, b], True, ThrowMessage[msg, Compl[a, b], b, args]];
LookupOrThrow[dict_, key_, msg_Str, args___]          := Lookup[dict, Key @ key, ThrowMessage[msg, key, args]];
LookupListOrThrow[dict_, keys_List, msg_Str, args___] := Lookup[dict, keys, ThrowMessage[msg, Compl[keys, Keys @ dict], args]];

(**************************************************************************************************)

SetExcepting @ SetStrict[ThrowOnUnknownOptions, MessageOnUnknownOptions];

ThrowOnUnknownOptions[head_Symbol] := Null;

ThrowOnUnknownOptions[head_Symbol, key_ -> _] :=
  If[!OptionKeyQ[head, key], ThrowUnknownOptionMessage[head, key]];

ThrowOnUnknownOptions[head_Symbol, opts___] := Locals[
  validKeys = OptionKeys @ head;
  actualKeys = Keys @ FlatList @ opts;
  badKeys = Compl[actualKeys, validKeys];
  If[NonEmptyQ[badKeys], ThrowUnknownOptionMessage[head, First @ badKeys]];
];

MessageOnUnknownOptions[args___] := Block[
  {ThrowUnknownOptionMessage = IssueUnknownOptionMessage},
  ThrowOnUnknownOptions[args]
];

(**************************************************************************************************)

Clear[IssueUnknownOptionMessage, ThrowUnknownOptionMessage];

SetExcepting @ SetCurry1[IssueUnknownOptionMessage, ThrowUnknownOptionMessage]

IssueUnknownOptionMessage[head_, key_]   := ErrorMessage["unknownOption", key, head, UnlimitedRow[OptionKeys @ head, ","]];
ThrowUnknownOptionMessage[head_, key_]   := ThrowMessage["unknownOption", key, head, UnlimitedRow[OptionKeys @ head, ","]];
IssueUnknownOptionMessage[General, key_] := ErrorMessage["unknownOptionAnon", key];
ThrowUnknownOptionMessage[General, key_] := ThrowMessage["unknownOptionAnon", key];

(**************************************************************************************************)

SetExcepting @ SetStrict[IssueOptionMessage, ThrowOptionMessage]

IssueOptionMessage[opt_, val_] := ErrorMessage["invalidOption", opt, val];
ThrowOptionMessage[opt_, val_] := ThrowMessage["invalidOption", opt, val];

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
  IssueMessage[sym, name, args],
  unhandledMessage[sloc];
  Panic[]
];

UnhandledException[LiteralException[sloc_, value_]] :=
  Then[unhandledMessage[sloc], Panic[]];

UnhandledException[e_] :=
  Then[unhandledMessage[None], Panic[]];

unhandledMessage[None | {}]  := GeneralMessage["uncaughtException"];
unhandledMessage[sloc_] := GeneralMessage["uncaughtExceptionSrc", sloc];

General::uncaughtException = "Exception occurred without a handler set. Aborting.";
General::uncaughtExceptionSrc = "Exception occurred without a handler set. Aborting. Source location: ``.";

(**************************************************************************************************)

SetPred1 @ SetHoldC @ SrcLocQ;

SrcLocQ[SrcLoc[StrP, IntP]] := True;

MakeBox[sl_SrcLoc ? srcLocQ, StandardForm] := SrcLoxBox @ sl;

(**************************************************************************************************)

SetHoldC[SrcLoxBox];

SrcLoxBox[SrcLoc[path_Str, line_Int]] := ClickBox[
  CodeStyleBox @ StrJoin["../", FileNameTake @ path, ":", IntStr @ line],
  LogPrint[path -> line]; SublimeSeek[path, line]
];

SrcLoxBox[expr_] := MakeBoxes @ expr

