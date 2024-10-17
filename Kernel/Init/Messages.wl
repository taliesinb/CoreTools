PackageExports[
  "ControlFlow",
    Try, TryElse, ErrorIsThrow,

  "MessageFunction",
    GeneralMessage,
    ErrorMessage,

    CatchMessages,
    CatchAsFailure,

    ThrowException,
    ThrowErrorValue,

    IssueMessage,
    ThrowMessage,

    CheckMessageFree,
    AssertMessageFree,
    ErrorInternal,
    ThrowInternal,

    CheckIntQ, CheckBoolQ, CheckListQ, CheckDictQ, CheckStrQ, CheckNumQ, CheckFnQ,
    AssertIntQ, AssertBoolQ, AssertListQ, AssertDictQ, AssertStrQ, AssertNumQ, AssertFnQ,

    CheckInQ, CheckSameQ, CheckSameLenQ, CheckSameSetQ, CheckSubsetOfQ, CheckLookup, CheckLookupList, CheckHeadQ, CheckOptKeys,
    AssertInQ, AssertSameQ, AssertSameLenQ, AssertSameSetQ, AssertSubsetOfQ, AssertLookup, AssertLookupList, AssertHeadQ, AssertOptKeys,
    ErrorOptIn, ErrorOptKey, ErrorOptVal, CheckOptVal, AssertOptVal,
    ThrowOptIn, ThrowOptKey, ThrowOptVal, AssertOptsValid,

    AssertOpts,
    ErrorOptKeyFn, ErrorOptValFn,
    ThrowOptKeyFn, ThrowOptValFn,

    AssertThat, SetThrowing,

  "DebuggingFunction", Panic,
  "SpecialFunction",   Unimplemented, InternalError,
  "Predicate",         GeneralMessageQ, SymbolMessageQ, MessageNameActiveQ
];

PrivateExports[
  "Function",        AttachSrcLocs, QuietStack, QuietStackID,
  "SymbolicHead",    MessageException, FailureException, LiteralException,
  "ControlFlow",     SetSrcLoc, HandleExceptions, DisableHandleExceptions, UnhandledException, ExceptionHandlerStack,
  "SpecialFunction", TryElseHandler, TryHandler, CatchAsMessageHandler, CatchAsFailureHandler, MessageFreeTrapFn,
  "TagSymbol",       ExceptionTag, MessageFreeTag,
  "MetaFunction",    MakeAsserting,
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
General::invalidUsage      = "Invalid arguments: ``."
General::unimplemented     = "An unimplemented code path was encountered.";
General::internalError     = "An internal error occurred.";
General::internalErrorInfo = "An internal error occurred: ``.";

Unimplemented := ThrowMessage["unimplemented"];
InternalError := ThrowMessage["internalError"];

(**************************************************************************************************)

SetHoldC @ ErrorIsThrow;

ErrorIsThrow[body_] := Block[{ErrorMessage = ThrowMessage}, body];

(**************************************************************************************************)

SetHoldF @ ErrorMessage;

ErrorMessage[msg_Str, args___]                       := IssueMessage[General, msg, args];
ErrorMessage[sym_Sym -> msg_Str, args___]            := IssueMessage[sym, msg, args];
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

"Panic[] calls Abort[], and could save more information in future."

Panic[] := With[
  {stack = ExceptionHandlerStack[]},
  If[stack =!= {}, ErrorPrint["Stack: ", stack]];
  Abort[]
];

(**************************************************************************************************)

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

"Try[body$] catches exceptions and displays them, returning $Failed."

SetHoldC @ Try;

Try[head_Sym, body_] := HandleExceptions[body, TryHandler[head]];

TryHandler[head_][exception_] := TryHandler[head, exception];
TryHandler[head_, LiteralException[sloc_, value_]]                       := value;
TryHandler[head_, MessageException[sloc_, Inherited, name_Str, args___]] := IssueMessage[head -> sloc, name, args];
TryHandler[head_, MessageException[sloc_, symbol_, name_Str, args___]]   := IssueMessage[symbol -> sloc, name, args];

(**************************************************************************************************)

QuietStackID[] := getId @ Lookup[Internal`QuietStatus[], Stack];

getId[HoldForm[{}]] := 0;
getId[h_]           := Part[h, 1, -1, 1];

QuietStack[]    := First @ Apply[Hold, Lookup[Internal`QuietStatus[], Stack], {2}];
QuietStack[id_] := Select[QuietStack[], First /* GreaterThan[id]];

SetHoldF[MessageNameActiveQ]

MessageNameActiveQ[msg:MessageName[_, name_], id_] := FreeQ[QuietStack[id], All | HoldP[msg] | HoldP[General::name]];

(**************************************************************************************************)

"CatchMessages[head$, body$] catches errors thrown by ThrowMessage.
CatchMessages[body$] uses the current head."

SetHoldC @ CatchMessages;

CatchMessages[head_Symbol, body_] := HandleExceptions[body, CatchAsMessageHandler[head]];

CatchAsMessageHandler[head_][exception_] := CatchAsMessageHandler[head, exception];
CatchAsMessageHandler[head_, LiteralException[sloc_, value_]]                       := value;
CatchAsMessageHandler[head_, MessageException[sloc_, Inherited, name_Str, args___]] := IssueMessage[head -> sloc, name, args];
CatchAsMessageHandler[head_, MessageException[sloc_, symbol_, name_Str, args___]]   := IssueMessage[symbol -> sloc, name, args];
cm:CatchAsMessageHandler[_, __] := ErrorPrint["Failure to handle: ", HoldForm[cm]];

(**************************************************************************************************)

SetHoldC @ CatchAsFailure;

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

DeclareSeqScan[MakeAsserting];

toErrorSym1[name_] := toErrorSym2 @ StringReplace[name, {"Throw" -> "Error", "Assert" -> "Check"}];
toErrorSym2[name_] := If[NameQ[name], Symbol @ name, ErrorPrint["No symbol ", name], $dummy];

$toAssertRules = {
  ErrorMessage   -> ThrowMessage,
  ErrorInternal  -> ThrowInternal,
  ErrorOptKey    -> ThrowOptKey,
  ErrorOptVal    -> ThrowOptVal
};

MakeAsserting[throwFn_Symbol] := With[
  {errorFn = toErrorSym1 @ SymName @ throwFn},
  SetExcepting[throwFn]; Attributes[throwFn] = Attributes[errorFn];
  DownValues[throwFn] = DownValues[errorFn] /. (errorFn -> throwFn) /. $toAssertRules;
];

(**************************************************************************************************)

SetHoldC @ CheckMessageFree;

CheckMessageFree[body_] := With[
  {id = QuietStackID[]},
  Catch[
    TrapMessages[body, MessageFreeTrapFn[id]],
    MessageFreeTag[id],
    Seq1 /* ErrorInternal
  ]
];

MessageFreeTrapFn[id_][Hold[msg:Message[msgName_, ___], _]] /; MessageNameActiveQ[msgName, id] :=
  Throw[Hold[msg], MessageFreeTag[id]];

MessageFreeTrapFn[_][_] := Null;

MakeAsserting[AssertMessageFree];

(**************************************************************************************************)

SetStrict[ErrorInternal];

ErrorInternal[]                                 := ErrorMessage["internalError"];
ErrorInternal[msg_Str]                          := ErrorMessage["internalErrorInfo", msg];
ErrorInternal[Hold[msg:Message[msgName_, ___]]] := ErrorMessage["internalErrorInfo", HoldForm @ msgName];

MakeAsserting[ThrowInternal];

(**************************************************************************************************)

SetStrict[CheckIntQ, CheckBoolQ, CheckListQ, CheckDictQ, CheckStrQ, CheckNumQ, CheckFnQ];

CheckIntQ[a_, msg_:"notInt", args___]     := Or[IntQ[a],     ErrorMessage[msg, a, args]; False];
CheckNumQ[a_, msg_:"notNum", args___]     := Or[NumQ[a, b],  ErrorMessage[msg, a, args]; False];
CheckBoolQ[a_, msg_:"notBool", args___]   := Or[BoolQ[a],    ErrorMessage[msg, a, args]; False];
CheckListQ[a_, msg_:"notList", args___]   := Or[ListQ[a],    ErrorMessage[msg, a, args]; False];
CheckDictQ[a_, msg_:"notDict", args___]   := Or[DictQ[a],    ErrorMessage[msg, a, args]; False];
CheckStrQ[a_, msg_:"notStr", args___]     := Or[StrQ[a],     ErrorMessage[msg, a, args]; False];
CheckFnQ[a_, msg_:"notFn", args___]       := Or[MaybeFnQ[a], ErrorMessage[msg, a, args]; False];

General::notInt = "Value not an integer: ``.";
General::notNum = "Value not a number: ``.";
General::notBool = "Value not a boolean: ``.";
General::notList = "Value not a list: ``.";
General::notDict = "Value not a dict: ``.";
General::notStr = "Value not a string: ``.";
General::notFn = "Value not a function: ``.";

DeclaredHere[AssertIntQ, AssertBoolQ, AssertListQ, AssertDictQ, AssertStrQ, AssertNumQ, AssertFnQ];
MakeAsserting[AssertIntQ, AssertBoolQ, AssertListQ, AssertDictQ, AssertStrQ, AssertNumQ, AssertFnQ];

(**************************************************************************************************)

SetStrict[CheckInQ, CheckSameQ, CheckSameLenQ, CheckSameSetQ, CheckSubsetOfQ, CheckLookup, CheckLookupList, CheckHeadQ];

CheckInQ[a_, b_, msg_:"notIn", args___]                  := If[ElementQ[a, b], True, ErrorMessage[msg, a, b, args]; False];
CheckSameQ[a_, b_, msg_:"notSame", args___]              := If[a === b, True, ErrorMessage[msg, a, b, args]; False];
CheckSameLenQ[a_, b_, msg_:"notSameLen", args___]        := If[Len[a] === Len[b], True, ErrorMessage[msg, Len[a], Len[b], args]; False];
CheckSameSetQ[a_, b_, msg_:"notSameSet", args___]        := If[SameSetQ[a, b], True, ErrorMessage[msg, Compl[a, b], Compl[b, a], args]; False];
CheckSubsetOfQ[a_, b_, msg_:"notSubset", args___]        := If[SubsetOfQ[a, b], True, ErrorMessage[msg, Compl[a, b], b, args]; False];
CheckHeadQ[e_, h_Sym, msg_:"notHead", args___]           := If[Head[e] === h, True, ErrorMessage[msg, h, Head @ e, args]; False];
CheckLookup[d_, key_, msg_:"badKey", args___]            := Lookup[d, Key @ key, ErrorMessage[msg, key, args]];
CheckLookupList[d_, keys_List, msg_:"badKeys", args___]  := Lookup[d, keys, ErrorMessage[msg, Compl[keys, Keys @ d], args]];

 DeclaredHere[AssertSameQ, AssertSameLenQ, AssertSameSetQ, AssertSubsetOfQ, AssertLookup, AssertLookupList, AssertHeadQ];
MakeAsserting[AssertSameQ, AssertSameLenQ, AssertSameSetQ, AssertSubsetOfQ, AssertLookup, AssertLookupList, AssertHeadQ];

General::notIn = "Value `` not one of ``.";
General::notSame = "Value `` is not ``.";
General::notSameLen = "Length `` does not match ``.";
General::notSameSet = "Sets not same, with diff `` and ``.";
General::notSubset = "Set `` not a subset of ``.";
General::notHead = "Value had head ``, not ``.";
General::badKey = "Key not found: ``.";
General::badKeys = "Keys not found: ``.";

(**************************************************************************************************)

SetStrict[CheckOptVal, ErrorOptKey, ErrorOptVal];

CheckOptVal[opt_, val_, list_List]          := If[ElementQ[val, list], val, ErrorOptVal[opt, val, list]; First @ list];
CheckOptVal[opt_, val_, test_, def_:Auto]   := If[TrueQ[test[val]], val,    ErrorOptVal[opt, val, test]; def];
CheckOptVal[opt_, val_, test_, def_, desc_] := If[TrueQ[test[val]], val,    ErrorOptVal[opt, val, desc]; def];

ErrorOptKey[head_, key_]          := ErrorMessage["optKeyNotIn", key, head, FullRow[OptionKeys @ head, ","]];
ErrorOptKey[General, key_]        := ErrorMessage["optKey", key];

ErrorOptVal[opt_, val_]           := ErrorMessage["optVal", opt, val];
ErrorOptVal[opt_, val_, in_List]  := ErrorMessage["optValNotIn", opt, val, FullRow[in, ", "]];
ErrorOptVal[opt_, val_, IntQ]     := ErrorMessage["optValNotInt", opt, val];
ErrorOptVal[opt_, val_, BoolQ]    := ErrorMessage["optValNotBool", opt, val];
ErrorOptVal[opt_, val_, NumQ]     := ErrorMessage["optValNotNum", opt, val];
ErrorOptVal[opt_, val_, StrQ]     := ErrorMessage["optValNotStr", opt, val];
ErrorOptVal[opt_, val_, ListQ]    := ErrorMessage["optValNotList", opt, val];
ErrorOptVal[opt_, val_, DictQ]    := ErrorMessage["optValNotDict", opt, val];
ErrorOptVal[opt_, val_, _]        := ErrorMessage["optVal", opt, val];

ErrorOptVal[opt_, val_, desc:(_Str|_StrForm|_LitStr)] := ErrorMessage["optValInfo", opt, val, desc];

 DeclaredHere[AssertOptVal, ThrowOptKey, ThrowOptVal];
MakeAsserting[AssertOptVal, ThrowOptKey, ThrowOptVal];

General::optKey      = "`` is not a known option.";
General::optKeyNotIn = "`` is not a known option to ``, which are: ``.";
General::optVal      = "The setting `` -> `` is not valid.";
General::optValInfo  = "The setting `` -> `` is not valid: ``.";

General::optValNotIn   = "The setting `` -> `` should be one of ``.";
General::optValNotInt  = "The setting `` -> `` should be an integer.";
General::optValNotBool = "The setting `` -> `` should be True or False.";
General::optValNotNum  = "The setting `` -> `` should be a number.";
General::optValNotStr  = "The setting `` -> `` should be a string.";
General::optValNotList = "The setting `` -> `` should be a list.";
General::optValNotDict = "The setting `` -> `` should be a dictionary.";

(**************************************************************************************************)

SetStrict @ CheckOptKeys;

CheckOptKeys[head_Symbol] := Null;
CheckOptKeys[head_Symbol, key_ -> _]            := If[!OptionKeyQ[head, key], ErrorOptKey[head, key]];
CheckOptKeys[head_Symbol, opts___]              := CheckOptKeys[head, OptionKeys @ head, opts];
CheckOptKeys[head_Symbol -> keys_List, opts___] := CheckOptKeys[head, keys, opts];
CheckOptKeys[head_Symbol, validKeys_List, dict_Dict] := CheckOptKeys[head, validKeys, Normal @ dict];
CheckOptKeys[head_Symbol, validKeys_List, opts__] := Module[{actualKeys, badKeys},
  actualKeys = Keys @ FlatList @ opts;
  badKeys = Compl[actualKeys, validKeys];
  If[NonEmptyQ[badKeys], ErrorOptKey[head, First @ badKeys]];
];

DeclaredHere[AssertOptKeys];
MakeAsserting[AssertOptKeys];

(**************************************************************************************************)

ErrorOptKeyFn[head_][key_] := ErrorOptKey[key, key];
ErrorOptValFn[head_][key_] := ErrorOptVal[key, key];
ThrowOptKeyFn[head_][key_] := ThrowOptKey[key, key];
ThrowOptValFn[head_][key_] := ThrowOptVal[key, key];

(**************************************************************************************************)

SetStrict[AssertOptsValid, assertOpt];

AssertOptsValid[rules___Rule] := Scan[assertOpt, {rules}];

assertOpt[key_ -> val_ -> test_] := AssertOptVal[key, val, test];

(**************************************************************************************************)

"ThrowErrorValue[value$] returns value$ from the containing HandleException-based body."

ThrowErrorValue[value_] := ThrowException @ LiteralException[None, value];

(**************************************************************************************************)

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

DefinePseudoMacro[ErrorMessage,
  HoldPattern[ErrorMessage[msg_Str, args___]] :> IssueMessage[$MacroHead, msg, args]
];

DefinePseudoMacro[Try,
  HoldPattern[Try[body_]] :> Try[$MacroHead, body]
];

DefinePseudoMacro[CatchMessages,
  HoldPattern[CatchMessages[body_]] :> CatchMessages[$MacroHead, body]
];
