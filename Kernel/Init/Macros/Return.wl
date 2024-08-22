PackageExports[
  "MessageFunction", ReturnFailed, ReturnMessage
];

(*************************************************************************************************)

SetExcepting @ SetHoldF @ ReturnMessage;

DefinePartialMacro[ReturnMessage,
  ReturnMessage[args___] :> FunctionReturn[ErrorMessage[args]]
];

ReturnMessage /: SetDelayed[$LHS_, ReturnMessage[args___]] :=
  SetDelayed @@ Hold[$LHS, ErrorMessage[args]];

(*************************************************************************************************)

DefineSimpleMacro[ReturnFailed, {
  ReturnFailed[]                    :> Return[$Failed, Block],
  ReturnFailed[msg_String, args___] :> (issueMessage[$MacroParentSymbol, msg, args]; Return[$Failed, Block])
}]

issueMessage[$MacroParentSymbol, msgName_String, args___] := issueMessage[General, msgName, args];
issueMessage[msgSym_, msgName_String, args___] := Message[MessageName[msgSym, msgName], args];
