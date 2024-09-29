PackageExports[
  "MessageFunction", ReturnFailed, ReturnMessage
];

(*************************************************************************************************)

SetExcepting @ SetHoldF @ ReturnMessage;

ReturnMessage[args___] := Return @ ErrorMessage[args];

ReturnMessage /: SetDelayed[$LHS_, ReturnMessage[args___]] :=
  SetDelayed @@ Hold[$LHS, ErrorMessage[args]];

DefinePartialMacro[ReturnMessage,
  HoldPattern[ReturnMessage[args___]] :> FunctionReturn[ErrorMessage[args]]
];

(*************************************************************************************************)

SimpleMacroDefs[
  ReturnFailed[]                    := Return[$Failed, Block],
  ReturnFailed[msg_String, args___] := (issueMessage[$MacroHead, msg, args]; Return[$Failed, Block])
];

ReturnFailed[___] := Return[$Failed];

issueMessage[$MacroHead, msgName_String, args___] := issueMessage[General, msgName, args];
issueMessage[msgSym_, msgName_String, args___]    := Message[MessageName[msgSym, msgName], args];
