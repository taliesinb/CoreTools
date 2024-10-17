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

SetHoldF[ReturnFailed];

SimpleMacroDefs[
  ReturnFailed[]                                := Return[$Failed, Block],
  ReturnFailed[msg_MessageName, args___]        := Return[Message[msg, args]; $Failed, Block],
  ReturnFailed[head_Sym -> msg_Str, args___]    := Return[Message[head::msg, args]; $Failed, Block],
  ReturnFailed[msg_Str, args___]                := Return[issueMessage[$MacroHead, msg, args]; $Failed, Block]
];

ReturnFailed[e___] := Return[ErrorPrint[e]; $Failed];

issueMessage[$MacroHead, msgName_String, args___] := issueMessage[General, msgName, args];
issueMessage[msgSym_, msgName_String, args___]    := Message[MessageName[msgSym, msgName], args];
