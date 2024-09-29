PackageExports[
  "Function", MessagesFileFind,
  "Variable", $InstallationMessagesFile
];

(**************************************************************************************************)

Initially[
  $InstallationMessagesFile = PathJoin[$InstallationDirectory, "SystemFiles", "Kernel", "TextResources", "English", "Messages.m"];
];

MessagesFileFind[glob_Str] := Find[$InstallationMessagesFile, glob];