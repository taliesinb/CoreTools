nameAttributes[name_String] := If[NameQ[name], ToExpression[name, InputForm, Attributes], $Failed];

$sysNames = Names["System`*"];
$sysAttrs = Quiet @ AssociationMap[nameAttributes, $sysNames];
$sysAttrs //= Select[ListQ];
$sysAttrs //= Map[DeleteCases[Protected | ReadProtected]];
$sysAttrs //= Select[NotEmptyQ];

LevelIndex[$sysAttrs, 2]