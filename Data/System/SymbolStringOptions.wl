nameOptionKeys[name_String] := If[NameQ[name], ToExpression[name, InputForm, HoldOptionKeys], $Failed];

$sysNames = Names["System`*"];
$sysOptions = Quiet @ AssociationMap[nameOptionKeys, $sysNames];
$sysOptions //= Select[ListQ];
$sysOptions //= Map[Select[StringQ]];
$sysOptions //= Select[NotEmptyQ];

$sysOptions