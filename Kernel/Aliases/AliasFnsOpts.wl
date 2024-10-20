PackageExports[
  "Function",     OptVals, MethodVals, TakeOpts, DropOpts,
  "HoldFunction", HOptVals, HMethodVals, HArgsOnly, HArgsAndRules, HTestOpts,
  "Predicate",    OptRuleTreeQ
];

(*************************************************************************************************)

DefineAliasRules[
  ORuleTreeQ    -> OptionQ
];

(* ORuleTreeQ accepts Rule, {Rule...} but also {Rule..., {Rule...}} *)

DefineAliasRules[
  OptVals       -> System`Utilities`GetOptionValues,
  MethodVals    -> System`Utilities`GetMethodOptionValues,
  TakeOpts      -> System`Utilities`FilterOptions,
  DropOpts      -> System`Utilities`FilterOutOptions
];

DefineAliasRules[
  HOptVals      -> System`Utilities`GetHeldOptionValues,
  HMethodVals   -> System`Utilities`GetHeldMethodOptionValues,
  HArgsOnly     -> System`Private`Arguments,
  HArgsAndRules -> System`Private`ArgumentsWithRules,
  HTestOpts     -> System`Private`TestOptions
];



