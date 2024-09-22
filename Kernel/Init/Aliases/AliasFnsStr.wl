PackageExports[
  "Function",
    Char1, CharN, StrFirst, StrRest, StrMost, StrLast, StrFirstRest, StrMostLast,
    StrPosL, StrPosR, StrCaseF, StrCaseL,
    StrSelect, StrDiscard, StrSelectDiscard,
    StrSplitPos, StrSplitL, StrSplitR,
    StrSegment, StrSegmentL, StrSegmentR,
    StrPre, StrApp,
    NatStr, IntStr, HexStr
];

(*************************************************************************************************)

DefineAliasRules[
  Char1            -> StringFirst,
  CharN            -> StringLast,
  StrFirst         -> StringFirst,
  StrRest          -> StringRest,
  StrMost          -> StringMost,
  StrLast          -> StringLast,
  StrFirstRest     -> StringFirstRest,
  StrMostLast      -> StringMostLast,
  StrPosL          -> StringPositionLeft,
  StrPosR          -> StringPositionRight,
  StrCaseF         -> StringCaseFirst,
  StrCaseL         -> StringCaseLast,
  StrSelect        -> StringSelect,
  StrDiscard       -> StringDiscard,
  StrSelectDiscard -> StringSelectDiscard,
  StrSplitPos      -> StringSplitPositions,
  StrSplitL        -> StringSplitBefore,
  StrSplitR        -> StringSplitAfter,
  StrSegment       -> StringSegment,
  StrSegmentL      -> StringSegmentBefore,
  StrSegmentR      -> StringSegmentAfter,
  StrPre           -> StringPrepend,
  StrApp           -> StringAppend
];

(*************************************************************************************************)

DefineAliasRules[
  NatStr           -> NaturalNumberString,
  IntStr           -> FullIntegerString,
  HexStr           -> HexString
];
