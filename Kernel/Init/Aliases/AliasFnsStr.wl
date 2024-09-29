PackageExports[
  "Function",
    Char1, CharN, StrFirst, StrRest, StrMost, StrLast, StrFirstRest, StrMostLast,
    StrPosL, StrPosR, StrCaseF, StrCaseL,
    StrSelect, StrDiscard, StrSelectDiscard,
    StrSplitPos, StrSplitL, StrSplitR,
    StrSegment, StrSegmentL, StrSegmentR,
    StrPre, StrApp,
    NatStr, IntStr, HexStr,
    StrLines,
    DelimStr, DelimStrRow,
    BraceStr, BraceStrRow,
    AngleStr, AngleStrRow,
    ParenStr, ParenStrRow,
    BracketStr, BracketStrRow,
    DQuotedStr, SQuotedStr,
  "StrPatSym",
    WhiteC, DigitC, HexC, LetterC, WordC, PuncC, WordB, WhiteSP,
    SpaceC, UCaseC, LCaseC, AlphaC, AlphaNumC,
    UCaseSP, LCaseSP, AlphaSP, AlphaNumSP, DigitSP, HexSP, LetterSP, WordSP, SpaceSP, MSpaceSP,
  "StrPatHead",
    RegexSP, MaybeSP, CharSP,
  "Function",
    ParseStrPat, ToRegex
];

(*************************************************************************************************)

DefineAliasRules[
  RegexSP     -> StringPattern`Dump`RE,
  MaybeSP     -> StringPattern`Dump`QuestionMark,
  CharSP      -> StringPattern`Dump`CharacterGroup,
  ParseStrPat -> StringPattern`PatternConvert
];

DefineAliasRules[
  WhiteC     -> WhitespaceCharacter,
  DigitC     -> DigitCharacter,
  HexC       -> HexadecimalCharacter,
  LetterC    -> LetterCharacter,
  WordC      -> WordCharacter,
  PuncC      -> PunctuationCharacter,
  WordB      -> WordBoundary,
  WhiteSP    -> Whitespace
];

DefineLiteralRules[
  UCaseC      -> Evaluate[CharSP["[:upper:]"]],
  LCaseC      -> Evaluate[CharSP["[:lower:]"]],
  AlphaC      -> Evaluate[CharSP["[:alpha:]"]],
  AlphaNumC   -> Evaluate[CharSP["[:alnum:]"]],
  SpaceC      -> Evaluate[CharSP["[:space:]"]]
];

DefineLiteralRules[
  UCaseSP     -> UCaseC..,
  LCaseSP     -> LCaseC..,
  AlphaSP     -> AlphaC..,
  AlphaNumSP  -> AlphaC..,
  DigitSP     -> DigitC..,
  HexSP       -> HexC..,
  LetterSP    -> LetterC..,
  WordSP      -> WordC..,
  SpaceSP     -> SpaceC..,
  MSpaceSP    -> SpaceC...
];

(*************************************************************************************************)

ToRegex[patt_] := Module[
  {result = ParseStrPat @ patt},
  If[ListQ[result], Regex @ StringTrimLeft[First @ result, "(?ms)"], $Failed]
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
  StrApp           -> StringAppend,
  StrLines         -> StringLines,
  DelimStr         -> DelimitedString,
  DelimStrRow      -> DelimitedStringRow,
  BraceStr         -> BraceString,
  BraceStrRow      -> BraceStringRow,
  AngleStr         -> AngleString,
  AngleStrRow      -> AngleStringRow,
  ParenStr         -> ParenString,
  ParenStrRow      -> ParenStringRow,
  BracketStr       -> BracketString,
  BracketStrRow    -> BracketStringRow,
  DQuotedStr       -> DQuotedString,
  SQuotedStr       -> SQuotedString
];

(*************************************************************************************************)

DefineAliasRules[
  NatStr           -> NaturalNumberString,
  IntStr           -> FullIntegerString,
  HexStr           -> HexString
];
