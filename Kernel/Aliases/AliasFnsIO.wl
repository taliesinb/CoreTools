PackageExports[
  "IOFunction",
    ReadRawJSONFile, ReadRawJSONStream, WriteRawJSONFile, WriteRawJSONStream, CellInformation,
  "SpecialFn",
    ToInputStr, HToInputStr, FromInputStr,
  "Function",
    ReadRawJSONString, WriteRawJSONString,
    DecodeBase64, EncodeBase64, DecodeBase64ToByteArray,
    StringHash
];

(*************************************************************************************************)

DefineAliasRules[
  HToInputStr              -> HoldToInputString,
  ToInputStr               -> ToInputString,
  FromInputStr             -> FromInputString
];

DefineAliasRules[
  ReadRawJSONFile          -> Developer`ReadRawJSONFile,
  ReadRawJSONStream        -> Developer`ReadRawJSONStream,
  WriteRawJSONFile         -> Developer`WriteRawJSONFile,
  WriteRawJSONStream       -> Developer`WriteRawJSONStream,
  CellInformation          -> Developer`CellInformation
];

DefineAliasRules[
  ReadRawJSONString        -> Developer`ReadRawJSONString,
  WriteRawJSONString       -> Developer`WriteRawJSONString,
  DecodeBase64             -> Developer`DecodeBase64,
  EncodeBase64             -> Developer`EncodeBase64,
  DecodeBase64ToByteArray  -> Developer`DecodeBase64ToByteArray
];

DefineAliasRules[
  StringHash               -> Data`StringHash
];