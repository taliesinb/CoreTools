SystemExports[
  "Function",
    ColorToHex, ColorListToHex, HexToColorList,
    ChooseNumericColorFunction, ApplyAutomaticColoring,
  "Variable",
    $MediumColorPalette, $MediumColorPalette, $DarkColorPalette, $LightColorPalette, $BooleanColors, $DiscreteColorPalette,
    $Blue, $Red, $Yellow, $Green, $Pink, $Teal, $Orange, $Purple, $Gray, $White, $Black,
    $DarkBlue, $DarkRed, $DarkYellow, $DarkGreen, $DarkPink, $DarkTeal, $DarkGray, $DarkOrange, $DarkPurple, $DarkWhite, $DarkBlack,
    $LightRed, $LightBlue, $LightGreen, $LightOrange, $LightPurple, $LightTeal, $LightGray, $LightPink, $LightYellow, $LightWhite, $LightBlack
];

(*************************************************************************************************)

DeclareListable[ColorToHex]

ColorToHex[c:ColorP] := toHexColor @ ColorToRGBArray @ c;

toHexColor[c_List] := toHexColor[c] = StringJoin["#", IntegerString[Floor[(255 * c) + 1 / 2], 16, 2]];

(*************************************************************************************************)

ColorListToHex[list_ ? ColorVectorQ] := StringRiffle[ColorToHex @ list, " "];
HexToColorList[str_Str] := Map[RGBColor, StringSplit @ str];

$MediumColorPalette   = ColorPalette["Medium"]   = HexToColorList @ "#e1432d #3e81c3 #4ea82a #dc841a #8b7ebe #47a5a7 #929292 #c74883 #f6e259 #f3f3f3 #404040";
$DarkColorPalette     = ColorPalette["Dark"]     = HexToColorList @ "#b50700 #165e9d #217f00 #ae5900 #665996 #0e7c7e #6b6b6b #9e1f61 #bba700 #b4b4b4 #2d2d2d";
$LightColorPalette    = ColorPalette["Light"]    = HexToColorList @ "#ff775e #6caff4 #82dd63 #ffbb5f #bbaff2 #7fdbdc #c5c5c5 #fb77b0 #ffffa9 #ffffff #595959";
$DiscreteColorPalette = ColorPalette["Discrete"] = HexToColorList @ "#da3b26 #eebb40 #4ba526 #4aa59d #4184c6 #ca4a86 #6b6b6b #929292 #c5c5c5";

{$Red, $Blue, $Green, $Orange, $Purple, $Teal, $Gray, $Pink, $Yellow, $White, $Black} = $MediumColorPalette;
{$DarkRed, $DarkBlue, $DarkGreen, $DarkOrange, $DarkPurple, $DarkTeal, $DarkGray, $DarkPink, $DarkYellow, $DarkWhite, $DarkBlack} = $DarkColorPalette;
{$LightRed, $LightBlue, $LightGreen, $LightOrange, $LightPurple, $LightTeal, $LightGray, $LightPink, $LightYellow, $LightWhite, $LightBlack} = $LightColorPalette;

$BooleanColors = {GrayLevel[0.9], GrayLevel[0.1]};
