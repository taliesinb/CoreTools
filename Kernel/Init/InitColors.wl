SystemExports[
  "Function",
    ColorToHex, ColorListToHex, HexToColorList, ColorPalette, NiceColor, SetOpacity, RemoveOpacity,
  "Variable",
    $SystemColorPalette, $LightColorPalette, $MediumColorPalette, $DarkColorPalette, $DiscreteColorPalette, $BoolColors,
    $LightRed, $LightBlue, $LightGreen, $LightOrange, $LightPurple, $LightTeal, $LightGray, $LightPink, $LightYellow, $LightWhite, $LightBlack,
    $Red, $Blue, $Green, $Orange, $Purple, $Teal, $Gray, $Pink, $Yellow, $White, $Black, $LightDim, $MediumDim, $VeryDim,
    $DarkRed, $DarkBlue, $DarkGreen, $DarkOrange, $DarkPurple, $DarkTeal, $DarkGray, $DarkPink, $DarkYellow, $DarkWhite, $DarkBlack
];

PackageExports[
  "Function",
    NiceColor
];

(*************************************************************************************************)

SetListable[ColorToHex]

ColorToHex[c:ColorP] := toHexColor @ ColorToRGBArray @ c;

toHexColor[c_List] := toHexColor[c] = StringJoin["#", IntegerString[Floor[(255 * c) + 1 / 2], 16, 2]];

(*************************************************************************************************)

ColorListToHex[list_ ? ColorVectorQ] := StringRiffle[ColorToHex @ list, " "];
HexToColorList[str_Str] := Map[RGBColor, StringSplit @ str];

(*************************************************************************************************)

DeclaredHere[ColorPalette];

$SystemColorPalette   = ColorPalette["System"]   = {Red, Blue, Green, Orange, Purple, Cyan, Gray, Pink, Yellow, LightGray, GrayLevel[0.2]};
$LightColorPalette    = ColorPalette["Light"]    = HexToColorList @ "#ff775e #6caff4 #82dd63 #ffbb5f #bbaff2 #7fdbdc #c5c5c5 #fb77b0 #ffffa9 #ffffff #595959";
$MediumColorPalette   = ColorPalette["Medium"]   = HexToColorList @ "#e1432d #3e81c3 #4ea82a #dc841a #8b7ebe #47a5a7 #929292 #c74883 #f6e259 #f3f3f3 #404040";
$DarkColorPalette     = ColorPalette["Dark"]     = HexToColorList @ "#b50700 #165e9d #217f00 #ae5900 #665996 #0e7c7e #6b6b6b #9e1f61 #bba700 #b4b4b4 #2d2d2d";
$DiscreteColorPalette = ColorPalette["Discrete"] = HexToColorList @ "#da3b26 #eebb40 #4ba526 #4aa59d #4184c6 #ca4a86 #6b6b6b #929292 #c5c5c5";

DeclaredHere[$LightRed, $LightBlue, $LightGreen, $LightOrange, $LightPurple, $LightTeal, $LightGray, $LightPink, $LightYellow, $LightWhite, $LightBlack];
DeclaredHere[$Red, $Blue, $Green, $Orange, $Purple, $Teal, $Gray, $Pink, $Yellow, $White, $Black];
DeclaredHere[$DarkRed, $DarkBlue, $DarkGreen, $DarkOrange, $DarkPurple, $DarkTeal, $DarkGray, $DarkPink, $DarkYellow, $DarkWhite, $DarkBlack];

{$LightRed, $LightBlue, $LightGreen, $LightOrange, $LightPurple, $LightTeal, $LightGray, $LightPink, $LightYellow, $LightWhite, $LightBlack} = $LightColorPalette;
{$Red, $Blue, $Green, $Orange, $Purple, $Teal, $Gray, $Pink, $Yellow, $White, $Black} = $MediumColorPalette;
{$DarkRed, $DarkBlue, $DarkGreen, $DarkOrange, $DarkPurple, $DarkTeal, $DarkGray, $DarkPink, $DarkYellow, $DarkWhite, $DarkBlack} = $DarkColorPalette;

$BoolColors = {GrayLevel[0.9], GrayLevel[0.1]};

{$LightDim, $MediumDim, $VeryDim} = Opacity /@ {0.6, 0.4, 0.2};

(*************************************************************************************************)

NiceColor[c_ -> o_]                         := NiceColor[c, o];
NiceColor[0]                                := Black;
NiceColor[i_Int /; 1 <= i <= 8, o_:Inherit] := SetOpacity[Part[$DiscreteColorPalette, i], o];
NiceColor[p:UnitRealP, o_:Inherit]          := SetOpacity[GrayLevel[p], o];
NiceColor[c:ColorP, o_:Inherit]             := SetOpacity[Replace[c, $toNiceCols], o];
NiceColor[___]                              := Pink;

$toNiceCols = Dispatch @ RuleThread[Take[$SystemColorPalette, 9], Take[$MediumColorPalette, 9]];

(**************************************************************************************************)

SetOpacity[c_, Inherit]                     := c;
SetOpacity[c_, None]                        := RemoveOpacity @ c;
SetOpacity[GrayLevel[r_, ___], o_]          := GrayLevel[r, o];
SetOpacity[RGBColor[{r_, g_, b_, ___}], o_] := RGBColor[{r, g, b, o}];
SetOpacity[RGBColor[r_, g_, b_, ___], o_]   := RGBColor[r, g, b, o];
SetOpacity[expr_, o_]                       := expr /. $setOpacityRD /. ($opacity -> o)

SetOpacity[o_][c_] := SetOpacity[c, o];

$setOpacityRD = Dispatch[{
  RGBColor[r_, g_, b_] :> RGBColor[r, g, b, $opacity],
  RGBColor[{r_, g_, b_}] :> RGBColor[r, g, b, $opacity],
  GrayLevel[g_] :> GrayLevel[g, $opacity],
  Hue[h_] :> Hue[h, 1, 1, $opacity],
  c:(_Hue | _XYZColor | _LABColor | _LCHColor | _LUVColor) /; Len[c] === 3 :> Append[c, $opacity]
}];

(**************************************************************************************************)

RemoveOpacity[c:GrayLevel[_]]        := c;
RemoveOpacity[c:RGBColor[_, _, _]]   := c;
RemoveOpacity[c:RGBColor[{_, _, _}]] := c;
RemoveOpacity[expr_]                 := expr /. $removeOpacityRD;

$removeOpacityRD = Dispatch[{
  Opacity[_, c_] :> c,
  RGBColor[r_, g_, b_, _] :> RGBColor[r, g, b],
  RGBColor[{r_, g_, b_, _}] :> RGBColor[r, g, b],
  GrayLevel[g_, _] :> GrayLevel[g],
  c:(_Hue | _XYZColor | _LABColor | _LCHColor | _LUVColor) /; Len[c] === 4 :> Take[c, 3]
}];


