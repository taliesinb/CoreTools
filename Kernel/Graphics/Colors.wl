SystemExports[
  "GraphicsDirective", HashColor, OKColor, OKHue, ComplexHue, NiceHue,
  "Function",          HashToColor, UniqueColor, DuplicateColoring, ColorDuplicates, ColorUnique
];

PackageExports[
  "Function",
    ColorToRGBArray, RGBArrayToColor,
    ColorToOKArray,  OKArrayToColor,
    OKArrayToRGBArray, RGBArrayToOKArray,
    LCHArrayToRGBArray
];

(**************************************************************************************************)

DuplicateColoring[expr_, patt_, baseColor_:$DarkGray] := Locals[
  occs = Occurrences[expr, patt];
  dups = Union @@ Duplicates @ occs;
  colors = UniqueColor /@ Range @ Len @ dups;
  dupRules = RuleThread[dups, colors];
  baseRules = ConstantRules[Compl[occs, dups], baseColor];
  UDict[dupRules, baseRules]
];

(**************************************************************************************************)

ColorDuplicates[expr_, patt_, fn_:Style, baseColor_:$DarkGray] := Locals[
  dups = Occurrences[expr, patt];
  dups = Union @@ Duplicates @ dups;
  If[dups === {}, Return @ expr];
  colors = UniqueColor /@ Range @ Len @ dups;
  lhs = PatternLHS @ patt;
  If[FailureQ[lhs],
    rules = ZipMap[Rule[Verbatim[#1], fn[#1, #2]]&, dups, colors];
    expr /. rules
  ,
    colorDict = DictThread[dups, colors];
    expr /. e:lhs :> RuleEval @ fn[e, Lookup[colorDict, Replace[e, patt], baseColor]]
  ]
];

(**************************************************************************************************)

ColorUnique[expr_, patt_, fn_:Style, baseColor_:$DarkGray] := Locals[
  dups = Occurrences[expr, patt];
  dups = Union @@ Duplicates @ dups;
  colors = UniqueColor /@ Range @ Len @ dups;
  lhs = PatternLHS @ patt;
  colorDict = DictThread[dups, colors];
  expr /. e:patt :> RuleEval @ fn[e, Lookup[colorDict, e, baseColor]]
];

(**************************************************************************************************)

HashColor[e_] := HashToColor @ Hash @ e;

(**************************************************************************************************)

HashToColor[list_List] := Map[HashToColor, list];
HashToColor[hash_, l_:0] := Locals[
  {c1, c2} = IntegerDigits[hash, 256, 2];
  NiceHue[Mod[c1 / 256., 1], 0.5 + 0.5*Mod[c2 / 256., 1], l]
]

(*************************************************************************************************)

$goldenRatioConj = 0.618033988749895;
UniqueColor[n_Int] := ColorConvert[NiceHue @ Mod[n * $goldenRatioConj, 1], RGBColor];

(*************************************************************************************************)

RGBArrayToColor::notArray = "Input was not an array: ``.";

RGBArrayToColor[rgb_List ? NumberVectorQ] := RGBColor @ rgb;
RGBArrayToColor[rgb_List ? NumberArrayQ]  := Map[RGBColor, rgb, {-2}];
RGBArrayToColor[arr_]                     := (Message[RGBArrayToColor::notArray, arr]; $Failed);

(*************************************************************************************************)

ColorToRGBArray::notColors = "Input was not a color or an array of colors: ``.";

ColorToRGBArray[colors_] := EnsurePackedReals[
  ReplaceAll[colors, $toRGBRules],
  Message[ColorToRGBArray::notColors, colors]; $Failed
];

(* ColorToRGBArray[colors_] := EnsurePackedReals[
  ColorConvert[colors, "RGB"] /. RGBColor -> List,
  Message[ColorToRGBArray::notColors, colors]; $Failed
];
 *)

$toRGBRules = Dispatch[{
  RGBColor[r_, g_, b_, ___] :> {r, g, b},
  RGBColor[{r_, g_, b_, ___}] :> {r, g, b},
  c:(_GrayLevel | _XYZColor | _CMYKColor | _Hue | _XYZColor | _LABColor | _LCHColor | _LUVColor) :>
    RuleCondition[List @@ ColorConvert[c, "RGB"]]
}];

(*************************************************************************************************)

OKColor[lab:{_, _, _}] := RGBColor @ OKArrayToRGBArray @ lab;
OKColor[l_, a_, b_]    := RGBColor @ OKArrayToRGBArray @ {l, a, b};

(*************************************************************************************************)

OKArrayToColor[arr_]  := RGBArrayToColor @ OKArrayToRGBArray @ arr;
ColorToOKArray[cols_] := RGBArrayToOKArray @ ColorToRGBArray @ cols;

(*************************************************************************************************)

OKArrayToRGBArray[lab_List] := Clip[convert$srgb$rgb @ convert$ok$srgb @ lab, {0., 1.}];
RGBArrayToOKArray[rgb_List] := convert$srgb$ok @ convert$rgb$srgb @ rgb;

OKArrayToRGBArray[_] := $Failed;
RGBArrayToOKArray[_] := $Failed;

(* TODO: transpose these so we don't have to map *)
convert$ok$srgb[lab_List ? VectorQ] := Dot[$lms$srgb, Dot[$ok$lms, lab]^3];
convert$ok$srgb[lab_List] := Map[convert$ok$srgb, lab, {-2}];

convert$srgb$ok[srgb_List ? VectorQ] := Dot[$lms$ok, CubeRoot @ Dot[$srgb$lms, srgb]];
convert$srgb$ok[srgb_List] := Map[convert$srgb$ok, srgb, {-2}];

SetListable[convert$rgb$srgb, convert$srgb$rgb]
convert$srgb$rgb[x_] := If[x >= 0.0031308, 1.055 * x^(1.0/2.4) - 0.055, 12.92 * x];
convert$rgb$srgb[x_] := If[x >= 0.04045, ((x + 0.055)/(1 + 0.055))^2.4, x / 12.92];

$ok$lms = ToPackedReals @ {
  {+1, +0.3963377774, +0.2158037573},
  {+1, -0.1055613458, -0.0638541728},
  {+1, -0.0894841775, -1.2914855480}};

$lms$srgb = ToPackedReals @ {
  {+4.0767416621, -3.3077115913, +0.2309699292},
  {-1.2684380046, +2.6097574011, -0.3413193965},
  {-0.0041960863, -0.7034186147, +1.7076147010}};

$srgb$lms = ToPackedReals @ {
  {0.4122214708, 0.5363325363, 0.0514459929},
  {0.2119034982, 0.6806995451, 0.1073969566},
  {0.0883024619, 0.2817188376, 0.6299787005}};

$lms$ok = ToPackedReals @ {
  {+0.2104542553, +0.7936177850, -0.0040720468},
  {+1.9779984951, -2.4285922050, +0.4505937099},
  {+0.0259040371, +0.7827717662, -0.8086757660}};

(*************************************************************************************************)

SetListable[ComplexHue]

ComplexHue[c_Complex] := ComplexHue[Arg[c] / Tau, Min[Sqrt[Abs[c]]/1.2,1], .9];

(**************************************************************************************************)

NiceHue[arr_] := fastBalancedHue @ arr;
NiceHue[arr_, s:NumP] := modifySat[s, fastBalancedHue @ arr];
NiceHue[arr_, s:NumP, l:NumP] := modifySatLum[s, l, fastBalancedHue @ arr];

SetListable[fastBalancedHue];

fastBalancedHue[h_] := Blend["SoftBalancedHue", h];

modifySat[sat_, expr_] :=
  ReplaceAll[expr, LABColor[l_, a_, b_] :> LABColor[l, a * sat, b * sat]];

modifySatLum[sat_, lbias_, expr_] := With[
  {b1 = 1-Abs[lbias], b2 = UnitStep[lbias] * lbias},
  ReplaceAll[expr, LABColor[l_, a_, b_] :> LABColor[l * b1 + b2, sat * a, sat * b]]
];

(*************************************************************************************************)

LCHArrayToRGBArray[arr_] := EnsurePackedReals[
  ToPackedReals[ColorConvert[arr, "LCH" -> "RGB"] /. RGBColor -> List],
  $Failed
];

