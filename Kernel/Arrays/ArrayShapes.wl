SystemExports[
  "Function",
    Length2, LengthN, DimensionN,
    FlatSum, FlatProduct,
    ArrayShape,
    ArrayPartsArray,
    ArrayPartsList
];

(**************************************************************************************************)

SetStrict[Length2, LengthN, DimensionN];

(* this doesn't handle lists of assocs *)
Length2[{}]          := None;
Length2[arr_]        := FastQuietCheck[Part[Dims[arr, 2], 2], None];
LengthN[arr_]        := Last[Dimensions @ arr, None];

DimensionN[{}, n_]   := None;
DimensionN[arr_, n_] := FastQuietCheck[Part[Dims[arr, n], n], None];

(**************************************************************************************************)

FlatSum[e_]     := Total[e, All];
FlatProduct[e_] := Apply[Times, e, All];

(**************************************************************************************************)

SetStrict @ ArrayShape;

"ArrayShape[array$] gives the dimensions of a rectagular array.
ArrayShape[array$, axis$] gives the dimension of a particular axis$.
ArrayShape[array$, {axis$1, axis$2, $$}] gives the shape for specific axes.";

ArrayShape[array_]             := Dimensions @ array;
ArrayShape[array_, All]        := Dimensions @ array;
ArrayShape[array_, n:PosIntP]  := Last @ Dimensions[array, n];
ArrayShape[array_, n_Int]      := Part[Dimensions[array], n];
ArrayShape[array_, ns:{__Int}] := Part[Dimensions[array, Max @ ns], ns];

(**************************************************************************************************)

SetStrict @ ArrayPartsArray;

"ArrayPartsArray[array$] gives an array of all cell indices of array$.
ArrayPartsArray[array$, axis$] gives an array of all cell indices of array$ along axis axis$.
ArrayPartsArray[array$, {axis$1, axis$2, $$}] gives an array of tuples for specific axes.";

ArrayPartsArray[array_, spec_:All] := RangeArray @ ArrayShape[array, spec];

(**************************************************************************************************)

SetStrict @ ArrayPartsList;

"ArrayPartsList[array$] gives a list of all cell indices of array$.
ArrayPartsList[array$, axis$, array$] gives a list of cell indices of array$ along axis axis$.
ArrayPartsList[array$, {axis$1, axis$2, $$}] gives a list of cell indices for specific axes.";

ArrayPartsList[array_, n_Int]     := Range @ ArrayShape[array, n];
ArrayPartsList[array_, spec_:All] := Tuples @ ArrayShape[spec, array];