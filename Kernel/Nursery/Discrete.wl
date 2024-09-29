PackageExports[
  "Function",
    PreimageFunction,
    ImageFunction,
    UniqueInverse,
  "Predicate",
    InjectiveQ,
    SurjectiveQ,
    BijectiveQ,
  "Head",
    DiscreteFunction,
    StochasticFunction
];

(*************************************************************************************************)

f_DiscreteFunction ? UnsealedQ := constructDF @ f;
(f_DiscreteFunction ? SealedQ)[x_] := evalDF[f, x];

SetHoldC[constructDF, evalDF];

DiscreteFunction::badArguments = "Expected an association or list of rules, and an optional set as a second argument.";
constructDF[f_] := ReturnMsg[DiscreteFunction::badArguments, HoldForm @ f];
constructDF[DiscreteFunction[rules:{__Rule}, set_List:Auto]] := makeFSF[Dict @ rules, set];
constructDF[DiscreteFunction[assoc:DictP, set_List:Auto]]    := makeFSF[assoc, set];

makeFSF[assoc_, set_] := MakeSealed[
  DiscreteFunction,
  UDict @ assoc, UDict @ PositionIndex @ assoc, IfAuto[set, Union @ Vals @ assoc]
];

evalDF[DiscreteFunction[fwd_, bwd_, _], x_] := Lookup[fwd, Key @ x, Indeterminate];

   ImageFunction[HoldP @ DiscreteFunction[fwd_, bwd_, _]][x_List] := Lookup[fwd, x, Indeterminate];
PreimageFunction[HoldP @ DiscreteFunction[fwd_, bwd_, _]][x_List] := Catenate @ Lookup[bwd, x, Indeterminate];

UniqueInverse[HoldP @ DiscreteFunction[fwd_, bwd_, set_]] := makeFSF[First /@ Select[bwd, SingleQ], Auto];
   InjectiveQ[HoldP @ DiscreteFunction[fwd_, bwd_, set_]] := AllTrue[bwd, SingleQ];
  SurjectiveQ[HoldP @ DiscreteFunction[fwd_, bwd_, set_]] := SameLengthQ[fwd, set];
   BijectiveQ[HoldP @ DiscreteFunction[fwd_, bwd_, set_]] := SameLengthQ[fwd, set] && AllTrue[bwd, SingleQ];

(* util function to express a DF as a surjection then injection *)
(*
InjectiveSubfunction, SurjectiveSubfunction, BijectiveSubfunction,
InjectiveFunction, BijectiveFunction, SurjectiveFunction,
Relation[fromset, function, toset]
*)

CoreBox[DiscreteFunction[fwd_, bwd_, set_] ? SealedQ] :=
  NiceObjectBoxes["DiscreteFunction", {RiffledRowBox["\[Rule]"] @ Map[NatStr, {Len @ fwd, Len @ set}]}];

(*************************************************************************************************)

SetHoldC[constructFSF, evalFSF];

f_StochasticFunction ? UnsealedQ := constructFSF @ f;
(f_StochasticFunction ? SealedQ)[x_] := evalFSF[f, x];

StochasticFunction::badArguments = "Expected either one arg (assoc), two args (assoc, outputs) or three argument (inputs, weights, outputs)."
constructFSF[f_] := ReturnMsg[StochasticFunction::badArguments, HoldForm @ f];

StochasticFunction::arg1notAssocList = "First argument was not an association of non-empty lists."
constructFSF[StochasticFunction[assoc_]] := Locals @ CatchMessages[StochasticFunction,
  If[!ListValuesQ[assoc] || MemberQ[assoc, {}], ReturnFailed["arg1notAssoc"]];,
  MakeSealed[StochasticFunction, assoc]
];

CoreBox[StochasticFunction[a_] ? SealedQ] :=
  NiceObjectBoxes["StochasticFunction",
    {RiffledRowBox["\[Rule]"] @ Map[ToBoxes, Len @ a, CountUnique @ Catenate @ a]}];

evalFSF[StochasticFunction[a_], x_]     := RandomChoice @ Lookup[a, x, badKeyMsg2[a, x]];
evalFSF[StochasticFunction[a_], x_List] := RandomChoiceArray @ Lookup[a, x, badKeyMsg2[a, x]];

(*************************************************************************************************)

StochasticFunction::arg1notAssocParts = "First argument was not an association whose values are lists of parts."
StochasticFunction::arg2notList = "Second argument was not a list of values."
StochasticFunction::arg2badList = "Second argument was a list with too few values `` < ``."
constructFSF[StochasticFunction[assoc_, values_]] := Locals @ CatchMessages[StochasticFunction,
  If[!ValuesTrue[assoc, PositiveIntegerVectorQ] || MemberQ[assoc, {}], ReturnFailed["arg1notAssocParts"]];
  If[!ListQ[values], ReturnFailed["arg2notList"]];
  If[(max = Max[assoc]) > (len = Len[values]), ReturnFailed["arg2notList", max, len]];
  MakeSealed[StochasticFunction, assoc, values]
];

CoreBox[StochasticFunction[a_, t_] ? SealedQ] :=
  NiceObjectBoxes["StochasticFunction",
    {RiffledRBox["\[Rule]"][NatStr @ Len @ a, NatStr @ Len @ t]}];

evalFSF[StochasticFunction[a_, o_], x_]     := RandomChoice @ Part[o, Lookup[a, x, badKeyMsg2[a, x]]];
evalFSF[StochasticFunction[a_, o_], x_List] := RandomChoiceArray @ Part[o, Lookup[a, x, badKeyMsg2[a, x]]];

badKeyMsg2[a_, x_] := (Message[StochasticFunction::unknownInput2, x]; First @ a)
StochasticFunction::unknownInput1 = "Input to StochasticFunction was not in the association: ``.";

(* sparse relation objection, which you can query with specific discrete values
takes an column-oriented association.
maybe make Columnar[assoc_] and Columnar[list], which are Held

 *)

(*************************************************************************************************)

StochasticFunction::badWeights = "Weights should be a matrix of positive numbers.";
StochasticFunction::badInputSpec = "Input `` should be a list, Automatic, or a function returning integers.";
StochasticFunction::badOutputSpec = "Output should be a list or Automatic.";
StochasticFunction::rowMismatch = "Weight matrix row count `` didn't match input size ``.";
StochasticFunction::colMismatch = "Weight matrix column count `` didn't match output size `` ";
constructFSF[StochasticFunction[ispec_, weights_, ospec_]] := Locals @ CatchMessages[StochasticFunction,
  If[!NumberMatrixQ[weights] || Min[weights] < 0, ThrowMsg["badWeights"]];
  {numRows, numCols} = Dims[weights];
  inputFn = Which[
    ListQ[ispec],
      If[Len[ispec] =!= numRows, ThrowMsg["rowMismatch", numRows, Len[ispec]]];
      UDictRange @ ispec,
    AutoQ[ispec],     Id,
    MaybeFnQ[ispec],  Id,
    True,             ThrowMsg["badInputSpec", ispec]
  ];
  outputs = Which[
    ListQ[ospec],
      If[Len[ospec] =!= numCols, ThrowMsg["colMismatch", numCols, Len[ospec]]];
      ospec,
    AutoQ[ispec], Range @ numCols,
    True,         ThrowMsg["badOutputSpec", ospec]
  ];
  MakeSealed[StochasticFunction, inputFn, weights, outputs]
];

evalFSF[StochasticFunction[f_Dict, w_, t_], x_] := WeightedRandomChoice[t, Part[w, Lookup[f, x, badKeyMsg1[x]]]]
badKeyMsg1[x_] := (Message[StochasticFunction::unknownInput2, x]; 1)
StochasticFunction::unknownInput2 = "Input to StochasticFunction was not in the alphabet: ``.";

evalFSF[StochasticFunction[f_, w_, t_], x_]     := WeightedRandomChoice[t, PartOr[w, f[x], badPartMsg[x]]];
evalFSF[StochasticFunction[f_, w_, t_], x_List] := WeightedRandomChoice[t, PartOr[w, Map[f, x], badPartMsg[x]]];
badPartMsg[x_] := (Message[StochasticFunction::invalidInput, x]; 1);
StochasticFunction::invalidInput = "Input to StochasticFunction was invalid: ``.";

CoreBox[StochasticFunction[_, w_ ? HPackedQ, _] ? SealedQ] :=
  NiceObjectBoxes["StochasticFunction", {RiffledRowBox["\[Times]"] @ Map[ToBoxes, Dimensions @ w]}];
