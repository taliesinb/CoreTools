PackageExports[
  "Function",
    NullifyFn, PostCompose, FnComposeBody,
  "ControlFlow",
    MakeHoldComplete, ApplyHoldComplete,
    Applied,
    NullFn, ConstFn, HoldConstFn,
    TrueFn, FalseFn,
    Supply1, Supply2, Supply3,
    AttributeFn, HoldFirstFn, HoldRestFn, HoldAllFn, HoldCompFn,
  "SymbolicHead",
    Const
];

(**************************************************************************************************)

NullifyFn = CaseOf[
  VFn[b_]              := Fn[b;];
  VFn[v_, b_]        := Fn[v, b;];
  VFn[v_, b_, a_] := Fn[v, b;, a];
  fn_                     := fn /* NullFn;
];

(**************************************************************************************************)

(* TODO: finish using this in PostCompose
this gives false negatives for nested functions, but that's ok, it's best effort
arity1Q = CaseOf[
  fn:AnonFnP    := FreeQ[fn, Slot[n_Int /; n > 1] | SlotSequence],
  Fn[_Sym, ___] := True;
  Fn[{_}, ___]  := True;
  _             := False;
];*)

(**************************************************************************************************)

(* TODO: allow composing a function with a one-arg function and inlining them *)
PostCompose = CaseOf[
  $[f1_Fn, f2_]  := FnComposeBody[f1, f2];
  $[f1_, f2_]     := f1 /* f2;
  $[f1_, NullFn]  := NullifyFn @ fn;
];

FnComposeBody = CaseOf[
  $[VFn[b_], f_]         := Fn[f @ b];
  $[VFn[v_, b_],     f_] := Fn[v, f @ b];
  $[VFn[v_, b_, a_], f_] := Fn[v, f @ b, a];
  $[VFn[v_, b_, a_], f_] := Fn[v, f @ b, a];
];

(*
PreCompose = CaseOf[
  $[VFn[s:(_Sym | {_Sym}), b1_], VFn[b_]] := HoldComp[b] /. Slot[1] :> ;
  $[VFn[v_, b_],     f_] := Fn[v, f @ b];
  $[VFn[v_, b_, a_], f_] := Fn[v, f @ b, a];
  $[f1_,             f2_] := f1 /* f2;
];
*)
(**************************************************************************************************)

MakeHoldComplete[fn_Fn, a___]          := PostCompose[fn, HoldComplete][a];
MakeHoldComplete[Apply[fn_Fn], {a___}] := PostCompose[fn, HoldComplete][a];
MakeHoldComplete[fn_, a___]         := HoldComplete[fn[a]];

ApplyHoldComplete[fn_Fn, _[a___]] := PostCompose[fn, HoldComplete][a];
ApplyHoldComplete[fn_,   _[a___]] := HoldComplete[fn[a]];

(**************************************************************************************************)

SetHoldA[HoldConstFn]

ConstFn[v_]     := Fn[Null, v];
HoldConstFn[v_] := Fn[Null, v];

NullFn[___]  := Null;
TrueFn[___]  := True;
FalseFn[___] := False;

(**************************************************************************************************)

SetHoldA[AttributeFn, HoldFirstFn, HoldRestFn, HoldAllFn, HoldCompFn];

AttributeFn[attr_, vars_, body_] := Fn[vars, body, attr];
AttributeFn[attr_, body_]        := Fn[Null, body, attr];

HoldFirstFn[args___] := AttributeFn[HoldFirst, args];
HoldRestFn[args___]  := AttributeFn[HoldRest, args];
HoldAllFn[args___]   := AttributeFn[HoldAll, args];
HoldCompFn[args___]  := AttributeFn[HoldAllComplete, args];

(**************************************************************************************************)

Supply1[a1_]     := Fn @ Construct[Sequence, a1,         ##1];
Supply2[a2_]     := Fn @ Construct[Sequence, #1, a2,     ##2];
Supply3[a3_]     := Fn @ Construct[Sequence, #1, #2, a3, ##3];

Supply1[f_, a1_] := Fn @ f[a1,         ##1];
Supply2[f_, a2_] := Fn @ f[#1, a2,     ##2];
Supply3[f_, a3_] := Fn @ f[#1, #2, a3, ##3];

(**************************************************************************************************)

$pp = 1 | 2 | 3 | 4 | 5 | 6 | 7 | 9 | _String;

Applied[fn_, i:$pp -> j:$pp]   := Fn @ fn[Part[Slot @ i, j]];
Applied[fn_, i:$pp, j:$pp]     := Fn @ fn[Slot @ i, Slot @ j];
Applied[fn_, i:$pp, Const[c_]] := Fn @ fn[Slot @ i, c];
Applied[fn_, Const[c_], j:$pp] := Fn @ fn[c, Slot @ j];

Applied[fn_, is___]            := toApplied[fn, {is}];

(**************************************************************************************************)

(* Applied[s_String]             := Key[str];
Applied[i_Integer]            := Applied[i]    = Slot[i]&;
Applied[i_Integer, j_Integer] := Applied[i, j] = Fn @ Construct[Sequence, Slot @ i, Slot @ j];
Applied[is___]                := toApplied[Sequence, {is}];
 *)
(**************************************************************************************************)

Clear[toApplied, makeAppFn, parseAppSpec0, parseAppSpec1];

toApplied[fn_, spec_] := Block[
  {body, $canCache = True, $postFn = Identity},
  body = parseAppSpec0 @ spec;
  appFn = $postFn @ makeAppFn[fn, body];
  If[$canCache && SymbolQ[fn], toApplied[fn, spec] = appFn, appFn]
];

makeAppFn[Sequence, {arg_}]    := Fn @ arg;
makeAppFn[Sequence, {args___}] := Fn @ Construct[Sequence, args];
makeAppFn[fn_, {arg_}]         := Fn @ fn[arg];
makeAppFn[fn_, {args___}]      := Fn @ fn[args];

(**************************************************************************************************)

parseAppSpec0[spec_] := (
  body = parseAppSpec1[spec];
  If[$canCache, parseAppSpec0[spec] = body];
  body
);

parseAppSpec1 = CaseOf[
  i_Integer ? Positive       := Slot[i];
  s_String                   := Slot[$canCache = False; s];
  Const[data_]               := (checkIfSimple[data]; data);
  Span[i_Integer, All]       := SlotSequence[i];
  Span[i_Integer, j_Integer] := Splice @ Birange[i, j];
  Rule[s_, r_]               := parseAppRule[$postFn = removeSlotHold; parseAppSpec1 @ s, r];
  list_List                  := Map[parseAppSpec1, list];
  spec_                      := ThrowMsg["badAppliedSpec", spec];
];

checkIfSimple = CaseOf[
  $[i_Integer /; -32 < i < 32]             := Null;
  s_Symbol                                 := Null;
  {_ ? checkIfSimple, _ ? checkIfSimple}   := Null;
  _                                        := ($canCache = False);
];

General::badAppliedSpec = "Unknown Applied specification ``.";

(**************************************************************************************************)

parseAppRule[in_, Rule[a_, b_]] := parseAppRule[parseAppRule[in, a], b];
parseAppRule[in_, s__String]    := slotHold[in[s]];
parseAppRule[in_, p_Integer]    := slotHold[Part[in, p]];
parseAppRule[in_, {p:$pp..}]    := slotHold[Part[in, p]];
parseAppRule[in_, f_Symbol]     := slotHold[f[in]];
parseAppRule[_, f_]             := ThrowMsg["badAppliedSlotFunction", f];

General::badAppliedSlotFunction = "Cannot use complex Applied slot function ``.";

(**************************************************************************************************)

SetHoldA[slotHold]

removeSlotHold[e_] := e //. slotHold[f_] :> f;

(**************************************************************************************************)

(*

SUPER DUPER THING

you have multiple query arms:

qvary[pspec_1, pspec_2, pspec_3, pspec_4] 

and a single result arm.

each psec_i can be:

3 -> spec     -- shorthand for (_, _, spec), these combine
1, "Foo"      -- a specific sub-part
i_            -- any list subpart, the index gets bound during traversal to 'i'

the above refer to parts. conceptually they are matching against Position lists. they don't get to refer to the values.
to refer to actual values, which is nice when you are recursing inside arbitrary trees, not lists and assocs, we can do:

Valued[_Foo]     -- any part whose value matches _Foo
Valued[_ ? blah] -- test the expression before recursing

e.g.:
{i:Valued[_Integer]} is an integer within a list

these bindings are shared across arms, so tensor product:

x * y <- x[i_, j_], y[j_, k_] 

for named axes we'd have:

x * y <- x["I" -> i_, "J" -> j_], y["J" -> j_, "K" -> k_] 

we need to specify the outputs... so maybe:

Fn[{i, j, k}, x * y, {x["I" -> i_, "J" -> j_], y["J" -> j_, "K" -> k_]}]

ok, for arbitrary dim bindings, can do:

x[i_, j___, k_]
y[k_, l___, m_]

maybe if x and y happen to be named-axis arrays, a symbol on its own matches the parts of this named axis:

x[i, ___, j]

to deal with unique factorizations that come from named axes, do:

x[i___, j___], x[j___, k___]

this won't be well defined for ordinary arrays, but will be for name darays.

these then imply names for the results, if we write:

Fn[{i, k}, x * y, {x[i___, j___], y[j___, k___]}, {j -> Total}]

but this ___ is tedius. it makes sense that general i and j just match a subset of axes, conceptually
treating them as one axis that is product typed:

Fn[{i, k}, x * y, {x[i_, j_], y[j_, k_]}, j -> Total]

if we were to use them in the body, they'd be an association of axis name to index

let's re-arrange, to order the inputs on the left:

Fn[{x[i_, j_], y[j_, k_]}, x * y, {i, j -> Total, k}]

we can now read left-to-right. maybe this is even easier, and makes the binding a bit clearer:

Fn[{x[i_, j_], y[j_, k_]} :> x * y, {i, j -> Total, k}]

if we don't wish to discharge, we'd have:

Fn[{x_[i_, j_], y_[j_, k_]} :> x * y]

the rule for output shape is that every bound or unbound part comes along for the ride. 
so the explicit output form need only say:

Fn[{x_[i_, j_], y_[j_, k_]} :> x * y, j -> Total] 

this just says keep all the vars you see on the LHS, but drop this particular one, or multiple ones.
there can only be one aggregator because the aggregator has to operate on scalars.
what happens if you mention what you want but fail to use some parts that were indexed:

Fn[{x_[i_, j_], y_[j_, k_]} :> x * y, {i}] 

then here we collect the results into a bag. 

express FirstPosition this way:

Fn[_[Valued[p___, patt]] :> p, p -> First] 

or general Position:

Fn[_[Valued[p___, patt]] :> p] 

Valued has to be a wrapper around both a position and a value pattern.

how about Matching[p___, patt] and Filtering[p___, test] ?

it would be nice if the full definition is recursive, so that having multiple input arrays uses the same
logic as a single input array.

{q1, q2, q3} vs q1

how about ** to separate sub-queries

a thing appearing is a literal
** introduces traversal

{q1_List, q2_List, q3_List} binds values

p ** q, here p represents a position pattern, and q a value pattern

so p1 ** {p2 ** {p3 ** ..}} is pretty ugly

{p1, p2, p3} *** _List is a bit nicer

q[p] is the same as p ** q

we need to distinguish outside from inside, bracket notation?

< outer value | p | inner value |>
< _List | p | s |

< _List | p1 | _Foo >< p2 | s |>

condense this to:

< _List | p1 | _Foo | p2 | s >

Fn[ < _ | p___ | patt > :> {p}, p -> First] 

GroupBy:

List<p | v> => (f[v] -> p)

when we have a rule in there, we effectively discharge multiple values by gathering them in lists
in an association. 

PositionIndex:

List<p | v> => (v -> p)

or explicitly:

List<p | v> => Assoc<v | p>

we can reason about this: if the key of the assoc is unique, we don't need to listify it,
otherwise we do.

ok this < ... > notation should just have the head on outside:

List< p... > for unbound
List< p... | v > for bound
List< p... | Foo < q... | v >> for nested


the rule would be that everywhere you see a 

if there were any blanks, the query result is conceptually either a flat list or a structured array
but really we want to map something!

we can specify an aggregragotor if we want to discharge listiness, like Total etc.

if a given level of tree consists of not {...} or <|...|> but instead foo[...], we can specify
i_



*)