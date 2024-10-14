PackageExports[
  "GraphicsDirective",   FaceEdge,
  "GraphicsFunction",    ExprTreePlot, TreeNodeColor,
  "Function",            AllTreesDepth, AllTrees, ToTree, UnfoldTree, UnfoldTreeData, RemoveTreeData,
  "PatternSymbol",       TreeNodeP,
  "Predicate",           TreeNodeQ, ValidTreeQ,
  "Head",                TreeNode, TreeLeaf, DataNode, DataLeaf, TreeSeed,
  "Function",            ToTree,
  "TransientVariable",   $CurrentTreeDepth, $MaxTreeDepth,
  "Option",              NodePattern
];

(**************************************************************************************************)

DefinePatternRules[
  TreeNodeP -> Alt[_TreeNode, DataNode[_][___]]
];

DeclarePatternPredicates[TreeNodeQ];

(**************************************************************************************************)

SetPred1 @ ValidTreeQ

ValidTreeQ = CaseOf[
  TreeNode[___ ? ValidTreeQ]    := True;
  DataNode[_][___ ? ValidTreeQ] := True;
  TreeSeed[_] := True;
];

(**************************************************************************************************)

TreeLeaf     := TreeNode[];
DataLeaf[d_] := DataNode[d][];

CoreBox[t_TreeSeed ? ValidTreeQ]   := treeNodeBoxes[t];
CoreBox[t_TreeNode ? ValidTreeQ]   := treeNodeBoxes @ t;

SetCoreSubBox[DataNode];

MakeCBox[t:(DataNode[_][___]) ? ValidTreeQ] := treeNodeBoxes @ t;

(**************************************************************************************************)

$treeNodePlotOpts = Seq[NodePattern -> TreeNodeP | TreeSeed[_], EdgeColor -> GrayLevel[0.8]];

treeNodeBoxes = CaseOf[
  s:TreeSeed[_]  := rootNodeBoxes["\[EmptyCircle]", s];
  s:TreeLeaf     := rootNodeBoxes["\[FilledCircle]", s];
  s:DataLeaf[_]  := rootNodeBoxes["\[FilledCircle]", s];
  expr_          := Which[
    LeafCount[expr] > 256, ElidedBox @ expr,
    Depth[expr] > 24,      ToBoxes @ ExprTreePlot[Replace[expr, e_ -> TreeSeed[e], {16}], $treeNodePlotOpts],
    True,                  ToBoxes @ ExprTreePlot[expr, $treeNodePlotOpts]
  ]
];

rootNodeBoxes[str_, node_] := NiceTooltipBox[
  StyleBox[str, Bold, FontSize -> Inherited-2, Last @ toNodeColor @ t],
  ToBoxes @ Unformatted @ node
];

(**************************************************************************************************)

Options[ExprTreePlot] = JoinOptions[
  {NodePattern -> Auto, NodeColor -> TreeNodeColor, GraphScale -> 20},
  Options @ NiceTreePlot
];

ExprTreePlot[expr_, opts:OptionsPattern[]] := Locals[
  UnpackOptions[nodePattern, graphScale, nodeColor];
  SetAuto[nodePattern, _];
  paths = FindExprPaths[expr, nodePattern];
  nodes = Extract[expr, List @@@ paths];
  graph = PrefixGraph @ paths;
  isLeaf = Map[ZeroQ, VertexOutDegree @ graph];
  NiceTreePlot[graph,
    NodeData -> <|"Expression" -> nodes, "IsLeaf" -> isLeaf|>,
    GraphScale -> graphScale,
    NodeColor -> "Expression" -> nodeColor,
    NarrowOptions @ opts
  ]
];

(**************************************************************************************************)

TreeNodeColor[e_] := toNodeColor @ e;

toNodeColor = CaseOf[
  i:NatP            := Part[$MediumColorPalette, i + 1];
  c:ColorP          := c;
  TreeNode[___]     := $DarkGray;
  DataNode[i_][___] := $ @ i;
  TreeSeed[i_]      := FaceEdge[White, $ @ i];
  None | Null       := $Gray;
  i_Real ? UnitNumberQ := GrayLevel[i];
  i_Real            := NiceHue @ i;
  e_                := HashToColor @ Hash @ Head @ e;
];

(**************************************************************************************************)

AllTrees[d_Int] := AllTrees[d, 2];
AllTrees[0,     b_Int] := {};
AllTrees[d_Int, b_Int] := AllTrees[d, b] = Join[AllTrees[d-1, b], AllTreesDepth[d, b]];

AllTreesDepth[d_Int, b_Int] := AllTreesDepth[d, b] = Map[ToTree, Groupings[d, b]];


(**************************************************************************************************)

ToTree[expr_] := toTree @ expr;

toTree = CaseOf[
  list_List         := Map[$, Apply[TreeNode, list]];
  head_Sym[args___] := Map[$, DataNode[head][args]];
  expr_             := If[MatchQ[expr, TreeNodeP], expr, DataNode[expr][]];
];

(**************************************************************************************************)

treeSeedFn[fn_][TreeSeed[s_]] := toSeedTree @ fn[s];
treeSeedFn[fn_][e_] := e;

toSeedTree = CaseOf[
  list_List   := Map[$, Apply[TreeNode, list]];
  t:TreeNodeP := Map[$, t];
  s_TreeSeed  := s;
  s_          := TreeSeed @ s;
];

(**************************************************************************************************)

toTreeSeedFn = CaseOf[
  dict_Dict           := KeyMapValueMap[toSeedTree, toSeedTree, dict];
  rule:RuleLP         := $ @ List @ rule;
  rules:RuleLVecP     := KeyMapValueMap[toTreePattern, expr];
  fn_                 := treeSeedFn @ fn
];

SetHoldA @ toTreePattern;

toTreePattern = CaseOf[
  list:{__Rule}            := Map[$, list];
  (h:Rule|RuleD)[lhs_, rhs_] := Make[h, $ @ lhs, $ @ rhs];
  p_Pattern                := TreeSeed @ p;
  c_Condition              := TreeSeed @ c;
  list_List                := TreeNode @@ HoldMap[$, list];
  t_TreeSeed               := t;
  t_TreeNode               := Map[$, t];
  e_                       := TreeSeed @ e;
];


(**************************************************************************************************)

UnfoldTree[f_, s_, maxD_:4, decorated_:False] := Locals[
  $unfoldFn = toTreeSeedFn @ f;
  If[decorated, $unfoldFn //= decoratedFn];
  $CurrentTreeDepth = 0; $MaxTreeDepth = maxD;
  unfoldStep0 @ TreeSeed @ s
];

UnfoldTreeData[f_, s_, maxD_:4] := UnfoldTree[f, s, maxD, True];

decoratedFn[f_][s_] := decorateNode[s, f[s]];
decorateNode[TreeSeed[s_], t_TreeNode] := Apply[DataNode[s], t];
decorateNode[s_, other_]     := other;

unfoldStep0[s_] /; $CurrentTreeDepth >= $MaxTreeDepth := s;
unfoldStep0[s_] := BlockIncrement[$CurrentTreeDepth, unfoldStep1 @ $unfoldFn @ s];

unfoldStep1 = CaseOf[
  t:(DataNode[_][___]) := Map[$, t];
  t_TreeNode           := Map[$, t];
  s_TreeSeed           := unfoldStep0 @ s;
  e_                   := e;
];

unfoldStep1[$[e_]] := unfoldStep0[e];

(**************************************************************************************************)

RemoveTreeData[tree_] := ReplaceAll[tree, {_DataNode -> TreeNode, TreeSeed[d_] :> TreeSeed[None]}];

