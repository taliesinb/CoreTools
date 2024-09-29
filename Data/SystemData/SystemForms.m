$form1 = {
    Item, Row, Column, Grid, Pane, Framed, EventHandler,
    TableForm, MatrixForm, Annotation, Style, Framed
    Minus
};
$form2 = {Rule, RuleDelayed, Labeled, Superscript, Subscript, Underscript, Overscript, Tooltip, Power, Divide, Equal, Unequal, Element, NotElement};
$form3 = {Subsuperscript};
$formA = {Greater, GreaterEqual, Less, LessEqual, Equal, Unequal, And, Or, Not, SameQ, UnsameQ, DirectedEdge, UndirectedEdge, Times, Plus, Implies, Equivalent, ForAll, Exists};

Flatten @ Map[Thread] @ List[$form1 -> 1, $form2 -> 2, $form3 -> 3, $formA -> All]