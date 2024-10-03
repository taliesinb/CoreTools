SystemExports[
  "Function",
    LineDist, InfLineDist,
    LineNearest, InfLineNearest
];

(**************************************************************************************************)

"LineDist[path$, p$] returns the shortest distance from point p$ to line path$."
"InfLineDist[{a$, b$}, p$] returns the distance from line passing through a$, b$ to point p$."

SetStrict[LineDist, InfLineDist];

LineDist[{a_, b_}, p_]  := Max[Norm @ VecReject[p - b, a - b], Min[Dist[p, a], Dist[p, b]]];
LineDist[line_List, p_] := RegionDistance[Line @ line, p];

InfLineDist[{a_, b_}, p_] := Norm @ VecReject[b - p, a - b];

(**************************************************************************************************)

"LineNearest[path$, p$] returns the point on line path$ closest to point p$."
"InfLineNearest[{a$, b$}, p$] returns the point on line passing through a$, b$ that is closest to p$."

SetStrict[LineNearest, InfLineNearest];

LineNearest[line_List, p_]   := RegionNearest[Line @ line, p];
InfLineNearest[{a_, b_}, p_] := p + VecReject[b - p, a - b];

(* TODO: Region`Polygon`LineSegmentIntersection? *)

