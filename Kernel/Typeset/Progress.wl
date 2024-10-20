PackageExports[
  "IOFunction", WithProgress, MonitorProgress
];

(*************************************************************************************************)

SetHoldA @ WithProgress;

WithProgress[curr_, max_, cutoff_, body_] := iWithProgress[max, cutoff, Hold[curr, body]];

iWithProgress[max_, cutoff_, Hold[curr_, body_]] /; max < cutoff := body;

iWithProgress[max_, cutoff_, Hold[curr_, body_]] := Module[
  {dynamic, cell},
  dynamic = Dynamic[N[curr / max], UpdateInterval -> 0.1, TrackedSymbols :> {}];
  WithLocalSettings[
    cell = PrintTemporary[RawBoxes @ ProgressIndicatorBox @ dynamic],
    body,
    Quiet @ NotebookDelete @ cell
  ]
];

(*************************************************************************************************)

SetStrict @ SetHoldA @ MonitorProgress;

MonitorProgress[(head:Map|Scan|MapP|ScanP|ZipMap|ZipScan|MapApply|KeyValueMap|KeyValueScan)[fn_, arg1_, argN___]] := Module[
  {$arg1 = arg1, $counter = 0},
  WithProgress[$counter, Len[$arg1], 2, CheckMessageFree @ head[Fn[Null, $counter++; fn[##]], $arg1, argN]]
];

MonitorProgress::cantMonitor = "Cannot monitor ``.";
MonitorProgress[body_] := body /; (Message[MonitorProgress::cantMonitor, Head[body, HoldForm]]; True);

