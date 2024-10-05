SystemExports[
  "FormHead",
    UnderlinedForm, ItalicForm, SemiBoldForm, BoldForm, PlainForm,
    VeryLargeForm, LargeForm, MediumForm, SmallForm, VerySmallForm, TinyForm,
    RedForm, BlueForm, GreenForm, OrangeForm, PurpleForm, TealForm, GrayForm, PinkForm, YellowForm, WhiteForm, BlackForm, DimForm
];

PackageExports[
  "BoxFn",
    VeryLargeBox, LargeBox, MediumBox, SmallBox, VerySmallBox, TinyBox,
    RedBox, BlueBox, GreenBox, OrangeBox, PurpleBox, TealBox, GrayBox, PinkBox, YellowBox, WhiteBox, BlackBox, DimBox
];

PrivateExports[
  "MetaFunction",
    DefineStyleFormBox,
    DefineSeqRowFormBox
];

(**************************************************************************************************)

DeclareThenScan[DefineStyleFormBox]

DefineStyleFormBox[Then[boxSym_, formSym_, style_]] := (
  MakeBox[formSym[expr_], _] := ApplyStyleBox[style] @ MakeBox @ expr;
  boxSym[boxes_]             := ApplyStyleBox[style] @ boxes;
);

(**************************************************************************************************)

SetForm1[VeryLargeForm, LargeForm, MediumForm, SmallForm, VerySmallForm, TinyForm];
SetBoxFn[VeryLargeBox, LargeBox, MediumBox, SmallBox, VerySmallBox, TinyBox];

DefineStyleFormBox[
  VeryLargeBox  ; VeryLargeForm  ; Large,
  LargeBox      ; LargeForm      ; Larger,
  MediumBox     ; MediumForm     ; Medium,
  SmallBox      ; SmallForm      ; Smaller,
  VerySmallBox  ; VerySmallForm  ; Small,
  TinyBox       ; TinyForm       ; Tiny
];

(**************************************************************************************************)

SetForm1[RedForm, BlueForm, GreenForm, OrangeForm, PurpleForm, TealForm, GrayForm, PinkForm, YellowForm, WhiteForm, BlackForm, DimForm];
SetBoxFn[RedBox, BlueBox, GreenBox, OrangeBox, PurpleBox, TealBox, GrayBox, PinkBox, YellowBox, WhiteBox, BlackBox, DimBox];

DefineStyleFormBox[
  RedBox    ; RedForm    ; $Red,
  BlueBox   ; BlueForm   ; $Blue,
  GreenBox  ; GreenForm  ; $Green,
  OrangeBox ; OrangeForm ; $Orange,
  PurpleBox ; PurpleForm ; $Purple,
  TealBox   ; TealForm   ; $Teal,
  GrayBox   ; GrayForm   ; $Gray,
  PinkBox   ; PinkForm   ; $Pink,
  YellowBox ; YellowForm ; $Yellow,
  WhiteBox  ; WhiteForm  ; $White,
  BlackBox  ; BlackForm  ; $Black,
  DimBox    ; DimForm    ; Opacity[0.3]
];

