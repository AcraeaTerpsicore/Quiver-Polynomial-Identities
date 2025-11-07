SeedRandom[1337];

projectRoot = ExpandFileName@FileNameJoin[{DirectoryName[$InputFileName], ".."}];
Get[FileNameJoin[{projectRoot, "src", "QuiverPI", "QuiverPI.wl"}]];

ClearAll[Commutator];
Commutator[A_, B_] := A.B - B.A;

(* Example: type A2 quiver *)
qA2 = CreateQuiver[{1, 2}, {QuiverArrow["alpha", 1, 2]}];
piA2 = {
  QuiverZeroPath[qA2, 1],
  QuiverArrowPath[qA2, "alpha"]
};
maskA2 = QuiverIncidenceMask[qA2, piA2];

polyA2 = Function[{vars},
  Commutator[vars["x1"], vars["x2"]].vars["x3"]
];

test1 = VerificationTest[
  maskA2 === {{1, 1}, {0, 0}},
  True,
  TestID -> "A2 incidence mask"
];

test2 = VerificationTest[
  QuiverVerifyPolynomialIdentity[qA2, polyA2, {"x1", "x2", "x3"},
    "PathGenerators" -> piA2,
    "Samples" -> 16
  ],
  True,
  TestID -> "A2 identity [x1,x2] x3"
];

(* Example with three vertices and loops *)
arrowsEx = {
  QuiverArrow["gamma", 1, 1],
  QuiverArrow["alpha1", 1, 2],
  QuiverArrow["beta", 2, 2],
  QuiverArrow["alpha2", 3, 2],
  QuiverArrow["delta", 3, 3]
};
qEx = CreateQuiver[{1, 2, 3}, arrowsEx];

polyMalcev = Function[{vars},
  Module[
    {c12 = Commutator[vars["x1"], vars["x2"]],
     c34 = Commutator[vars["x3"], vars["x4"]]},
    c12.c34
  ]
];

test3 = VerificationTest[
  QuiverVerifyPolynomialIdentity[qEx, polyMalcev, {"x1", "x2", "x3", "x4"},
    "Samples" -> 24,
    "Density" -> 0.7
  ],
  True,
  TestID -> "Full quiver Malcev identity"
];

maskFull = QuiverIncidenceMask[qEx];
expectedMaskFull = {{1, 1, 0}, {0, 1, 0}, {0, 1, 1}};
test4 = VerificationTest[
  maskFull === expectedMaskFull,
  True,
  TestID -> "Full incidence mask"
];

pathAlpha1 = QuiverArrowPath[qEx, "alpha1"];
phiAlpha1 = QuiverPhiMatrix[qEx, pathAlpha1];
test5 = VerificationTest[
  phiAlpha1 === SparseArray[{{1, 2} -> 1}, {3, 3}],
  True,
  TestID -> "Phi(alpha1)"
];

piRestricted = DeleteCases[
  Flatten[{
    QuiverZeroPath[qEx, #] & /@ {1, 2},
    QuiverArrowPath[qEx, #] & /@ {"gamma", "alpha1", "beta", "alpha2"}
  }],
  _Failure
];

test6 = VerificationTest[
  QuiverVerifyPolynomialIdentity[qEx, polyMalcev, {"x1", "x2", "x3", "x4"},
    "PathGenerators" -> piRestricted,
    "Samples" -> 24
  ],
  True,
  TestID -> "Restricted quiver Malcev identity"
];

test7 = VerificationTest[
  Length[QuiverEnumeratePaths[qA2, 2]] === 3,
  True,
  TestID -> "Enumerate paths (A2, length<=2)"
];

report = TestReport[{test1, test2, test3, test4, test5, test6, test7}];
Print[report];
If[report["TestsFailed"] > 0,
  Exit[1],
  Exit[0]
];
