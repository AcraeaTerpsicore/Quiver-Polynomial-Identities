SeedRandom[1337];

projectRoot = ExpandFileName@FileNameJoin[{DirectoryName[$InputFileName], ".."}];
Get[FileNameJoin[{projectRoot, "src", "QuiverPI", "QuiverPI.wl"}]];

ClearAll[Commutator];
Commutator[A_, B_] := A.B - B.A;

ClearAll[x1, x2, x3, x4, x5, x6, x7, x8];

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

test8 = VerificationTest[
  PIQuiverQ[qEx],
  True,
  TestID -> "PIQuiverQ (Example quiver)"
];

qNonPI = CreateQuiver[{1, 2}, {
   QuiverArrow["loop", 1, 1],
   QuiverArrow["forward", 1, 2],
   QuiverArrow["back", 2, 1]
}];

test14 = VerificationTest[
  Length[cycleEmbeddings] == 1 && cycleEmbeddings[[1, "Length"]] == 3,
  True,
  TestID -> "Cycle embedding detection"
];

test15 = VerificationTest[
  cyclePhi[{"Arrow", "c12"}] === SparseArray[{{1, 2} -> 1}, {3, 3}],
  True,
  TestID -> "Cycle embedding phi"
];

qCycle = CreateQuiver[{1, 2, 3}, {
   QuiverArrow["c12", 1, 2],
   QuiverArrow["c23", 2, 3],
   QuiverArrow["c31", 3, 1]
}];

test9 = VerificationTest[
  PIQuiverQ[qNonPI],
  False,
  TestID -> "PIQuiverQ detects multiple cycles"
];

test10 = VerificationTest[
  QuiverVertexCycleCount[qNonPI] === <|1 -> 2, 2 -> 1|>,
  True,
  TestID -> "Vertex cycle counts (non-PI)"
];

test11 = VerificationTest[
  Length[QuiverOrientedCycles[qNonPI]] === 2,
  True,
  TestID -> "Oriented cycles enumeration"
];

stdS2 = QuiverStandardPolynomial[1, {x1, x2}];
test12 = VerificationTest[
  stdS2 === NonCommutativeMultiply[x1, x2] - NonCommutativeMultiply[x2, x1],
  True,
  TestID -> "Standard polynomial S2"
];

test13 = VerificationTest[
  Length[cycleEmbeddings] == 1 && cycleEmbeddings[[1, "Length"]] == 3,
  True,
  TestID -> "Cycle embedding detection"
];

test14 = VerificationTest[
  cyclePhi[{"Arrow", "c12"}] === SparseArray[{{1, 2} -> 1}, {3, 3}],
  True,
  TestID -> "Cycle embedding phi"
];

predA2 = QuiverPIIdealPrediction[qA2, "PathGenerators" -> piA2];
test16 = VerificationTest[
  predA2["TIdealStructure", "ChainLabels"] === {"T1 T0"},
  True,
  TestID -> "PI ideal prediction (A2)"
];

predFull = QuiverPIIdealPrediction[qEx];
test16 = VerificationTest[
  predFull["TIdealStructure", "ChainLabels"] === {"T1 T1"},
  True,
  TestID -> "PI ideal prediction (full example)"
];

predRestricted = QuiverPIIdealPrediction[qEx, "PathGenerators" -> piRestricted];
test17 = VerificationTest[
  Sort[predRestricted["TIdealStructure", "ChainLabels"]] === Sort[{"T1 T1", "T0 T1"}],
  True,
  TestID -> "PI ideal prediction (restricted example)"
];

comm12 = NonCommutativeMultiply[x1, x2] - NonCommutativeMultiply[x2, x1];
expectedT10 = {NonCommutativeMultiply[comm12, x3]};
test19 = VerificationTest[
  QuiverTIdealGenerators[{"T1", "T0"}] === expectedT10,
  True,
  TestID -> "T-ideal generators T1 T0"
];

chainGenAssoc = AssociationThread[
  predRestricted["Decomposition", "Chains"][[All, "ChainLabel"]],
  predRestricted["Decomposition", "Chains"][[All, "Generators"]]
];

expectedT0T1 = {NonCommutativeMultiply[x1, NonCommutativeMultiply[x2, x3] - NonCommutativeMultiply[x3, x2]]};
expectedT1T1 = {NonCommutativeMultiply[comm12, NonCommutativeMultiply[x3, x4] - NonCommutativeMultiply[x4, x3]]};
test19 = VerificationTest[
  chainGenAssoc["T0 T1"] === expectedT0T1 && chainGenAssoc["T1 T1"] === expectedT1T1,
  True,
  TestID -> "Chain generators export"
];

linCombo = QuiverPhiLinear[qA2, {{2, 1}, {3, "alpha"}}];
test20 = VerificationTest[
  linCombo === SparseArray[{{1, 1} -> 2, {1, 2} -> 3}, {2, 2}],
  True,
  TestID -> "Phi linear combination (sparse)"
];

linDense = QuiverPhiLinear[qA2, <|1 -> 5, "alpha" -> 7|>, "Sparse" -> False];
test21 = VerificationTest[
  linDense === {{5, 7}, {0, 0}},
  True,
  TestID -> "Phi linear combination (dense association)"
];

structA2 = QuiverIncidenceStructureConstants[qA2, "PathGenerators" -> piA2];
test22 = VerificationTest[
  structA2["Constants"][{{1, 1}, {1, 2}}] === {1, 2},
  True,
  TestID -> "Incidence structure constant"
];

incProd = QuiverIncidenceMultiply[qA2, {{3, {1, 1}}}, {{4, {1, 2}}}];
test23 = VerificationTest[
  incProd === <|{1, 2} -> 12|>,
  True,
  TestID -> "Incidence multiply association"
];

incMatrix = QuiverIncidenceMultiply[qA2, {{2, {1, 1}}}, {{5, {1, 1}}}, "Output" -> "Matrix"];
test24 = VerificationTest[
  incMatrix === {{10, 0}, {0, 0}},
  True,
  TestID -> "Incidence multiply matrix output"
];

report = TestReport[{test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12, test13, test14, test15, test16, test17, test18, test19, test20, test21, test22, test23, test24}];
Print[report];
If[report["TestsFailed"] > 0,
  Exit[1],
  Exit[0]
];
canonA = QuiverIncidenceCanonicalForm[qA2, "PathGenerators" -> piA2];
canonCycle = QuiverIncidenceCanonicalForm[qCycle];
test22 = VerificationTest[
  !QuiverIncidenceIsomorphicQ[qA2, qCycle, "PathGeneratorsA" -> piA2],
  True,
  TestID -> "Incidence isomorphism negative"
];
