(* ::Package:: *)

BeginPackage["QuiverPI`"];

QuiverArrow::usage = "QuiverArrow[name, source, target] constructs a labeled arrow specification joining two vertices.";
CreateQuiver::usage = "CreateQuiver[vertices, arrows] builds an internal quiver representation with adjacency data.";
QuiverZeroPath::usage = "QuiverZeroPath[quiver, vertex] returns the idempotent path e_vertex.";
QuiverArrowPath::usage = "QuiverArrowPath[quiver, name] returns the single-arrow path associated with the given label.";
QuiverCompositePath::usage = "QuiverCompositePath[quiver, arrowNames] composes a list of arrow names into a path, when possible.";
QuiverPathCompose::usage = "QuiverPathCompose[p, q] composes two compatible paths or returns a Failure if they do not meet.";
QuiverPathToString::usage = "QuiverPathToString[path] renders a human readable description of a path.";
QuiverPhiMatrix::usage = "QuiverPhiMatrix[quiver, path] evaluates the map \\!\\(\\*OverscriptBox[\\(\\[Phi]\\), \\(^\\)]\\)_Q on the given path, producing a matrix unit.";
QuiverIncidenceMask::usage = "QuiverIncidenceMask[quiver, paths] gives the zero-one mask of the associated incidence algebra.";
QuiverIncidenceBasis::usage = "QuiverIncidenceBasis[quiver, paths] returns the list of matrix units spanning the incidence algebra.";
QuiverEnumeratePaths::usage = "QuiverEnumeratePaths[quiver, maxLength] lists all paths of length up to the specified bound.";
QuiverRandomIncidenceElement::usage = "QuiverRandomIncidenceElement[quiver, opts] samples a random linear combination of basis matrices.";
QuiverVerifyPolynomialIdentity::usage = "QuiverVerifyPolynomialIdentity[quiver, polyFun, vars, opts] Monte-Carlo checks that a matrix-valued noncommutative polynomial vanishes on the incidence algebra.";
QuiverMatrixZeroQ::usage = "QuiverMatrixZeroQ[matrix, tol] tests whether all entries of the given matrix are numerically negligible.";
QuiverOrientedCycles::usage = "QuiverOrientedCycles[quiver] returns the vertex lists of all simple oriented cycles in the quiver.";
QuiverVertexCycleCount::usage = "QuiverVertexCycleCount[quiver] returns an association vertex -> number of oriented cycles passing through it.";
PIQuiverQ::usage = "PIQuiverQ[quiver] evaluates the criterion from [CDP] that a quiver is PI iff no vertex belongs to more than one oriented cycle.";

Begin["`Private`"];

ClearAll[
  QuiverArrow,
  CreateQuiver,
  QuiverZeroPath,
  QuiverArrowPath,
  QuiverCompositePath,
  QuiverPathCompose,
  QuiverPathToString,
  QuiverPhiMatrix,
  QuiverIncidenceMask,
  QuiverIncidenceBasis,
  QuiverEnumeratePaths,
  QuiverRandomIncidenceElement,
  QuiverVerifyPolynomialIdentity,
  QuiverMatrixZeroQ,
  QuiverOrientedCycles,
  QuiverVertexCycleCount,
  PIQuiverQ
];

QuiverArrow[name_, source_, target_] := <|
  "Name" -> name,
  "Source" -> source,
  "Target" -> target
|>;

CreateQuiver[vertices_List, arrows_List] := Module[
  {indexAssoc, arrowAssoc, edges, graph},
  indexAssoc = AssociationThread[vertices, Range[Length[vertices]]];
  arrowAssoc = Association @ Table[arrows[[k, "Name"]] -> arrows[[k]], {k, Length[arrows]}];
  edges = DirectedEdge @@@ ({#["Source"], #["Target"]} & /@ arrows);
  graph = Graph[vertices, edges, VertexLabels -> "Name", GraphLayout -> "LayeredDigraphEmbedding"];
  <|
    "Vertices" -> vertices,
    "Arrows" -> arrowAssoc,
    "Graph" -> graph,
    "IndexMap" -> indexAssoc
  |>
];

QuiverZeroPath[quiver_Association, vertex_] := Module[
  {},
  If[! KeyExistsQ[quiver["IndexMap"], vertex],
    Message[QuiverZeroPath::unknownvertex, vertex];
    Return[$Failed];
  ];
  <|"Start" -> vertex, "End" -> vertex, "Arrows" -> {}, "Length" -> 0|>
];
QuiverZeroPath::unknownvertex = "Vertex `1` is not part of the quiver.";

QuiverArrowPath[quiver_Association, name_] := Module[
  {arrow},
  arrow = Lookup[quiver["Arrows"], name, Missing["UnknownArrow"]];
  If[MissingQ[arrow],
    Message[QuiverArrowPath::unknownarrow, name];
    Return[$Failed];
  ];
  <|"Start" -> arrow["Source"], "End" -> arrow["Target"], "Arrows" -> {name}, "Length" -> 1|>
];
QuiverArrowPath::unknownarrow = "Arrow `1` is not part of the quiver.";

QuiverCompositePath[quiver_Association, names_List] /; names === {} :=
  (Message[QuiverCompositePath::emptypath]; Return[$Failed]);
QuiverCompositePath::emptypath = "Provide at least one arrow to build a composite path.";
QuiverCompositePath[quiver_Association, names_List] := Module[
  {singlePaths, composed},
  singlePaths = QuiverArrowPath[quiver, #] & /@ names;
  If[MemberQ[singlePaths, _Failure],
    Return[$Failed];
  ];
  composed = Catch[
    Fold[
      Function[{acc, next},
        With[{prod = QuiverPathCompose[acc, next]},
          If[FailureQ[prod], Throw[prod]];
          prod
        ]
      ],
      First[singlePaths],
      Rest[singlePaths]
    ]
  ];
  composed
];

QuiverPathCompose[left_Association, right_Association] := Module[
  {},
  If[left["End"] =!= right["Start"],
    Return[Failure["NonComposable", <|"Left" -> left, "Right" -> right|>]];
  ];
  <|
    "Start" -> left["Start"],
    "End" -> right["End"],
    "Arrows" -> Join[left["Arrows"], right["Arrows"]],
    "Length" -> left["Length"] + right["Length"]
  |>
];

QuiverPathToString[path_Association] := Module[
  {arrows = path["Arrows"]},
  Which[
    arrows === {}, ToString@path["Start"],
    Length[arrows] == 1, StringJoin["(", ToString[path["Start"]], " -", arrows[[1]], "-> ", ToString[path["End"]], ")"],
    True, StringJoin["(", ToString[path["Start"]], " ⇝ ", ToString[path["End"]], ") via ", StringRiffle[arrows, " ∘ "]]
  ]
];

QuiverPhiMatrix[quiver_Association, path_Association] := Module[
  {idx = quiver["IndexMap"], n = Length[quiver["Vertices"]], i, j},
  i = idx[path["Start"]];
  j = idx[path["End"]];
  SparseArray[{{i, j} -> 1}, {n, n}]
];

QuiverDefaultPathGenerators[quiver_Association] := Module[
  {zeroes, arrows},
  zeroes = QuiverZeroPath[quiver, #] & /@ quiver["Vertices"];
  arrows = QuiverArrowPath[quiver, #] & /@ Keys[quiver["Arrows"]];
  DeleteCases[Join[zeroes, arrows], _Failure]
];

QuiverPathGraph[quiver_Association, generators_List] := Module[
  {edges},
  edges = DirectedEdge[Lookup[#, "Start"], Lookup[#, "End"]] & /@ generators;
  Graph[quiver["Vertices"], edges, GraphLayout -> "LayeredDigraphEmbedding"]
];

QuiverIncidenceMask[quiver_Association, generators_: Automatic] := Module[
  {paths, graph, adj, reach, power, n},
  paths = If[generators === Automatic, QuiverDefaultPathGenerators[quiver], generators];
  graph = QuiverPathGraph[quiver, paths];
  adj = Normal@AdjacencyMatrix[graph];
  n = Length[adj];
  If[n == 0, Return[{}]];
  reach = ConstantArray[0, {n, n}];
  power = Unitize[adj];
  Do[
    reach = Unitize[reach + power];
    power = Unitize[power . adj];
    ,
    {n}
  ];
  reach
];

QuiverIncidenceBasis[quiver_Association, generators_: Automatic] := Module[
  {mask, verts = quiver["Vertices"], n, units = {}},
  mask = QuiverIncidenceMask[quiver, generators];
  n = Length[verts];
  Do[
    If[mask[[i, j]] == 1,
      AppendTo[units, <|
        "Pair" -> {verts[[i]], verts[[j]]},
        "Matrix" -> SparseArray[{{i, j} -> 1}, {n, n}]
      |>]
    ],
    {i, n}, {j, n}
  ];
  units
];

QuiverEnumeratePaths[quiver_Association, maxLength_Integer?NonNegative] := Module[
  {paths = {}, vertices = quiver["Vertices"], arrowNames = Keys[quiver["Arrows"]], level = <||>, current},
  paths = QuiverZeroPath[quiver, #] & /@ vertices;
  If[maxLength == 0, Return[paths]];
  level[1] = DeleteCases[QuiverArrowPath[quiver, #] & /@ arrowNames, _Failure];
  paths = Join[paths, Lookup[level, 1, {}]];
  Do[
    level[len] = DeleteCases[
      Flatten[
        Table[
          With[{prod = QuiverPathCompose[p, QuiverArrowPath[quiver, name]]},
            If[FailureQ[prod], Nothing, prod]
          ],
          {p, level[len - 1]},
          {name, arrowNames}
        ],
        1
      ],
      _Failure
    ];
    paths = Join[paths, Lookup[level, len, {}]];
    ,
    {len, 2, maxLength}
  ];
  paths
];

QuiverOrientedCycles[quiver_Association] := Module[
  {graph = quiver["Graph"], rawCycles, vertexCycles, loopCycles},
  rawCycles = FindCycle[graph, Infinity, All];
  vertexCycles = First /@ # & /@ rawCycles;
  loopCycles = ({#["Source"]} &) /@ Select[Values[quiver["Arrows"]], #["Source"] === #["Target"] &];
  Join[vertexCycles, loopCycles]
];

QuiverVertexCycleCount[quiver_Association] := Module[
  {cycles = QuiverOrientedCycles[quiver], vertices = quiver["Vertices"], counts},
  counts = Counts[Flatten[cycles]];
  AssociationThread[vertices, Lookup[counts, vertices, 0]]
];

PIQuiverQ[quiver_Association] := Module[
  {counts = QuiverVertexCycleCount[quiver]},
  AllTrue[Values[counts], # <= 1 &]
];

Options[QuiverRandomIncidenceElement] = {
  "PathGenerators" -> Automatic,
  "CoefficientRange" -> {-1, 1},
  "Density" -> 1.0
};
QuiverRandomIncidenceElement[quiver_Association, OptionsPattern[]] := Module[
  {basis = QuiverIncidenceBasis[quiver, OptionValue["PathGenerators"]], range, density, chosen, coeffs, n},
  n = Length[quiver["Vertices"]];
  If[basis === {},
    Return[SparseArray[{}, {n, n}]];
  ];
  range = OptionValue["CoefficientRange"];
  density = Clip[OptionValue["Density"], {0, 1}];
  chosen = Select[basis, RandomReal[] <= density];
  If[chosen === {}, chosen = {RandomChoice[basis]}];
  coeffs = RandomReal[range, Length[chosen]];
  Total[MapThread[#1 #2["Matrix"] &, {coeffs, chosen}]]
];

QuiverMatrixZeroQ[m_, tol_: 10^-9] := Module[
  {absMax},
  absMax = Max[Abs[Normal[m]]];
  absMax < tol
];

Options[QuiverVerifyPolynomialIdentity] = {
  "PathGenerators" -> Automatic,
  "Samples" -> 32,
  "CoefficientRange" -> {-1, 1},
  "Density" -> 1.0,
  "Tolerance" -> 10^-9
};
QuiverVerifyPolynomialIdentity[quiver_Association, poly_Function, vars_List, OptionsPattern[]] := Module[
  {samples = OptionValue["Samples"], tol = OptionValue["Tolerance"], generatorOpt = OptionValue["PathGenerators"],
   range = OptionValue["CoefficientRange"], density = OptionValue["Density"], i, assignment, result},
  Do[
    assignment = AssociationThread[
      vars,
      Table[
        QuiverRandomIncidenceElement[quiver,
          "PathGenerators" -> generatorOpt,
          "CoefficientRange" -> range,
          "Density" -> density
        ],
        {Length[vars]}
      ]
    ];
    result = poly[assignment];
    If[!QuiverMatrixZeroQ[result, tol],
      Return[False]
    ];
    ,
    {i, samples}
  ];
  True
];

End[];

EndPackage[];
