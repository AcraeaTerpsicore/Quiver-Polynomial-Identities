(* ::Package:: *)

BeginPackage["QuiverPI`"];

QuiverArrow::usage = "QuiverArrow[name, source, target] constructs a labeled arrow specification joining two vertices.";
CreateQuiver::usage = "CreateQuiver[vertices, arrows] builds an internal quiver representation with adjacency data.";
QuiverZeroPath::usage = "QuiverZeroPath[quiver, vertex] returns the idempotent path e_vertex.";
QuiverArrowPath::usage = "QuiverArrowPath[quiver, name] returns the single-arrow path associated with the given label.";
QuiverCompositePath::usage = "QuiverCompositePath[quiver, arrowNames] composes a list of arrow names into a path, when possible.";
QuiverPhiLinear::usage = "QuiverPhiLinear[quiver, terms, opts] extends \\!\\(\\*SubscriptBox[\\(\\varphi\\), \\(Q\\)]\\) to linear combinations of paths.";
QuiverPathCompose::usage = "QuiverPathCompose[p, q] composes two compatible paths or returns a Failure if they do not meet.";
QuiverPathToString::usage = "QuiverPathToString[path] renders a human readable description of a path.";
QuiverPhiMatrix::usage = "QuiverPhiMatrix[quiver, path] evaluates the map \\!\\(\\*OverscriptBox[\\(\\[Phi]\\), \\(^\\)]\\)_Q on the given path, producing a matrix unit.";
QuiverIncidenceMask::usage = "QuiverIncidenceMask[quiver, paths] gives the zero-one mask of the associated incidence algebra.";
QuiverIncidenceBasis::usage = "QuiverIncidenceBasis[quiver, paths] returns the list of matrix units spanning the incidence algebra.";
QuiverIncidencePoset::usage = "QuiverIncidencePoset[quiver, opts] returns the transitive relation, Hasse diagram, and other poset data induced by the path generators.";
QuiverPosetDecomposition::usage = "QuiverPosetDecomposition[quiver, opts] decomposes the incidence poset into strongly connected component blocks of type T_n or T_0.";
QuiverPIIdealPrediction::usage = "QuiverPIIdealPrediction[quiver, opts] predicts \\!\\(\\*SubscriptBox[\\(\\text{id}\\), \\(T\\)]\\)(FQ_\\!\\(\\*SubscriptBox[\\(\\pi\\), \\(\\)]\\)) using the T-ideal decomposition described in the reference paper.";
QuiverTIdealGenerators::usage = "QuiverTIdealGenerators[blockTypes] returns symbolic generator strings for the T-ideal associated with the sequence of block types (e.g., {\"T1\",\"T0\"}).";
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
  QuiverPhiLinear,
  QuiverPathCompose,
  QuiverPathToString,
  QuiverNormalizePath,
  QuiverNormalizeLinearTerms,
  QuiverPhiMatrix,
  QuiverIncidenceMask,
  QuiverIncidenceBasis,
  QuiverIncidencePoset,
  QuiverPosetDecomposition,
  QuiverPIIdealPrediction,
  QuiverTIdealGenerators,
  QuiverEnumeratePaths,
  QuiverRandomIncidenceElement,
  QuiverVerifyPolynomialIdentity,
  QuiverResolveGenerators,
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

QuiverResolveGenerators[quiver_Association, Automatic] := QuiverDefaultPathGenerators[quiver];
QuiverResolveGenerators[_, generators_List] := generators;
QuiverResolveGenerators[_, generators_] := generators;

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

QuiverNormalizePath[quiver_Association, spec_] := Module[
  {arrows = quiver["Arrows"], idx = quiver["IndexMap"]},
  Which[
    AssociationQ[spec] && And @@ (KeyExistsQ[spec, #] & /@ {"Start", "End", "Arrows", "Length"}),
      spec,
    MatchQ[spec, {"Zero", v_}],
      QuiverZeroPath[quiver, spec[[2]]],
    KeyExistsQ[idx, spec],
      QuiverZeroPath[quiver, spec],
    MatchQ[spec, {"Arrow", name_}],
      QuiverArrowPath[quiver, spec[[2]]],
    KeyExistsQ[arrows, spec],
      QuiverArrowPath[quiver, spec],
    MatchQ[spec, {"Composite", names_List}],
      QuiverCompositePath[quiver, spec[[2]]],
    ListQ[spec] && AllTrue[spec, KeyExistsQ[arrows, #] &],
      QuiverCompositePath[quiver, spec],
    True,
      Message[QuiverNormalizePath::unknown, spec];
      Failure["UnknownPath", <|"Specification" -> spec|>]
  ]
];
QuiverNormalizePath::unknown = "Cannot interpret `1` as a path specification.";

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

QuiverNormalizeLinearTerms[data_] := Module[
  {termsList},
  Which[
    AssociationQ[data],
      termsList = Normal[data],
    ListQ[data],
      termsList = data,
    True,
      termsList = {data}
  ];
  termsList = termsList /. Rule[s_, c_] :> {c, s};
  If[!VectorQ[termsList, MatchQ[#, {_, _}] &],
    Message[QuiverPhiLinear::term, termsList];
    Return[$Failed];
  ];
  termsList
];

Options[QuiverPhiLinear] = {
  "Sparse" -> True
};
QuiverPhiLinear[quiver_Association, terms_, OptionsPattern[]] := Module[
  {normalized = QuiverNormalizeLinearTerms[terms], idx = quiver["IndexMap"], n = Length[quiver["Vertices"]],
   matrix},
  If[FailureQ[normalized], Return[$Failed]];
  matrix = ConstantArray[0, {n, n}];
  Do[
    With[{coeff = term[[1]], spec = term[[2]], path = QuiverNormalizePath[quiver, term[[2]]]},
      If[FailureQ[path],
        Message[QuiverPhiLinear::badpath, spec];
        Continue[];
      ];
      matrix[[idx[path["Start"]], idx[path["End"]]]] += coeff;
    ],
    {term, normalized}
  ];
  If[TrueQ[OptionValue["Sparse"]],
    SparseArray[matrix],
    matrix
  ]
];
QuiverPhiLinear::term = "Unable to interpret `1` as {coefficient, pathSpecification} terms.";
QuiverPhiLinear::badpath = "Skipping unrecognized path specification `1`.";

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
  paths = QuiverResolveGenerators[quiver, generators];
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

Options[QuiverIncidencePoset] = {
  "PathGenerators" -> Automatic
};
QuiverIncidencePoset[quiver_Association, OptionsPattern[]] := Module[
  {paths = QuiverResolveGenerators[quiver, OptionValue["PathGenerators"]],
   mask, vertices = quiver["Vertices"], n, relationEdges, coverEdges, relationGraph, coverGraph},
  mask = QuiverIncidenceMask[quiver, paths];
  n = Length[vertices];
  relationEdges = DeleteDuplicates @ Flatten[
      Table[
        If[mask[[i, j]] == 1 && i =!= j,
          DirectedEdge[vertices[[i]], vertices[[j]]],
          Nothing
        ],
        {i, n}, {j, n}
      ],
      1
    ];
  coverEdges = Select[relationEdges, QuiverCoverEdgeQ[#, mask, vertices] &];
  relationGraph = Graph[vertices, relationEdges, GraphLayout -> "LayeredDigraphEmbedding"];
  coverGraph = Graph[vertices, coverEdges, GraphLayout -> "LayeredDigraphEmbedding"];
  <|
    "Vertices" -> vertices,
    "Mask" -> mask,
    "RelationEdges" -> relationEdges,
    "RelationGraph" -> relationGraph,
    "CoverEdges" -> coverEdges,
    "HasseDiagram" -> coverGraph,
    "PathGenerators" -> paths
  |>
];

Options[QuiverPosetDecomposition] = {
  "PathGenerators" -> Automatic
};
QuiverPosetDecomposition[quiver_Association, OptionsPattern[]] := Module[
  {paths = QuiverResolveGenerators[quiver, OptionValue["PathGenerators"]],
   vertices = quiver["Vertices"], vertexIndex, components, componentInfo,
   vertexComponent, mask, componentEdges, condGraph, chainIDs, componentAssoc,
   chainDescriptors},
  mask = QuiverIncidenceMask[quiver, paths];
  vertexIndex = AssociationThread[vertices, Range[Length[vertices]]];
  components = QuiverStronglyConnectedComponents[mask, vertices];
  componentInfo = MapIndexed[
    With[{verts = Sort[#1], id = First[#2]},
      <|
        "ID" -> id,
        "Vertices" -> verts,
        "Size" -> Length[verts],
        "Type" -> QuiverBlockTypeLabel[verts, mask, vertexIndex]
      |>
    ] &,
    components
  ];
  vertexComponent = Association @ Flatten[
      Table[
        # -> componentInfo[[id, "ID"]] & /@ componentInfo[[id, "Vertices"]],
        {id, Length[componentInfo]}
      ]
    ];
  componentEdges = DeleteDuplicates @ Flatten[
      Table[
        With[{from = vertexComponent[vertices[[i]]], to = vertexComponent[vertices[[j]]]},
          If[from =!= to && mask[[i, j]] == 1,
            DirectedEdge[from, to],
            Nothing
          ]
        ],
        {i, Length[vertices]}, {j, Length[vertices]}
      ],
      1
    ];
  condGraph = Graph[
    componentInfo[[All, "ID"]],
    componentEdges,
    VertexLabels -> Table[
      componentInfo[[i, "ID"]] -> Row[{componentInfo[[i, "ID"]], " (" <> componentInfo[[i, "Type"]] <> ")"}],
      {i, Length[componentInfo]}
    ],
    GraphLayout -> "LayeredDigraphEmbedding"
  ];
  chainIDs = QuiverComponentChainsFromEdges[componentInfo[[All, "ID"]], componentEdges];
  componentAssoc = AssociationThread[componentInfo[[All, "ID"]], componentInfo];
  chainDescriptors = Table[
    With[{blocks = Lookup[componentAssoc, chainIDs[[k]]]},
      With[{types = blocks[[All, "Type"]]},
        With[{label = StringRiffle[types, " "]},
          <|
            "ComponentIDs" -> chainIDs[[k]],
            "Blocks" -> blocks,
            "BlockTypes" -> types,
            "ChainLabel" -> label,
            "IdealLabel" -> "I(" <> label <> ")",
            "Generators" -> QuiverTIdealGenerators[types]
          |>
        ]
      ]
    ],
    {k, Length[chainIDs]}
  ];
  <|
    "Components" -> componentInfo,
    "ComponentGraph" -> condGraph,
    "Chains" -> chainDescriptors,
    "PathGenerators" -> paths
  |>
];

Options[QuiverPIIdealPrediction] = {
  "PathGenerators" -> Automatic
};
QuiverPIIdealPrediction[quiver_Association, OptionsPattern[]] := Module[
  {paths = QuiverResolveGenerators[quiver, OptionValue["PathGenerators"]],
   poset, decomposition, chainLabels, uniqueLabels, expression},
  poset = QuiverIncidencePoset[quiver, "PathGenerators" -> paths];
  decomposition = QuiverPosetDecomposition[quiver, "PathGenerators" -> paths];
  chainLabels = decomposition["Chains"][[All, "ChainLabel"]];
  uniqueLabels = DeleteDuplicates[chainLabels];
  expression = If[uniqueLabels === {},
    "Trivial",
    StringRiffle["I(" <> # <> ")" & /@ uniqueLabels, " ∩ "]
  ];
  <|
    "Poset" -> poset,
    "Decomposition" -> decomposition,
    "TIdealStructure" -> <|
      "ChainLabels" -> uniqueLabels,
      "Expression" -> expression,
      "ChainDescriptors" -> decomposition["Chains"]
    |>
  |>
];

QuiverCoverEdgeQ[DirectedEdge[u_, v_], mask_, vertices_] := Module[
  {i = First@FirstPosition[vertices, u], j = First@FirstPosition[vertices, v], n = Length[vertices]},
  If[i === j || mask[[i, j]] == 0, Return[False]];
  !AnyTrue[
    Range[n],
    Function[k,
      k =!= i && k =!= j && mask[[i, k]] == 1 && mask[[k, j]] == 1
    ]
  ]
];

QuiverBlockTypeLabel[verts_List, mask_, vertexIndex_Association] := Module[
  {size = Length[verts]},
  Which[
    size == 0, "T0",
    size == 1,
      With[{idx = Lookup[vertexIndex, First[verts], None]},
        If[idx === None,
          "T0",
          If[mask[[idx, idx]] == 1, "T1", "T0"]
        ]
      ],
    True,
      "T" <> ToString[size]
  ]
];

QuiverStronglyConnectedComponents[mask_, vertices_List] := Module[
  {remaining = Range[Length[vertices]], components = {}, idx, forward, backward, compIdx},
  While[remaining =!= {},
    idx = First[remaining];
    forward = Flatten@Position[mask[[idx]], 1];
    backward = Flatten@Position[mask[[All, idx]], 1];
    compIdx = Union[{idx}, Intersection[forward, backward]];
    AppendTo[components, vertices[[compIdx]]];
    remaining = Complement[remaining, compIdx];
  ];
  components
];

QuiverComponentChainsFromEdges[ids_List, edges_List] := Module[
  {chains = {}, succMap, indegree, mins, dfs},
  succMap = AssociationThread[ids, ConstantArray[{}, Length[ids]]];
  indegree = AssociationThread[ids, ConstantArray[0, Length[ids]]];
  Do[
    Module[{edgeList = List @@ edge, from, to},
      from = edgeList[[1]];
      to = edgeList[[2]];
      succMap[from] = Append[succMap[from], to];
      indegree[to] = indegree[to] + 1;
    ],
    {edge, edges}
  ];
  mins = Select[ids, indegree[#] == 0 &];
  If[mins === {}, mins = ids];
  dfs[node_, current_] := Module[{succ = Lookup[succMap, node, {}]},
    If[succ === {} || succ === Null,
      AppendTo[chains, current],
      Do[dfs[next, Append[current, next]], {next, succ}]
    ]
  ];
  Do[dfs[min, {min}], {min, mins}];
  DeleteDuplicates[chains]
];

QuiverTIdealGenerators[types_List] := Module[
  {counter = 1, blockGenerators, combine},
  blockGenerators["T0"] := Module[{v1 = counter++}, {"x" <> ToString[v1]}];
  blockGenerators["T1"] := Module[{v1 = counter++, v2 = counter++}, {"[x" <> ToString[v1] <> ", x" <> ToString[v2] <> "]"}];
  blockGenerators["T" ~~ nStr_] := Module[{n = Quiet@Check[ToExpression[nStr], -1]},
    Which[
      n >= 2, {"S_" <> ToString[2 n]},
      n == 1, blockGenerators["T1"],
      True, {"I(T" <> nStr <> ")"}
    ]
  ];
  blockGenerators[_] := {"I(Unknown)"};
  combine = Fold[
    Function[{acc, block},
      Flatten@Table[
        StringTrim @ StringReplace[accStr <> " " <> blockStr, StartOfString ~~ " " -> ""],
        {accStr, acc},
        {blockStr, blockGenerators[block]}
      ]
    ],
    {""},
    types
  ];
  DeleteCases[combine, ""]
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
