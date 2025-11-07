# Quiver Polynomial Identities

Implementation of the constructions outlined in `reference_paper/BCDP_3.tex` (Berele–Cerulli Irelli–De Loera Chávez–Pascucci). The project exposes a Wolfram Language toolkit for building finite quivers, enumerating their path semigroups, constructing the associated incidence algebras, and empirically checking polynomial identities that follow from the paper.

## Project Layout

- `src/QuiverPI/QuiverPI.wl` – Wolfram Language package containing quiver/ path algebra helpers, incidence algebra computations, and stochastic PI verification.
- `tests/run_tests.wl` – automated regression tests that recreate the paper's examples in Mathematica.
- `FORMULAS.md` – formulas cited from the reference paper that drive the code.
- `TEST_SUMMARY.md` – description of the verification commands that were executed.
- `reference_paper/` – original TeX source (ignored from version control per requirements).

## Usage

1. Ensure `wolframscript.exe` (Mathematica 14) is available at `D:\Software\Wolfram Research\Mathematica\14.0\wolframscript.exe`.
2. From the project root, load the package:

   ```wolfram
   Get["D:/ExplorerDownload/arXiv-2511.03536v1/src/QuiverPI/QuiverPI.wl"];
   ```

3. Build a quiver, extract its incidence algebra, and verify a PI:

   ```wolfram
   q = CreateQuiver[{1, 2}, {QuiverArrow["alpha", 1, 2]}];
   pi = {QuiverZeroPath[q, 1], QuiverArrowPath[q, "alpha"]};
   mask = QuiverIncidenceMask[q, pi]; (* {{1, 1}, {0, 0}} *)

   poly = Function[{vars}, (vars["x1"].vars["x2"] - vars["x2"].vars["x1"]).vars["x3"]];
   QuiverVerifyPolynomialIdentity[q, poly, {"x1", "x2", "x3"},
     "PathGenerators" -> pi,
     "Samples" -> 32
   ]
   ```

4. Enumerate paths (e.g., for exporting into other systems):

   ```wolfram
   pathsUpTo2 = QuiverEnumeratePaths[q, 2];
   ```

Key exported utilities are described inline within `src/QuiverPI/QuiverPI.wl`.

## Running Tests

From the project root in WSL:

```bash
"/mnt/d/Software/Wolfram Research/Mathematica/14.0/wolframscript.exe" -file tests/run_tests.wl
```

The suite covers:

- The $A_2$ quiver subalgebra from Example 1 ($[x_1,x_2]x_3 = 0$).
- The three-vertex quiver (Examples 2–3) satisfying $[x_1,x_2][x_3,x_4]=0$ in both full and restricted path settings.
- Structural helpers like $\varphi_Q$ images, incidence masks, and bounded path enumeration.

See `TEST_SUMMARY.md` for a concise log of executed commands.

## References

- Berele, Cerulli Irelli, De Loera Chávez, Pascucci. *Polynomial identities for quivers via incidence algebras*, arXiv:2511.03536.
- Berele. *Incidence algebras, polynomial identities, and an $A \otimes B$ counterexample*, Comm. Algebra (1984).
