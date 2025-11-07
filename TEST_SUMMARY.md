# Test Summary

| Command | Description | Result |
| --- | --- | --- |
| `"/mnt/d/Software/Wolfram Research/Mathematica/14.0/wolframscript.exe" -file tests/run_tests.wl` | Runs eleven VerificationTests that recreate Examples 1–3 and exercise the new PI detection helpers: incidence masks for $A_2$ and the three-vertex quiver, $\varphi_Q$ evaluations, bounded path enumeration, Monte Carlo checks that $[x_1,x_2]x_3$ and $[x_1,x_2][x_3,x_4]$ vanish, plus validations that `QuiverOrientedCycles`, `QuiverVertexCycleCount`, and `PIQuiverQ` agree with the Cerulli–De Loera–Pascucci criterion on PI vs. non-PI quivers. | Pass (all tests succeeded) |

All tests were executed from `/mnt/d/ExplorerDownload/arXiv-2511.03536v1` inside WSL (Arch) using Mathematica 14's `wolframscript`.
