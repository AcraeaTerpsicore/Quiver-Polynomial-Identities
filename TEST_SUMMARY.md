# Test Summary

| Command | Description | Result |
| --- | --- | --- |
| `"/mnt/d/Software/Wolfram Research/Mathematica/14.0/wolframscript.exe" -file tests/run_tests.wl` | Runs fourteen VerificationTests that recreate Examples 1–3, validate the PI-detection helpers, and now also ensure `QuiverIncidencePoset`, `QuiverPosetDecomposition`, and `QuiverPIIdealPrediction` produce the expected T-ideal expressions (`I(T_1 T_0)`, `I(T_1 T_1)`, `I(T_1 T_1) ∩ I(T_0 T_1)`) for the $A_2$ quiver and its variants. | Pass (all tests succeeded) |

All tests were executed from `/mnt/d/ExplorerDownload/arXiv-2511.03536v1` inside WSL (Arch) using Mathematica 14's `wolframscript`.
