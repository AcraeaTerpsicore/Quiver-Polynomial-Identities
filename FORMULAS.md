# Formulas

All formulas reference the notation from `reference_paper/BCDP_3.tex`. They capture the algebraic relations implemented in `src/QuiverPI/QuiverPI.wl`.

## Path Algebra

- Paths concatenate when the tail of the left factor meets the head of the right factor:
  \[
    p \cdot q =
    \begin{cases}
      pq,& t(p) = s(q),\\
      0,& \text{otherwise.}
    \end{cases}
  \]
- The identity element is the sum of the trivial paths:
  \[
    1 = \sum_{i \in Q_0} e_i, \qquad s(e_i) = t(e_i) = i.
  \]

## Incidence Algebra Map

- For a quiver with vertices $Q_0 = \{1,\dots,n\}$ the map
  \[
    \varphi_Q : FQ \longrightarrow M_n(F), \qquad \varphi_Q(p) = e_{s(p), t(p)}
  \]
  sends any path to the corresponding matrix unit. Its image $A_Q = \mathrm{im}(\varphi_Q)$ is an incidence algebra.
- Given a set of generators $\pi$ (paths in $Q$), the closure $\tilde{\pi}$ determines
  \[
    A_\pi = \varphi_Q(FQ_\pi) = \{ A \in M_n(F) \mid A_{ij} = 0 \text{ if no path in } \tilde{\pi} \text{ goes from } i \text{ to } j \}.
  \]

## Polynomial Identities

- The commutator used throughout the code is
  \[
    [x_i, x_j] = x_i x_j - x_j x_i.
  \]
- Example 1 enforces the identity
  \[
    [x_1, x_2]\,x_3 = 0.
  \]
- Examples 2–3 enforce Malcev's identity for upper-triangular incidences:
  \[
    [x_1, x_2]\,[x_3, x_4] = 0.
  \]

These formulas feed the Wolfram Language helpers that build quiver incidence masks, enumerate paths, and run Monte Carlo PI checks.

## PI Criterion

Following \cite{CDP}, a finite quiver $Q$ is PI if and only if each vertex belongs to at most one oriented cycle. The helper `PIQuiverQ` implements this by enumerating all simple directed cycles (including loops) and counting their incidences:
\[
Q \text{ is PI } \Longleftrightarrow \forall v \in Q_0,\quad \#\{\text{oriented cycles containing } v\} \le 1.
\]
