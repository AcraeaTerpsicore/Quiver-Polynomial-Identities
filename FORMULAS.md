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
- The map extends linearly to combinations $f = \sum_k \lambda_k p_k$ via
  \[
    \varphi_Q(f) = \sum_k \lambda_k\, e_{s(p_k), t(p_k)},
  \]
  implemented by `QuiverPhiLinear` which accepts terms like $\{ \lambda_k, p_k \}$ or vertex/arrow specifications.
- Incidence basis elements multiply according to
  \[
    e_{i,j} \cdot e_{k,\ell} =
    \begin{cases}
      e_{i,\ell}, & j = k,\\
      0, & \text{otherwise,}
    \end{cases}
  \]
  and the helper `QuiverIncidenceMultiply` enforces this rule while checking whether $(i,\ell)$ belongs to the allowed mask.

## Cycle Embeddings

- Any oriented cycle $(v_1 \to v_2 \to \cdots \to v_n \to v_1)$ yields a canonical embedding $\Phi_{\text{cycle}}: FQ_{\text{cycle}} \to M_n(F)$ defined by $\Phi(p) = e_{s(p), t(p)}$ after relabeling vertices so that $v_i$ corresponds to row/column $i$. 
- `QuiverCycleEmbeddings` detects such cycles and provides the matrix units together with a `Phi` evaluator that rejects paths leaving the cycle.

## Incidence-Isomorphism Invariant

- Following \cite{B}, incidence algebras $A_\pi$ and $A_{\pi'}$ are isomorphic iff their posets (and structure constants with respect to a compatible basis) coincide.
- The canonical form stored by `QuiverIncidenceCanonicalForm` includes the path generators chosen, the mask $M$ (reachable pairs), sorted chain decomposition, and the structure tensor 
  \[
    c_{(i,j),(k,\ell)} =
    \begin{cases}
        (i,\ell), & j = k \text{ and } (i,\ell) \in M, \\
        \text{None}, & \text{otherwise.}
    \end{cases}
  \]
- `QuiverIncidenceIsomorphicQ` compares these canonical forms to decide isomorphism.

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

## T-Ideal Decomposition

- We distinguish two basic building blocks following \cite{B}: $T_0$ is a singleton with no relation, whereas $T_n$ is the $n$-element set endowed with the full relation $T_n \times T_n$ (PI-equivalent to $M_n(F)$). A strongly connected component of size $n$ corresponds to $T_n$; a singleton without a loop corresponds to $T_0$.
- The incidence poset factors through its strongly connected components. Every maximal chain of components yields an ordinal product such as $T_1 T_0$ or $T_1 T_1$, and the predicted $\mathrm{id}(FQ_\pi)$ is the intersection of the $I(\cdot)$ ideals coming from all maximal chains.
- Example: for $\pi = \{e_1, \alpha\}$ in the $A_2$ quiver, the component chain is $T_1 T_0$, hence $I(T_1 T_0) = \langle [x_1,x_2]x_3\rangle_T$. For the three-vertex quiver of Example 2, the chains are $T_1 T_1$, so $\mathrm{id}(FQ_\pi) = I(T_1 T_1) = \langle [x_1,x_2][x_3,x_4]\rangle_T$.
- The helper `QuiverTIdealGenerators` converts any chain of block types into explicit symbolic generators by multiplying the canonical templates ($x_i$ for $T_0$, $[x_i,x_j]$ for $T_1$, the standard polynomial $S_{2n}$ for $T_n$).
