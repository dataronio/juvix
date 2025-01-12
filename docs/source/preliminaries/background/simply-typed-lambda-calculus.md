# Simply Typed Lambda Calculus

- Eliminate "bad" uses of lambda calculus
- No recursion, always terminates
- No polymorphism
- No guarantees on termination bounds (complexity) or copying
- Extrinsic: assigning type to lambda terms. Intrinsic: type is part of term.

- Set $B$ of base types, set $C$ of term constants (e.g. natural numbers).
- $τ ::= T\ |\ τ\ →\ τ$ with $T \in B$.
- $e ::= x\ |\ λx:τ.e\ |\ e\ e\ |\ c$ with $c \in C$.

## Typing rules for simply-typed lambda calculus

$$
\begin{prooftree}
\AxiomC{$x:τ \in Γ$}
\RightLabel{var}
\UnaryInfC{$Γ ⊢ x:τ$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$c \in C$}
\RightLabel{const}
\UnaryInfC{$Γ ⊢ c:T$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$Γ, x:τ_1 ⊢ e:τ_2$}
\RightLabel{lam}
\UnaryInfC{$Γ ⊢ (λx:τ_1.e) : (τ_1 → τ_2)$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$Γ ⊢ e_1 : τ_1 → τ_2$}
\AxiomC{$Γ ⊢ e_2 : τ_1$}
\RightLabel{app}
\BinaryInfC{$Γ ⊢ e_1 e_2 : τ_2$}
\end{prooftree}
$$