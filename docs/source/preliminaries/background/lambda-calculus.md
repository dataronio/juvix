# Lambda Calculus

## Basics

- Define terms $t ::= x\ |\ λx.t\ |\ tt$.
- Define the set of free variables of a term $t$, $FV(t)$, as all referenced but not bound variables in $t$.
- Define β-reduction as: $(λx.t_1) t_2$ = $t_1 [x := t_2]$ (capture-avoiding substitution).
- Two terms are α-equivalent if they are equal up to renaming of bound variables.
- β-reduction is confluent modulo α-equivalence.
- Define η-conversion as $λx. f x$ = $f$ iff. $x \notin FV(f)$.
- de Brujin indices: numbers for bound variables. $λx.x$ = $λ.1$, $λx.λy.x$ = $λ.2$, etc.

## Universality

- Lambda calculus can express non-terminating computations, e.g. $(λx. x x) (λx. x x)$.
- Lambda calculus is Turing-complete.

## Efficiency

Beta-reduction is nontrivial, must search for & replace all instances of variable being substituted for.

Choice of reduction strategy: given $(λx. t_1) t_2$, which of $t_1$ or $t_2$ to reduce first.

- Normal order: leftmost-outermost redex reduced first (arguments substituted before reduction).

- Call-by-name: as normal order except that no reductions are performed inside abstractions.

- Call-by-value: only outermost redexes reduced, only when right-hand side has reduced to a value (variable or lambda abstraction) - reduce $t_2$ first, substitute in for $x$ after reduction.

- Call-by-need: create thunk for evaluation of $t_2$, pass into $t_1$, if reduced value will be shared. Challenge: "computing under a lambda", sharing a function. Haskell et al. don't do this.

Computing under a function: $(λx.λy.map (λz. z + x + y) zs) 2 3$, $x + y$ should be evaluated once, GHC's STG machine does not do this (only evaluates function once all arguments are passed).

(easy to optimise if 2 & 3 are known at compile time, but that's not the case in general)

e.g. $n 2 I a$ with $n$ and $2$ Church-encoded naturals, exponential in $n$ because the applications of $I$ will be duplicated.

Example term: $λx.(x (λw.w)) λy.(λx.x x)(y z)$.

Define optimality using Levy's framework.

## Encoding data structures

Church encoding of natural numbers:

- $n = λs.λz.s {... n} z$.
- $Z = λs.λz.z$, ,$S = λk.λs.λz.(s (k s z))$.
- $plus = λa.λb.λs.λz.a s (b s z)$.
- $mult = λa.λb.λs.λz.a (b s) z$.
- $exp  = λa.λb.λs.λz.(b a) s z$.
- $pred = λa.λs.λz.a (λg.λh.h (g s)) (λu.z) (λz.u)$.

Church encoding of booleans:

- $true = λt.λf.t$.
- $false = λt.λf.f$.

Church encoding of pairs:

- $pair = λx.λy.λz.z x y$.
- $fst  = λp.p (λx.λy.x)$.
- $snd  = λp.p (λx.λy.y)$.

Scott encoding of constructor $c_i$ of datatype $D$ with arity $A_i$:

- $λx_1 ... x_A . λc_1 ... c_N . c_i x_1 ... x_{A_i}$.
- Compare with Church encoding: $λx_1 ... x_A . λc_1 ... c_N . c_i (x_1 c_1 ... c_N) ... (x_{A_i} c_1 ... c_N)$.
- Scott-encoded datatypes are their own pattern matching functions.
