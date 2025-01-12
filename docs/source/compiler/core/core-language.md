# Core Language

## Basics

Juvix Core is the core language in the Juvix compiler stack, defining the canonical syntax & semantics on which all compilers & interpreters must agree. Lower-level evaluation choices may safely differ between implementations as long as they respect the core semantics.

Juvix Core is primarily inspired by Quantitative Type Theory {footcite}`quantitative-type-theory`, Formality {footcite}`formality`, and Cedille {footcite}`the-calculus-of-dependent-lambda-eliminations`. It fuses full-spectrum dependent types (types & terms can depend on types & terms) with linear logic using the contemplation — computation distinction introduced by QTT, adds the self-types of Cedille & Formality to enable derivation of induction for pure lambda terms, introduces the full connective set of linear logic, dependent where appropriate, to express different varieties of conjunction & disjunction, and defines an extension system for opaque user-defined datatypes & primitives (such as integers with addition & multiplication, bytestrings with concatenation & indexing, or cryptographic keys with construction & signature checking).

```{note}
At present, the substructural typing in the core language is not required for optimal reduction — separate elementary affine logic assignments are inferred in the lower-level stage. Substructural typing is used in Juvix Core to provide additional precision to the programmer and enable optimisations in the bespoke compilation path to custom rewrite rules (such as avoiding garbage collection). In the future a closer fusion allowing the more precise usage information to inform interaction net construction is expected; this is an open research question.
```

## Changes from QTT

### Primitive types, constants, and functions

Nothing too exciting here. These will vary based on the machine target.

### Additional linear logic connectives

#### Multiplicative disjunction

Why?

- Explicit parallelism
- Hints for interaction net runtime
- Translated as multiplicative conjunction, but must be de-structured as a whole

#### Dependent additive conjunction

Why?

- Resourceful production
- Translated as multiplicative conjunction, but only one of $fst$ or $snd$ can be used
- Avoids duplication of resources at runtime
- Can be represented as a function, but wouldn't have the same evaluation semantics

#### Self types

Why?

- Elegant induction for lambda-encoded data
- No runtime presence

### Usage polymorphism

An example, with Church-encoded naturals:

```
one :: 1 (1 (a -> a) -> 1 a -> a)

two :: 1 (2 (a -> a) -> 1 a -> a)

three :: 1 (3 (a -> a) -> 1 a -> a)
```

where, ideally, we have

```
succ :: 1 (1 (n (a -> a) -> 1 a -> a) -> ((n + 1) (a -> a) -> 1 a -> a)
```

#### Typeclass-style usage polymorphism in the frontend language

Frontend language level usage polymorphism, where $succ$ must be instantiated for an $n$ known at compile time at each call site (like a typeclass), and must be instantiated with $ω$ if $n$ is unknown at the call site. This is easy, but probably not that useful, since often we won't know $n$ for the argument at compile time.

#### Usage polymorphism in the type theory

Add a $∀ u .\, T$, where $u$ ranges over the semiring, to the core type theory, and can then appear as a variable for any usage in $T$. This then will require n-variable polynomial constraint satisfiability checks during typechecking, but should have zero runtime cost. It may impact the kinds of memory management we can automate, not sure yet, but we should still have more information than without quantisation at all (or, equivalently, with $ω$).

#### Dependent usaging in the type theory

Add a $↑u$ term to lift a term to a usage, such that usages in $T$ in a dependent function of type $(x \overset{π}{:} S) → T$ can depend on $x$ (we must then choose some canonical bijective mapping between semiring elements and terms), and some sort of beta-equivalence proofs of usage correctness will be required of programmers using this kind of lifting (in order for the term to typecheck). (possibly also add a usage-to-term $↓u$, not sure)

## Preliminaries

A *semiring* $R$ is a set $R$ with binary operations $+$ (addition) and $⋅$ (multiplication), such that $(R, +)$ is a commutative monoid with identity $0$,
$(R, ⋅)$ is a monoid with identity $1$, multiplication left and right distribute over addition, and multiplication by $0$ annihilates $R$.

The core type theory must be instantiated over a particular semiring. Choices include the boolean semiring $(0, 1)$, the zero-one-many semiring $(0, 1, ω)$, and the natural numbers with addition and multiplication.

In canonical Juvix Core the type theory is instantiated over the semiring of natural numbers plus ω, which is the most expressive option — terms can be $0$-usage ("contemplated"), $n$-usage ("computed $n$ times"), or $ω$-usage ("computed any number of times"). 

Let $S$ be a set of sorts $(i, j, k)$ with a total order. 

Let $K$ be the set of primitive types, $C$ be the set of primitive constants, and $⋮$ be the typing relation between primitive constants and primitive types, which must assign to each primitive constant a unique primitive type and usage.

Let $F$ be the set of primitive functions, where each $f$ is related to a function type, including an argument usage annotation, by the $⋮$ relation and endowed with a reduction operation $→_{f}$, which provided an argument of the function input type computes an argument of the function output type.

Primitive types, primitive constants, and primitive functions are threaded-through to the untyped lambda calculus to which Core is erased, so they must be directly supported by the low-level execution model. The core type theory and subsequent compilation pathways are parameterised over $K$, $C$, $F$, $⋮$, and the reduction operations $→_{f}$, which are assumed to be available as implicit parameters.

## Syntax

Inspired by the bidirectional syntax of Conor McBride in I Got Plenty o’ Nuttin’ {footcite}`plenty-o-nuttin`.

Let $R, S, T, s, t$ be types & terms and $d, e, f$ be eliminations, where types can be synthesised for eliminations but must be specified in advance for terms.

The three columns, in order, are: syntax utilised in this paper, text description, and syntax utilised in the ASCII parser.

### Core Syntax
$$
\begin{align*}
R, S, T, s, t &::= ∗_i\ & \text{sort $i$} &\ &\ *i \\
&\ \ \ \ \ \ |\ \kappa \in K & \text{primitive type} &\ &\ \text{(varies)} \\
&\ \ \ \ \ \ |\ (x \overset{π}{:} S) → T\ & \text{function type} &\ &\ \text{[π]\ S -> T} \\
&\ \ \ \ \ \ |\ (x \overset{π}{:} S) ⊗ T\ & \text{dependent multiplicative conjunction type} &\ &\ ([π] S,\ T) \\
&\ \ \ \ \ \ |\ (x \overset{π}{:} S)\ \&\ T\ & \text{dependent additive conjunction type} &\ &\ / \backslash \\
&\ \ \ \ \ \ |\ T \parr T & \text{non-dependent multiplicative disjunction type} &\ &\ \backslash /  \\
&\ \ \ \ \ \ |\ ιx.T\ & \text{self-type} &\ &\ @x.T \\
&\ \ \ \ \ \ |\ λx.t\ & \text{abstraction} &\ &\ \backslash x.t \\
&\ \ \ \ \ \ |\ e\ & \text{elimination} &\ &\ e \\
\end{align*}
$$

$$
\begin{align*}
d, e, f &::= x\ & \text{variable} &\ &\ &\ x\\
&\ \ \ \ \ \ |\ c \in C & \text{primitive constant} &\ &\ &\ \text{(varies)}\\
&\ \ \ \ \ \ |\ f \in F & \text{primitive function} &\ &\ &\ \text{(varies)}\\
&\ \ \ \ \ \ |\ f s\ & \text{application} &\ &\ &\ f s \\
&\ \ \ \ \ \ |\ (s, t)\ & \text{multiplicative conjunction} &\ &\ &\ (s, t) \\
&\ \ \ \ \ \ |\ s\ \epsilon\ t\ & \text{additive conjunction} &\ &\ &\ TBD \\
&\ \ \ \ \ \ |\ s\ \gamma\ t\ & \text{multiplicative disjunction} &\ &\ &\ TBD \\
&\ \ \ \ \ \ |\ fst_{\&}\ M\ & \text{first projection for additive conjunction} &\ &\ &\ fst\ M \\
&\ \ \ \ \ \ |\ snd_{\&}\ M\ & \text{second projection for additive conjunction} &\ &\ &\ snd\ M \\
&\ \ \ \ \ \ |\ let\ (x, y) = d\ in\ e\ & \text{dependent multiplicative conjunction pattern match} &\ &\ &\ let\ (x, y) = d\ in\ e \\
&\ \ \ \ \ \ |\ ⊙ e & \text{multiplicative disjunction destructor} &\ &\ &\ join\ e \\
&\ \ \ \ \ \ |\ s \overset{π}{:} S & \text{type \& usage annotation} &\ &\ &\ s : [π]\ S \\
\end{align*}
$$

Sorts $∗_i$ are explicitly levelled. Dependent function types, dependent conjunction types, and type annotations include a usage annotation $π$.

<!-- Judgements have the following form:

$$ x_1 \overset{ρ_1}{:} S_1, ..., x_n \overset{ρ_n}{:} S_n \vdash\ M \overset{σ}{:} T $$

where $ρ_1 ... ρ_n$ are elements of the semiring and $σ$ is either the $0$ or $1$ of the semiring. -->

Further define the syntactic categories of usages $ρ, π$ and precontexts $Γ$:

$$
\begin{align*}
ρ,π ∈ R \\
Γ := ⋄\ |\ Γ,x \overset{ρ}{:} S
\end{align*}
$$

The symbol ⋄ denotes the empty precontext.

Precontexts contain usage annotations $ρ$ on constituent variables. Scaling a precontext, $πΓ$, is defined as follows:

$$
\begin{align}
π(⋄)=⋄ \\
π(Γ,x \overset{ρ}{:} S) = πΓ,x \overset{πρ}{:} S
\end{align}
$$

Usage annotations in types are not affected.

By the definition of a semiring, $0Γ$ sets all usage annotations to $0$.

Addition of two precontexts $Γ_1 + Γ_2$ is defined only when $0Γ_1 = 0Γ_2$:

$$
\begin{align*}
⋄+⋄=⋄\\
(Γ_1,x \overset{ρ_1}{:} S) + (Γ_2,x \overset{ρ_2}{:} S) = (Γ_1+Γ_2), x \overset{ρ_1 + ρ_2}{:} S
\end{align*}
$$

Contexts are identified within precontexts by the judgement $Γ\vdash$, defined by the following rules:

$$
\begin{prooftree}
\AxiomC{}
\RightLabel{Emp}
\UnaryInfC{⋄ ⊢}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$Γ ⊢$}
\AxiomC{$0Γ ⊢ S$}
\RightLabel{Ext}
\BinaryInfC{$Γ,x \overset{ρ}{:} S ⊢$}
\end{prooftree}
$$

$0Γ ⊢ S$ indicates that $S$ is well-formed as a type in the context of $0Γ$.
$Emp$, for "empty", builds the empty context, and $Ext$, for "extend", extends a context $Γ$ with a new variable $x$ of type $S$ and usage annotation $ρ$.
All type formation rules yield judgements where all usage annotations in $Γ$ are $0$ — that is to say, type formation requires no computational resources).

Term judgements have the form:

$$
\begin{align}
Γ ⊢ M \overset{σ}{:} S
\end{align}
$$

where $σ \in {0,1}$.

Primitive constant term judgements have the form:

$$
\begin{align}
⊢ M \overset{γ}{:} S
\end{align}
$$

where $γ$ is any element in the semiring.

A judgement with $σ = 0$ constructs a term with no computational content, while a judgement with $σ = 1$ constructs a term which will be computed with.

For example, consider the following judgement:

$$
\begin{align*}
    n \overset{0}{:} Nat, x \overset{1}{:} Fin(n) ⊢ x \overset{\sigma}{:} Fin(n)
\end{align*}
$$

When $σ = 0$, the judgement expresses that the term can be typed:

$$
\begin{align*}
    n \overset{0}{:} Nat, x \overset{1}{:} Fin(n) ⊢ x \overset{0}{:} Fin(n)
\end{align*}
$$

Because the final colon is annotated to zero, this represents contemplation, not computation. When type checking, $n$ and $x$ can appear arbitrary times.

Computational judgement:
$$
\begin{align*}
    n \overset{0}{:} Nat, x \overset{1}{:} Fin(n) ⊢ x \overset{1}{:} Fin(n)
\end{align*}
$$

Because the final colon is annotated to one, during computation, $n$ is used exactly $0$ times, $x$ is used exactly one time.
$x$ can also be annotated as $ω$, indicating that it can be used (computed with) an arbitrary number of times.

## Typing rules

### Universe (set type)

Let $S$ be a set of sorts ${i, j, k}$ with a total order.

#### Formation rule

$$
\begin{prooftree}
\AxiomC{$0Γ ⊢$}
\AxiomC{$i < j$}
\RightLabel{∗}
\BinaryInfC{$0Γ ⊢ ∗_i \overset{0}{:} ∗_j$}
\end{prooftree}
$$

#### Introduction rule

$$
\begin{prooftree}
\AxiomC{$0Γ ⊢ V \overset{0}{:} ∗_i$}
\AxiomC{$0Γ, x \overset{0}{:} V ⊢ R \overset{0}{:} ∗_i$}
\RightLabel{∗-Pi}
\BinaryInfC{$Γ ⊢ (x \overset{π}{:} V) → R \overset{0}{:} ∗_i$}
\end{prooftree}
$$

$σ = 0$ fragment only.

### Primitive constants, functions & types

#### Constants

##### Formation & introduction rule

$$
\begin{prooftree}
\AxiomC{$c \in C$}
\AxiomC{$\kappa \in K$}
\AxiomC{$c ⋮ (γ, \kappa)$}
\RightLabel{Prim-Const}
\TrinaryInfC{$⊢ c \overset{γ}{:} \kappa$}
\end{prooftree}
$$

Primitive constants are typed according to the primitive typing relation, and they can be produced in any computational quantity wherever desired.

#### Functions

##### Formation & introduction rule

$$
\begin{prooftree}
\AxiomC{$f \in F$}
\AxiomC{$f ⋮ (γ, (x \overset{π}{:} S) → T)$}
\RightLabel{Prim-Fn}
\BinaryInfC{$⊢ f \overset{γ}{:} (x \overset{π}{:} S) → T$}
\end{prooftree}
$$

Primitive functions are typed according to the primitive typing relation, and they can be produced in any computational quantity wherever desired.

Primitive functions can be dependently-typed.

#### Elimination rule

Primitive functions use the same elimination rule as native lambda abstractions.

### Dependent function types

Function types $(x \overset{π}{:} S) → T$ record usage of the argument.

#### Formation rule

$$
\begin{prooftree}
\AxiomC{$0Γ ⊢ S$}
\AxiomC{$0Γ,x \overset{0}{:} S ⊢ T$}
\RightLabel{Pi}
\BinaryInfC{$0Γ ⊢ (x \overset{π}{:} S) → T$}
\end{prooftree}
$$

#### Introduction rule

$$
\begin{prooftree}
\AxiomC{$Γ,x \overset{σπ}{:} S ⊢ M \overset{σ}{:} T$}
\RightLabel{Lam}
\UnaryInfC{$Γ ⊢ λx.M \overset{σ}{:} (x \overset{π}{:} S) → T$}
\end{prooftree}
$$

The usage annotation $π$ is not used in judgement of whether $T$ is a well-formed type. It is used
in the introduction and elimination rules to track how $x$ is used, and how to multiply the resources
required for the argument, respectively:

#### Elimination rule

$$
\begin{prooftree}
\AxiomC{$Γ_1 ⊢ M \overset{σ}{:} (x \overset{π}{:} S) → T$}
\AxiomC{$Γ_2 ⊢ N \overset{σ'}{:} S$}
\AxiomC{$0Γ_1 = 0Γ_2$}
\AxiomC{$σ' = 0 ⇔ (π = 0 ∨ σ = 0)$}
\RightLabel{App}
\QuaternaryInfC{$Γ_1 + π Γ_2 ⊢ M N \overset{σ}{:} T[x := N]$}
\end{prooftree}
$$

- $0Γ_1 = 0Γ_2$ means that $Γ_1$ and $Γ_2$ have the same variables with the same types
- In the introduction rule, the abstracted variable $x$ has usage $σπ$ so that non-computational production requires no computational input
- In the elimination rule, the resources required by the function and its argument, scaled to the amount required by the function, are summed
- The function argument $N$ may be judged in the 0-use fragment of the system if and only if we are already in the 0-use fragment ($σ = 0$) or the function will not use the argument ($π = 0$).

### Dependent multiplicative conjunction (tensor product)

Colloquially, "pair". Can be dependent.

#### Formation rule

$$
\begin{prooftree}
\AxiomC{$0Γ ⊢ A$}
\AxiomC{$0Γ,x \overset{0}{:} S ⊢ T$}
\RightLabel{⊗}
\BinaryInfC{$0Γ ⊢ (x \overset{π}{:} S) ⊗ T$}
\end{prooftree}
$$

Type formation does not require any resources.

#### Introduction rule

$$
\begin{prooftree}
\AxiomC{$Γ_1 ⊢ M \overset{σ}{:} S$}
\AxiomC{$Γ_2 ⊢ N \overset{σ}{:} T[x := M]$}
\AxiomC{$0Γ_1 = 0Γ_2$}
\TrinaryInfC{$π Γ_1 + Γ_2 ⊢ (M,N) \overset{σ}{:} (x \overset{π}{:} S) ⊗ T$}
\end{prooftree}
$$

This is similar to the introduction rule for dependent function types above.

#### Elimination rules

$$
\begin{prooftree}
\AxiomC{$Γ ⊢ M \overset{0}{:} (x \overset{π}{:} S) ⊗ T$}
\UnaryInfC{$Γ ⊢ fst_⊗\ M \overset{0}{:} S$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$Γ ⊢ M \overset{0}{:} (x \overset{π}{:} S) ⊗ T$}
\UnaryInfC{$Γ ⊢ snd_⊗\ M \overset{0}{:} T[x := fst_⊗(M)]$}
\end{prooftree}
$$

Under the erased ($σ=0$) part of the theory, projection operators can be used as normal.

$$
\begin{prooftree}
\AxiomC{$0Γ_1, z \overset{0}{:} (x \overset{π}{:} S) ⊗ T ⊢ U$}
\AxiomC{$Γ_1 ⊢ M \overset{σ}{:} (x \overset{π}{:} S) ⊗ T$}
\AxiomC{$Γ_2, x \overset{σπ}{:} S, y \overset{σ}{:} T ⊢ N \overset{σ}{:} U[z := (x,y)]$}
\AxiomC{$0Γ_1 = 0Γ_2$}
\RightLabel{$⊗$ Elim}
\QuaternaryInfC{$Γ_1+Γ_2 ⊢ let\ (x,y) = M\ in\ N \overset{σ}{:} U[z := M]$}
\end{prooftree}
$$

Under the resourceful part, both elements of the conjunction must be matched and consumed.

To-do:

- Simplifies to fst, snd in $σ=0$ fragment (should we combine the rules?)
- If we lambda-encoded pairs, is that isomorphic?

### Additive conjunction

Colloquially, "choose either". Can be dependent.

#### Formation rule

$$
\begin{prooftree}
\AxiomC{$Γ ⊢ A \overset{σ}{∶} S$}
\AxiomC{$Γ ⊢ B \overset{σ}{:} T$}
\RightLabel{$\&$}
\BinaryInfC{$Γ ⊢ (A \overset{σ}{∶} S)\ \&\ (B \overset{σ'}{:} T)$}
\end{prooftree}
$$

To-do: can we construct with $σ' /= σ$?

#### Introduction rule

$$
\begin{prooftree}
\AxiomC{$Γ_1 ⊢ M \overset{σ}{:} S$}
\AxiomC{$0Γ_1 = 0Γ_2$}
\AxiomC{$Γ_2 ⊢ N \overset{σ}{:} T[x := M]$}
\TrinaryInfC{$πΓ_1 + Γ_2 ⊢ M\ \epsilon\ N \overset{σ}{:} (x \overset{π}{:} S)\ \&\ T$}
\end{prooftree}
$$

#### Elimination rules

$$
\begin{prooftree}
\AxiomC{$Γ ⊢ M\ \epsilon\ N \overset{σ}{:} (x \overset{π}{:} S)\ \&\ T$}
\UnaryInfC{$Γ ⊢ fst_{\&}\ (M \epsilon N) \overset{πσ}{:} S$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$Γ ⊢ M\ \epsilon\ N \overset{σ}{:} (x \overset{π}{:} S)\ \&\ T$}
\UnaryInfC{$Γ ⊢ snd_{\&}\ (M \epsilon N) \overset{σ}{:} T[x := M]$}
\end{prooftree}
$$

### Multiplicative disjunction

Colloquially, "both separately in parallel". Cannot be dependent.

Should be able to provide guarantee of parallelism in low-level execution, both in bespoke & interaction net paths.

#### Formation rule

$$
\begin{prooftree}
\AxiomC{$Γ ⊢ (A \overset{σ}{:} S), (B \overset{σ'}{:} S')$}
\RightLabel{$\parr$}
\UnaryInfC{$Γ ⊢ (A \overset{σ}{:} S) \parr (B \overset{σ'}{:} S')$}
\end{prooftree}
$$

#### Introduction rule

$$
\begin{prooftree}
\AxiomC{$Γ_1 ⊢ M \overset{σ}{:} S$}
\AxiomC{$0Γ_1 = 0Γ_2$}
\AxiomC{$Γ_2 ⊢ N \overset{σ}{:} T$}
\RightLabel{$\parr$-Intro}
\TrinaryInfC{$Γ_1 + Γ_2 ⊢ M\ \gamma\ N \overset{σ}{:} S \parr T$}
\end{prooftree}
$$

#### Elimination rule

$$
\begin{prooftree}
\AxiomC{$Γ ⊢ M \gamma\ N \overset{σ}{:} S \parr T$}
\RightLabel{$\parr$-Elim}
\UnaryInfC{$Γ ⊢ ⊙\ (M \gamma\ N) \overset{σ'}{:} (S ⊗ T)$}
\end{prooftree}
$$

$⊙$ can be thought of as a syntax-directed "join" operator.

### Self types

#### Formation rule

$$
\begin{prooftree}
\AxiomC{$Γ, x: ιx.T ⊢ T : ∗_i$}
\RightLabel{Self}
\UnaryInfC{$Γ ⊢ ιx.T$}
\end{prooftree}
$$

#### Introduction rule

$$
\begin{prooftree}
\AxiomC{$Γ ⊢ t : [x := t]T$}
\AxiomC{$Γ ⊢ ιx.T : ∗_i$}
\RightLabel{Self-Gen}
\BinaryInfC{$Γ ⊢ t : ιx.T$}
\end{prooftree}
$$


#### Elimination rule

$$
\begin{prooftree}
\AxiomC{$Γ ⊢ t : ιx.T$}
\RightLabel{Self-Inst}
\UnaryInfC{$Γ ⊢ t : [x := t]T$}
\end{prooftree}
$$

To-do: syntax for self types?

### Variable & conversion rules

The variable rule selects an individual variable, type, and usage annotation from the context:

$$
\begin{prooftree}
\AxiomC{$⊢ 0Γ,x \overset{σ}{:} S, 0Γ′$}
\RightLabel{Var}
\UnaryInfC{$0Γ,x \overset{σ}{:} S, 0Γ′ ⊢ x \overset{σ}{:} S$}
\end{prooftree}
$$

The conversion rule allows conversion between judgementally equal types:

$$
\begin{prooftree}
\AxiomC{$Γ ⊢ M \overset{σ}{:} S$}
\AxiomC{$0Γ ⊢ S≡T$}
\RightLabel{Conv}
\BinaryInfC{$Γ ⊢ M \overset{σ}{:} T$}
\end{prooftree}
$$

```{note}
Type equality is judged in a context with no resources.
```

### Equality judgements

Types are judgementally equal under beta reduction:

$$
\begin{prooftree}
\AxiomC{$Γ ⊢ S$}
\AxiomC{$Γ ⊢ T$}
\AxiomC{$S →_{β} T$}
\RightLabel{≡-Type}
\TrinaryInfC{$Γ ⊢ S ≡ T$}
\end{prooftree}
$$

Terms with the same type are judgementally equal under beta reduction:

$$
\begin{prooftree}
\AxiomC{$Γ ⊢ M \overset{σ}{:} S$}
\AxiomC{$Γ ⊢ N \overset{σ}{:} S$}
\AxiomC{$M →_{β} N$}
\RightLabel{≡-Term}
\TrinaryInfC{$Γ ⊢ M ≡ N \overset{σ}{:} S$}
\end{prooftree}
$$

To-do: do we need a rule for term equality?

### Inductive primitive types

Primitive types & values can also be defined which reference terms & eliminators, respectively, and come with custom derivation rules.

For example, consider the equality type `Eq` and constructor `Refl`.

Formation rule:

$$
\begin{prooftree}
\AxiomC{$Γ ⊢ t \overset{0}{:} ∗_i$}
\AxiomC{$Γ ⊢ x \overset{0}{:} t$}
\AxiomC{$Γ ⊢ y \overset{0}{:} t$}
\RightLabel{Eq-Form}
\TrinaryInfC{$Γ ⊢ Eq\ t\ x\ y\ \overset{0}{:} ∗_i$}
\end{prooftree}
$$

Introduction rule:

$$
\begin{prooftree}
\AxiomC{$Γ ⊢ t \overset{0}{:} ∗_i$}
\AxiomC{$Γ ⊢ x \overset{0}{:} t$}
\RightLabel{Eq-Intro}
\BinaryInfC{$Γ ⊢ Refl\ t\ x\ \overset{0}{:} Eq\ t\ x\ x$}
\end{prooftree}
$$

If $x →_{β} y$, the type $Eq\ t\ x\ y$ should reduce to $Eq\ t\ x\ x$ by the conversion rule, so $Refl\ t\ x\ \overset{0}{:} Eq\ t\ x\ y$ should typecheck.

Elimination rule:

$$
\begin{prooftree}
\AxiomC{$Γ ⊢ t \overset{0}{:} ∗_i$}
\AxiomC{$Γ ⊢ m \overset{0}{:} (x \overset{0}{:} t) → (y \overset{0}{:} t) → (e \overset{0}{:} Eq\ t\ x\ y) → ∗_i$}
\AxiomC{$Γ ⊢ n \overset{0}{:} (z \overset{0}{:} t) → m\ z\ z\ (Refl\ t\ z)$}
\AxiomC{$Γ ⊢ x \overset{0}{:} t, y \overset{0}{:} t, e \overset{0}{:} Eq\ t\ x\ y$}
\RightLabel{Eq-Elim}
\QuaternaryInfC{$Γ ⊢ eqElim\ t\ m\ n\ x\ y\ e\ \overset{0}{:} m\ x\ y\ e$}
\end{prooftree}
$$

(evaluates to `n x`)

Colloquially, if you have a type dependent on two values & a proof of their equality, and a function from
one value to a value of that type, you can eliminate two distinct values & a proof of their equality to
derive a value of the type, dependent on both values, which evaluates as the function applied to either one.

### Sub-usaging

To-do: check if we can safely allow sub-usaging if the ring is the natural numbers, discuss here.

## Reduction semantics

Contraction is $(λx.t : (π x : S) → T)\ s ⇝_{β} (t:T)[x := s:S]$.

De-annotation is $(t : T) ⇝_ν t$.

The reflexive transitive closure of $⇝_{β}$ and $⇝_ν$ yields beta reduction $→_{β}$ as usual.

### Confluence

A binary relation $R$ has the diamond property iff. $∀ s p q . s R p ∧ s R q \implies ∃r . p R r ∧ q R r$.

### Parallel-step reduction

Let parallel reduction be $▷$, operating on usage-erased terms, by mutual induction.

```{note}
The theorem prover will support a special n-step beta equality, where the step count refers to contraction rule applications.
```

#### Basic lambda calculus

$$
\begin{prooftree}
\AxiomC{$$}
\UnaryInfC{$∗_i ▷ ∗_i$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$$}
\UnaryInfC{$x ▷ x$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$S ▷ S'$}
\AxiomC{$T ▷ T'$}
\BinaryInfC{$(x : S) → T ▷ (x : S') → T'$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$t ▷ t'$}
\UnaryInfC{$λx.t  ▷ λx.t'$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$f ▷ f'$}
\AxiomC{$s ▷ s'$}
\BinaryInfC{$f s ▷ f' s'$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$t ▷ t'$}
\AxiomC{$T ▷ T'$}
\BinaryInfC{$t : T ▷ t' : T'$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$t ▷ t'$}
\AxiomC{$S ▷ S'$}
\AxiomC{$T ▷ T'$}
\AxiomC{$s ▷ s'$}
\QuaternaryInfC{$(λx.t : (x : S) → T) s ▷ (t' : T') [x := s' : S']$}
\end{prooftree}
$$

#### Linear connectives

##### Multiplicative conjunction

$$
\begin{prooftree}
\AxiomC{$S ▷ S'$}
\AxiomC{$T ▷ T'$}
\BinaryInfC{$(x : S) ⊗ T ▷ (x : S') ⊗ T'$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$s ▷ s'$}
\AxiomC{$t ▷ t'$}
\BinaryInfC{$(s, t) ▷ (s', t')$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$z ▷ (m, n)$}
\AxiomC{$m ▷ m'$}
\AxiomC{$n ▷ n'$}
\AxiomC{$s ▷ s'$}
\QuaternaryInfC{$let\ (x, y)\ =\ z\ in\ s ▷ s' [x := m', y := n']$}
\end{prooftree}
$$

Reduction takes place inside a multiplicative conjunction.

##### Multiplicative disjunction

$$
\begin{prooftree}
\AxiomC{$S ▷ S'$}
\AxiomC{$T ▷ T'$}
\BinaryInfC{$S \parr T ▷ S' \parr T'$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$s ▷ s'$}
\AxiomC{$t ▷ t'$}
\BinaryInfC{$s\ \gamma\ t ▷ s'\ \gamma\ t'$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$t ▷ m\ \gamma\ n$}
\AxiomC{$m ▷ m'$}
\AxiomC{$n ▷ n'$}
\TrinaryInfC{$⊙\ t ▷ (m', n')$}
\end{prooftree}
$$

Reduction takes place inside a multiplicative disjunction.

##### Additive disjunction

$$
\begin{prooftree}
\AxiomC{$S ▷ S'$}
\AxiomC{$T ▷ T'$}
\BinaryInfC{$(x : S) \& T ▷ (x : S') ⊗ T'$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$t ▷ m\ \epsilon\ n$}
\AxiomC{$m ▷ m'$}
\BinaryInfC{$fst_{\&}\ t ▷ m'$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$t ▷ m\ \epsilon\ n$}
\AxiomC{$n ▷ n'$}
\BinaryInfC{$snd_{\&}\ t ▷ n'$}
\end{prooftree}
$$

Reduction does not take place until a destructor has been applied.

### Self types

$$
\begin{prooftree}
\AxiomC{$T ▷ T'$}
\UnaryInfC{$ιx.T ▷ ιx.T'$}
\end{prooftree}
$$

### Primitives

$$
\begin{prooftree}
\AxiomC{$\kappa \in K$}
\UnaryInfC{$\kappa ▷ \kappa$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$c \in C$}
\UnaryInfC{$c ▷ c$}
\end{prooftree}
$$

Primitive types and primitive constants reduce to themselves.

$$
\begin{prooftree}
\AxiomC{$f \in F$}
\AxiomC{$x ▷ x'$}
\AxiomC{$x' →_{f} y$}
\TrinaryInfC{$f x ▷ y$}
\end{prooftree}
$$

Primitive functions reduce according to the reduction operation defined for the function.

## Typechecking

To-do: Lay out syntax-directed typechecker following McBride's paper.
## Examples

### SKI combinators

#### S combinator

The dependent S ("substitution") combinator can be typed as: $λt1.λt2.λt3.λx.λy.λz.x z (y z) \overset{1}{:} (x \overset{1}{:} ((a \overset{1}{:} t1) → (b \overset{1}{:} t2) → t3)) → (y \overset{1}{:} ((a \overset{1}{:} t1) → t2)) → (z \overset{2}{:} t1) → t3$.

This should also typecheck if the $x$, $y$, and $z$ argument usages are replaced with $w$ (instead of $1$ and $2$).

#### K combinator

The dependent K ("constant") combinator can be typed as: $⊢ λt1.λt2.λx.λy.x \overset{1}{:} (t1 \overset{0}{:} ∗_i) → (t2 \overset{0}{:} ∗_i) → (x \overset{1}{:} t1) → (y \overset{0}{:} t2) → t1$.

This should also typecheck if the $x$ and $y$ argument usages are replaced with $w$ (instead of $1$ and $0$).

#### I combinator

The dependent I ("identity") combinator can be typed as: $⊢ λt.λx.(x \overset{1}{:} t) \overset{1}{:} (t \overset{0}{:} ∗_i) → (x \overset{1}{:} t) → t$.

This should also typecheck if the $x$ argument usage is replaced with $w$ (instead of $1$).

### Church-encoded natural numbers

The dependent Church-encoded natural $n$ can be typed as $⊢ λt.λs.z.s {...} s z \overset{1}{:} (s \overset{n}{:} ((a \overset{1}{:} t) → t)) → (z \overset{1}{:} t) → t$ where $s$ is applied $n$ times.

This should also typecheck if the $s$ argument usage is replaced with $w$ (instead of $n$ for some specific $n$).

### Primitive equality

Assume a primitive type of naturals, including literals, and a primitive addition function.

Define an `Eq` type:

$$
\begin{prooftree}
\AxiomC{$t \overset{0}{:} ∗_i$}
\AxiomC{$x \overset{0}{:} t$}
\AxiomC{$y \overset{0}{:} t$}
\RightLabel{$Eq-Form$}
\TrinaryInfC{$Eq t x y \overset{0}{:} ∗_j$}
\end{prooftree}
$$

And a single `Refl` constructor:

$$
\begin{prooftree}
\AxiomC{$t \overset{0}{:} ∗_i$}
\AxiomC{$x \overset{0}{:} t$}
\RightLabel{$Refl$}
\BinaryInfC{$Refl \overset{0}{:} Eq t x x$}
\end{prooftree}
$$

Then $Refl \overset{0}{:} Eq Nat (1 + 1) 2$ can be derived, since the conversion rule will be applied when checking the (annotated) $Refl$.

### Linear induction

To-do: Linear induction example.

```{footbibliography}
```