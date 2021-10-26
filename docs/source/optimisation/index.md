# Core Optimisation

Juvix combines compiler-directed & user-directed optimising transformations into a single whole-program core optimisation function $ψ$, defined in this chapter, which maps core terms to core terms, preserving the evaluation semantics defined in the previous chapter.

```{note}
Whole-program core optimisation is one of the less theoretically risky parts of the compiler design and thus is omitted in the initial release. At present the optimisation function $ψ$ is simply the identity. Future releases are expected to incorporate optimising transformations discussed herein.
```

```{toctree}
---
maxdepth: 1
---

graph-transformations
optimisation-by-normalisation
user-directed-optimisations
```