# Frontend

This chapter defines the high-level dependently-typed frontend syntax in which developers are expected to write, referred to as the "Juvix frontend language" or merely "Juvix" where unambiguous, and how that syntax is elaborated to core. Juvix aims to keep the core language as simple as possible to minimise the chance of mistakes in the type theory or type-checker implementation and facilitate easy optimisation, so most high-level frontend features such as data-types, pattern matching, and type-classes are elaborated into plain core terms in the transformation from the frontend language.

```{toctree}
---
maxdepth: 1
---

features/index
s-expression-syntax
desugar
usages
```