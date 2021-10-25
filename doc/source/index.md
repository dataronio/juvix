Documentation for the Juvix Language
======================================

Juvix synthesises a high-level frontend syntax, dependent-linearly-typed core language, and low-level parallelisable
optimally-reducing execution model into a single unified stack for writing formally verifiable, efficiently executable
smart contracts which can be deployed to a variety of distributed ledgers.
\par
Juvix's compiler architecture is purpose-built from the ground up for the particular requirements and economic trade-offs
of the smart contract use case — it prioritises behavioural verifiability, semantic precision, and output code efficiency over compilation speed,
syntactical familiarity, and backwards compatibility with existing blockchain virtual machines.
\par
Machine-assisted proof search, declarative deployment tooling, type & usage inference, and alternative spatiotemporal dataflow representations facilitate
integration of low-developer-overhead property verification into the development process.
An interchain abstraction layer representing ledgers as first-class objects enables seamless cross-chain programming and type-safe runtime reconfiguration.
\par
This document is designed to be a first-principles explanation of Juvix. No familiarity with the theoretical background is assumed.
Readers previously acquainted with the lambda calculus, sequent calculus, simply-typed lambda calculus, the calculus of constructions,
linear logic, interaction nets, elementary affine logic, and Lamping's optimal reduction algorithm may skip the associated subsections in chapter five.

```{toctree}
motivation
typographical-conventions
prior-work
desiderata
ingredients
background/index
frontend/index
core/index
low-level-execution-model
cost-accounting
backends/index
distributed-ledger-integration
future-directions
examples/index
optimisation/index
```

