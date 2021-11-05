Juvix ![GitHub](https://img.shields.io/github/license/anoma/juvix)
[![Build Status](https://ci.heliax.dev/api/badges/anoma/juvix/status.svg)](https://ci.heliax.dev/anoma/juvix)
![GitHub issues](https://img.shields.io/github/issues/anoma/juvix)
[![stability-experimental](https://img.shields.io/badge/stability-experimental-orange.svg)](https://github.com/mkenney/software-guides/blob/master/STABILITY-BADGES.md#experimental)
![GitHub release (latest SemVer including pre-releases)](https://img.shields.io/github/v/release/anoma/juvix?include_prereleases)
====

Description
-----------

<img align="right" width="300" height="300" src="Juvix_logo.png">

Juvix is a dependently functional programming language for writing
efficient formally-verified smart contracts, which can be
deployed to various distributed ledgers. Juvix addresses many issues that we have
experienced while trying to write and deploy decentralised
applications present in the ecosystem of smart-contracts: 
- the difficulty of
adequate program verification, 
- the ceiling of compositional
complexity, 
- the illegibility of execution costs, and
- the lock-in to
particular backends.

The Juvix compiler synthesises a high-level frontend syntax with
support for dependent-linearly types and several other cutting-edge
research ideas from programming language design and type theory. For
more details, see [the Juvix language
reference WIP](./doc/reference/language-reference.pdf).

Prerequisites
--------------

* Install [Stack](https://haskellstack.org):

  - For Ubuntu        : `apt install stack`
  - For Debian        : `apt install haskell-stack`
  - For Arch Linux    : `pacman -S stack`
  - For macOS : `brew install haskell-stack`
  - For Windows, following the instructions
  [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/#windows).
  
  It is required at least 8GB RAM for `stack` installation.

* You might need to install the following third-party libraries.

  - [libff](https://github.com/scipr-lab/libff),
  - [libsecp256k1](https://github.com/bitcoin-core/secp256k1), and
  - [openssl libssl](https://wiki.openssl.org/index.php/Libssl_API).


Installation
------------

1. To install Juvix, you can download its sources using
   [Git](http://git-scm.com/) from the [Github
   repository](https://github.com/anoma/juvix.git). Then, the program
   can be downloaded and installed with the following commands:

   ````bash
   $ git clone https://github.com/anoma/juvix.git
   $ cd juvix
   $ make install
   ````
   
   If the installation succeeds, you must be able to run the juvix command
   from any location. To get the complete list of commands, please run `juvix --help`.
   
   ```
   $  juvix version
   Juvix version 0.1.1.19-a2111a3
   Branch: develop
   Commit: a2111a389be0e17291f2903d1ec7d7b94bd60cf8
   Date: Mon Sep 13 13:26:18 2021 -0500
   ```

2. For a more experienced user, after cloning Juvix into a local
   directory, to build and install the binary to the local path, you
   can run the following make commands: `make` or for full optimisations
   (but slower compile times): `make build-prod`.

   For Windows users, to use the command *make*, please visit [this
   link](https://stackoverflow.com/questions/32127524/how-to-install-and-use-make-in-windows).
   

Examples
--------

### Writing and compiling your first `.ju` contract

To write Juvix programs, you can use any text editor. However, we
recommend to use VSCode with the [Juvix syntax highlighting
package](https://marketplace.visualstudio.com/items?itemName=metastate.language-juvix).
In the following example, we will code a smart contract using the backend for
[Michelson](https://www.michelson.org/), the language of [Tezos](https://tezos.com/).

1. Located at the Juvix folder, e.g.,

```bash
$ pwd
/tmp/juvix
```

2. create the file `identity.ju`. The file content should be as follows.

```haskell
mod identity where

open Prelude
open Prelude.Michelson

sig cons-pair : list operation -> int -> pair (list operation) int
let cons-pair = %Michelson.pair

sig nil : list operation
let nil = %Michelson.nil

sig car : pair int int -> int
let car = %Michelson.car

sig main : pair int int -> pair (list operation) int
let main = \params ->
  cons-pair nil (car params)
```

3. Then, you can run the following commands using the Michelson backend.

- To simply type-check your code:

  ```bash
  $ juvix typecheck identity.ju -b michelson
  ```

- To compile your code to a Michelson `.tz` file, please run the following command:

  ```bash
  $ juvix compile identity.ju identity.tz -b michelson
  ()
  ```
  
  As a result, you obtain a contract that can be deployed using
  standard procedures outlined [in the Tezos documentation](https://tezos.gitlab.io/alpha/cli-commands.html?highlight=originate).
  Direct integration with the Juvix toolchain is still work in progress.
  
  ```c++
  $ cat identity.tz
  parameter int;
  storage int;
  code { { DIG 0;
         DUP;
         DUG 1;
         CAR;
         NIL operation;
         PAIR;
         DIP { DROP } } };
  ```

More examples of Juvix programs can be found in [`examples`](./examples) and [`test`](./test) folders.


Testing
-------

We have plenty of examples in the folder `test`. To test Juvix against
these examples, please run the command:

```bash
$ make test
```

To open a REPL , you can run one the following commands:

```bash
$ make repl-lib    # REPL with the library scoped
$ make repl-exe    # REPL with the executable scoped
```
    
Known limitations
-----------------

This is a software released for experimentation and research purposes
only. Do not expect API stability. Expect divergence from canonical
protocol implementations. No warranty is provided or implied.

Contributing
------------

We welcome contributions to the development of Juvix. See
[CONTRIBUTING.md](./doc/CONTRIBUTING.md) for contribution guidelines.

Community
---------

We would love to hear what you think of Juvix! Join our community:

- Follow us on [Twitter](https://twitter.com/juvixlang)
- Subscribe to our [newsletter](https://juvix.org/)
