---
title: Getting Started
position: 1
---

# Getting Started

## Installing Juvix

Please see [the README](https://github.com/anoma/juvix#installation).

## Writing your first smart contract

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

More examples of Juvix programs can be found in [`examples`](https://github.com/anoma/juvix/examples) and [`test`](https://github.com/anoma/juvix/test) folders.
