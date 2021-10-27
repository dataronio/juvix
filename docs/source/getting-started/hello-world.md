# 'Hello world' in Juvix

```agda
mod HelloWorld where

open Prelude
open LLVM

sig main : string
let main = "hello-world"
```

1. `string` is imported from the LLVM library