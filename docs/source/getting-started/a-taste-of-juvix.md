# A Taste of Juvix

```{warning} 
**TODO: Adapt this to Juvix**
```

The objective of this section is to provide a glimpse of Agda with some examples. The first one is a demonstration of dependently typed programming, and the second shows how to use Agda as a proof assistant. Finally, we build a complete program and compile it to an executable program with the GHC and Javascript backends.


## Full example token contract:

```haskell
mod TokenContract where

open Prelude
open LLVM

let Map = Data.Map

-- common module interfaces for data structures in Common?
sig MonoidMeasure : Common.Measure
let MonoidMeasure =
  type T = Nat

  -- (<>) is normal semi-group type class function
  let (<>)    = (<>)
  let measure = id
end

-- need OrdMap, as modules that one goes to can't be modules?
let Accounts = Map.OrdMap Data.String MonoidMeasure

-- we send in the string module, as it has a total order
-- function named compare, that is also part of a general type class
let Token =
  type Address = s : String.T {String.length s == 36}

  type Storage = {
    total-supply : Nat.T,
    accounts     : Accounts.T { Accounts.measure-value == total-supply }
  }

  sig empty-storage : Storage
  let empty-storage = {
    total-supply = 0,
    accounts     = Accounts.empty,
  }

  type T = {
    storage : Storage,
    version : Nat.T,
    name    : String.T,
    symbol  : Char.T,
    owner   : Address,
  }
end

let Transaction =
  type Transfer = {
    from-account : Token.Address,
    to-account   : Token.Address,
    amount      : Nat.T,
  }

  type Mint = {
    mint-amount     : Nat.T,
    mint-to-account : Token.Address,
  }

  type Burn = {
    burn-amount       : Nat.T,
    burn-from-account : Token.Address,
  }

  type Data =
    | Transfer : Transfer -> Data
    | Mint     : Mint     -> Data
    | Burn     : Burn     -> Data

  type T = {
    data               : Data,
    authorized-account : Token.Address,
  }
end

sig has-n : Accounts.T -> Token.Address -> Nat -> Bool
let has-n accounts add to-transfer =
  case Accounts.select accounts add of
  | Just n  -> to-transfer <= n
  | Nothing -> False


sig account-sub : acc : Accounts.T
               -> add : Token.Address
               -> num : Nat.T {has-n acc add num}
               -> Accounts.T
let account-sub accounts add number =
  case Accounts.select accounts add of
  | Just balance ->
     Accounts.put accounts ~key:add ~val:(balance - number)

sig account-add : Accounts.T -> Token.Address -> Nat.T -> Accounts.T
let account-add accounts add number =
  Accounts.update accounts ~f:(number +) ~key:add



sig transfer-stor : stor   : Token.Storage
                 -> ~from : Token.Address
                 -> ~to   : Token.Address
                 -> num   : Nat.T {has-n stor.accounts from num}
                 -> Token.Storage
let transfer-stor stor ~from:add_from ~to:add_to num =
  let new-acc = account-add (account-sub stor.accounts add_from) add-to num in
  -- This is valid, because the monoid and measure of account track this
  { total-supply = stor.total-supply
  , accounts     = new-acc
  }



sig Validation : Module
  type T = Token.T -> Transaction.T -> Bool

  transfer : T
  burn     : T
  mint     : T
end
let Validation =
  type T = Token.T -> Transaction.T -> Bool

  let mint token tx =
    case tx.data of
    | Transaction.Mint ->
      token.owner == tx-tx-authorized-account
    | Transaction.Burn _ | Transaction.Transfer _ ->
      false

  let transfer token tx =
    case tx.data of
    | Transaction.Transfer {from-account, amount} ->
      has-n token.storage.accounts from-account amount
      && tx.authroized-account == from-account
    | Transaction.Burn _ | Transaction.Mint _ ->
      false

  let Burn token tx =
    case tx.data of
    | Transaction.Burn {burn-from-account, burn-amount} ->
      has-n token.storage.accounts burn-from-account burn-amount
      && tx.authroized-account == burn-from-account
    | Transaction.Mint _ | Transaction.Transfer _ ->
      false
end

let Operation =

  type TokenTransaction f =
    tok : Token.T -> tx : tx { f tok tx } -> Token.T

  sig transfer : TokenTransaction Validation.transfer
  let transfer token transaction =
    case transaction.data of
    | Transfer {from-account, to-account, amount} ->
      { token
        with storage =
          transfer-stor token.storage
                        ~from:from_account
                        ~to:to_account
                        transfer_amount
      }

  sig mint : TokenTransaction Validation.mint
  let mint token transaction =
    case transaction.data of
    | Mint {mint-amount, mint-to-account} ->
      { token
        -- again safe to do because of tree structure
        with storage = {
          total-suply = token.storage.total_supply + mint-amount,
          accounts    = account-add token.storage.accounts mint-to-account mint-amount,
        }}


  sig burn : TokenTransaction Validation.mint
  let burn token transaction =
    case transaction.data of
    | Burn {burn-amount, burn-from-account} ->
      { token
        -- again safe to do because of tree structure
        with storage = {
          total-suply = token.storage.total_supply - burn-amount,
          accounts    = account-sub token.storage.accounts burn-from-account burn-amount,
        }}

  type Error =
    | NotEnoughFunds
    | NotSameAccount
    | NotOwnerToken
    | NotEnoughTokens

  sig exec : Token.T -> Transaction.T -> Either.T Error Token.T
  let exec token tx =
    case tx.data of
    | Transfer _ ->
      if | Validation.transfer token tx = Right (transfer token tx)
         | else                         = Left NotEnoughFunds
    | Mint _ ->
      if | Validation.mint token tx = Right (mint  token tx)
         | else                     = Left NotEnoughFunds
    | Burn _ ->
      if | Validation.burn token tx = Right (burn token tx)
         | else                     = Left NotEnoughTokens
end
```