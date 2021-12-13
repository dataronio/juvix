{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Juvix.Sexp.Serialize where

import qualified Data.Char8 as Char8
import GHC.Generics as Generics
import qualified GHC.Types as Types
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Sexp.Types as Sexp hiding (double)

data Test a
  = Test
  | Test2 a
  | Test3 a a
  deriving (Show, Generic)

class Serialize a where
  serialize :: a -> T
  deserialize :: T -> Maybe a

class GSerialize f where
  gput :: f a -> T
  gget :: T -> Maybe (f a)

instance GSerialize U1 where
  gput U1 = Nil
  gget _xs = Just U1

----------------------------------------
-- M1
----------------------------------------

instance (Selector i, GSerialize a) => GSerialize (S1 i a) where
  gput y@(M1 x) =
    gput x
  gget = undefined

instance (Constructor i, GSerialize a) => GSerialize (C1 i a) where
  gput y@(M1 x) =
    let name = mlNameToReservedLispName (conName y)
     in Sexp.Cons (Sexp.Atom (A name Nothing)) (gput x)
  gget xs =
    case maybeX of
      Just t ->
        let m1 = M1 t
            name = conName m1
         in case xs of
              Cons (Atom (A name1 _)) _
                | name1 == mlNameToReservedLispName name ->
                  Just m1
              _ -> Nothing
      Nothing -> Nothing
    where
      maybeX = gget xs

----------------------------------------
-- Sum
----------------------------------------

instance (GSerialize a, GSerialize b) => GSerialize (a :+: b) where
  gput (L1 x) = gput x
  gput (R1 x) = gput x
  gget = undefined

instance (GSerialize a, GSerialize b) => GSerialize (a :*: b) where
  gput = undefined
  gget = undefined

----------------------------------------
-- K1
----------------------------------------

instance (Serialize a) => GSerialize (K1 i a) where
  gput (K1 x) = Sexp.Cons (serialize x) Nil
  gget = undefined

instance Serialize Integer where
  serialize i = Atom (N i Nothing)
  deserialize (Atom (N i Nothing)) = Just i
  deserialize _ = Nothing

instance Serialize () where
  serialize () = Nil
  deserialize _ = Nothing

mlNameToReservedLispName :: [Char] -> NameSymbol.T
mlNameToReservedLispName [] = NameSymbol.fromString ""
mlNameToReservedLispName (x : xs) =
  let upperToDash x
        | Char8.isUpper x = [Char8.toLower x, '-']
        | otherwise = [x]
      properName =
        xs >>= upperToDash
   in NameSymbol.fromString (':' : Char8.toLower x : properName)

test = do
  let M1 t = from (Test :: Test ())
      L1 t2 = t
  (gput t2 |> gget :: Maybe (C1 ('MetaCons "Test" 'PrefixI 'False) U1 p))
