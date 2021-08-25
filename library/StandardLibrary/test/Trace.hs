module Trace where

import Juvix.Library
import qualified Juvix.Library.Trace.Format as Format
import qualified Juvix.Library.Trace as Trace
import qualified Juvix.Library.Trace.Environment as Env

add1 :: (Trace.Eff m, Show b, Enum b) => b -> m b
add1 n =
  Trace.withScope "Prelude.add1" [show n] $ do
    pure (succ n)

add2 :: (Trace.Eff m, Show b, Enum b) => b -> m b
add2 n =
  Trace.withScope "Prelude.add2" [show n] $ do
    n <- add1 n
    add1 n

addBreak :: (Trace.Eff m, Show b, Num b, MonadIO m) => b -> b -> m b
addBreak x y =
  Trace.withScope "Prelude.add" [show x, show y] $ do
    Trace.break
    pure (x + y)

multiply :: (Trace.Eff m, Show b, Num b) => b -> b -> m b
multiply x y =
  Trace.withScope "Prelude.multiply" [show x, show y] $ do
    pure (x * y)

formula :: (Trace.Eff m, Show b, MonadIO m, Fractional b) => b -> b -> m b
formula x y =
  Trace.withScope "multiply-add" [show x, show y] $ do
    addSquares <- addBreak (x ^ 2) (y ^ 2)
    pure (addSquares / 2)


