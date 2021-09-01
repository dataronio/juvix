module Test.Example.Polynomial where

import Juvix.Backends.Plonk (AnnTerm)
import qualified Juvix.Backends.Plonk as P
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import Juvix.Library hiding (Type, exp)
import Juvix.Library.BLS12381 (Fr)
import qualified Juvix.Library.Usage as Usage
import Test.AnnTerm

circuitPolynomial1 :: P.ArithCircuit Fr
circuitPolynomial1 = P.execCircuitBuilder $ P.compileTermWithWire corePolynomial1
  where
    -- \x y -> x^3 - 2x^2 + 4 = y
    corePolynomial1 :: AnnTerm Fr
    corePolynomial1 =
      ErasedAnn.Ann Usage.Omega (ErasedAnn.PrimTy P.PField) $
        ErasedAnn.LamM [] ["x", "y"] $
          app eq [rhs, lhs]
      where
        rhs = var "y"
        lhs =
          app
            sub
            [ app add [app exp [var "x", val 3], val 4],
              app mul [val 2, app exp [var "x", val 2]]
            ]
