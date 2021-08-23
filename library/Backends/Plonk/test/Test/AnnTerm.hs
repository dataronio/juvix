-- Finite fields example
module Test.AnnTerm where

import Data.Curve.Weierstrass.BLS12381 (Fr)
import qualified Juvix.Backends.Plonk as P
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import Juvix.Library hiding (Type, exp)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

sig :: P.Type Fr
sig =
  ErasedAnn.Pi (Usage.SNat 1) (ErasedAnn.PrimTy P.PField) $
    ErasedAnn.Pi (Usage.SNat 1) (ErasedAnn.PrimTy P.PField) $
      ErasedAnn.PrimTy P.PField

add, sub, mul, exp :: P.AnnTerm Fr
add = ErasedAnn.Ann Usage.Omega sig $ ErasedAnn.Prim P.PAdd
sub = ErasedAnn.Ann Usage.Omega sig $ ErasedAnn.Prim P.PSub
mul = ErasedAnn.Ann Usage.Omega sig $ ErasedAnn.Prim P.PMul
exp = ErasedAnn.Ann Usage.Omega sig $ ErasedAnn.Prim P.PExp

eq :: P.AnnTerm Fr
eq = ErasedAnn.Ann Usage.Omega sig $ ErasedAnn.Prim P.PAssertEq

val :: Fr -> P.AnnTerm Fr
val = ErasedAnn.Ann (Usage.SNat 1) (ErasedAnn.PrimTy P.PField) . ErasedAnn.Prim . P.PConst

var :: NameSymbol.T -> P.AnnTerm Fr
var = ErasedAnn.Ann (Usage.SNat 1) (ErasedAnn.PrimTy P.PField) . ErasedAnn.Var

app :: P.AnnTerm Fr -> [P.AnnTerm Fr] -> P.AnnTerm Fr
app f xs = ErasedAnn.Ann (Usage.SNat 2) (ErasedAnn.PrimTy P.PField) $ ErasedAnn.AppM f xs
