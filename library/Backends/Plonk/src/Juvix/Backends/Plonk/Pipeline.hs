{-# LANGUAGE UndecidableInstances #-}

module Juvix.Backends.Plonk.Pipeline
  ( BPlonk (..),
    compileCircuit,
    prettifyCircuit,
  )
where

import qualified Data.Aeson as A
import Data.Field.Galois (GaloisField)
import qualified Data.Text.Lazy as LazyText
import qualified Juvix.Backends.Plonk.Builder as Builder
import qualified Juvix.Backends.Plonk.Circuit as Circuit
import qualified Juvix.Backends.Plonk.Compiler as Compiler
import qualified Juvix.Backends.Plonk.Dot as Dot
import qualified Juvix.Backends.Plonk.Parameterization as Parameterization
import qualified Juvix.Backends.Plonk.Types as Types
import qualified Juvix.Core.Base.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.Typechecker.Types as TypeChecker
import Juvix.Core.Parameterisation
  ( CanApply (ApplyErrorExtra, Arg),
    TypedPrim,
  )
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library
import Juvix.Pipeline as Pipeline
import qualified Text.PrettyPrint.Leijen.Text as Pretty

data BPlonk f = BPlonk
  deriving (Eq, Show, Read)

instance
  ( GaloisField f,
    Eq f,
    Integral f,
    Show (Param.PrimApplyError (Types.PrimTy f)),
    Show (Param.PrimApplyError (Types.PrimVal f)),
    A.ToJSON (Circuit.ArithCircuit f)
  ) =>
  HasBackend (BPlonk f)
  where
  type Ty (BPlonk f) = Types.PrimTy f
  type Val (BPlonk f) = Types.PrimVal f
  type Err (BPlonk f) = Types.CompilationError f
  stdlibs _ = ["stdlib/Circuit.ju", "stdlib/Circuit/Field.ju"]
  typecheck ctx = Pipeline.typecheck' ctx (Parameterization.param @f)
  compile out term = do
    let circuit = compileCircuit term
    liftIO $ Dot.dotWriteSVG out (Dot.arithCircuitToDot circuit)
    writeout (out <> ".pretty") $ prettifyCircuit circuit
    writeout (out <> ".json") $ show $ A.encode circuit

prettifyCircuit :: (ConvertText LazyText.Text c, Pretty.Pretty a) => a -> c
prettifyCircuit = toS . Pretty.displayT . Pretty.renderPretty 1 120 . Pretty.pretty

compileCircuit ::
  (Integral f, Show f) =>
  ErasedAnn.AnnTermT (Types.PrimTy f) (Types.PrimVal f) ->
  Circuit.ArithCircuit f
compileCircuit term = Builder.execCircuitBuilder . Compiler.compileTermWithWire $ ErasedAnn.toRaw term
