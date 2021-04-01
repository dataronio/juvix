{-# Language LiberalTypeSynonyms #-}

module Test where

-- import qualified Compile
import qualified Juvix.Backends.Michelson.Parameterisation as Param
import qualified Juvix.Core as Core
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.Context.Traverse as Traverse
-- import qualified Juvix.ToCore.FromFrontend as FF
import qualified Juvix.Desugar as Desugar
import qualified Juvix.Frontend as Frontend
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Sexp as SexpTrans
import qualified Juvix.Frontend.Types as Initial
import qualified Juvix.Frontend.Types.Base as Frontend
import qualified Juvix.FrontendDesugar as FrontDesugar
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymb
import qualified Juvix.Library.Sexp as Sexp
import qualified Juvix.Pipeline as Pipeline
-- import Options
import Prelude (error)

import qualified Juvix.Library.Feedback as Feedback
import qualified Compile
import Options

-- val :: IO (Either GHC.Base.String [(NameSymb.T, [Initial.TopLevel])])
val = Frontend.ofPath ["test/examples/test/foo.ju", "test/examples/test/foo-helper.ju"]

val2 = Frontend.ofPath ["test/examples/demo/identity.ju"]

core = Pipeline.toCore ["stdlib/Prelude.ju", "stdlib/Michelson.ju", "test/"]

ordered fin = do
  Right c <- Pipeline.toCore ["stdlib/Prelude.ju", "stdlib/Michelson.ju", fin]
  let ordered = Traverse.recGroups c
  pure $ fmap (\(x :| []) -> Traverse.name x) ordered

sent = do
  Right ctx <- Pipeline.toCore ["test/examples/coreify/minimal.ju", "test/examples/coreify/types.ju"]
  pure $ Pipeline.contextToCore ctx Param.michelson


coreify = do
  Right ctx <- Pipeline.toCore ["test/examples/coreify/minimal.ju", "test/examples/coreify/add.ju"]
  Feedback.runFeedbackT (Compile.typecheck Michelson ctx)

----------------------------------------------------------------------
-- Give me sexp terms helpers
----------------------------------------------------------------------

parseDesugarSexp :: ByteString -> [Sexp.T]
parseDesugarSexp = Desugar.op . parsedSexp

parsedSexp :: ByteString -> [Sexp.T]
parsedSexp xs = ignoreHeader (Parser.parse xs) >>| SexpTrans.transTopLevel

ignoreHeader :: Either a (Frontend.Header topLevel) -> [topLevel]
ignoreHeader (Right (Frontend.NoHeader xs)) = xs
ignoreHeader _ = error "not no header"

ignoreRight :: Either a p -> p
ignoreRight (Right x) = x
ignoreRight (Left _) = error "not right"
