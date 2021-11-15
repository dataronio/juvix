module Test.Context.Helpers where

import qualified Juvix.Context as Context
import qualified Juvix.Contextify as Contextify
import qualified Juvix.Contextify.ToContext.ResolveOpenInfo as Contextify
import qualified Juvix.Contextify.ToContext.Types as Contextify
import qualified Juvix.Desugar as Desugar
import Juvix.Library
import qualified Juvix.Parsing.Parser as Parser
import qualified Juvix.Parsing.Types.Base as Parsing
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Translate.Pipeline.TopLevel as TopLevel

----------------------------------------------------------------------
-- Give me sexp terms helpers
----------------------------------------------------------------------

contextualizeFoo ::
  ByteString ->
  IO
    ( Either
        Context.PathError
        (Contextify.ContextSexp, [Contextify.PreQualified])
    )
contextualizeFoo byte =
  Contextify.contextify (("Foo", parseDesugarSexp byte) :| [])

parseDesugarSexp :: ByteString -> [Sexp.T]
parseDesugarSexp = Desugar.op . parsedSexp

parsedSexp :: ByteString -> [Sexp.T]
parsedSexp xs = ignoreHeader (Parser.parse xs) >>| TopLevel.transTopLevel

ignoreHeader :: Either a (Parsing.Header topLevel) -> [topLevel]
ignoreHeader (Right (Parsing.NoHeader xs)) = xs
ignoreHeader _ = panic "not no header"
