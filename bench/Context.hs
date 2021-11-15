module Context where

import qualified Criterion.Main as Criterion
import qualified Juvix.Backends.LLVM as LLVM
import qualified Juvix.Desugar as Desugar
import Juvix.Library hiding (mod)
import qualified Juvix.Library.Feedback as Feedback
import qualified Juvix.Parsing.Parser as Parser
import qualified Juvix.Pipeline as Pipeline
import qualified Juvix.Pipeline.ToSexp as ToSexp
import qualified Text.Megaparsec as P

bench :: Criterion.Benchmark
bench =
  Criterion.bgroup
    "desugar"
    [guardTest]

guardTest :: Criterion.Benchmark
guardTest =
  Criterion.env
    ( liftIO $
        Feedback.runFeedbackT $
          Pipeline.toML
            LLVM.BLLVM
            ( ""
                <> "mod Let where\n"
                <> "open Prelude\n"
                <> "sig (==) : field -> field -> bool\n"
                <> "let (==) = %LLVM.eq\n"
                <> "declare infixl (==) 2\n"
                <> "let foo | x == 3 = 3 | else = 2"
            )
    )
    ( \ ~(Feedback.Success _ t) ->
        Criterion.bgroup
          "guardTest"
          [ Criterion.bench "guard small WHNF" $
              Criterion.whnfIO (either (panic . show) identity <$> ToSexp.contextify t),
            Criterion.bench "guard small NF" $
              Criterion.nfIO (either (panic . show) identity <$> ToSexp.contextify t)
          ]
    )
