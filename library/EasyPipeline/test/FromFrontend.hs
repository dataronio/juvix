module FromFrontend where

import qualified Easy
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.IR.Types as IR
import Juvix.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

--------------------------------------------------------------------------------
-- Top
--------------------------------------------------------------------------------

top :: T.TestTree
top =
  T.testGroup
    "From Frontend Tests:"
    [patternVarTests]

--------------------------------------------------------------------------------
-- tests
--------------------------------------------------------------------------------

patternVarTests =
  T.testGroup
    "pattern Var Tests"
    [ T.testCase
        "pattern vars should be tracked"
        ( do
            pattern' <- rawPattern
            rawPatternShouldBe T.@=? pattern'
        ),
      T.testCase
        "Multiple variables should be tracked"
        ( do
            pattern' <- rawPatternAdd
            rawPatternAddShouldBe T.@=? pattern'
        )
    ]
  where
    rawPattern = do
      (_patMap, x) <- Easy.coreify "sig foo : int let foo x = x" Easy.defMichelson
      pure (grabSingleBody (Easy.lookupCoreFunction x Easy.defMichelson "foo"))
    rawPatternShouldBe =
      IR.Elim (IR.Free (Core.Pattern 0))
    rawPatternAdd = do
      (_patMap, x) <-
        Easy.coreify
          "open Prelude\
          \ open Michelson\
          \ open Alias\
          \ sig foo : int -> int -> int\
          \ let foo x y = x + y"
          Easy.defMichelson
      pure $ grabSingleBody (Easy.lookupCoreFunction x Easy.defMichelson "foo")
    rawPatternAddShouldBe =
      IR.Elim
        ( IR.App
            ( IR.App
                (IR.Free (Core.Global "Prelude.Michelson.Alias.+"))
                (IR.Elim (IR.Free (Core.Pattern 0)))
            )
            (IR.Elim (IR.Free (Core.Pattern 1)))
        )

grabSingleBody
  ( Just
      ( Core.RawGFunction
          Core.RawFunction
            { Core.rawFunClauses = Core.RawFunClause {Core.rawClauseBody = body} :| []
            }
        )
    ) =
    body
