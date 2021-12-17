module Sexp.Serialize where

import Juvix.Library hiding (identity)
import qualified Juvix.Sexp as Sexp
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

data Test a
  = Test
  | Test2 a
  | TestRec a (Test a)
  | TestRec' (Test a)
  | Test3 {fstt :: a, sndd :: Integer}
  deriving (Show, Generic)

-- let's turn this to deriving via
instance (Sexp.Serialize a) => Sexp.Serialize (Test a)

top :: T.TestTree
top =
  T.testGroup
    "automatic serialization"
    [ compositionIsJust,
      serializerTest
    ]

type TInt = Test Integer

compositionIsJust :: T.TestTree
compositionIsJust =
  T.testGroup
    "Composing Serialize and Deserialize is Just"
    [ T.testCase "No Argument" (idCheck Test),
      T.testCase "One Argument" (idCheck (Test2 3)),
      T.testCase "2 named arguments" (idCheck (Test3 4 5)),
      T.testCase "recursive constructor" (idCheck (TestRec 3 (Test2 2))),
      T.testCase "nested serialize" (isJust (identityRec (Test2 (Test2 3))) T.@=? True)
    ]
  where
    idCheck x =
      x
        |> identity
        |> isJust
        |> (T.@=? True)

serializerTest :: T.TestTree
serializerTest =
  T.testGroup
    "Serialization form testing"
    [ T.testCase "name is expected" $
        Right (Sexp.serialize (TestRec (3 :: Integer) Test))
          T.@=? Sexp.parse "(:test-rec 3 :test)"
    ]

identityRec :: Test (Test Integer) -> Maybe (Test (Test Integer))
identityRec = Sexp.deserialize . Sexp.serialize

identity :: TInt -> Maybe TInt
identity = Sexp.deserialize . Sexp.serialize

test ::
  Maybe (C1 ('MetaCons "Test" 'PrefixI 'False) U1 p)
test = do
  let M1 t = from (Test :: Test ())
      L1 t2 = t
  (Sexp.gput t2 |> Sexp.gget :: Maybe (C1 ('MetaCons "Test" 'PrefixI 'False) U1 p))
