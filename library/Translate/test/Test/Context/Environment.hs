module Test.Context.Environment (top) where

import qualified Data.HashSet as Set
import qualified Juvix.Closure as Closure
import qualified Juvix.Contextify as Contextify
import qualified Juvix.Contextify.Binders as Bind
import qualified Juvix.Contextify.Environment as Env
import Juvix.Library
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Sexp as Sexp
import Test.Context.Helpers (contextualizeFoo, parseDesugarSexp)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

--------------------------------------------------------------------------------
-- Top Level Test
--------------------------------------------------------------------------------

top :: T.TestTree
top =
  T.testGroup
    "testing environment functions"
    [passContext]

--------------------------------------------------------------------------------
-- Environment Runner Types
--------------------------------------------------------------------------------
data Capture = Cap
  { closure :: Closure.T,
    report :: [Closure.T]
  }
  deriving (Generic, Show)

type CaptureAlias =
  ExceptT Env.ErrorS (State Capture)

newtype Context a = Ctx {_run :: CaptureAlias a}
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasReader "closure" Closure.T,
      HasSource "closure" Closure.T
    )
    via ReaderField "closure" CaptureAlias
  deriving
    ( HasWriter "report" [Closure.T],
      HasSink "report" [Closure.T]
    )
    via WriterField "report" CaptureAlias
  deriving
    (HasThrow "error" Env.ErrorS)
    via MonadError CaptureAlias

runCtx :: Context a -> Capture -> (Either Env.ErrorS a, Capture)
runCtx (Ctx c) = runState (runExceptT c)

emptyClosure :: Capture
emptyClosure = Cap (Closure.T Map.empty) []

-- recordClosure ::
--   (HasReader "closure" a m, HasWriter "report" [a] m) => Env.Pass m
recordClosure ctx t rec' =
  case t of
    Sexp.P (Bind.Other (PrintClosure _)) _ -> do
      c <- ask @"closure"
      tell @"report" [c]
      -- Just drop the given atom
      pure t >>| Sexp.Atom
    _ -> Env.handleAtom ctx t rec' >>| Sexp.Atom

atomClosure ctx t rec' =
  case t of
    Sexp.A a _ -> do
      c <- ask @"closure"
      tell @"report" [c]
      -- Just drop the given atom
      pure t >>| Sexp.Atom
    Sexp.P (Bind.Other (PrintClosure _)) _ ->
      Env.handleAtom ctx t rec' >>| Sexp.Atom
    _ -> Env.handleAtom ctx t rec' >>| Sexp.Atom

printRename :: Sexp.Options
printRename =
  Sexp.changeName
    (Sexp.defaultOptions @PrintClosure)
    (Map.fromList [("PrintClosure", "print-closure")])

data PrintClosure
  = PrintClosure (Sexp.B (Bind.BinderPlus PrintClosure))
  deriving (Show, Eq, Generic)

instance Sexp.DefaultOptions PrintClosure

instance Sexp.Serialize PrintClosure where
  serialize = Sexp.serializeOpt printRename
  deserialize = Sexp.deserializeOpt printRename

--------------------------------------------------------------------------------
-- PassContext Tests
--------------------------------------------------------------------------------

passContext :: T.TestTree
passContext =
  T.testGroup
    "testing passContext closures"
    [letTest, typeTest, caseTest, lambdaTest, declaimTest, openTest]

----------------------------------------
-- Let Test
----------------------------------------

letTest :: T.TestTree
letTest =
  T.testGroup
    "Lets properly add to the closure"
    [ T.testCase "let-match" $ do
        [x, y] <-
          capture
            "let f = let g = 3 in \
            \ let foo (Nil x)      = print-closure 0 in \
            \ let foo (Cons a y) z = print-closure 0 in \
            \ 3"
        Closure.keys x T.@=? firstClosure
        Closure.keys y T.@=? secondClosure,
      --
      T.testCase "let binds for its own arguments" $ do
        [a, x, y, b, foo] <-
          captureOnAtom "let f a = let foo x y = b in foo"
        Closure.keys a T.@=? Set.fromList ["a"]
        Closure.keys x T.@=? argumentBinding
        Closure.keys y T.@=? argumentBinding
        Closure.keys b T.@=? argumentBinding
        Closure.keys foo T.@=? Set.fromList ["a", "foo"]
    ]
  where
    firstClosure =
      Set.fromList ["g", "foo", "x"]
    secondClosure =
      Set.fromList ["g", "foo", "a", "y", "z"]
    argumentBinding =
      Set.fromList ["a", "foo", "x", "y"]

typeTest :: T.TestTree
typeTest =
  T.testGroup
    "Types properly add all to to the closure"
    [ T.testCase "top level type" $ do
        [print] <-
          capture "type foo a b c = Cons (print-closure 3)"
        Closure.keys print T.@=? Set.fromList ["a", "b", "c"],
      --
      T.testCase "let-type properly adds constructors" $ do
        [inside, body] <-
          capture
            "let f = \
            \ let type foo a b c = \
            \  | Cons (print-closure 3) \
            \  | Nil \
            \ in print-closure 4"
        Closure.keys inside T.@=? constructors
        Closure.keys body T.@=? constructors
    ]
  where
    constructors =
      Set.fromList ["a", "b", "c", "Nil", "Cons"]

caseTest :: T.TestTree
caseTest =
  T.testGroup
    "Case binder"
    [ T.testCase "case properly adds bound arguments" $ do
        [cons, nil] <-
          capture
            "let f = \
            \ case foo of \
            \  | Cons a b -> (print-closure 3) \
            \  | Nil -> (print-closure 3)"
        Closure.keys cons T.@=? Set.fromList ["a", "b"]
        Closure.keys nil T.@=? Set.fromList []
    ]

lambdaTest :: T.TestTree
lambdaTest =
  T.testGroup
    "λ binder"
    [ T.testCase "λ properly adds binders" $ do
        [lamb] <-
          capture
            "let f = \\(Cons a b) -> (print-closure 3)"
        Closure.keys lamb T.@=? Set.fromList ["a", "b"]
    ]

declaimTest :: T.TestTree
declaimTest =
  T.testGroup
    "declaim binder"
    [ T.testCase "declaim properly adds info" $ do
        [lamb] <-
          capture
            "let f = declare infixl (+) 7 in print-closure 2"
        -- we could check for info, but this is sufficient for it
        -- properly working
        Closure.keys lamb T.@=? Set.fromList ["+"]
    ]

openTest :: T.TestTree
openTest =
  T.testGroup
    "open Tests"
    [ T.testCase "open properly adds symbols" $ do
        Right (ctx, _) <-
          Contextify.contextify
            ( ("Foo", parseDesugarSexp "let f = open A in print-closure 2")
                :| [("A", parseDesugarSexp "let bar = 3")]
            )
        let (_, Cap _ [Closure.T capture]) =
              runCtx (Env.contextPassStar ctx recordClosure) emptyClosure
        Map.toList capture T.@=? [("bar", Closure.Info Nothing [] (Just "A"))]
    ]

capture :: ByteString -> IO [Closure.T]
capture str = do
  Right (ctx, _) <-
    contextualizeFoo str
  let (_, Cap _ capture) =
        runCtx (Env.contextPassStar ctx recordClosure) emptyClosure
  pure capture

captureOnAtom :: ByteString -> IO [Closure.T]
captureOnAtom str = do
  Right (ctx, _) <-
    contextualizeFoo str
  let (_, Cap _ capture) =
        runCtx (Env.contextPassStar ctx atomClosure) emptyClosure
  pure capture

-- Right (ctx,_) <- contextualizeFoo "let f = 3"
-- (_, capture) = runCtx (Env.passContextSingle ctx recordClosure) emptyClosure
