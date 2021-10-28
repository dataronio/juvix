{-# LANGUAGE OverloadedLists #-}

-- | Tests for the type checker and evaluator in Core/IR/Typechecker.hs
module Typechecker (coreCheckerEval) where

import qualified Juvix.Core.Application as App
import qualified Juvix.Core.Base as Core
import qualified Juvix.Core.Base.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.CheckTerm as TC
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.IR.Typechecker as Typed
import qualified Juvix.Core.Parameterisation as P
import qualified Juvix.Core.Parameterisations.All as All
import qualified Juvix.Core.Parameterisations.Naturals as Nat
import qualified Juvix.Core.Parameterisations.Unit as Unit
import Juvix.Core.Types
import Juvix.Library hiding (identity)
import Juvix.Library.HashMap as Map
import qualified Juvix.Library.Usage as Usage
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

type TermT ty val = Core.Term IR.T (KindedType ty) (TypedPrim ty val)

type ElimT ty val = Core.Elim IR.T (KindedType ty) (TypedPrim ty val)

type NatTerm = Core.Term IR.T Nat.Ty Nat.Val

type NatTermT = TermT Nat.Ty Nat.Val

type NatElimT = ElimT Nat.Ty Nat.Val

type NatElim = IR.Elim Nat.Ty Nat.Val

type NatValueT = Typed.ValueT IR.T Nat.Ty Nat.Val

type NatAnnotation = Typed.AnnotationT IR.T Nat.Ty Nat.Val

type UnitAnnotation = Typed.AnnotationT IR.T Unit.Ty Unit.Val

type AllTerm = Core.Term IR.T All.Ty All.Val

type AllAnnotation = Typed.AnnotationT IR.T All.Ty All.Val

-- | The Bool parameter means "expect right".
assertEitherIsAsExpected ::
  (HasCallStack, Show a, Show b) =>
  Bool ->
  Either a b ->
  T.Assertion
assertEitherIsAsExpected True (Right _) = pure ()
assertEitherIsAsExpected True (Left l) =
  T.assertFailure $
    "expected a Right, got\n\t"
      ++ "Left ("
      ++ show l
      ++ ")"
assertEitherIsAsExpected False (Right r) =
  T.assertFailure $
    "expected a Left, got\n\t"
      ++ "Right ("
      ++ show r
      ++ ")"
assertEitherIsAsExpected False (Left _) = pure ()

-- unit test generator for typeTerm
assertCheckResultWith ::
  ( HasCallStack,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    CanPrimApply P.Star primTy,
    CanPrimApply primTy primVal,
    Eq (PrimApplyError primTy),
    Show (PrimApplyError primTy),
    Eq (PrimApplyError primVal),
    Show (PrimApplyError primVal),
    Typed.PrimSubstValue primTy primVal,
    Typed.PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal,
    Eval.HasPatSubstType
      (OnlyExts.T Typed.T)
      primTy
      (TypedPrim primTy primVal)
      primTy
  ) =>
  Bool ->
  T.TestName ->
  Parameterisation primTy primVal ->
  Typed.GlobalsT IR.T IR.T primTy primVal ->
  Typed.Context primTy primVal ->
  Core.Term IR.T primTy primVal ->
  Typed.AnnotationT IR.T primTy primVal ->
  T.TestTree
assertCheckResultWith expectSuccess name param globals ctx term ann =
  -- TODO: take out the logs and put them in an IO monad.
  let (res, _) = Typed.exec globals $ TC.typeTermWith param mempty ctx term ann
   in T.testCase name $ assertEitherIsAsExpected expectSuccess res

-- unit test generator for typeTerm
shouldCheckWith ::
  ( HasCallStack,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    CanPrimApply P.Star primTy,
    CanPrimApply primTy primVal,
    Eq (PrimApplyError primTy),
    Show (PrimApplyError primTy),
    Eq (PrimApplyError primVal),
    Show (PrimApplyError primVal),
    Typed.PrimSubstValue primTy primVal,
    Typed.PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal,
    Eval.HasPatSubstType
      (OnlyExts.T Typed.T)
      primTy
      (TypedPrim primTy primVal)
      primTy
  ) =>
  T.TestName ->
  Parameterisation primTy primVal ->
  Typed.GlobalsT IR.T IR.T primTy primVal ->
  Typed.Context primTy primVal ->
  Core.Term IR.T primTy primVal ->
  Typed.AnnotationT IR.T primTy primVal ->
  T.TestTree
shouldCheckWith = assertCheckResultWith True

-- unit test generator for typeTerm
shouldFailWith ::
  ( HasCallStack,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    CanPrimApply P.Star primTy,
    CanPrimApply primTy primVal,
    Eq (PrimApplyError primTy),
    Show (PrimApplyError primTy),
    Eq (PrimApplyError primVal),
    Show (PrimApplyError primVal),
    Typed.PrimSubstValue primTy primVal,
    Typed.PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal,
    Eval.HasPatSubstType
      (OnlyExts.T Typed.T)
      primTy
      (TypedPrim primTy primVal)
      primTy
  ) =>
  T.TestName ->
  Parameterisation primTy primVal ->
  Typed.GlobalsT IR.T IR.T primTy primVal ->
  Typed.Context primTy primVal ->
  Core.Term IR.T primTy primVal ->
  Typed.AnnotationT IR.T primTy primVal ->
  T.TestTree
shouldFailWith = assertCheckResultWith False

assertCheckResult ::
  ( HasCallStack,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    CanPrimApply P.Star primTy,
    CanPrimApply primTy primVal,
    Eq (PrimApplyError primTy),
    Show (PrimApplyError primTy),
    Eq (PrimApplyError primVal),
    Show (PrimApplyError primVal),
    Typed.PrimSubstValue primTy primVal,
    Typed.PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal,
    Eval.HasPatSubstType
      (OnlyExts.T Typed.T)
      primTy
      (TypedPrim primTy primVal)
      primTy
  ) =>
  Bool ->
  T.TestName ->
  Parameterisation primTy primVal ->
  Core.Term IR.T primTy primVal ->
  Typed.AnnotationT IR.T primTy primVal ->
  T.TestTree
assertCheckResult expectSuccess name param =
  assertCheckResultWith expectSuccess name param mempty []

shouldCheck ::
  ( HasCallStack,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    CanPrimApply P.Star primTy,
    CanPrimApply primTy primVal,
    Eq (PrimApplyError primTy),
    Show (PrimApplyError primTy),
    Eq (PrimApplyError primVal),
    Show (PrimApplyError primVal),
    Typed.PrimSubstValue primTy primVal,
    Typed.PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal,
    Eval.HasPatSubstType
      (OnlyExts.T Typed.T)
      primTy
      (TypedPrim primTy primVal)
      primTy
  ) =>
  T.TestName ->
  Parameterisation primTy primVal ->
  Core.Term IR.T primTy primVal ->
  Typed.AnnotationT IR.T primTy primVal ->
  T.TestTree
shouldCheck = assertCheckResult True

shouldFail ::
  ( HasCallStack,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    CanPrimApply P.Star primTy,
    CanPrimApply primTy primVal,
    Eq (PrimApplyError primTy),
    Show (PrimApplyError primTy),
    Eq (PrimApplyError primVal),
    Show (PrimApplyError primVal),
    Typed.PrimSubstValue primTy primVal,
    Typed.PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal,
    Eval.HasPatSubstType
      (OnlyExts.T Typed.T)
      primTy
      (TypedPrim primTy primVal)
      primTy
  ) =>
  T.TestName ->
  Parameterisation primTy primVal ->
  Core.Term IR.T primTy primVal ->
  Typed.AnnotationT IR.T primTy primVal ->
  T.TestTree
shouldFail = assertCheckResult False

-- unit test generator for typeElim
shouldInferWith ::
  ( HasCallStack,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    CanPrimApply P.Star primTy,
    CanPrimApply primTy primVal,
    Eq (PrimApplyError primTy),
    Show (PrimApplyError primTy),
    Eq (PrimApplyError primVal),
    Show (PrimApplyError primVal),
    Typed.PrimSubstValue primTy primVal,
    Typed.PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal,
    Eval.HasPatSubstType
      (OnlyExts.T Typed.T)
      primTy
      (TypedPrim primTy primVal)
      primTy
  ) =>
  T.TestName ->
  Parameterisation primTy primVal ->
  Typed.GlobalsT IR.T IR.T primTy primVal ->
  Typed.Context primTy primVal ->
  IR.Elim primTy primVal ->
  Typed.AnnotationT IR.T primTy primVal ->
  T.TestTree
shouldInferWith name param globals ctx elim ann@(Typed.Annotation {annUsage = σ}) =
  let (res, _) = Typed.exec globals $ TC.typeElimWith param mempty ctx elim σ
      resTy = Typed.getElimAnn . TC.loValue <$> res
   in T.testCase name $ resTy T.@?= Right ann

shouldInfer ::
  ( HasCallStack,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    CanPrimApply P.Star primTy,
    CanPrimApply primTy primVal,
    Eq (PrimApplyError primTy),
    Show (PrimApplyError primTy),
    Eq (PrimApplyError primVal),
    Show (PrimApplyError primVal),
    Typed.PrimSubstValue primTy primVal,
    Typed.PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal,
    Eval.HasPatSubstType
      (OnlyExts.T Typed.T)
      primTy
      (TypedPrim primTy primVal)
      primTy
  ) =>
  T.TestName ->
  Parameterisation primTy primVal ->
  IR.Elim primTy primVal ->
  Typed.AnnotationT IR.T primTy primVal ->
  T.TestTree
shouldInfer name param = shouldInferWith name param mempty []

-- unit test generator for evalTerm
shouldEval' ::
  ( HasCallStack,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    CanApply primVal,
    CanApply primTy,
    Eq (Eval.Error IR.T IR.T primTy primVal),
    Show (Eval.Error IR.T IR.T primTy primVal),
    Eval.HasPatSubstType (OnlyExts.T IR.T) primTy primVal primTy,
    Eval.HasPatSubstTerm (OnlyExts.T IR.T) primTy primVal primVal,
    Eval.HasSubstValueType IR.T primTy primVal primTy,
    Eval.HasSubstValue IR.T primTy primVal primVal,
    Eval.HasWeak primVal
  ) =>
  T.TestName ->
  Core.Globals IR.T IR.T primTy primVal ->
  Core.Term IR.T primTy primVal ->
  Core.Value IR.T primTy primVal ->
  T.TestTree
shouldEval' name g term res =
  T.testCase name $ (IR.evalTerm (IR.lookupFun' g) term) T.@=? Right res

shouldEval ::
  ( HasCallStack,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    CanApply primVal,
    CanApply primTy,
    Eq (Eval.Error IR.T IR.T primTy primVal),
    Show (Eval.Error IR.T IR.T primTy primVal),
    Eval.HasPatSubstType (OnlyExts.T IR.T) primTy primVal primTy,
    Eval.HasPatSubstTerm (OnlyExts.T IR.T) primTy primVal primVal,
    Eval.HasSubstValueType IR.T primTy primVal primTy,
    Eval.HasSubstValue IR.T primTy primVal primVal,
    Eval.HasWeak primVal
  ) =>
  T.TestName ->
  Core.Term IR.T primTy primVal ->
  Core.Value IR.T primTy primVal ->
  T.TestTree
shouldEval name = shouldEval' name mempty

infix 1 `ann`

ann :: Usage.T -> Core.Value IR.T primTy primVal -> Typed.Annotation IR.T primTy primVal
ann = Typed.Annotation

coreCheckerEval :: T.TestTree
coreCheckerEval =
  T.testGroup
    "Core type checker and evaluator tests"
    [ skiComp,
      natComp,
      dependentFunctionComp,
      dependentPairComp,
      letComp,
      evaluations,
      skiCont,
      subtype,
      inlinings
    ]

skiComp :: T.TestTree
skiComp =
  T.testGroup
    "SKI combinators Computational typing"
    [ shouldCheck "I [nat]" Nat.t identity identityNatCompTy,
      shouldCheck "I [unit]" Unit.t identity identityUnitCompTy,
      shouldCheck "I 1" Nat.t identityApplication natTy,
      shouldInfer "I (I 1)" Nat.t identityAppINat1 natTy,
      shouldInfer "I I" Nat.t identityAppI identityNatCompTy,
      shouldCheck "K [nat]" Nat.t kcombinator kCompTy,
      shouldCheck "K [unit]" All.t kcombinator kCompTyWithUnit,
      shouldInfer "I K" Nat.t identityAppK kCompTy,
      shouldCheck "K (I : _)" Nat.t (IR.Elim kAppI) kAppICompTy,
      shouldCheck "K I" Nat.t (IR.Elim kAppINotAnnotated) kAppICompTy,
      shouldInfer "K 1 : nat" Nat.t kApp1 natToNatTy,
      shouldInfer "K 1 : nat -> nat" Nat.t kFunApp1 kFunApp1CompTy
    ]

natComp :: T.TestTree
natComp =
  T.testGroup
    "Nat Computational typing"
    [ shouldCheck "nat" Nat.t natT' (mempty `ann` IR.VStar 0),
      shouldCheck "1" Nat.t (nat 1) (Usage.SAny `ann` natT),
      shouldCheck "add" Nat.t (IR.Prim Nat.Add) (Usage.SAny `ann` addTy),
      shouldFail "add : nat" Nat.t (IR.Prim Nat.Add) (Usage.SAny `ann` natT)
    ]

dependentFunctionComp :: T.TestTree
dependentFunctionComp =
  T.testGroup
    "Dependent Functions Computational typing"
    [ shouldCheck "λA x. (1 x: A)" All.t depIdentity depIdentityCompTy,
      shouldCheck "λA x. x" All.t depIdentity' depIdentityCompTy,
      shouldCheck "λA x. (ω x: A)" All.t depIdentity depIdentityCompTySAny,
      shouldCheck "λA B x y. (1 x: A)" All.t depK depKCompTy,
      shouldCheck
        "(0 A : ⋆₀) → 1 A → A"
        All.t
        depIdentityCompTyT
        (mempty `ann` IR.VStar 1),
      shouldCheck
        "(0 A B : ⋆₀) → 1 A → 0 B → A"
        All.t
        depKCompTyT
        (mempty `ann` IR.VStar 1)
    ]

letComp :: T.TestTree
letComp =
  T.testGroup
    "'let' Computational typing"
    [ shouldCheck
        "let 0 x = 0 in 0"
        Nat.t
        (IR.Let mempty nzero (IR.Elim nzero))
        (Usage.SAny `ann` natT),
      shouldCheck
        "let ω x = 0 in x"
        Nat.t
        (IR.Let Usage.SAny nzero (IR.Elim (IR.Bound 0)))
        (Usage.SAny `ann` natT),
      -- λx. let 0 y = 0 in x
      shouldCheck
        "λx. let 0 y = 0 in x"
        Nat.t
        (IR.Lam (IR.Let mempty nzero (IR.Elim (IR.Bound 1))))
        (natToNatTy' one)
    ]
  where
    nzero = IR.Ann Usage.SAny (nat 0) natT' 0

evaluations :: T.TestTree
evaluations =
  T.testGroup
    "Evaluations"
    [ shouldEval "add 1 2" add12 (natVT 3),
      shouldEval "sub 5 2" sub52 (natVT 3),
      shouldEval "I 1" identityApplicationT (natVT 1),
      shouldEval "I I 1" (IR.Elim identityAppINat1T) (natVT 1),
      shouldEval "I I" (IR.Elim identityAppIT) videntity,
      shouldEval "K 1 2" (IR.Elim kApp1_2T) (natVT 1),
      shouldEval' "ty" typGlobals (IR.Elim (IR.Free (Core.Global "ty"))) (IR.VStar 0),
      shouldEval' "tz" typGlobals (name "tz") (vname "tz"),
      shouldEval' "B" typGlobals (name "B") (vname "A"),
      shouldEval' "C" typGlobals (name "C") (vname "A")
    ]
  where
    add12 = IR.Elim $ add `IR.App` nat' 1 `IR.App` nat' 2
    sub52 = IR.Elim $ sub `IR.App` nat' 5 `IR.App` nat' 2
    binPrim op = App.Return {retTerm = op, retType = [Nat.Ty, Nat.Ty, Nat.Ty]}
    sub = IR.Ann Usage.SAny (IR.Prim $ binPrim Nat.Sub) addTyT' 0
    add = IR.Ann Usage.SAny (IR.Prim $ binPrim Nat.Add) addTyT' 0
    videntity = IR.VLam $ Core.VBound 0
    name = IR.Elim . IR.Free . Core.Global
    vname = Core.VFree . Core.Global

inlinings :: T.TestTree
inlinings =
  T.testGroup
    "Inlining"
    [ shouldInlineE "F" (name "F") (name "F"),
      shouldInlineE "B" (name "B") (name "A"),
      shouldInlineE "C" (name "C") (name "A"),
      shouldInlineE
        "F B"
        (name "F" `IR.App` tname "B")
        (name "F" `IR.App` tname "A"),
      shouldInlineT "*₀" (IR.Star 0) (IR.Star 0)
    ]
  where
    name = IR.Free . Core.Global
    tname = IR.Elim . name
    shouldInlineE lbl e e' =
      T.testCase lbl $
        IR.inlineAllGlobalsElim e look mempty T.@?= e' -- No patterns, so mempty.
    shouldInlineT lbl t t' =
      T.testCase lbl $
        IR.inlineAllGlobals t look mempty T.@?= t' -- No pattersn, so memty.
    look :: Core.GlobalName -> Maybe (IR.Elim Unit.Ty Unit.Val)
    look "B" = Just $ name "A"
    look "C" = Just $ name "B"
    look _ = Nothing

skiCont :: T.TestTree
skiCont =
  T.testGroup
    "SKI combinators contemplational typing"
    [ shouldCheck "0 · I" Nat.t identity identityNatContTy
    ]

subtype :: T.TestTree
subtype =
  T.testGroup
    "Subtyping"
    [ shouldCheckWith "A : ⋆₀" Unit.t typGlobals [] aTerm $ mempty `ann` IR.VStar 0,
      shouldCheckWith "A : ⋆₁" Unit.t typGlobals [] aTerm $ mempty `ann` IR.VStar 1,
      shouldCheckWith
        "F : ⋆₁ → ⋆₁"
        Unit.t
        typGlobals
        []
        fTerm
        $ mempty `ann` typ2typ 1 1,
      shouldCheckWith
        "F : ⋆₀ → ⋆₁"
        Unit.t
        typGlobals
        []
        fTerm
        $ mempty `ann` typ2typ 0 1,
      shouldCheckWith
        "F : ⋆₁ → ⋆₂"
        Unit.t
        typGlobals
        []
        fTerm
        $ mempty `ann` typ2typ 1 2,
      shouldInferWith
        "F A : ⋆₁"
        Unit.t
        typGlobals
        []
        faElim
        $ mempty `ann` IR.VStar 1
    ]
  where
    typ2typ i j = IR.VPi mempty (IR.VStar i) (IR.VStar j)

-- \x. x
identity :: forall primTy primVal. Core.Term IR.T primTy primVal
identity = IR.Lam (IR.Elim (IR.Bound 0))

-- computation annotation of identity: (1, 1 Nat -> Nat)
identityNatCompTy :: NatAnnotation
identityNatCompTy = one `ann` IR.VPi one natT natT

-- computation annotation of identity: (1, 1 Unit -> Unit)
identityUnitCompTy :: UnitAnnotation
identityUnitCompTy = one `ann` IR.VPi one unitT unitT

-- contemplation annotation of identity: (0, 0 Nat -> Nat)
identityNatContTy :: NatAnnotation
identityNatContTy = mempty `ann` IR.VPi mempty natT natT

-- dependent identity function, \t.\x.x 1: t
depIdentity :: forall primTy primVal. Core.Term IR.T primTy primVal
depIdentity =
  IR.Lam -- first input \t.
    ( IR.Lam -- second input \x.
        ( IR.Elim -- output
            ( IR.Ann -- annotation is of
                one -- 1 usage
                (IR.Elim (IR.Bound 0))
                -- x is the output, which has annotation (1, t)
                (IR.Elim (IR.Bound 1)) -- of type t
                0
            )
        )
    )

-- dependent identity function without ann, \t.\x.x
depIdentity' :: forall primTy primVal. Core.Term IR.T primTy primVal
depIdentity' = IR.Lam $ IR.Lam $ IR.Elim $ IR.Bound 0

-- computation dependent identity annotation (1, 0 * -> 1 t -> t)
depIdentityCompTyT :: AllTerm
depIdentityCompTyT =
  IR.Pi mempty (IR.Star 0) $
    IR.Pi one (IR.Elim $ IR.Bound 0) $
      IR.Elim $ IR.Bound 1

-- computation dependent identity annotation (1, 0 * -> 1 t -> t)
depIdentityCompTy :: AllAnnotation
depIdentityCompTy =
  one
    `ann` IR.VPi
      mempty
      (IR.VStar 0)
      (IR.VPi one (Core.VBound 0) (Core.VBound 1))

-- computation dependent identity annotation (1, 0 * -> w t -> t)
depIdentityCompTySAny :: AllAnnotation
depIdentityCompTySAny =
  one
    `ann` IR.VPi
      mempty
      (IR.VStar 0)
      (IR.VPi Usage.SAny (Core.VBound 0) (Core.VBound 1))

-- (\x.x) 1
identityApplication :: NatTerm
identityApplication =
  IR.Elim $
    IR.Ann one identity (IR.Pi one natT' natT') 0 `IR.App` nat 1

-- (\x.x) 1
identityApplicationT :: NatTermT
identityApplicationT =
  IR.Elim $
    IR.Ann one identity (IR.Pi one natTK natTK) 0 `IR.App` nat' 1

-- computation annotation (1, Nat)
natTy :: NatAnnotation
natTy = one `ann` natT

-- I:(Nat->Nat)->(Nat->Nat) I:(Nat->Nat) type checked to (Nat->Nat)
-- I:(Nat->Nat) I:(Nat->Nat) correctly does not type checked
identityAppI :: NatElim
identityAppI =
  let natToNat = (IR.Pi one natT' natT')
      idAt ty = IR.Ann one identity (IR.Pi one ty ty) 0
   in idAt natToNat `IR.App` IR.Elim (idAt natT')

-- I:(Nat->Nat)->(Nat->Nat) I:(Nat->Nat) type checked to (Nat->Nat)
-- I:(Nat->Nat) I:(Nat->Nat) correctly does not type checked
identityAppIT :: NatElimT
identityAppIT =
  let natToNat = (IR.Pi one natTK natTK)
      idAt ty = IR.Ann one identity (IR.Pi one ty ty) 0
   in idAt natToNat `IR.App` IR.Elim (idAt natTK)

-- (I:1 (1 Nat->Nat) -> (1 Nat->Nat) I:(1 Nat->Nat) ) 1 type checked to Nat.Ty
identityAppINat1 :: NatElim
identityAppINat1 = identityAppI `IR.App` nat 1

-- (I:1 (1 Nat->Nat) -> (1 Nat->Nat) I:(1 Nat->Nat) ) 1 type checked to Nat.Ty
identityAppINat1T :: NatElimT
identityAppINat1T = identityAppIT `IR.App` nat' 1

kcombinator :: forall primTy primVal. Core.Term IR.T primTy primVal -- K = \x.\y.x
kcombinator = IR.Lam (IR.Lam (IR.Elim (IR.Bound 1)))

-- K has annotation (1, 1 Nat -> 0 Nat -> Nat )
kCompTy :: NatAnnotation
kCompTy =
  one
    `ann` IR.VPi -- the sig usage of k
    -- first input, 1 Nat
      one -- is used once in the output
      natT -- of type Nat
      ( IR.VPi -- second input, 0 Nat
          mempty -- is not used in the output
          natT -- of type Nat
          natT -- the output is of type Nat
      )

-- K computation annotation (1, 1 Nat -> 0 () -> Nat)
kCompTyWithUnit :: AllAnnotation
kCompTyWithUnit =
  one `ann` IR.VPi one natTAll (IR.VPi mempty unitTAll natTAll)

-- I K computation annotation
-- (1, 1 (1 Nat -> 0 Nat -> Nat) ->
--       (1 Nat -> 0 Nat -> Nat) -> (1 Nat -> 0 Nat -> Nat))
identityAppK :: NatElim
identityAppK =
  IR.App -- applying I to K
    ( IR.Ann -- I
        one -- sig usage, the first 1 in the annotation
        identity -- annotation (1, (1 Nat -> 0 Nat -> Nat) -> ( 1 Nat -> 0 Nat -> Nat) )
        ( IR.Pi
            one -- sig usage, the first 1 in the annotation
            ( IR.Pi
                one -- the second 1 in the annotation
                natT' -- (1 Nat ->
                (IR.Pi mempty natT' natT') -- 0 Nat -> Nat)
                -- ->
            )
            ( IR.Pi
                one
                natT' -- (1 Nat ->
                (IR.Pi mempty natT' natT') -- 0 Nat -> Nat)
            )
        )
        0
    ) -- K
    ( IR.Elim
        ( IR.Ann
            one -- sig usage
            kcombinator -- annotation (1, (1 Nat -> 0 Nat-> Nat))
            ( IR.Pi
                one
                natT' -- (1 Nat ->
                (IR.Pi mempty natT' natT') -- 0 Nat -> Nat)
            )
            0
        )
    )

-- (K: Nat -> Nat -> Nat 1) should type check to Nat -> Nat
kApp1 :: NatElim
kApp1 =
  IR.Ann -- K
    one -- sig usage
    kcombinator -- annotation (1, (1 Nat -> 0 Nat -> Nat))
    ( IR.Pi
        one
        natT' -- (1 Nat ->
        (IR.Pi mempty natT' natT') -- 0 Nat -> Nat)
    )
    0
    `IR.App` nat 1

-- (K: Nat -> Nat -> Nat 1) should type check to Nat -> Nat
kApp1T :: NatElimT
kApp1T =
  IR.Ann -- K
    one -- sig usage
    kcombinator -- annotation (1, (1 Nat -> 0 Nat -> Nat))
    ( IR.Pi
        one
        natTK -- (1 Nat ->
        (IR.Pi mempty natTK natTK) -- 0 Nat -> Nat)
    )
    0
    `IR.App` nat' 1

kApp1_2T :: NatElimT
kApp1_2T = kApp1T `IR.App` nat' 2

-- computation annotation (π Nat -> Nat)
natToNatTy' :: Usage.T -> NatAnnotation
natToNatTy' π = one `ann` IR.VPi π natT natT

natToNatTy :: NatAnnotation
natToNatTy = natToNatTy' mempty

-- 0 Nat -> Nat

-- (1 (K: 1 Nat -> 0 (1 Nat -> Nat) -> Nat) 1)
-- should type check to 0 (1 Nat -> Nat) -> Nat
kFunApp1 :: NatElim
kFunApp1 =
  IR.App -- applying K to 1
    ( IR.Ann
        one -- sig usage
        kcombinator -- annotation (1, (1 Nat -> 0 (1 Nat -> Nat) -> Nat))
        ( IR.Pi
            one
            natT' -- 1 Nat ->
            ( IR.Pi
                mempty -- usage of (1 Nat -> Nat )
                (IR.Pi one natT' natT')
                -- (1 Nat -> Nat ) ->
                natT' -- Nat
            )
        )
        0
    )
    (nat 1) -- 1
    -- computation annotation (1, 0 (1 Nat -> Nat) -> Nat)

kFunApp1CompTy :: NatAnnotation
kFunApp1CompTy = one `ann` IR.VPi mempty (IR.VPi one natT natT) natT

-- 1 K: 1 (1 Nat -> Nat) -> 0 Nat -> (1 Nat -> Nat) 1 I: 1 Nat -> Nat
-- type checks to 0 Nat -> (1 Nat -> Nat)
kAppI :: NatElim
kAppI =
  IR.App -- applying K to I
    ( IR.Ann
        one -- sig usage
        kcombinator
        ( IR.Pi
            one -- usage of (1 Nat -> Nat)
            (IR.Pi one natT' natT')
            -- (1 Nat -> Nat) ->
            ( IR.Pi
                mempty
                natT' -- 0 Nat ->
                (IR.Pi one natT' natT')
                -- (1 Nat -> Nat)
            )
        )
        0
    )
    ( IR.Elim
        ( IR.Ann -- I
            one -- usage of identity
            identity
            (IR.Pi one natT' natT') -- 1 Nat -> Nat
            0
        )
    )

-- 1 K: 1 (1 Nat -> Nat) -> 0 Nat -> (1 Nat -> Nat) I
-- type checks to 0 Nat -> (1 Nat -> Nat)
kAppINotAnnotated :: NatElim
kAppINotAnnotated =
  IR.App
    ( IR.Ann
        one -- sig usage
        kcombinator
        ( IR.Pi
            one -- usage of (1 Nat -> Nat)
            (IR.Pi one natT' natT')
            -- (1 Nat -> Nat) ->
            ( IR.Pi
                mempty
                natT' -- 0 Nat ->
                (IR.Pi one natT' natT')
                -- (1 Nat -> Nat)
            )
        )
        0
    )
    identity

-- computation annotation (1, 0 Nat -> (1 Nat -> Nat))
kAppICompTy :: NatAnnotation
kAppICompTy = one `ann` IR.VPi mempty natT (IR.VPi one natT natT)

-- dependent k, \t1.\t2.\x:t1.\y:t2.x 1: t1
depK :: forall primTy primVal. Core.Term IR.T primTy primVal
depK =
  IR.Lam -- first input t1, Bound 3 counting from output
    ( IR.Lam -- second input t2, Bound 2 counting from output
        ( IR.Lam -- third input x, Bound 1 counting from output
            ( IR.Lam -- forth input y, Bound 0 counting from output
                ( IR.Elim -- output
                    (IR.Bound 1)
                )
            )
        )
    )

-- computation dependent k annotation
-- \t1.\t2.\x.\y.x 1: (t1 0: *0) -> (t2 0: *0) -> (x 1: t1) -> (y 0: t2) -> t1
depKCompTy :: AllAnnotation
depKCompTy =
  one
    `ann` IR.VPi
      mempty
      (IR.VStar 0)
      ( IR.VPi
          mempty
          (IR.VStar 0)
          ( IR.VPi
              one
              (Core.VBound 1)
              ( IR.VPi
                  mempty
                  (Core.VBound 1)
                  (Core.VBound 3)
              )
          )
      )

depKCompTyT :: AllTerm
depKCompTyT =
  IR.Pi mempty (IR.Star 0) $
    IR.Pi mempty (IR.Star 0) $
      IR.Pi one (IR.Elim $ IR.Bound 1) $
        IR.Pi mempty (IR.Elim $ IR.Bound 1) $
          IR.Elim $ IR.Bound 3

dependentPairComp :: T.TestTree
dependentPairComp =
  T.testGroup
    "Dependent pair typing"
    [ shouldCheck "(nat, 1) : Σ(A: ⋆₀). A" Nat.t boxNat boxNatAnn,
      shouldCheck
        "(unit, Unit) : Σ(A: ⋆₀). A"
        All.t
        unitTypeUnitValuePair
        allAnn,
      shouldFail
        "¬ (unit, 1) :  Σ(A: ⋆₀). A"
        All.t
        unitTypeNatValuePair
        allAnn,
      shouldFail
        "¬ (nat, Unit) :  Σ(A: ⋆₀). A"
        All.t
        natTypeUnitValuePair
        allAnn,
      shouldCheck
        "(nat, 1) : Σ(A: ⋆₀). A [all]"
        All.t
        natTypeNatValuePair
        allAnn,
      shouldCheck "(Σ(A: ⋆₀). A) : ⋆₁" All.t (allSig 0) (starAnn 1)
    ]

boxNatAnn :: NatAnnotation
boxNatAnn = one `ann` IR.VSig mempty (IR.VStar 0) (Core.VBound 0)

boxNat :: NatTerm
boxNat = IR.Pair natT' (nat 1)

allAnn :: AllAnnotation
allAnn = one `ann` IR.VSig mempty (IR.VStar 0) (Core.VBound 0)

allNatTy :: AllTerm
allNatTy = IR.PrimTy (All.NatTy Nat.Ty)

allNat :: Natural -> AllTerm
allNat n = IR.Prim (All.NatVal (Nat.Val n))

unitTypeUnitValuePair :: AllTerm
unitTypeUnitValuePair = IR.Pair IR.UnitTy IR.Unit

unitTypeNatValuePair :: AllTerm
unitTypeNatValuePair = IR.Pair IR.UnitTy (allNat 0)

natTypeUnitValuePair :: AllTerm
natTypeUnitValuePair = IR.Pair allNatTy IR.Unit

natTypeNatValuePair :: AllTerm
natTypeNatValuePair = IR.Pair allNatTy (allNat 0)

starAnn :: Natural -> AllAnnotation
starAnn n = zero `ann` IR.VStar n

allSig :: Natural -> AllTerm
allSig n = IR.Sig (Usage.SNat 0) (IR.Star n) (IR.Elim (IR.Bound 0))

addTyT' :: NatTermT
addTyT' = IR.Pi Usage.SAny natTK $ IR.Pi Usage.SAny natTK $ natTK

addTy :: NatValueT
addTy = IR.VPi Usage.SAny natT $ IR.VPi Usage.SAny natT $ natT

typGlobals :: Typed.GlobalsT IR.T IR.T Unit.Ty Unit.Val
typGlobals =
  Map.fromList
    [ ("A", Core.GAbstract (Core.Abstract "A" Core.GZero (IR.VStar 0))),
      ( "F",
        Core.GAbstract
          ( Core.Abstract
              "F"
              Core.GZero
              (IR.VPi mempty (IR.VStar 1) (IR.VStar 1))
          )
      ),
      def "ty" Core.GZero (IR.VStar 1) (IR.Star 0),
      def
        "B"
        Core.GZero
        (IR.VStar 0)
        (IR.Elim (IR.Free (Core.Global "A"))),
      def
        "C"
        Core.GZero
        (IR.VStar 0)
        (IR.Elim (IR.Free (Core.Global "B")))
    ]
  where
    def name π ty rhs =
      ( name,
        Core.GFunction $
          Core.Function
            { funName = name,
              funUsage = π,
              funType = ty,
              funClauses = [Core.FunClause [] [] rhs Nothing False Nothing]
            }
      )

aTerm :: Core.Term IR.T primTy primVal
aTerm = IR.Elim aElim

aElim :: IR.Elim primTy primVal
aElim = IR.Free (Core.Global "A")

fTerm :: Core.Term IR.T primTy primVal
fTerm = IR.Elim fElim

fElim :: IR.Elim primTy primVal
fElim = IR.Free (Core.Global "F")

faElim :: IR.Elim primTy primVal
faElim = fElim `IR.App` aTerm

nat :: Natural -> Core.Term IR.T primTy Nat.Val
nat = IR.Prim . Nat.Val

nat' :: Natural -> Core.Term IR.T primTy (TypedPrim Nat.Ty Nat.Val)
nat' n = IR.Prim $ App.Return {retTerm = Nat.Val n, retType = [Nat.Ty]}

natVT :: Natural -> Typed.ValueT IR.T Nat.Ty Nat.Val
natVT n = IR.VPrim $ App.Return {retTerm = Nat.Val n, retType = [Nat.Ty]}

natT' :: Core.Term IR.T Nat.Ty primVal
natT' = IR.PrimTy Nat.Ty

natTK :: TermT Nat.Ty primVal
natTK = IR.PrimTy $ App.Return {retTerm = Nat.Ty, retType = [P.STAR]}

natT :: Typed.ValueT IR.T Nat.Ty primVal
natT = IR.VPrimTy $ App.Return {retTerm = Nat.Ty, retType = [P.STAR]}

natTAll :: Typed.ValueT IR.T All.Ty primVal
natTAll = IR.VPrimTy $ App.Return {retTerm = All.NatTy Nat.Ty, retType = [P.STAR]}

unitT :: Typed.ValueT IR.T Unit.Ty primVal
unitT = IR.VPrimTy $ App.Return {retTerm = Unit.Ty, retType = [P.STAR]}

unitTAll :: Typed.ValueT IR.T All.Ty primVal
unitTAll =
  IR.VPrimTy $
    App.Return {retTerm = All.UnitTy Unit.Ty, retType = [P.STAR]}
