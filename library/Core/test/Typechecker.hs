{-# LANGUAGE OverloadedLists #-}

-- | Tests for the type checker and evaluator in Core/IR/Typechecker.hs
module Typechecker where

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

type NatTerm = Core.Term IR.T Nat.Ty Nat.Val

type NatElim = IR.Elim Nat.Ty Nat.Val

type NatValue = Core.Value IR.T Nat.Ty Nat.Val

type NatValueT = Typed.ValueT IR.T Nat.Ty Nat.Val

type NatAnnotation = Typed.AnnotationT IR.T Nat.Ty Nat.Val

type UnitTerm = Core.Term IR.T Unit.Ty Unit.Val

type UnitElim = IR.Elim Unit.Ty Unit.Val

type UnitValue = Core.Value IR.T Unit.Ty Unit.Val

type UnitValueT = Typed.ValueT IR.T Unit.Ty Unit.Val

type UnitAnnotation = Typed.AnnotationT IR.T Unit.Ty Unit.Val

type AllTerm = Core.Term IR.T All.Ty All.Val

type AllElim = IR.Elim All.Ty All.Val

type AllValue = Core.Value IR.T All.Ty All.Val

type AllValueT = Typed.ValueT IR.T All.Ty All.Val

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

assertIsRight :: (HasCallStack, Show a, Show b) => Either a b -> T.Assertion
assertIsRight = assertEitherIsAsExpected True

assertIsLeft :: (HasCallStack, Show a, Show b) => Either a b -> T.Assertion
assertIsLeft = assertEitherIsAsExpected False

-- unit test generator for typeTerm
assertCheckResultWith ::
  ( HasCallStack,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    CanApply (TypedPrim primTy primVal),
    CanApply primTy,
    Eq (Arg primTy),
    Show (Arg primTy),
    Eq (Arg (TypedPrim primTy primVal)),
    Show (Arg (TypedPrim primTy primVal)),
    Eq (ApplyErrorExtra primTy),
    Show (ApplyErrorExtra primTy),
    Eq (ApplyErrorExtra (TypedPrim primTy primVal)),
    Show (ApplyErrorExtra (TypedPrim primTy primVal)),
    Typed.PrimSubstValue primTy primVal,
    Typed.PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal,
    Eval.HasPatSubstTerm
      (OnlyExts.T Typed.T)
      primTy
      (TypedPrim primTy primVal)
      primTy
  ) =>
  Bool ->
  Parameterisation primTy primVal ->
  Typed.GlobalsT IR.T IR.T primTy primVal ->
  Typed.Context primTy primVal ->
  Core.Term IR.T primTy primVal ->
  Typed.AnnotationT IR.T primTy primVal ->
  T.TestTree
assertCheckResultWith expectSuccess param globals ctx term ann =
  -- TODO: take out the logs and put them in an IO monad.
  let (res, _) = Typed.exec globals $ TC.typeTermWith param mempty ctx term ann
   in T.testCase
        ( show term
            <> " should "
            <> (if expectSuccess then "check" else "fail")
            <> " as type "
            <> show ann
        )
        $ assertEitherIsAsExpected expectSuccess res

-- unit test generator for typeTerm
shouldCheckWith ::
  ( HasCallStack,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    CanApply (TypedPrim primTy primVal),
    CanApply primTy,
    Eq (Arg primTy),
    Show (Arg primTy),
    Eq (Arg (TypedPrim primTy primVal)),
    Show (Arg (TypedPrim primTy primVal)),
    Eq (ApplyErrorExtra primTy),
    Show (ApplyErrorExtra primTy),
    Eq (ApplyErrorExtra (TypedPrim primTy primVal)),
    Show (ApplyErrorExtra (TypedPrim primTy primVal)),
    Typed.PrimSubstValue primTy primVal,
    Typed.PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal,
    Eval.HasPatSubstTerm
      (OnlyExts.T Typed.T)
      primTy
      (TypedPrim primTy primVal)
      primTy
  ) =>
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
    CanApply (TypedPrim primTy primVal),
    CanApply primTy,
    Eq (Arg primTy),
    Show (Arg primTy),
    Eq (Arg (TypedPrim primTy primVal)),
    Show (Arg (TypedPrim primTy primVal)),
    Eq (ApplyErrorExtra primTy),
    Show (ApplyErrorExtra primTy),
    Eq (ApplyErrorExtra (TypedPrim primTy primVal)),
    Show (ApplyErrorExtra (TypedPrim primTy primVal)),
    Typed.PrimSubstValue primTy primVal,
    Typed.PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal,
    Eval.HasPatSubstTerm
      (OnlyExts.T Typed.T)
      primTy
      (TypedPrim primTy primVal)
      primTy
  ) =>
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
    CanApply (TypedPrim primTy primVal),
    CanApply primTy,
    Eq (Arg primTy),
    Show (Arg primTy),
    Eq (Arg (TypedPrim primTy primVal)),
    Show (Arg (TypedPrim primTy primVal)),
    Eq (ApplyErrorExtra primTy),
    Show (ApplyErrorExtra primTy),
    Eq (ApplyErrorExtra (TypedPrim primTy primVal)),
    Show (ApplyErrorExtra (TypedPrim primTy primVal)),
    Typed.PrimSubstValue primTy primVal,
    Typed.PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal,
    Eval.HasPatSubstTerm
      (OnlyExts.T Typed.T)
      primTy
      (TypedPrim primTy primVal)
      primTy
  ) =>
  Bool ->
  Parameterisation primTy primVal ->
  Core.Term IR.T primTy primVal ->
  Typed.AnnotationT IR.T primTy primVal ->
  T.TestTree
assertCheckResult expectSuccess param =
  assertCheckResultWith expectSuccess param mempty []

shouldCheck ::
  ( HasCallStack,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    CanApply (TypedPrim primTy primVal),
    CanApply primTy,
    Eq (Arg primTy),
    Show (Arg primTy),
    Eq (Arg (TypedPrim primTy primVal)),
    Show (Arg (TypedPrim primTy primVal)),
    Eq (ApplyErrorExtra primTy),
    Show (ApplyErrorExtra primTy),
    Eq (ApplyErrorExtra (TypedPrim primTy primVal)),
    Show (ApplyErrorExtra (TypedPrim primTy primVal)),
    Typed.PrimSubstValue primTy primVal,
    Typed.PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal,
    Eval.HasPatSubstTerm
      (OnlyExts.T Typed.T)
      primTy
      (TypedPrim primTy primVal)
      primTy
  ) =>
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
    CanApply (TypedPrim primTy primVal),
    CanApply primTy,
    Eq (Arg primTy),
    Show (Arg primTy),
    Eq (Arg (TypedPrim primTy primVal)),
    Show (Arg (TypedPrim primTy primVal)),
    Eq (ApplyErrorExtra primTy),
    Show (ApplyErrorExtra primTy),
    Eq (ApplyErrorExtra (TypedPrim primTy primVal)),
    Show (ApplyErrorExtra (TypedPrim primTy primVal)),
    Typed.PrimSubstValue primTy primVal,
    Typed.PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal,
    Eval.HasPatSubstTerm
      (OnlyExts.T Typed.T)
      primTy
      (TypedPrim primTy primVal)
      primTy
  ) =>
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
    CanApply (TypedPrim primTy primVal),
    CanApply primTy,
    Eq (Arg primTy),
    Show (Arg primTy),
    Eq (Arg (TypedPrim primTy primVal)),
    Show (Arg (TypedPrim primTy primVal)),
    Eq (ApplyErrorExtra primTy),
    Show (ApplyErrorExtra primTy),
    Eq (ApplyErrorExtra (TypedPrim primTy primVal)),
    Show (ApplyErrorExtra (TypedPrim primTy primVal)),
    Typed.PrimSubstValue primTy primVal,
    Typed.PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal,
    Eval.HasPatSubstTerm
      (OnlyExts.T Typed.T)
      primTy
      (TypedPrim primTy primVal)
      primTy
  ) =>
  Parameterisation primTy primVal ->
  Typed.GlobalsT IR.T IR.T primTy primVal ->
  Typed.Context primTy primVal ->
  IR.Elim primTy primVal ->
  Typed.AnnotationT IR.T primTy primVal ->
  T.TestTree
shouldInferWith param globals ctx elim ann@(Typed.Annotation {annUsage = σ}) =
  let (res, _) = Typed.exec globals $ TC.typeElimWith param mempty ctx elim σ
      resTy = Typed.getElimAnn . TC.loValue <$> res
   in T.testCase (show term <> " should infer to type " <> show ann) $
        resTy T.@?= Right ann

shouldInfer ::
  ( HasCallStack,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    CanApply (TypedPrim primTy primVal),
    CanApply primTy,
    Eq (Arg primTy),
    Show (Arg primTy),
    Eq (Arg (TypedPrim primTy primVal)),
    Show (Arg (TypedPrim primTy primVal)),
    Eq (ApplyErrorExtra primTy),
    Show (ApplyErrorExtra primTy),
    Eq (ApplyErrorExtra (TypedPrim primTy primVal)),
    Show (ApplyErrorExtra (TypedPrim primTy primVal)),
    Typed.PrimSubstValue primTy primVal,
    Typed.PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal,
    Eval.HasPatSubstTerm
      (OnlyExts.T Typed.T)
      primTy
      (TypedPrim primTy primVal)
      primTy
  ) =>
  Parameterisation primTy primVal ->
  IR.Elim primTy primVal ->
  Typed.AnnotationT IR.T primTy primVal ->
  T.TestTree
shouldInfer param = shouldInferWith param mempty []

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
    Eval.HasPatSubstTerm (OnlyExts.T IR.T) primTy primVal primTy,
    Eval.HasPatSubstTerm (OnlyExts.T IR.T) primTy primVal primVal,
    Eval.HasSubstValue IR.T primTy primVal primTy,
    Eval.HasSubstValue IR.T primTy primVal primVal,
    Eval.HasWeak primVal
  ) =>
  Core.Globals IR.T IR.T primTy primVal ->
  Core.Term IR.T primTy primVal ->
  Core.Value IR.T primTy primVal ->
  T.TestTree
shouldEval' g term res =
  T.testCase (show term <> " should evaluate to " <> show res) $
    (IR.evalTerm (IR.lookupFun' g) term) T.@=? Right res

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
    Eval.HasPatSubstTerm (OnlyExts.T IR.T) primTy primVal primTy,
    Eval.HasPatSubstTerm (OnlyExts.T IR.T) primTy primVal primVal,
    Eval.HasSubstValue IR.T primTy primVal primTy,
    Eval.HasSubstValue IR.T primTy primVal primVal,
    Eval.HasWeak primVal
  ) =>
  Core.Term IR.T primTy primVal ->
  Core.Value IR.T primTy primVal ->
  T.TestTree
shouldEval = shouldEval' mempty

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
    [ shouldCheck Nat.t identity identityNatCompTy,
      shouldCheck Unit.t identity identityUnitCompTy,
      shouldCheck Nat.t identityApplication natTy,
      shouldInfer Nat.t identityAppINat1 natTy,
      shouldInfer Nat.t identityAppI identityNatCompTy,
      shouldCheck Nat.t kcombinator kCompTy,
      shouldCheck All.t kcombinator kCompTyWithUnit,
      shouldInfer Nat.t identityAppK kCompTy,
      shouldCheck Nat.t (IR.Elim kAppI) kAppICompTy,
      shouldCheck Nat.t (IR.Elim kAppINotAnnotated) kAppICompTy,
      shouldInfer Nat.t kApp1 natToNatTy,
      shouldInfer
        Nat.t
        kFunApp1
        kFunApp1CompTy
    ]

natComp :: T.TestTree
natComp =
  T.testGroup
    "Nat Computational typing"
    [ shouldCheck Nat.t natT' (mempty `ann` IR.VStar 0),
      shouldCheck Nat.t (nat 1) (Usage.Omega `ann` natT),
      shouldCheck Nat.t (IR.Prim Nat.Add) (Usage.Omega `ann` addTy),
      shouldFail Nat.t (IR.Prim Nat.Add) (Usage.Omega `ann` natT)
    ]

dependentFunctionComp :: T.TestTree
dependentFunctionComp =
  T.testGroup
    "Dependent Functions Computational typing"
    [ shouldCheck
        All.t
        depIdentity
        depIdentityCompTy,
      shouldCheck
        All.t
        depIdentity'
        depIdentityCompTy,
      shouldCheck
        All.t
        depIdentity
        depIdentityCompTyOmega,
      shouldCheck
        All.t
        depK
        depKCompTy,
      shouldCheck
        All.t
        depIdentityCompTyT
        (mempty `ann` IR.VStar 1),
      shouldCheck
        All.t
        depKCompTyT
        (mempty `ann` IR.VStar 1)
    ]

letComp :: T.TestTree
letComp =
  T.testGroup
    "'let' Computational typing"
    [ -- let 0 x = 0 in 0
      shouldCheck
        Nat.t
        (IR.Let mempty nzero (IR.Elim nzero))
        (Usage.Omega `ann` natT),
      -- let ω x = 0 in x
      shouldCheck
        Nat.t
        (IR.Let Usage.Omega nzero (IR.Elim (IR.Bound 0)))
        (Usage.Omega `ann` natT),
      -- λx. let 0 y = 0 in x
      shouldCheck
        Nat.t
        (IR.Lam (IR.Let mempty nzero (IR.Elim (IR.Bound 1))))
        (natToNatTy' one)
    ]
  where
    nzero = IR.Ann Usage.Omega (nat 0) natT' 0

evaluations :: T.TestTree
evaluations =
  T.testGroup
    "Evaluations"
    [ shouldEval add12 (natV 3),
      shouldEval sub52 (natV 3),
      shouldEval identityApplication (natV 1),
      shouldEval (IR.Elim identityAppINat1) (natV 1),
      shouldEval (IR.Elim identityAppI) videntity,
      shouldEval (IR.Elim kApp1_2) (natV 1),
      shouldEval' typGlobals (IR.Elim (IR.Free (Core.Global "ty"))) (IR.VStar 0),
      shouldEval' typGlobals (name "tz") (vname "tz"),
      shouldEval' typGlobals (name "B") (vname "A"),
      shouldEval' typGlobals (name "C") (vname "A")
    ]
  where
    add12 = IR.Elim $ add `IR.App` nat 1 `IR.App` nat 2
    sub52 = IR.Elim $ sub `IR.App` nat 5 `IR.App` nat 2
    sub = IR.Ann Usage.Omega (IR.Prim Nat.Sub) addTyT 0
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
      shouldInlineE "F B"
        (name "F" `IR.App` tname "B") (name "F" `IR.App` tname "A"),
      shouldInlineT "*₀" (IR.Star 0) (IR.Star 0)
    ]
  where
    name = IR.Free . Core.Global
    tname = IR.Elim . name
    shouldInlineE lbl e e' = T.testCase lbl $
      IR.inlineAllGlobalsElim e look mempty T.@?= e' -- No patterns, so mempty.
    shouldInlineT lbl t t' = T.testCase lbl $
      IR.inlineAllGlobals t look mempty T.@?= t' -- No pattersn, so memty.

    look :: Core.GlobalName -> Maybe (IR.Elim Unit.Ty Unit.Val)
    look "B" = Just $ name "A"
    look "C" = Just $ name "B"
    look _   = Nothing

skiCont :: T.TestTree
skiCont =
  T.testGroup
    "SKI combinators contemplational typing"
    [ shouldCheck Nat.t identity identityNatContTy
    ]

subtype :: T.TestTree
subtype =
  T.testGroup
    "Subtyping"
    [ shouldCheckWith Unit.t typGlobals [] aTerm $ mempty `ann` IR.VStar 0,
      shouldCheckWith Unit.t typGlobals [] aTerm $ mempty `ann` IR.VStar 1,
      shouldCheckWith Unit.t typGlobals [] fTerm $ mempty `ann` typ2typ 1 1,
      shouldCheckWith Unit.t typGlobals [] fTerm $ mempty `ann` typ2typ 0 1,
      shouldCheckWith Unit.t typGlobals [] fTerm $ mempty `ann` typ2typ 1 2,
      shouldFailWith Unit.t typGlobals [] aTerm $ mempty `ann` typ2typ 1 2,
      shouldInferWith Unit.t typGlobals [] faElim $ mempty `ann` IR.VStar 1
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
depIdentityCompTyOmega :: AllAnnotation
depIdentityCompTyOmega =
  one
    `ann` IR.VPi
      mempty
      (IR.VStar 0)
      (IR.VPi Usage.Omega (Core.VBound 0) (Core.VBound 1))

-- \x.x 1
identityApplication :: NatTerm
identityApplication =
  IR.Elim
    ( IR.App -- Applying
        ( IR.Ann -- the function that has annotation of
            one -- usage 1
            identity -- the identity function,
            -- which has annotation (1, 1 Nat -> Nat)
            (IR.Pi one natT' natT')
            -- type of 1 Nat -> Nat
            0
        )
        (IR.Prim (Nat.Val 1)) -- applies to 1
    )

-- computation annotation (1, Nat)
natTy :: NatAnnotation
natTy = one `ann` natT

-- (I:1 (1 Nat->Nat) -> (1 Nat->Nat) I:(1 Nat->Nat) ) 1 type checked to Nat.Ty
identityAppINat1 :: NatElim
identityAppINat1 =
  IR.App -- applying (identity to identity) to 1
    ( IR.App -- applying identity to identity
        ( IR.Ann
            one -- sig usage, the first 1 in the annotation
            identity -- has annotation (1, 1 ((1 Nat -> Nat)) -> (1 Nat -> Nat))
            ( IR.Pi
                one -- the second 1 in the annotation
                (IR.Pi one natT' natT')
                -- the third 1 in the annotation, (1 Nat -> Nat)
                (IR.Pi one natT' natT')
                -- the forth 1 in the annotation, (1 Nat -> Nat)
            )
            0
        )
        ( IR.Elim
            ( IR.Ann
                one -- sig usage, the first 1 in the annotation
                identity -- has annotation (1, 1 Nat -> Nat)
                ( IR.Pi
                    one -- the second 1 in the annotation
                    natT' -- 1 Nat ->
                    natT' -- Nat
                )
                0
            )
        )
    )
    (IR.Prim (Nat.Val 1))

-- I:(Nat->Nat)->(Nat->Nat) I:(Nat->Nat) type checked to (Nat->Nat)
-- I:(Nat->Nat) I:(Nat->Nat) correctly does not type checked
identityAppI :: NatElim
identityAppI =
  IR.App -- applying identity to identity
    ( IR.Ann
        one -- sig usage, the first 1 in the annotation
        identity -- has annotation (1, (1, (1 Nat -> Nat) ) -> (1 Nat -> Nat) ) )
        ( IR.Pi
            one -- the second 1 in the annotation
            (IR.Pi one natT' natT')
            -- the third 1 in the annotation 1 Nat -> Nat
            (IR.Pi one natT' natT')
            -- the forth 1 in the annotation 1 Nat -> Nat
        )
        0
    )
    ( IR.Elim
        ( IR.Ann
            one -- sig usage, the first 1 of the annotation
            identity -- annotation (1, 1 Nat -> Nat)
            (IR.Pi one natT' natT')
            -- the second 1 of the annotation, 1 Nat -> Nat
            0
        )
    )

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
  one
    `ann` IR.VPi -- sig usage of k
    -- first input, 1 Nat
      one -- is used once in the output
      (IR.VPrimTy (All.NatTy Nat.Ty)) -- of type Nat
      ( IR.VPi -- second input, 0 Unit
          mempty -- is not used in the output
          (IR.VPrimTy (All.UnitTy Unit.Ty)) -- of type Unit
          (IR.VPrimTy (All.NatTy Nat.Ty))
          -- the output is of type Nat
      )

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

kApp1_2 :: NatElim
kApp1_2 = kApp1 `IR.App` nat 2

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

-- S combinator: Sxyz = xz(yz)
-- Because S returns functions, it's not general because of the annotations.
-- For example, S KSK = KK (SK) = K:Nat-> Nat-> Nat
-- this S takes in KSK, and has x and y annotated as follows:
-- (x = K that takes inputs
--     (1) K, with type signature of z, and
--     (2) SK, the S takes in K and 2 Nats, and has the signature (Nat -> Nat -> Nat) -> Nat -> Nat -> Nat,
--             the K has the type signature of z. So SK has the signature of Nat -> Nat -> Nat
-- so x has the signature of (Nat -> Nat -> Nat) -> (Nat -> Nat -> Nat) -> (Nat -> Nat -> Nat)
-- (y = S that takes in K and 2 Nats and returns a Nat:) (Nat -> Nat-> Nat) -> Nat -> Nat -> Nat
-- (z = K:) Nat -> Nat -> Nat
-- (returns z) -> Nat -> Nat -> Nat
-- To sum, type signature of S in this example is:
-- ((Nat -> Nat -> Nat) -> (Nat -> Nat -> Nat) -> (Nat -> Nat -> Nat)) ->
-- ((Nat -> Nat -> Nat) -> Nat -> Nat -> Nat)
-- (Nat -> Nat -> Nat)
-- this example is too long, not doing this atm

-- example of s combinator with the following signature:
-- the first has type signature of 1 Nat -> 0 Nat -> Nat
-- the second input has type signature 1 Nat -> Nat
-- the third input is Nat
-- type signature of this S is 1 (1 Nat -> 0 Nat -> Nat) -> 1 (1 Nat -> Nat) -> 2 Nat -> Nat
scombinator :: NatTerm -- S = \x.\y.\z. (xz) (yz)
scombinator =
  IR.Lam --x/first input (Bound 2, counting from output)
    ( IR.Lam --y/second input (Bound 1, counting from output)
        ( IR.Lam --z/third input (Bound 0, counting from output)
            ( IR.Elim
                ( IR.App -- xz applies to yz
                    ( IR.Ann
                        one
                        ( IR.Elim
                            ( IR.App -- x applies to z
                                ( IR.Ann
                                    one -- usage of x
                                    (IR.Elim (IR.Bound 2)) -- x
                                    ( IR.Pi
                                        one
                                        natT'
                                        (IR.Pi mempty natT' natT') -- Annotation of x: (1 Nat -> 0 Nat -> Nat)
                                    )
                                    0
                                )
                                (IR.Elim (IR.Bound 0)) -- z
                            )
                        )
                        ( IR.Pi -- Annotation of xz: 0 Nat -> Nat
                            mempty
                            natT'
                            natT'
                        )
                        0
                    )
                    ( IR.Elim
                        ( IR.App -- y applies to z
                            ( IR.Ann
                                one -- usage of y
                                (IR.Elim (IR.Bound 1)) -- y
                                (IR.Pi one natT' natT') -- Annotation of y, (1 Nat -> Nat)
                                0
                            )
                            (IR.Elim (IR.Bound 0)) -- z
                        )
                    )
                )
            )
        )
    )

-- S xyz = (xz) (yz)
-- computation annotation of S (1, 1 (1 Nat -> 0 Nat -> Nat) -> 1 (1 Nat -> Nat) -> 2 Nat -> Nat )
scombinatorCompNatTy :: NatAnnotation
scombinatorCompNatTy =
  one
    `ann` IR.VPi -- sig usage of S
    --
      one -- usage of 1 (1 Nat -> 0 Nat -> Nat)
      ( IR.VPi
          one
          natT -- (1 Nat ->
          ( IR.VPi
              mempty
              natT -- 0 Nat ->
              natT -- Nat) ->
          )
      )
      ( IR.VPi -- second input, (1 Nat -> Nat)
          one -- usage of (1 Nat -> Nat)
          ( IR.VPi
              one
              natT --(1 Nat ->
              natT -- Nat) ->
          )
          ( IR.VPi
              Usage.Omega
              natT -- w Nat ->
              natT -- Nat
          )
      )

-- K 1 (I 1) = 1, so should type checked to (1, Nat)
ski1CompNatTy :: NatAnnotation
ski1CompNatTy = one `ann` natT

dependentPairComp :: T.TestTree
dependentPairComp =
  T.testGroup
    "Dependent pair typing"
    [ shouldCheck Nat.t boxNat boxNatAnn,
      shouldCheck All.t unitTypeUnitValuePair allAnn,
      shouldFail All.t unitTypeNatValuePair allAnn,
      shouldFail All.t natTypeUnitValuePair allAnn,
      shouldCheck All.t natTypeNatValuePair allAnn,
      shouldCheck All.t (allSig 0) (starAnn 1)
    ]

twoNatsAnn :: NatAnnotation
twoNatsAnn = one `ann` IR.VSig one natT natT

twoNats :: NatTerm
twoNats = IR.Pair (nat 0) (nat 1)

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

add :: NatElim
add = IR.Ann Usage.Omega (IR.Prim Nat.Add) addTyT 0

addTyT :: NatTerm
addTyT = IR.Pi Usage.Omega natT' $ IR.Pi Usage.Omega natT' $ natT'

addTy :: NatValueT
addTy = IR.VPi Usage.Omega natT $ IR.VPi Usage.Omega natT $ natT

one' :: forall primTy primVal. Core.Term IR.T primTy primVal
one' = IR.Lam $ IR.Lam $ IR.Elim $ IR.App (IR.Bound 1) (IR.Elim (IR.Bound 0))

oneCompTy :: NatAnnotation
oneCompTy = one `ann` IR.VPi one (IR.VPi one natT natT) (IR.VPi one natT natT)

two :: Core.Term IR.T primTy primVal
two =
  IR.Lam $
    IR.Lam $
      IR.Elim $
        IR.App (IR.Bound 1) (IR.Elim (IR.App (IR.Bound 1) (IR.Elim (IR.Bound 0))))

twoCompTy :: NatAnnotation
twoCompTy = one `ann` IR.VPi two (IR.VPi one natT natT) (IR.VPi one natT natT)
  where
    two = Usage.SNat 2

typGlobals :: Core.Globals IR.T IR.T Unit.Ty (TypedPrim Unit.Ty Unit.Val)
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

faTerm :: Core.Term IR.T primTy primVal
faTerm = IR.Elim faElim

faElim :: IR.Elim primTy primVal
faElim = fElim `IR.App` aTerm

nat :: Natural -> Core.Term IR.T primTy Nat.Val
nat = IR.Prim . Nat.Val

natV :: Natural -> Core.Value IR.T primTy Nat.Val
natV = IR.VPrim . Nat.Val

natT' :: Core.Term IR.T Nat.Ty primVal
natT' = IR.PrimTy Nat.Ty

natT :: Core.Value IR.T Nat.Ty primVal
natT = IR.VPrimTy Nat.Ty

unitT' :: Core.Term IR.T Unit.Ty primVal
unitT' = IR.PrimTy Unit.Ty

unitT :: Core.Value IR.T Unit.Ty primVal
unitT = IR.VPrimTy Unit.Ty

unit :: Core.Term IR.T primTy Unit.Val
unit = IR.Prim Unit.Val

unit' :: Core.Term IR.T Unit.Ty (TypedPrim Unit.Ty Unit.Val)
unit' = IR.Prim (App.Return {retType = P.PrimType [Unit.Ty], retTerm = Unit.Val})
