module Juvix.Pipeline.ToHR.Term (transformTermHR) where

import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import Juvix.Pipeline.ToHR.Env
import qualified Juvix.Pipeline.ToHR.Sig.Extract as Sig
import Juvix.Pipeline.ToHR.Types
import Juvix.Pipeline.ToHR.Usage
import qualified Juvix.Sexp as Sexp
import Prelude (error)

-- | Transform S-expression form into Human Readable form
-- N.B. doesn't deal with pattern variables since HR doesn't have them.
-- 'transformTermIR' does that.
transformTermHR ::
  (Show primTy, Show primVal, ReduceEff HR.T primTy primVal m) =>
  NameSymbol.Mod ->
  Sexp.T ->
  m (HR.Term primTy primVal)
transformTermHR _ (Sexp.Atom a@Sexp.N {}) =
  HR.Prim <$> Sig.getParamConstant a
transformTermHR q (Sexp.Atom Sexp.A {atomName}) = do
  term <- Sig.lookupSigWithSymbol (Just q) atomName
  pure $ toName term
  where
    toName = HR.Elim . HR.Var . maybe atomName fst
transformTermHR q p@(name Sexp.:> form)
  -- Unimplemented cases
  -- 1. _refinement_
  --    - TODO :: the name will only become relevant (outside of arrows)
  --              when refinements are supported
  -- 2. _universe names_
  --    - TODO :: for universe polymorphism
  -- 3. _declaim_
  --    - TODO :: We ignore the declration currently
  | named ":record-no-pun" = do
    throwFF $ RecordUnimplemented p
  | named ":refinement" = throwFF $ RefinementsUnimplemented p
  | named ":let-type" = throwFF $ ExprUnimplemented p
  | named ":list" = throwFF $ ListUnimplemented p
  | named "case" = do
    --   throwFF $ ExprUnimplemented p
    transformApplication q p
  | named ":u" = throwFF $ UniversesUnimplemented p
  -- Rest
  | named ":custom-arrow" = transformArrow form
  | named ":let-match" = transformSimpleLet form
  | named ":primitive" = transformPrim form
  | named ":declaim" = transformTermHR q (Sexp.cadr form) -- skip declar
  | named ":lambda" = transformSimpleLambda form
  | named ":progn" = transformTermHR q (Sexp.car form)
  | named ":paren" = transformTermHR q (Sexp.car form)
  | named ":tuple",
    Just xs <- Sexp.toList form =
    makeTuple <$> traverse (transformTermHR q) xs
  | otherwise = do
    transformApplication q p
  where
    named = Sexp.isAtomNamed name

    makeTuple :: [HR.Term primTy primVal] -> HR.Term primTy primVal
    makeTuple [] = HR.Unit
    makeTuple [t] = t
    makeTuple (t : ts) = HR.Pair t (makeTuple ts)
    transformArrow _f@(Sexp.List [π, xa, b]) = go π "" xa b
      where
        -- we never generate this form, so we should be able to erase this!?
        -- FE.NamedTypeE (FE.NamedType' x a) -> go π (getName x) a b

        go π x a b =
          HR.Pi <$> transformUsage q π
            <*> pure x
            <*> transformTermHR q a
            <*> transformTermHR q b
    transformArrow _ = error "malformed custom arrow"

    transformSimpleLet e@(Sexp.List [name, Sexp.List [arg, cbody], body])
      | Just Sexp.A {atomName} <- Sexp.atomFromT name,
        Just xs <- Sexp.toList arg = do
        args <- traverse parseVarArg xs
        cbody <- transformTermHR q cbody
        rhs <- toElim (Sexp.Cons (Sexp.atom ":let-match=") e) $ foldr HR.Lam cbody args
        HR.Let Usage.SAny atomName rhs <$> transformTermHR q body
    transformSimpleLet (Sexp.List [_name, fun, _body]) =
      throwFF $ ExprUnimplemented fun
    transformSimpleLet _ = error "malformed let"

    transformSimpleLambda (Sexp.List [args, body])
      | Just pats <- Sexp.toList args >>= NonEmpty.nonEmpty =
        foldr HR.Lam <$> transformTermHR q body <*> traverse parseVarPat pats
    -- TODO: Avoid throwing error
    transformSimpleLambda _ = error "malformed lambda"
    transformPrim (Sexp.List [parm])
      | Just Sexp.A {atomName = p} <- Sexp.atomFromT parm = do
        param <- ask @"param"
        maybe (throwFF $ UnknownPrimitive p) pure $
          primTy param p <|> primVal param p
      where
        primTy param p = HR.PrimTy <$> HM.lookup p (P.builtinTypes param)
        primVal param p = HR.Prim <$> HM.lookup p (P.builtinValues param)
    -- TODO: Avoid throwing error
    transformPrim _ = error "malformed prim"
transformTermHR _ Sexp.Nil = error "malformed term HR"

pattern NamedArgTerm ::
  Symbol -> HR.Term primTy primVal -> HR.Term primTy primVal
pattern NamedArgTerm x ty <-
  HR.Elim (HR.Ann _ (HR.Elim (HR.Var (x :| []))) ty _)

-- TODO
transformApplication ::
  ( Show primVal,
    Show primTy,
    ReduceEff HR.T primTy primVal m
  ) =>
  NameSymbol.Mod ->
  Sexp.T ->
  m (HR.Term primTy primVal)
transformApplication q a@(f Sexp.:> args)
  | Just xs <- Sexp.toList args = do
    mSig <- Sig.getSpecialSig q f
    go mSig xs
  where
    go Nothing xs = do
      f' <- toElim f =<< transformTermHR q f
      HR.Elim . foldl HR.App f' <$> traverse (transformTermHR q) xs
    go (Just s) xs = case s of
      ArrowS Nothing -> do
        ~[π, a, b] <- nargs s 3 xs
        π <- transformUsage q π
        go (Just (ArrowS (Just π))) [a, b]
      ArrowS (Just π) -> do
        ~[xa, b] <- nargs s 2 xs
        (x, a) <- namedArg q xa
        HR.Pi π x a <$> transformTermHR q b
      PairS Nothing -> do
        ~[π, a, b] <- nargs s 3 xs
        π <- transformUsage q π
        go (Just (PairS (Just π))) [a, b]
      PairS (Just π) -> do
        ~[xa, b] <- nargs s 2 xs
        (x, a) <- namedArg q xa
        HR.Sig π x a <$> transformTermHR q b
      CatProductS -> do
        ~[a, b] <- nargs s 2 xs
        a <- transformTermHR q a
        b <- transformTermHR q b
        pure $ HR.CatProduct a b
      CatCoproductS -> do
        ~[a, b] <- nargs s 2 xs
        a <- transformTermHR q a
        b <- transformTermHR q b
        pure $ HR.CatCoproduct a b
      CatProductIntroS -> do
        ~[a, b] <- nargs s 2 xs
        a <- transformTermHR q a
        b <- transformTermHR q b
        pure $ HR.CatProductIntro a b
      CatProductElimLeftS -> do
        ~[a] <- nargs s 1 xs
        a <- transformTermHR q a
        pure $ HR.CatProductElimLeft a
      CatProductElimRightS -> do
        ~[a] <- nargs s 1 xs
        a <- transformTermHR q a
        pure $ HR.CatProductElimRight a
      CatCoproductIntroLeftS -> do
        ~[a] <- nargs s 1 xs
        a <- transformTermHR q a
        pure $ HR.CatCoproductIntroLeft a
      CatCoproductIntroRightS -> do
        ~[a] <- nargs s 1 xs
        a <- transformTermHR q a
        pure $ HR.CatCoproductIntroRight a
      CatCoproductElimS -> do
        ~[c, a, b] <- nargs s 3 xs
        c <- transformTermHR q c
        a <- transformTermHR q a
        b <- transformTermHR q b
        pure $ HR.CatCoproductElim c a b
      ColonS -> do
        ~[a, b] <- nargs s 2 xs
        a <- transformTermHR q a
        b <- transformTermHR q b
        -- FIXME metavars for usage & universe
        pure $ HR.Elim $ HR.Ann (Usage.SNat 1) a b 0
      TypeS -> do
        ~[i] <- nargs s 1 xs
        HR.Star <$> transformUniverse i
      SAnyS ->
        throwFF UnexpectedSAny
    nargs s n xs
      | length xs == n = pure xs
      | otherwise = throwFF $ WrongNumberBuiltinArgs s n args

    namedArg q e =
      transformTermHR q e >>= \case
        NamedArgTerm x ty -> pure (NameSymbol.fromSymbol x, ty)
        ty -> pure ("" :| [], ty)
    transformUniverse (Sexp.Atom Sexp.N {atomNum = i}) | i >= 0 = pure $ fromIntegral i
    transformUniverse e = throwFF $ NotAUniverse e
transformApplication _ _ = error "malformed application"

-- | Unwrap Term
toElim ::
  HasThrowFF ext primTy primVal m =>
  -- | the original expression
  Sexp.T ->
  HR.Term primTy primVal ->
  m (HR.Elim primTy primVal)
toElim _ (HR.Elim e) = pure e
toElim e _ = throwFF $ NotAnElim e

-- | Check whether the S-expression form is a non-implicit string atom
parseVarArg ::
  HasThrowFF ext primTy primVal m =>
  Sexp.T ->
  m NameSymbol.T
parseVarArg p@(name Sexp.:> _rest)
  | Sexp.isAtomNamed name ":implicit-a" =
    throwFF $ PatternUnimplemented p
parseVarArg p = parseVarPat p

-- | Check whether the S-expression form is a string atom (i.e. not a number)
-- and return its name
parseVarPat ::
  HasThrowFF ext primTy primVal m =>
  Sexp.T ->
  m NameSymbol.T
parseVarPat p
  | Just Sexp.A {atomName} <- Sexp.atomFromT p =
    pure atomName
parseVarPat p =
  throwFF $ PatternUnimplemented p
