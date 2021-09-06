{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- The easy module serves as the stop shop for getting anywhere in the
-- code-base fast.
--
-- _The file is laid out where_
--  1. we lay out a phase
--     - We have 2 variants of each phase
--       1) <name>File
--       2) <name>Library
--     - This lasts up until context, as we can see if the prelude we
--       give it matches our expectations
--  2. We then give examples
--
-- We do 1. and 2. having each step rely on the last, and continue the
-- process until the compiler is at the full backends.
--
-- We can view this approach as giving us a quick way to play around
-- with any stage of the compiler while modifying the source code.
module Easy where

import qualified Data.ByteString as BS
import qualified Data.Field.Galois as Field
import qualified Data.HashMap.Strict as HM
import qualified Juvix.Backends.LLVM.Parameterization as LLVM.Param
import qualified Juvix.Backends.LLVM.Pipeline as LLVM
import qualified Juvix.Backends.LLVM.Primitive as LLVM.Prim
import qualified Juvix.Backends.Michelson.Parameterisation as Michelson.Param
import qualified Juvix.Backends.Michelson.Pipeline as Michelson
import qualified Juvix.Backends.Plonk as Plonk
import qualified Juvix.Context as Context
import qualified Juvix.Context.NameSpace as NameSpace
import qualified Juvix.Contextify as Contextify
import qualified Juvix.Contextify.ToContext.ResolveOpenInfo as ResolveOpen
import qualified Juvix.Contextify.ToContext.Types as ContextifyT
import qualified Juvix.Core.Base as Core
import qualified Juvix.Core.Base.TransformExt as TransformExt
import qualified Juvix.Core.Base.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.Common.Context.Traverse as Traverse
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.Parameterisation as Param
import qualified Juvix.Core.Types as Types
import qualified Juvix.Desugar as Desugar
import qualified Juvix.Frontend as Frontend
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Types as FrontendT
import qualified Juvix.Frontend.Types as Initial
import qualified Juvix.Frontend.Types.Base as Frontend
import Juvix.Library
import Juvix.Library.BLS12381 (Fr)
import qualified Juvix.Library.Feedback as Feedback
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymb
import qualified Juvix.Library.Usage as Usage
import qualified Juvix.Pipeline as Pipeline
import qualified Juvix.Pipeline.Compile as Compile
import qualified Juvix.Pipeline.ToHR as ToHR
import qualified Juvix.Pipeline.ToIR as ToIR
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Translate.Pipeline.TopLevel as SexpTrans
import qualified Text.Pretty.Simple as Pretty
import Prelude (error)
import qualified Prelude (Show (..))

--------------------------------------------------------------------------------
-- OPTIONS
--------------------------------------------------------------------------------

instance Prelude.Show (Param.Parameterisation primTy primVal) where
  show _ = "param"

data Options primTy primVal = Opt
  { prelude :: [FilePath],
    currentContextName :: NameSymb.T,
    param :: Param.Parameterisation primTy primVal,
    typeAgainst :: primTy
  }
  deriving (Show)

-- we can override defaults by saying def { newOptions }
def :: Options primTy primVal
def =
  Opt
    { -- to avoid being overwhelmed in the repl by giant text, we have
      -- a minimal file here. Our functions will take def, so we can
      -- replace it by the full library
      prelude = ["juvix/minimal.ju"],
      -- by default our code will live in Juvix-User
      currentContextName = "Juvix-User",
      param = undefined,
      typeAgainst = undefined
    }

-- @defMichelson@ gives us Michelson prelude
defMichelson :: Options Michelson.Param.PrimTy Michelson.Param.RawPrimVal
defMichelson =
  def
    { prelude =
        -- TODO: Avoid relative paths
        [ "../../stdlib/Prelude.ju",
          "../../stdlib/Michelson.ju",
          "../../stdlib/MichelsonAlias.ju"
        ],
      param = Michelson.Param.michelson,
      typeAgainst = Michelson.Param.Set
    }

-- @defLLVM@ gives us LLVM prelude
defLLVM :: Options LLVM.Prim.PrimTy LLVM.Prim.RawPrimVal
defLLVM =
  def
    { prelude =
        -- TODO: Avoid relative paths
        [ "../../stdlib/Prelude.ju",
          "../../stdlib/LLVM.ju"
        ],
      param = LLVM.Param.llvm,
      typeAgainst = LLVM.Prim.Set
    }

-- @defCircuit@ gives us the circuit prelude
-- defCircuit :: Options
defCircuitGeneric ::
  Field.GaloisField f =>
  Options (Plonk.PrimTy f) (Plonk.PrimVal f)
defCircuitGeneric =
  def
    { prelude =
        [ "../../stdlib/Prelude.ju",
          "../../stdlib/Circuit.ju"
        ],
      param = Plonk.param,
      typeAgainst = Plonk.PField
    }

defCircuit :: Options (Plonk.PrimTy Fr) (Plonk.PrimVal Fr)
defCircuit = defCircuitGeneric

-- These functions help us stop at various part of the pipeline

----------------------------------------
-- QUICK TIME LAPSE EXAMPLES
----------------------------------------

timeLapse1 :: IO ()
timeLapse1 =
  printTimeLapse "sig foo : int Prelude.-> int let foo x = x" defMichelson

--------------------------------------------------------------------------------
-- SEXP PHASE
--------------------------------------------------------------------------------

-- | here we stop at the first stage of the first step of the compiler
-- You may want to stop here if you want to see what some base forms look like
-- Text ⟶ ML AST ⟶ LISP AST
sexp :: ByteString -> [Sexp.T]
sexp xs = ignoreHeader (Parser.parse xs) >>| SexpTrans.transTopLevel

-- | Here we extend the idea of desugar but we run it on the prelude we
-- care about.
-- File ⟶ ML AST ⟶ LISP AST
sexpFile :: FilePath -> IO [Sexp.T]
sexpFile file = do
  f <- Frontend.parseSingleFile file
  case f of
    Right (_name, ast) ->
      fmap SexpTrans.transTopLevel ast
        |> pure
    Left err ->
      error (show err)

-- | here we run the sexp transformation on the library
-- Prelude ⟶ ML AST ⟶ LISP AST
sexpLibrary :: Options primTy primVal -> IO [(NameSymb.T, [Sexp.T])]
sexpLibrary def = do
  files <- Frontend.parseFiles (prelude def)
  case files of
    Right f ->
      pure (second (fmap SexpTrans.transTopLevel) <$> f)
    Left err ->
      error (show err)

----------------------------------------
-- SEXP Examples
----------------------------------------

-- here are some sexp examples you may want to play with

sexp1, sexp2 :: [Sexp.T]
sexp1 = sexp "type list a : ty -> ty = Cons a (List a) | Nil"
sexp2 =
  sexp
    "let foo (Cons x xs) = x + foo xs\
    \ let foo Nil = 0"

--------------------------------------------------------------------------------
-- DESUGAR PHASE
--------------------------------------------------------------------------------

-- | Here is our second stop of the compiler, we now run the desugar passes
-- you may want to stop here if you want to see the syntax before we
-- get dirtier output from everything being in the context
-- Text ⟶ ML AST ⟶ LISP AST ⟶ De-sugared LISP
desugar :: ByteString -> [Sexp.T]
desugar = Desugar.op . sexp

-- | This is like Desugar but our pipeline looks like
-- LISP AST ⟶ De-sugared LISP
desugarLisp :: [Sexp.T] -> [Sexp.T]
desugarLisp = Desugar.op

-- | Here we extend the idea of desugar but we run it on the file we
-- care about.
-- File ⟶ … ⟶ De-sugared LISP
desugarFile :: FilePath -> IO [Sexp.T]
desugarFile = fmap desugarLisp . sexpFile

-- | @desugarLibrary@ is run on the library to get the s-expression
-- Prelude ⟶ … ⟶ De-sugared LISP
desugarLibrary :: Options primTy primVal -> IO [(NameSymb.T, [Sexp.T])]
desugarLibrary def = do
  lib <- sexpLibrary def
  pure (second desugarLisp <$> lib)

----------------------------------------
-- DESUGAR Examples
----------------------------------------

desugar1, desugar2 :: [Sexp.T]
desugar1 = desugarLisp sexp2
desugar2 =
  desugar
    "let fi = \
    \ let foo (Cons x xs) True = foo xs False in \
    \ let foo (Nil) t = t in \
    \ foo [1,2,3,4]"

-- Example of the minimal prelude if you want to investigate it
desugarMinimalPrelude :: IO [(NameSymb.T, [Sexp.T])]
desugarMinimalPrelude = desugarLibrary def

--------------------------------------------------------------------------------
-- Context Phase
--------------------------------------------------------------------------------

-- | @contextifyGen@ is the generator function for the various contexitfy passes
contextifyGen ::
  (NonEmpty (NameSymb.T, [Sexp.T]) -> IO b) -> ByteString -> Options primTy primVal -> IO b
contextifyGen f text def = do
  lib <- desugarLibrary def
  let dusugared = desugar text
  f ((currentContextName def, dusugared) :| lib)

-- | @contextifyFileGen@ is like @contextifyGen@ but for the file variants
contextifyFileGen ::
  (NonEmpty (NameSymb.T, [Sexp.T]) -> IO b) -> FilePath -> Options primTy primVal -> IO b
contextifyFileGen f file def = do
  lib <- desugarLibrary def
  dusugared <- desugarFile file
  f ((currentContextName def, dusugared) :| lib)

-- | Here is our third stop in the compiler, we are now upon the
-- context. For this phase we'll want some version of the standard
-- library for the steps to come
--
-- You may want to stop here if you want to see the context before
-- resolving the opens and what that may entail
--
-- Text ⟶ ML AST ⟶ LISP AST ⟶ De-sugared LISP ⟶ Contextified LISP, Resolves
contextifyNoResolve ::
  ByteString ->
  Options primTy primVal ->
  IO (Contextify.PathError (ContextifyT.ContextSexp, [ResolveOpen.PreQualified]))
contextifyNoResolve = contextifyGen Contextify.contextify

-- | We do @contextifyNoResolve@ but on a file instead
-- File ⟶ ML AST ⟶ LISP AST ⟶ De-sugared LISP ⟶ Contextified LISP, Resolves
contextifyNoResolveFile ::
  FilePath ->
  Options primTy primVal ->
  IO (Contextify.PathError (ContextifyT.ContextSexp, [ResolveOpen.PreQualified]))
contextifyNoResolveFile = contextifyFileGen Contextify.contextify

----------------------------------------
-- CONTEXTIFY Examples
----------------------------------------

contextifyNoResolve1 ::
  IO (Contextify.PathError (ContextifyT.ContextSexp, [ResolveOpen.PreQualified]))
contextifyNoResolve1 =
  contextifyNoResolve
    "let fi = \
    \ let foo (Cons x xs) True = foo xs False in \
    \ let foo (Nil) t = t in \
    \ foo [1,2,3,4]"
    def

-- At this point the context is a bit unreadable, so to make our lives
-- easier we can write instead

contextifyNoResolve1Pretty :: IO ()
contextifyNoResolve1Pretty = do
  Right (ctx, _resolve) <- contextifyNoResolve1
  printDefModule def ctx

--------------------------------------------------------------------------------
-- Context Resolve Phase
--------------------------------------------------------------------------------

-- | Here we stop at the phase right before the context passes are run.
-- So you may want to stop here if you want to debug any of the context
-- desugar passes
--
-- Text ⟶ ML AST ⟶ LISP AST ⟶ De-sugared LISP ⟶ Contextified LISP ⟶ Resolved Contextified
contextify ::
  ByteString ->
  Options primTy primVal ->
  IO (Either Contextify.ResolveErr (Context.T Sexp.T Sexp.T Sexp.T))
contextify = contextifyGen Contextify.fullyContextify

-- | we do @contextify@ but on a file instead
-- Text ⟶ ML AST ⟶ LISP AST ⟶ De-sugared LISP ⟶ Contextified LISP ⟶ Resolved Contextified
contextifyFile ::
  FilePath ->
  Options primTy primVal ->
  IO (Either Contextify.ResolveErr (Context.T Sexp.T Sexp.T Sexp.T))
contextifyFile = contextifyFileGen Contextify.fullyContextify

--------------------------------------------------------------------------------
-- Context Resolve Phase
--------------------------------------------------------------------------------

-- | Here is where we want to stop when we want to see what the context
-- passes have done, and the final form before we run CotnexttoFrontend
-- Text ⟶ ML AST ⟶ LISP AST ⟶ De-sugared LISP ⟶ Contextified LISP ⟶ Resolved Contextified ⟶ Context Desugar
contextifyDesugar ::
  ByteString ->
  Options primTy primVal ->
  IO (Either Contextify.ResolveErr (Context.T Sexp.T Sexp.T Sexp.T))
contextifyDesugar = contextifyGen Contextify.op

-- | we do @contextifyDesugar@ but on a file instead
-- Text ⟶ ML AST ⟶ LISP AST ⟶ De-sugared LISP ⟶ Contextified LISP ⟶ Resolved Contextified ⟶ Context Desugar
contextifyDesugarFile ::
  FilePath ->
  Options primTy primVal ->
  IO (Either Contextify.ResolveErr (Context.T Sexp.T Sexp.T Sexp.T))
contextifyDesugarFile = contextifyFileGen Contextify.op

----------------------------------------
-- CONTEXTIFY Examples
----------------------------------------
contexify1 :: IO ()
contexify1 = do
  Right ctx <- contextifyDesugar "let foo = 3" defMichelson
  printDefModule defMichelson ctx

--------------------------------------------------------------------------------
-- Core Phase
--------------------------------------------------------------------------------

coreify ::
  ( Show primTy,
    Show primVal
  ) =>
  ByteString ->
  Options primTy primVal ->
  IO (Core.PatternMap Core.GlobalName, Core.RawGlobals IR.T primTy primVal)
coreify juvix options = do
  Right ctx <- contextifyDesugar juvix options
  case ToHR.contextToHR ctx (param options) of
    Left err -> do
      printCompactParens err
      error "failure at coreify"
    Right env ->
      pure . ToIR.hrToIRDefs $ env

coreifyFile ::
  ( Show primTy,
    Show primVal
  ) =>
  FilePath ->
  Options primTy primVal ->
  IO (Core.PatternMap Core.GlobalName, Core.RawGlobals IR.T primTy primVal)
coreifyFile juvix options = do
  Right ctx <- contextifyDesugarFile juvix options
  case ToHR.contextToHR ctx (param options) of
    Left err -> do
      printCompactParens err
      error "failure at coreify"
    Right env ->
      pure . ToIR.hrToIRDefs $ env

----------------------------------------
-- Coreify Examples
----------------------------------------

coreify1 :: IO ()
coreify1 = do
  x <- coreify "sig foo : int let foo = 3" defMichelson
  printCoreFunction (snd x) defMichelson "foo"

coreify2 :: IO ()
coreify2 = do
  -- Broken example that works currently
  x <- coreify "sig foo : int let foo x = x" defMichelson
  printCoreFunction (snd x) defMichelson "foo"

--------------------------------------------------------------------------------
-- Erasure Phase
--------------------------------------------------------------------------------

inline ::
  ( Show primTy,
    Show primVal,
    IR.HasPatSubstTerm (OnlyExts.T IR.T) primTy primVal primVal,
    IR.HasPatSubstTerm (OnlyExts.T IR.T) primTy primVal primTy
  ) =>
  ByteString ->
  Options primTy primVal ->
  IO
    ( Core.Term IR.T primTy primVal,
      Usage.Usage,
      Core.Term IR.T primTy primVal,
      Core.RawGlobals IR.T primTy primVal
    )
inline input options = do
  (patToSym, globalDefs) <- coreify input options
  let inlinedTerm = IR.inlineAllGlobals term lookupGlobal patToSym
      lookupGlobal = IR.rawLookupFun' globalDefs
      (usage, term, mainTy) = toLambda getMain

      getMain = case HM.elems $ HM.filter Compile.isMain globalDefs of
        [] -> error $ "No main function found in " <> toS (Pretty.pShowNoColor globalDefs)
        main : _ -> main

      toLambda main = case TransformExt.extForgetE <$> IR.toLambdaR @IR.T main of
        Just (IR.Ann usage term ty _) -> (usage, term, ty)
        _ -> error $ "Unable to convert main to lambda" <> toS (Pretty.pShowNoColor main)

  return (inlinedTerm, usage, mainTy, globalDefs)

inlineFile ::
  ( Show primTy,
    Show primVal,
    IR.HasPatSubstTerm (OnlyExts.T IR.T) primTy primVal primVal,
    IR.HasPatSubstTerm (OnlyExts.T IR.T) primTy primVal primTy
  ) =>
  FilePath ->
  Options primTy primVal ->
  IO
    ( Core.Term IR.T primTy primVal,
      Usage.Usage,
      Core.Term IR.T primTy primVal,
      Core.RawGlobals IR.T primTy primVal
    )
inlineFile fp options = do
  (patToSym, globalDefs) <- coreifyFile fp options
  let inlinedTerm = IR.inlineAllGlobals term lookupGlobal patToSym
      lookupGlobal = IR.rawLookupFun' globalDefs
      (usage, term, mainTy) = toLambda getMain

      getMain = case HM.elems $ HM.filter Compile.isMain globalDefs of
        [] -> error $ "No main function found in " <> toS (Pretty.pShowNoColor globalDefs)
        main : _ -> main

      toLambda main = case TransformExt.extForgetE <$> IR.toLambdaR @IR.T main of
        Just (IR.Ann usage term ty _) -> (usage, term, ty)
        _ -> error $ "Unable to convert main to lambda" <> toS (Pretty.pShowNoColor main)

  return (inlinedTerm, usage, mainTy, globalDefs)

erase ::
  ( ty ~ Pipeline.Ty b,
    val ~ Pipeline.Val b,
    Pipeline.Constraints b,
    Pipeline.HasBackend b
  ) =>
  ByteString ->
  Options ty val ->
  IO (ErasedAnn.AnnTermT ty val)
erase input options = do
  (patToSym, globalDefs) <- coreify input options
  feed <-
    Feedback.runFeedbackT (Pipeline.toErased (param options) (patToSym, globalDefs))
  case feed of
    Feedback.Success msg erased -> do
      printCompactParens msg
      pure erased
    Feedback.Fail failure -> do
      printCompactParens failure
      error "Failure on erase step"

eraseFile ::
  ( ty ~ Pipeline.Ty b,
    val ~ Pipeline.Val b,
    Pipeline.Constraints b,
    Pipeline.HasBackend b
  ) =>
  FilePath ->
  Options ty val ->
  IO (ErasedAnn.AnnTermT ty val)
eraseFile fp options = do
  (patToSym, globalDefs) <- coreifyFile fp options
  feed <-
    Feedback.runFeedbackT (Pipeline.toErased (param options) (patToSym, globalDefs))
  case feed of
    Feedback.Success msg erased -> do
      printCompactParens msg
      pure erased
    Feedback.Fail failure -> do
      printCompactParens failure
      error "Failure on erase step"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

printCompactParens :: (MonadIO m, Show a) => a -> m ()
printCompactParens =
  Pretty.pPrintOpt
    Pretty.CheckColorTty
    ( Pretty.defaultOutputOptionsDarkBg
        { Pretty.outputOptionsCompactParens = True,
          Pretty.outputOptionsCompact = True
        }
    )

-- | @printModule@ prints the module given to it
printModule ::
  (MonadIO m, Show ty, Show term, Show sum) => NameSymb.T -> Context.T term ty sum -> m ()
printModule name ctx =
  case Context.inNameSpace name ctx of
    Just ctx ->
      printCompactParens (Context.currentNameSpace ctx)
    Nothing ->
      pure ()

lookupCoreFunction ::
  Core.RawGlobals ext primTy1 primVal1 ->
  Options primTy2 primVal2 ->
  Symbol ->
  Maybe (Core.RawGlobal ext primTy1 primVal1)
lookupCoreFunction coreDefs option functionName =
  let name =
        currentContextName option <> NameSymb.fromSymbol functionName
   in Map.lookup name coreDefs

printCoreFunction ::
  (MonadIO m, Show primTy1, Show primVal1, Core.CoreShow ext primTy1 primVal1) =>
  Core.RawGlobals ext primTy1 primVal1 ->
  Options primTy2 primVal2 ->
  Symbol ->
  m ()
printCoreFunction core option functionName =
  lookupCoreFunction core option functionName |> printCompactParens

printTimeLapse ::
  ( ty ~ Pipeline.Ty b,
    val ~ Pipeline.Val b,
    Pipeline.Constraints b,
    Pipeline.HasBackend b
  ) =>
  ByteString ->
  Options ty val ->
  IO ()
printTimeLapse byteString option = do
  let sexpd = sexp byteString
  printCompactParens sexpd
  --
  let desugared = desugar byteString
  printCompactParens desugared
  --
  Right context <- contextifyDesugar byteString option
  printDefModule option context
  --
  let currentDefinedItems = definedFunctionsInModule option context
  --
  (_, cored) <- coreify byteString option
  traverse_ (printCoreFunction cored option) currentDefinedItems
  print "finished Cored"
  --
  (inlined, _, _, _) <- inline byteString option
  printCompactParens inlined
  print "finished Inline"
  -- --
  erased <- erase byteString option
  printCompactParens erased

printTimeLapseFile ::
  ( ty ~ Pipeline.Ty b,
    val ~ Pipeline.Val b,
    Pipeline.Constraints b,
    Pipeline.HasBackend b
  ) =>
  FilePath ->
  Options ty val ->
  IO ()
printTimeLapseFile file option = do
  sexpd <- sexpFile file
  printCompactParens sexpd
  --
  desugared <- desugarFile file
  printCompactParens desugared
  --
  Right context <- contextifyDesugarFile file option
  printDefModule option context
  --
  let currentDefinedItems = definedFunctionsInModule option context
  --
  (_, cored) <- coreifyFile file option
  traverse_ (printCoreFunction cored option) currentDefinedItems
  print "finished Cored"
  --
  (inlined, _, _, _) <- inlineFile file option
  printCompactParens inlined
  print "finished Inline"
  --
  erased <- eraseFile file option
  printCompactParens erased

printDefModule ::
  (MonadIO m, Show ty, Show term, Show sum) =>
  Options primTy primVal ->
  Context.T term ty sum ->
  m ()
printDefModule = printModule . currentContextName

ignoreHeader :: Either a (Frontend.Header topLevel) -> [topLevel]
ignoreHeader (Right (Frontend.NoHeader xs)) = xs
ignoreHeader _ = error "not no header"

definedFunctionsInModule ::
  Options primTy primVal -> Context.T term ty sumRep -> [Symbol]
definedFunctionsInModule option context =
  case Context.inNameSpace (currentContextName option) context of
    Just ctx ->
      ctx
        |> Context.currentNameSpace
        |> Context.recordContents
        |> NameSpace.toList1
        |> fmap fst
    Nothing -> []
