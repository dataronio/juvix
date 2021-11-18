module Juvix.Backends.LLVM.Pass.ClosureConversion (op) where

import qualified Juvix.Backends.LLVM.Pass.Types as Types
import qualified Juvix.Backends.LLVM.Primitive as Prim
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import Juvix.Library hiding (Map)
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Prelude as P (last)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Environment = Env
  { tyMap :: ParamaterMap,
    ofMap :: EnvOffsetMap
  }
  deriving (Show)

type ParamaterMap = HashMap.T NameSymbol.T (ErasedAnn.Type Prim.PrimTy)

type EnvOffsetMap = HashMap.T NameSymbol.T Types.IndexInto

type AnnTerm = ErasedAnn.AnnTerm Prim.PrimTy Prim.RawPrimVal

type Term = ErasedAnn.Term Prim.PrimTy Prim.RawPrimVal

type Type' = ErasedAnn.Type Prim.PrimTy

--------------------------------------------------------------------------------
-- Conversion Pass
--------------------------------------------------------------------------------

op :: AnnTerm -> Types.Annotated Types.TermClosure
op term = convert term (Env mempty mempty)

----------------------------------------
-- Term Passes
----------------------------------------

convert :: AnnTerm -> Environment -> Types.Annotated Types.TermClosure
convert (ErasedAnn.Ann {usage, type', term}) oldToNew =
  let newTerm =
        case term of
          ErasedAnn.LamM {} -> handleLambda (term, type') oldToNew
          ErasedAnn.Var nam -> handleName (ofMap oldToNew) nam
          ErasedAnn.Prim pr -> Types.Prim pr
          ErasedAnn.PairM p1 p2 ->
            Types.PairM (convert p1 oldToNew) (convert p2 oldToNew)
          ErasedAnn.CatProductIntroM p1 p2 ->
            Types.CatProductIntroM (convert p1 oldToNew) (convert p2 oldToNew)
          ErasedAnn.CatProductElimLeftM p1 p2 ->
            Types.CatProductElimLeftM (convert p1 oldToNew) (convert p2 oldToNew)
          ErasedAnn.CatProductElimRightM p1 p2 ->
            Types.CatProductElimRightM (convert p1 oldToNew) (convert p2 oldToNew)
          ErasedAnn.CatCoproductIntroLeftM p1 ->
            Types.CatCoproductIntroLeftM (convert p1 oldToNew)
          ErasedAnn.CatCoproductIntroRightM p1 ->
            Types.CatCoproductIntroRightM (convert p1 oldToNew)
          ErasedAnn.CatCoproductElimM t1 t2 t3 t4 t5 ->
            Types.CatCoproductElimM
              (convert t1 oldToNew)
              (convert t2 oldToNew)
              (convert t3 oldToNew)
              (convert t4 oldToNew)
              (convert t5 oldToNew)
          ErasedAnn.UnitM -> Types.UnitM
          ErasedAnn.AppM f xs ->
            Types.AppM (convert f oldToNew) (fmap (flip convert oldToNew) xs)
   in Types.Ann {usage, annTy = type', term = newTerm}

handleLambda :: (Term, Type') -> Environment -> Types.TermClosure
handleLambda (ErasedAnn.LamM {arguments, capture, body}, ty) old@Env {ofMap, tyMap}
  | null capture =
    Types.LamM {arguments, body = convert body newEnv}
  | otherwise =
    let closureArgs = lamArgumentToClosureArgs arguments
        closureCapt = lamMCapturesToCapture capture old
        newEnvMapping =
          foldMap argumentToEnv closureArgs
            <> foldMap captureToEnv closureCapt
            <> ofMap -- last as it gets overwritten by the other mapEnvs
     in Types.Closure
          { capture = closureCapt,
            argumentOffsets = closureArgs,
            body = convert body (newEnv {ofMap = newEnvMapping})
          }
  where
    -- Need to propagate bindings in lambda
    newEnv = Env {tyMap = typeMapFromPi arguments ty <> tyMap, ofMap}
handleLambda _ _ =
  panic "impossible"

handleName :: EnvOffsetMap -> NameSymbol.T -> Types.TermClosure
handleName ofMap name =
  case HashMap.lookup name ofMap of
    Just index -> Types.ArrayIndex index
    Nothing -> Types.Var name

-----------------------------------
-- Key Term Pass Functions
-----------------------------------

nameToCaptured :: NameSymbol.T -> EnvOffsetMap -> Types.CaptureFrom
nameToCaptured name envMapping =
  case HashMap.lookup name envMapping of
    Nothing -> Types.FromAmbientEnv name
    Just id -> Types.FromClosureEnv id

lamMCapturesToCapture :: [NameSymbol.T] -> Environment -> [Types.Capture]
lamMCapturesToCapture names Env {ofMap = envMapping, tyMap} =
  zipWith f names (enumFrom (0 :: Natural))
  where
    f name index =
      Types.Capture
        { location = nameToCaptured name envMapping,
          slot = Types.Slot {previousName = name, newIndex = Types.Index index},
          capType =
            case HashMap.lookup name tyMap of
              Just ty -> ty
              Nothing -> panic "unknown captured argument"
        }

lamArgumentToClosureArgs :: [NameSymbol.T] -> [Types.ArraySlot]
lamArgumentToClosureArgs names = zipWith f names (enumFrom (0 :: Natural))
  where
    f name idx =
      Types.Slot {previousName = name, newIndex = Types.Index idx}

---------------------------------
-- Add the new values to the Env
---------------------------------

captureToEnv :: Types.Capture -> EnvOffsetMap
captureToEnv (Types.Capture {slot}) =
  slotToEnvironment Types.ClosureEnvironment slot

argumentToEnv :: Types.ArraySlot -> EnvOffsetMap
argumentToEnv =
  slotToEnvironment Types.ArgumentEnvironemnt

slotToEnvironment :: Types.FunctionEnvironment -> Types.ArraySlot -> EnvOffsetMap
slotToEnvironment newEnvLoc slot =
  HashMap.singleton
    (Types.previousName slot)
    (Types.IndexInto {index = Types.newIndex slot, into = newEnvLoc})

----------------------------------------
-- Adding to the type map
----------------------------------------

typeMapFromPi ::
  (Hashable k, Eq k) => [k] -> Type' -> HashMap.HashMap k Type'
typeMapFromPi args ty =
  zipWith HashMap.singleton args (fst (functionType ty))
    |> HashMap.unions

-- TODO ∷ put in a utils library

-- | Construct a tuple of the types of the argument and return type of
-- a function type.
functionType ::
  ErasedAnn.Type primTy ->
  ([ErasedAnn.Type primTy], ErasedAnn.Type primTy)
functionType ty = (init tys, P.last tys)
  where
    tys = functionType' ty
    functionType' (ErasedAnn.Pi usage l r) = l : functionType' r
    functionType' ty = [ty]
