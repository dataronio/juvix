module Juvix.Backends.LLVM.Codegen.Closure
  ( register,
    reference,
    environmentPtr,
    environmentPtrDeref,
    functionPtr,
    mallocEnvironment,
    malloc,
    pointer,

    -- * Pointer Storage
    storeFunctionPtr,
    storeEnvironmentPtr,

    -- * Storage Lookup
    getFunctionPtr,
    loadFunctionPtr,
    getEnvironmentPtr,
    loadEnvironmentPtr,
  )
where

import qualified Juvix.Backends.LLVM.Codegen.Block as Block
import qualified Juvix.Backends.LLVM.Codegen.Types as Types
import Juvix.Library hiding (Type, local)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as Type

-- | @register@ registers the type with the name closure
register :: HasState "moduleDefinitions" [AST.Definition] m => m ()
register =
  Block.addType name type'

name :: IsString p => p
name = "closure"

pointer :: Type.Type
pointer = Types.pointerOf reference

-- | @reference@ the name to reference the closure type, this is what
-- one ought to use when referencing the type.
reference :: Type.Type
reference = Type.NamedTypeReference name

-- | @type'@ the base type of a closure
type' :: Type.Type
type' =
  Type.StructureType
    { isPacked = True,
      elementTypes =
        [ -- function pointer
          -- IMPORTANT Keep at the head so we can deference it with no
          -- effort
          functionPtr,
          -- the closure array
          environmentPtr
        ]
    }

sizeOf :: Num a => a
sizeOf = Types.addressSpace * 2

malloc :: Types.Call m => m AST.Operand
malloc = Block.malloc sizeOf pointer

mallocEnvironment :: Types.Call m => Integer -> m AST.Operand
mallocEnvironment length =
  -- is this the correct offset to get it to compile
  Block.malloc (Types.addressSpace * length) environmentPtr

-- We will cast this when we use it!
functionPtr :: Type.Type
functionPtr =
  Types.pointerOf Type.i8

environmentPtr :: Type.Type
environmentPtr =
  Types.pointerOf (Types.pointerOf Type.i8)

-- We want a pointer of pointers to simulate an array of pointers
environmentPtrDeref :: Type.Type
environmentPtrDeref =
  (Types.pointerOf Type.i8)

--------------------------------------------------------------------------------
-- Pointer Storage
--------------------------------------------------------------------------------

-- | @storeFunctionPtr@ takes the location of a closure and the
-- function pointer to store in it. The function is bitcasted to the
-- proper type before being stored
storeFunctionPtr :: Types.RetInstruction m => AST.Operand -> AST.Operand -> m ()
storeFunctionPtr location value = do
  fnLocation <- getFunctionPtr location
  -- cast the value for the caller
  lamBitCasted <- Block.bitCast value functionPtr
  Block.store fnLocation lamBitCasted

-- | @storeEnvironmentPtr@ takes the location of a closure and the
-- environment closure to store
storeEnvironmentPtr :: Types.RetInstruction m => AST.Operand -> AST.Operand -> m ()
storeEnvironmentPtr location value = do
  envLocation <- getEnvironmentPtr location
  Block.store envLocation value

--------------------------------------------------------------------------------
-- Environment Retrieval functions
--------------------------------------------------------------------------------

getFunctionPtr :: Types.RetInstruction m => AST.Operand -> m AST.Operand
getFunctionPtr location =
  Block.getElementPtr $
    Types.Minimal
      { type' = Types.pointerOf functionPtr,
        address' = location,
        indincies' = Block.constant32List [0, 0]
      }

loadFunctionPtr ::
  Types.RetInstruction m => AST.Operand -> Type.Type -> m AST.Operand
loadFunctionPtr location typ = do
  functionPtr <-
    Block.loadElementPtr $
      Types.Minimal
        { type' = functionPtr,
          address' = location,
          indincies' = Block.constant32List [0, 0]
        }
  Block.bitCast functionPtr typ

getEnvironmentPtr :: Types.RetInstruction m => AST.Operand -> m AST.Operand
getEnvironmentPtr location =
  Block.getElementPtr $
    Types.Minimal
      { type' = Types.pointerOf environmentPtr,
        address' = location,
        indincies' = Block.constant32List [0, 1]
      }

-- | @loadEnvironmentPtr@ Loads and Dereferences the given closure
-- environment in the given @location@
loadEnvironmentPtr :: Types.RetInstruction m => AST.Operand -> m AST.Operand
loadEnvironmentPtr location =
  Block.loadElementPtr $
    Types.Minimal
      { type' = environmentPtr,
        address' = location,
        indincies' = Block.constant32List [0, 1]
      }
