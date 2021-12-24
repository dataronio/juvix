{-# LANGUAGE ConstraintKinds #-}

module Juvix.Backends.LLVM.Codegen.Types
  ( module Juvix.Backends.LLVM.Codegen.Types,
    module Juvix.Backends.LLVM.Codegen.Types.Shared,
  )
where

import qualified Data.ByteString.Short as Short hiding (empty)
import qualified Distribution.System as System
import Juvix.Backends.LLVM.Codegen.Types.Shared
import Juvix.Backends.LLVM.Codegen.Types.CString (CString)
import qualified Juvix.Backends.LLVM.Codegen.Types.CString as CString
import qualified Juvix.Backends.LLVM.Codegen.Types.Sum as Sum
import Juvix.Library hiding (Type)
import qualified Juvix.Library.HashMap as Map
import LLVM.AST as AST
import qualified LLVM.AST.AddrSpace as AddrSpace
import LLVM.AST.DataLayout (DataLayout (..))
import Prelude ((!!))

--------------------------------------------------------------------------------
-- Codegen State
--------------------------------------------------------------------------------

data CodegenState = CodegenState
  { -- | Name of the active block to append to
    currentBlock :: Name,
    -- | Blocks for function
    blocks :: Map.T Name BlockState,
    -- | Function scope symbol table
    symTab :: SymbolTable,
    -- | Mapping from symbol to Type
    typTab :: TypeTable,
    -- | a mapping from the variants to the sum type
    varTab :: VariantToType,
    -- | Count of basic blocks
    blockCount :: Int,
    -- | Count of unnamed instructions
    count :: Word,
    -- | Name Supply
    names :: Names,
    -- | Mapping from string literal to its storing point
    strings :: StringsTable,
    -- | Module AST
    moduleAST :: AST.Module,
    -- | Debug level
    debug :: Int
  }
  deriving (Show, Generic)

data BlockState = BlockState
  { -- | Block index
    idx :: Int,
    -- | Stack of instructions
    stack :: [Named Instruction],
    -- | Block terminator
    term :: Maybe (Named Terminator)
  }
  deriving (Show, Generic)

data Errors
  = -- | Error when a block does not exist
    NoSuchBlock Text
  | -- | Error when a Variant does not exist
    NoSuchVariant Text
  | -- | Error that should never happen
    DoesNotHappen Text
  | -- | Error that happens when a variable out of scope is called
    VariableNotInScope Text
  | -- | Error that happens when a block lacks a terminator when it should have one
    BlockLackingTerminator Int
  | -- | Operation that is current unsupported
    UnsupportedOperation Text
  | -- | Operation has the wrong number of arguments
    WrongNumberOfArguments Text
  deriving (Show, Eq)

type CodegenAlias = ExceptT Errors (State CodegenState)

newtype Codegen a = CodeGen {runCodegen :: CodegenAlias a}
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState "currentBlock" Name,
      HasSink "currentBlock" Name,
      HasSource "currentBlock" Name
    )
    via StateField "currentBlock" CodegenAlias
  deriving
    ( HasState "blocks" (Map.T Name BlockState),
      HasSink "blocks" (Map.T Name BlockState),
      HasSource "blocks" (Map.T Name BlockState)
    )
    via StateField "blocks" CodegenAlias
  deriving
    ( HasState "symTab" SymbolTable,
      HasSink "symTab" SymbolTable,
      HasSource "symTab" SymbolTable
    )
    via StateField "symTab" CodegenAlias
  deriving
    ( HasState "strings" StringsTable,
      HasSink "strings" StringsTable,
      HasSource "strings" StringsTable
    )
    via StateField "strings" CodegenAlias
  deriving
    ( HasState "varTab" VariantToType,
      HasSink "varTab" VariantToType,
      HasSource "varTab" VariantToType
    )
    via StateField "varTab" CodegenAlias
  deriving
    ( HasState "typTab" TypeTable,
      HasSink "typTab" TypeTable,
      HasSource "typTab" TypeTable
    )
    via StateField "typTab" CodegenAlias
  deriving
    ( HasState "blockCount" Int,
      HasSink "blockCount" Int,
      HasSource "blockCount" Int
    )
    via StateField "blockCount" CodegenAlias
  deriving
    ( HasState "count" Word,
      HasSink "count" Word,
      HasSource "count" Word
    )
    via StateField "count" CodegenAlias
  deriving
    ( HasState "names" Names,
      HasSink "names" Names,
      HasSource "names" Names
    )
    via StateField "names" CodegenAlias
  deriving
    ( HasState "moduleAST" AST.Module,
      HasSink "moduleAST" AST.Module,
      HasSource "moduleAST" AST.Module
    )
    via StateField "moduleAST" CodegenAlias
  deriving
    ( HasReader "debug" Int,
      HasSource "debug" Int
    )
    via ReaderField "debug" CodegenAlias
  deriving (HasThrow "err" Errors) via MonadError CodegenAlias

instance HasState "moduleDefinitions" [Definition] Codegen where
  state_ _ state = do
    c <- get @"moduleDefinitions"
    let (a, res) = state c
    put @"moduleDefinitions" res
    pure a

instance HasSource "moduleDefinitions" [Definition] Codegen where
  await_ _ = moduleDefinitions <$> (get @"moduleAST")

instance HasSink "moduleDefinitions" [Definition] Codegen where
  yield_ _ x = do
    c <- get @"moduleAST"
    put @"moduleAST" (c {moduleDefinitions = x})

-- TODO ∷ see if this is still useful
newtype LLVM a = LLVM {runLLVM :: State AST.Module a}
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState "moduleName" Short.ShortByteString,
      HasSink "moduleName" Short.ShortByteString,
      HasSource "moduleName" Short.ShortByteString
    )
    via StateField "moduleName" (State AST.Module)
  deriving
    ( HasState "moduleSourceFileName" Short.ShortByteString,
      HasSink "moduleSourceFileName" Short.ShortByteString,
      HasSource "moduleSourceFileName" Short.ShortByteString
    )
    via StateField "moduleSourceFileName" (State AST.Module)
  deriving
    ( HasState "moduleDataLayout" (Maybe LLVM.AST.DataLayout.DataLayout),
      HasSink "moduleDataLayout" (Maybe LLVM.AST.DataLayout.DataLayout),
      HasSource "moduleDataLayout" (Maybe LLVM.AST.DataLayout.DataLayout)
    )
    via StateField "moduleDataLayout" (State AST.Module)
  deriving
    ( HasState "moduleTargetTriple" (Maybe Short.ShortByteString),
      HasSink "moduleTargetTriple" (Maybe Short.ShortByteString),
      HasSource "moduleTargetTriple" (Maybe Short.ShortByteString)
    )
    via StateField "moduleTargetTriple" (State AST.Module)
  deriving
    ( HasState "moduleDefinitions" [Definition],
      HasSink "moduleDefinitions" [Definition],
      HasSource "moduleDefinitions" [Definition]
    )
    via StateField "moduleDefinitions" (State AST.Module)

--------------------------------------------------------------------------------
-- Effect Aliases
--------------------------------------------------------------------------------

type Instruct m =
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.T Name BlockState) m,
    HasState "currentBlock" Name m,
    HasState "strings" StringsTable m
  )

type RetInstruction m =
  ( HasState "count" Word m,
    Instruct m
  )

type MallocSum m =
  ( RetInstruction m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m,
    HasState "symTab" SymbolTable m
  )

type NewBlock m =
  ( HasState "blockCount" Int m,
    HasState "blocks" (Map.T Name BlockState) m,
    HasState "names" Names m
  )

type AllocaSum m =
  ( RetInstruction m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m
  )

type Define m =
  ( RetInstruction m,
    Externf m,
    HasState "blockCount" Int m,
    HasState "moduleDefinitions" [Definition] m,
    HasState "names" Names m
  )

type External m =
  ( HasState "moduleDefinitions" [Definition] m,
    HasState "symTab" SymbolTable m
  )

type Externf m =
  ( HasState "symTab" SymbolTable m,
    HasThrow "err" Errors m
  )

type Call m =
  ( RetInstruction m,
    HasState "symTab" SymbolTable m
  )

type Debug m = HasReader "debug" Int m

--------------------------------------------------------------------------------
-- Haskell Types
--------------------------------------------------------------------------------

data MinimalPtr = Minimal
  { address' :: Operand,
    indincies' :: [Operand],
    type' :: Type
  }
  deriving (Show)

--------------------------------------------------------------------------------
-- LLVM Type Operations
--------------------------------------------------------------------------------

-- TODO :: Replace with safe lens call instead!
intoStructTypeErr :: Integral a => Type -> a -> Type
intoStructTypeErr typ' i = elementTypes typ' !! fromIntegral i

--------------------------------------------------------------------------------
-- System Architecture
--------------------------------------------------------------------------------

-- for all architectures we will want to know the addressing space
-- for architectures that are sufficiently big, we will want to really cut corners
-- with indirection.
-- In reality, it seems most architectures we should care about are 32 bit or higher
-- however, on the off chance, we are forced into an 8 bit architecture, we have variants
-- for port numbers, so we can support large number of arguments (is this even useful on small archs?)

addressSpace :: Num p => p
addressSpace =
  case System.buildArch of
    System.X86_64 -> 64
    System.I386 -> 32
    System.Mips -> 32
    System.PPC -> 32
    System.PPC64 -> 64
    System.Sparc -> 64
    System.Arm -> 32
    System.AArch64 -> 64
    -- has 16 bit instructions
    System.SH -> 32
    System.IA64 -> 64
    -- These have 24/31 bit addressing
    -- may be more apt to return 24/31?
    System.S390 -> 32
    System.Alpha -> 64
    -- seems to be 64?
    System.Hppa -> 64
    -- seems to be PowerPC architecture!?!??
    System.Rs6000 -> 32
    -- may have to lower to 24?
    System.M68k -> 32
    System.Vax -> 32
    -- 32 I guess?
    System.JavaScript -> 32
    -- otherwise assume it's a 32 bit architecture
    System.OtherArch _ -> 32

-- | 'bitSizeEncodingPoint' is used to determine if we need a layer of indirection
-- around all our number types to have a bigger argument list
bitSizeEncodingPoint :: Bool
bitSizeEncodingPoint = addressSpace >= (17 :: Int)

debugLevelOne :: HasReader "debug" Int m => m () -> m ()
debugLevelOne = whenM ((1 <=) <$> ask @"debug")

--------------------------------------------------------------------------------
-- LLVM Types
--------------------------------------------------------------------------------

-- | 'variantToType' takes the type out of the variant
variantToType :: Sum.VariantInfo -> Type
variantToType = Sum.typ'

pointerOf :: Type -> Type
pointerOf typ = PointerType typ (AddrSpace.AddrSpace 0)

pointerSizeInt :: Num p => p
pointerSizeInt = addressSpace

pointerSize :: Type
pointerSize = IntegerType addressSpace

voidStarTy :: Type
voidStarTy = pointerOf VoidType

voidTy :: Type
voidTy = VoidType

constStringTy :: CString -> Type
constStringTy = constStringTy' . fromIntegral . CString.length

constStringTy' :: Word64 -> Type
constStringTy' strLen = ArrayType strLen charTy

int8Ty :: Type
int8Ty = IntegerType 8

charTy :: Type
charTy = int8Ty

size_t :: Type
size_t = IntegerType addressSpace

size_t_int :: Num p => p
size_t_int = addressSpace
